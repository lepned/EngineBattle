#nullable enable
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace WebGUI.Services.Broadcast
{
    /// <summary>
    /// Streams a lichess broadcast round (GET /api/stream/broadcast/round/{id}.pgn).
    /// The endpoint sends the current PGN of every game on connect, then re-sends a
    /// game's full PGN whenever it updates. This client frames the line stream into
    /// complete single-game PGN chunks and forwards each to <c>onGamePgn</c>.
    /// Reconnects with a fixed backoff until disposed (a reconnect replays all games,
    /// which the consumer's diffing absorbs). Modeled on CeresStreamClient.
    /// </summary>
    public sealed class LichessBroadcastClient : IAsyncDisposable
    {
        private readonly string _roundId;
        private readonly Action<string> _onGamePgn;
        private readonly Action<string>? _onStatus;

        private CancellationTokenSource? _cts;
        private Task? _loop;

        private static readonly TimeSpan ReconnectDelay = TimeSpan.FromSeconds(5);

        private static readonly HttpClient Http = CreateHttpClient();

        private static HttpClient CreateHttpClient()
        {
            var client = new HttpClient { Timeout = Timeout.InfiniteTimeSpan };
            client.DefaultRequestHeaders.UserAgent.ParseAdd("EngineBattle/1.0 (broadcast viewer)");
            return client;
        }

        public LichessBroadcastClient(string roundId, Action<string> onGamePgn, Action<string>? onStatus = null)
        {
            _roundId = roundId;
            _onGamePgn = onGamePgn;
            _onStatus = onStatus;
        }

        public void Start()
        {
            _cts = new CancellationTokenSource();
            _loop = Task.Run(() => RunAsync(_cts.Token));
        }

        private async Task RunAsync(CancellationToken ct)
        {
            var url = $"https://lichess.org/api/stream/broadcast/round/{_roundId}.pgn";
            while (!ct.IsCancellationRequested)
            {
                try
                {
                    using var response = await Http.GetAsync(url, HttpCompletionOption.ResponseHeadersRead, ct);
                    response.EnsureSuccessStatusCode();
                    using var stream = await response.Content.ReadAsStreamAsync(ct);
                    using var reader = new StreamReader(stream);

                    _onStatus?.Invoke("connected");

                    var buffer = new List<string>();
                    bool bufferHasMovetext = false;

                    void Flush()
                    {
                        if (buffer.Count > 0 && buffer.Any(l => l.StartsWith("[Event ", StringComparison.Ordinal)))
                        {
                            var chunk = string.Join("\n", buffer);
                            try { _onGamePgn(chunk); }
                            catch { /* one bad game must not kill the stream */ }
                        }
                        buffer.Clear();
                        bufferHasMovetext = false;
                    }

                    string? line;
                    while ((line = await reader.ReadLineAsync(ct)) != null)
                    {
                        // A new tag section while we already collected movetext = next game (safety net;
                        // normally the result token below flushes first).
                        if (bufferHasMovetext && line.StartsWith("[Event ", StringComparison.Ordinal))
                            Flush();

                        buffer.Add(line);

                        var trimmed = line.Trim();
                        if (trimmed.Length > 0 && !trimmed.StartsWith('['))
                        {
                            bufferHasMovetext = true;
                            // A game's movetext ends with a result token ("*" while ongoing).
                            if (EndsWithResultToken(trimmed))
                                Flush();
                        }
                    }
                    _onStatus?.Invoke("stream ended");
                }
                catch (OperationCanceledException)
                {
                    break;
                }
                catch (Exception ex)
                {
                    _onStatus?.Invoke($"disconnected: {ex.Message}");
                }

                try { await Task.Delay(ReconnectDelay, ct); }
                catch (OperationCanceledException) { break; }
            }
        }

        private static bool EndsWithResultToken(string trimmedLine)
        {
            var lastSpace = trimmedLine.LastIndexOf(' ');
            var lastToken = lastSpace < 0 ? trimmedLine : trimmedLine[(lastSpace + 1)..];
            return lastToken is "1-0" or "0-1" or "1/2-1/2" or "*";
        }

        public async ValueTask DisposeAsync()
        {
            try { _cts?.Cancel(); } catch { }
            if (_loop != null)
            {
                try { await _loop; } catch { /* ignore */ }
            }
            _cts?.Dispose();
        }
    }
}
