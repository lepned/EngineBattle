#nullable enable
using System;
using System.IO;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace WebGUI.Services.Ceres
{
    /// <summary>
    /// A single CELT/1.0 TCP subscription to a Ceres tournament stream publisher. On connect the
    /// server greets with a <c>hello</c> line; this client then sends one <c>subscribe</c> request
    /// (scope "global" for tournament meta/results, or "thread"+threadId for one game's live feed)
    /// and forwards every subsequent NDJSON line to <paramref name="onLine"/>. Reconnects with a
    /// fixed backoff until disposed. The mapping/ingest of lines is the caller's concern.
    /// </summary>
    public sealed class CeresStreamClient : IAsyncDisposable
    {
        private readonly string _host;
        private readonly int _port;
        private readonly string _scope;     // "global" | "thread"
        private readonly int _threadId;     // -1 for global
        private readonly Action<string> _onLine;
        private readonly Action<string>? _onStatus;

        private CancellationTokenSource? _cts;
        private Task? _loop;

        private static readonly TimeSpan ReconnectDelay = TimeSpan.FromSeconds(2);

        public CeresStreamClient(string host, int port, string scope, int threadId,
                                 Action<string> onLine, Action<string>? onStatus = null)
        {
            _host = host;
            _port = port;
            _scope = scope;
            _threadId = threadId;
            _onLine = onLine;
            _onStatus = onStatus;
        }

        public void Start()
        {
            _cts = new CancellationTokenSource();
            _loop = Task.Run(() => RunAsync(_cts.Token));
        }

        private async Task RunAsync(CancellationToken ct)
        {
            while (!ct.IsCancellationRequested)
            {
                try
                {
                    using var client = new TcpClient { NoDelay = true };
                    await client.ConnectAsync(_host, _port, ct);
                    using var stream = client.GetStream();
                    using var reader = new StreamReader(stream, Encoding.UTF8);

                    // Server greets with "hello" then reads exactly one request line; send subscribe.
                    var sub = $"{{\"type\":\"subscribe\",\"scope\":\"{_scope}\",\"threadId\":{_threadId},\"protocol\":\"celt/1.0\"}}\n";
                    var bytes = Encoding.UTF8.GetBytes(sub);
                    await stream.WriteAsync(bytes, ct);
                    await stream.FlushAsync(ct);

                    _onStatus?.Invoke("connected");

                    string? line;
                    while ((line = await reader.ReadLineAsync(ct)) != null)
                    {
                        if (line.Length > 0)
                        {
                            try { _onLine(line); }
                            catch { /* mapping/ingest error on one line must not kill the stream */ }
                        }
                    }
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
