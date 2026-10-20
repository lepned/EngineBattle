#nullable enable
using ChessLibrary;
using Microsoft.FSharp.Core;

namespace WebGUI.Services;

/// <summary>
/// The Lichess bot of the app (ChessLibrary.LichessBot): one at a time, running in the background while the
/// app is open - leaving the page does not stop it. The page starts and stops it and shows its log and games.
/// </summary>
public sealed class LichessBotService : IDisposable
{
    private readonly ILogger<LichessBotService> _logger;
    private readonly object _lock = new();
    private readonly List<string> _log = new();
    private CancellationTokenSource? _cts;
    private Task? _run;
    private LichessBot? _bot;
    private LichessBotClient? _client;

    public LichessBotService(ILogger<LichessBotService> logger) => _logger = logger;

    /// <summary>Raised on every log line and when the bot starts or stops.</summary>
    public event Action? Changed;

    public bool IsRunning => _run is { IsCompleted: false };
    /// <summary>Running and past the engine's start: taking challenges.</summary>
    public bool IsOnline => IsRunning && (_bot?.IsOnline ?? false);
    /// <summary>Asked to go offline and not there yet (the engine is quitting, the games ending).</summary>
    public bool IsStopping => IsRunning && (_cts?.IsCancellationRequested ?? false);
    public string? LastError { get; private set; }

    public string[] Log { get { lock (_lock) return _log.ToArray(); } }
    public LichessBotGame[] Games => _bot?.Games ?? Array.Empty<LichessBotGame>();
    /// <summary>Challenges the bot sent that have no answer yet.</summary>
    public OutgoingChallenge[] Outgoing => _bot?.Outgoing ?? Array.Empty<OutgoingChallenge>();
    /// <summary>What became of the last challenge the bot sent.</summary>
    public string LastChallengeAnswer => _bot?.LastChallengeAnswer ?? "";

    /// <summary>Challenges a player to a standard game; null when it was sent, else why not.</summary>
    public async Task<string?> Challenge(string username, int seconds, int increment, bool rated, string color)
    {
        if (_bot == null || _cts == null || !IsOnline) return "The bot is not online.";
        var result = await _bot.Challenge(username, seconds, increment, rated, color, _cts.Token);
        Changed?.Invoke();
        return result.IsError ? result.ErrorValue : null;
    }

    /// <summary>The bots online on Lichess now, strongest in blitz first; an error when it cannot be read.</summary>
    public async Task<(LichessBotLogic.OnlineBot[] Bots, string? Error)> OnlineBots(int count = 1000)
    {
        if (_bot == null || _cts == null || !IsOnline) return (Array.Empty<LichessBotLogic.OnlineBot>(), "The bot is not online.");
        var result = await _bot.OnlineBots(count, _cts.Token);
        if (result.IsError) return (Array.Empty<LichessBotLogic.OnlineBot>(), result.ErrorValue);
        var bots = result.ResultValue.OrderByDescending(b => b.Blitz == null ? 0 : b.Blitz.Value).ToArray();
        return (bots, null);
    }

    /// <summary>Withdraws a challenge the bot sent.</summary>
    public async Task CancelChallenge(string challengeId)
    {
        if (_bot == null) return;
        await _bot.CancelChallenge(challengeId, CancellationToken.None);
        Changed?.Invoke();
    }

    private void Add(string line)
    {
        lock (_lock)
        {
            _log.Add($"{DateTime.Now:HH:mm:ss}  {line}");
            if (_log.Count > 300) _log.RemoveRange(0, _log.Count - 300);
        }
        Changed?.Invoke();
    }

    /// <summary>Goes online. False with a reason when it is running already.</summary>
    public bool Start(string token, LichessBotSettings settings, string server = "https://lichess.org")
    {
        if (IsRunning) return false;
        LastError = null;
        (_client as IDisposable)?.Dispose();
        _client = new LichessBotClient(server, token);
        _cts = new CancellationTokenSource();
        _bot = new LichessBot(_client, settings, _logger, FuncConvert.FromAction<string>(Add));
        var ct = _cts.Token;
        _run = Task.Run(async () =>
        {
            try
            {
                var result = await _bot.Run(ct);
                if (result.IsError) { LastError = result.ErrorValue; Add(result.ErrorValue); }
            }
            catch (Exception ex)
            {
                LastError = ex.Message;
                _logger.LogError(ex, "Lichess bot stopped");
                Add(ex.Message);
            }
            finally { Changed?.Invoke(); }
        });
        Changed?.Invoke();
        return true;
    }

    /// <summary>Goes offline: the games in play end with it (Lichess counts them as abandoned).</summary>
    public async Task Stop()
    {
        if (_cts == null || _run == null) return;
        _cts.Cancel();
        Changed?.Invoke();
        // the page shows "going offline" until the run has ended (Changed fires again then)
        try { await _run.WaitAsync(TimeSpan.FromSeconds(30)); } catch { }
        Changed?.Invoke();
    }

    /// <summary>The token's account: (name, is a bot account), or an error.</summary>
    public async Task<(string Name, bool IsBot, string? Error)> Account(string token, string server = "https://lichess.org")
    {
        using var client = new LichessBotClient(server, token);
        var result = await client.Account(CancellationToken.None);
        if (result.IsError) return ("", false, result.ErrorValue);
        var (_, name, title) = result.ResultValue;
        return (name, title == "BOT", null);
    }

    /// <summary>Turns the token's account into a bot account, for good. Null when it worked.</summary>
    public async Task<string?> Upgrade(string token, string server = "https://lichess.org")
    {
        using var client = new LichessBotClient(server, token);
        var reply = await client.UpgradeToBot(CancellationToken.None);
        return reply switch
        {
            LichessReply.Done => null,
            LichessReply.Refused r => $"Lichess refused the upgrade (HTTP {r.status}): {r.body}",
            _ => "Lichess asks for a pause: try again in a minute",
        };
    }

    public void Dispose()
    {
        _cts?.Cancel();
        (_client as IDisposable)?.Dispose();
    }
}
