#nullable enable
using ChessLibrary;

namespace WebGUI.Services
{
    /// <summary>
    /// Common abstraction over a source of tournament <see cref="TournamentTypes.Update"/> events.
    /// Implemented by <see cref="TournamentService"/> (internal engine runner) and
    /// <see cref="JsonFeedService"/> (external JSON feed, see LiveFeedContract.md), so the live
    /// visualization can render from either source through a single render path.
    /// </summary>
    public interface IUpdateFeed
    {
        /// <summary>True while a tournament/feed is active (between StartOfTournament and EndOfTournament).</summary>
        bool IsRunning { get; }

        /// <summary>Register the handler that receives every <see cref="TournamentTypes.Update"/>. Replaces any prior handler.</summary>
        void Subscribe(Action<TournamentTypes.Update> handler);

        /// <summary>Remove the current handler.</summary>
        void Unsubscribe();
    }
}
