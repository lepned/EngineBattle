// Tournaments: comparing what the two engines are thinking.
//
// Both engines publish a principal variation while they search, and the interesting question
// during a game is how far the two agree before they part company. That number drives the two
// PV boards under the main board - they follow the shared line and stop at the first move the
// engines disagree on - and it is what the deviation markers in the PV text are counted from.
//
// The comparison is on SAN, not on the raw engine output: one engine's UCI move and the other's
// are only comparable once both have been played onto the same board.

using ChessLibrary;
using Microsoft.JSInterop;
using MudBlazor;
using WebGUI.Services;
using static ChessLibrary.EngineTypes;
using static ChessLibrary.LayoutTypes;
using static ChessLibrary.MiscTypes;
using static ChessLibrary.PGNTypes;
using static ChessLibrary.TypesDef;
using static ChessLibrary.TypesDef.CoreTypes;

namespace WebGUI.Components.Pages.TournamentPages;

public partial class Tournaments
{
	private async Task UpdatePVBoards(string longPV, int depthToMove, int depthOpp, bool white, string newFen)
	{
		try
		{
			var moves = boardState.GetPVMoves(longPV, depthToMove, false).ToList();
			var moveFen = await boardState.PlayPVMoves(moves, newFen);
			if (white)
			{
				WhiteMoveAndFen = moveFen;
			}
			else
			{
				BlackMoveAndFen = moveFen;
			}
		}
		catch (Exception e)
		{
			logger.LogDebug($"Logging exception data: longPV: {longPV} depthToMove: {depthToMove} depthOpp: {depthOpp} white: {white} newFen: {newFen}");
			logger.LogError(e.Message);
		}
	}


	private int CountAgreements(string[] wArr, string[] bArr)
	{
		var shortest = Math.Min(wArr.Length, bArr.Length);
		int equalCount = 0;
		for (int i = 0; i < shortest; i++)
		{
			if (wArr[i].Equals(bArr[i]))
			{
				equalCount++;
			}

			else
				return equalCount;
		}
		return equalCount;
	}

	private async Task<int> CalcPVAgreement(bool whiteToPlay)
	{
		var newFen = fen;
		if (string.IsNullOrEmpty(whiteLongPV) || string.IsNullOrEmpty(blackLongPV))
		{
			if (string.IsNullOrEmpty(whiteLongPV) == false && whiteToMove)
			{
				whitePVda = string.Empty; whiteBefore = string.Empty;
				whiteRest = whitePV;
				if (showPVBoard)
					await UpdatePVBoards(whiteLongPV, 1, 0, whiteToPlay, newFen);
				return 0;
			}
			if (string.IsNullOrEmpty(blackLongPV) == false && whiteToMove == false)
			{
				blackPVda = String.Empty; blackBefore = string.Empty;
				blackRest = blackPV;
				if (showPVBoard)
					await UpdatePVBoards(blackLongPV, 1, 0, whiteToPlay, newFen);
				return 0;
			}
			if (showPVBoard)
			{
				if (whiteToPlay)
				{
					await UpdatePVBoards(whiteLongPV, 1, 0, whiteToPlay, newFen);
				}
				else
				{
					await UpdatePVBoards(blackLongPV, 1, 0, whiteToPlay, newFen);
				}
				return 0;
			}
		}

		var comparePV = whiteToPlay ? blackLongPV : whiteLongPV;
		var toMovePV = whiteToPlay ? whiteLongPV : blackLongPV;
		var toCompareArr = comparePV.Split(' ')[1..];
		var toMoveArr = toMovePV.Split(' ');
		int numberOfMovesAgreement = CountAgreements(toMoveArr, toCompareArr);
		var wArr = whitePV.Split(' ');
		var bArr = blackPV.Split(' ');
		var blackArr = bArr.Length > 1 ? bArr[1..] : Array.Empty<string>();
		var res = numberOfMovesAgreement;
		if (whiteToPlay)
		{
			var blackArrCompare = blackArr.Length > 1 ? blackArr[1..] : Array.Empty<string>();
			whiteBefore = String.Join(" ", wArr.Take(res));
			whitePVda = wArr.Length > res ? wArr[res] : "";
			whiteRest = String.Join(" ", wArr.Skip(res + 1));

			int blackTakeCount = Math.Min(bArr.Length, res + 2);
			blackBefore = String.Join(" ", bArr.Take(blackTakeCount));
			blackPVda = blackArrCompare.Length > res ? blackArrCompare[res] : "";
			blackRest = String.Join(" ", blackArrCompare.Skip(res + 1));
		}

		else
		{
			var whiteArr = wArr.Length > 1 ? wArr[1..] : Array.Empty<string>();
			res = CountAgreements(whiteArr, blackArr);
			int blackTakeCount = Math.Min(bArr.Length, res + 1);
			blackBefore = String.Join(" ", bArr.Take(blackTakeCount));
			blackPVda = blackArr.Length > res ? blackArr[res] : "";
			blackRest = String.Join(" ", blackArr.Skip(res + 1));

			int whiteTakeCount = Math.Min(wArr.Length, res + 1);
			whiteBefore = String.Join(" ", wArr.Take(whiteTakeCount));
			whitePVda = whiteArr.Length > res ? whiteArr[res] : "";
			whiteRest = String.Join(" ", whiteArr.Skip(res + 1));
		}

		if (showPVBoard)
		{
			var longPV = whiteToPlay ? whiteLongPV : blackLongPV;
			await UpdatePVBoards(longPV, numberOfMovesAgreement + 1, numberOfMovesAgreement + 2, whiteToPlay, newFen);
		}

		return numberOfMovesAgreement;
	}
}
