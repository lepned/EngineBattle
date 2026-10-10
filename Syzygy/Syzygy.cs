/*
Syzygy tablebase probing: a C# port of Fathom (tbprobe.c, tbprobe.h, tbchess.c, tbconfig.h),
https://github.com/jdart1/Fathom

Copyright (c) 2013-2018 Ronald de Man
Copyright (c) 2015 basil00
Modifications Copyright (c) 2016-2024 by Jon Dart

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

Port notes:
 - WDL and DTZ probing only (Fathom's DTM code is not ported). Up to 7 pieces, like Fathom with TB_PIECES 7.
 - Table files are memory-mapped read-only (System.IO.MemoryMappedFiles) on first use; lazy initialisation uses
   double-checked locking (Volatile ready flags + a lock), as Fathom does with its mutex. Probing is thread-safe.
 - Init/Free never unmap a table that has been mapped: the old entries (with their views, mapping objects and file
   streams) are moved to a 'retired' list that lives for the whole process, and the material hash is replaced by a
   new immutable one (published with Volatile.Write), so a thread still probing the previous generation only ever
   sees valid memory. The cost is a small leak (mapped address space and open file handles) per path change.
 - Robustness: the public probes validate the position (one king per side, side not to move not in check, sane ep
   square, no castling rights) before any table access, since the raw attack tables are indexed with lsb() of the
   king bitboards, and catch any exception (a corrupt table) as a failed probe. Table setup checks every pointer
   against the mapped file length, symbol ids against numSyms and the piece lists against the file name; a table
   that fails to load is flagged as failed per type (WDL / DTZ independently) and is not retried.
 - The chess helpers of tbchess.c are ported with Fathom's conventions (a1 = 0, turn true = white, ep 0 = none),
   using the engine's attack tables (Fathom's TB_*_ATTACKS hooks).
 - The tbScore expression in root_probe_dtz reproduces Fathom's unparenthesised max/min macros exactly, so that
   scores match the reference build.

EngineBattle: the public API takes a FEN and gives moves in UCI, the attack tables are the portable ones in
Attacks.cs (no BMI2), Init also splits on the platform's path separator (':' on Linux and macOS, as Fathom
there), and Init keeps TB_LARGEST until the new tables are published instead of zeroing it first.
*/

using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.IO;
using System.IO.MemoryMappedFiles;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Threading;

namespace EngineBattle.Tablebases;

/// <summary>WDL from the side to move's point of view (values as Fathom's TB_LOSS..TB_WIN).</summary>
public enum TbWdl { Failed = -1, Loss = 0, BlessedLoss = 1, Draw = 2, CursedWin = 3, Win = 4 }

/// <summary>Per-move result of the original Fathom root probe (tb_probe_root).</summary>
public sealed class TbMoveResult
{
    public string Uci;    // the move in UCI (e7e8q)
    public TbWdl Wdl;     // WDL after the move, from the root side's point of view, taking rule50 into account
    public int Dtz;       // |dtz| counted from the root position
}

/// <summary>What tb_probe_root says about a position: Fathom's [WDL], [DTZ] and move lists.</summary>
public sealed class TbRootResult
{
    public bool Ok;             // false: probe failed (missing table, castling rights, too many pieces, invalid position)
    public bool Checkmate, Stalemate;
    public string BestMove;     // UCI; null for mate/stalemate or a failed probe
    public TbWdl Wdl = TbWdl.Failed;  // value of the root position (rule50 taken into account); Loss when checkmated
    public int Dtz;             // |dtz| of the root position
    public TbMoveResult[] Moves = Array.Empty<TbMoveResult>();   // every legal move, in generation order
}

public static unsafe class Syzygy
{
    // ---------------------------------------------------------------- constants (tbprobe.h / tbconfig.h)
    const int TB_PIECES = 7;
    const int TB_HASHBITS = TB_PIECES < 7 ? 11 : 12;
    public const int MaxMoves = 192 + 1;      // TB_MAX_MOVES
    const int MaxCaptures = 64;               // TB_MAX_CAPTURES

    public const int TB_PROMOTES_NONE = 0, TB_PROMOTES_QUEEN = 1, TB_PROMOTES_ROOK = 2, TB_PROMOTES_BISHOP = 3, TB_PROMOTES_KNIGHT = 4;

    public const int TB_VALUE_PAWN = 100, TB_VALUE_MATE = 32000, TB_VALUE_INFINITE = 32767, TB_VALUE_DRAW = 0, TB_MAX_MATE_PLY = 255;

    const int WDL = 0, DTM = 1, DTZ = 2;
    const int PIECE_ENC = 0, FILE_ENC = 1, RANK_ENC = 2;

    static readonly string[] tbSuffix = { ".rtbw", ".rtbm", ".rtbz" };
    static readonly uint[] tbMagic = { 0x5d23e871, 0x88ac504b, 0xa50c66d7 };

    // ---------------------------------------------------------------- chess helpers (tbchess.c)
    const int PAWN = 1, KNIGHT = 2, BISHOP = 3, ROOK = 4, QUEEN = 5, KING = 6;
    const int W_PAWN = 1, W_KNIGHT = 2, W_BISHOP = 3, W_ROOK = 4, W_QUEEN = 5, W_KING = 6;
    const int B_PAWN = 9, B_KNIGHT = 10, B_BISHOP = 11, B_ROOK = 12, B_QUEEN = 13, B_KING = 14;

    const ulong PRIME_WHITE_QUEEN = 11811845319353239651UL;
    const ulong PRIME_WHITE_ROOK = 10979190538029446137UL;
    const ulong PRIME_WHITE_BISHOP = 12311744257139811149UL;
    const ulong PRIME_WHITE_KNIGHT = 15202887380319082783UL;
    const ulong PRIME_WHITE_PAWN = 17008651141875982339UL;
    const ulong PRIME_BLACK_QUEEN = 15484752644942473553UL;
    const ulong PRIME_BLACK_ROOK = 18264461213049635989UL;
    const ulong PRIME_BLACK_BISHOP = 15394650811035483107UL;
    const ulong PRIME_BLACK_KNIGHT = 13469005675588064321UL;
    const ulong PRIME_BLACK_PAWN = 11695583624105689831UL;

    const ulong BOARD_RANK_EDGE = 0x8181818181818181UL;
    const ulong BOARD_FILE_EDGE = 0xFF000000000000FFUL;

    const int BEST_NONE = 0xFFFF;
    const int SCORE_ILLEGAL = 0x7FFF;
    const int MOVE_STALEMATE = 0xFFFF;
    const int MOVE_CHECKMATE = 0xFFFE;

    const string piece_to_char = " PNBRQK  pnbrqk";

    struct Pos
    {
        public ulong white, black, kings, queens, rooks, bishops, knights, pawns;
        public byte rule50, ep;
        public bool turn;   // true = white to move
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)] static int popcount(ulong x) => BitOperations.PopCount(x);
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static int lsb(ulong x) => BitOperations.TrailingZeroCount(x);
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static ulong poplsb(ulong x) => x & (x - 1);
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static ulong board(int s) => 1UL << s;
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static int rank(int s) => s >> 3;
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static int file(int s) => s & 7;

    [MethodImpl(MethodImplOptions.AggressiveInlining)] static ulong king_attacks(int s) => Attacks.KingAtt[s];
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static ulong knight_attacks(int s) => Attacks.KnightAtt[s];
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static ulong bishop_attacks(int s, ulong occ) => Attacks.Bishop(s, occ);
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static ulong rook_attacks(int s, ulong occ) => Attacks.Rook(s, occ);
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static ulong queen_attacks(int s, ulong occ) => Attacks.Rook(s, occ) | Attacks.Bishop(s, occ);
    // Fathom: pawn_attacks(sq, color) with color true = white (attacks towards rank 8)
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static ulong pawn_attacks(int s, bool white) => Attacks.PawnAtt[(white ? 0 : 1) * 64 + s];

    static bool ColorOfPiece(int piece) => (piece >> 3) == 0;   // true = white
    static int TypeOfPiece(int piece) => piece & 7;

    static ulong pieces_by_type(Pos* pos, bool white, int p)
    {
        ulong mask = white ? pos->white : pos->black;
        switch (p)
        {
            case PAWN: return pos->pawns & mask;
            case KNIGHT: return pos->knights & mask;
            case BISHOP: return pos->bishops & mask;
            case ROOK: return pos->rooks & mask;
            case QUEEN: return pos->queens & mask;
            case KING: return pos->kings & mask;
            default: return 0;
        }
    }

    static int char_to_piece_type(char c)
    {
        for (int i = PAWN; i <= KING; i++)
            if (c == piece_to_char[i]) return i;
        return 0;
    }

    static ulong calc_key(Pos* pos, bool mirror)
    {
        ulong white = pos->white, black = pos->black;
        if (mirror) { ulong tmp = white; white = black; black = tmp; }
        return (ulong)popcount(white & pos->queens) * PRIME_WHITE_QUEEN +
               (ulong)popcount(white & pos->rooks) * PRIME_WHITE_ROOK +
               (ulong)popcount(white & pos->bishops) * PRIME_WHITE_BISHOP +
               (ulong)popcount(white & pos->knights) * PRIME_WHITE_KNIGHT +
               (ulong)popcount(white & pos->pawns) * PRIME_WHITE_PAWN +
               (ulong)popcount(black & pos->queens) * PRIME_BLACK_QUEEN +
               (ulong)popcount(black & pos->rooks) * PRIME_BLACK_ROOK +
               (ulong)popcount(black & pos->bishops) * PRIME_BLACK_BISHOP +
               (ulong)popcount(black & pos->knights) * PRIME_BLACK_KNIGHT +
               (ulong)popcount(black & pos->pawns) * PRIME_BLACK_PAWN;
    }

    static ulong calc_key_from_pcs(int[] pcs, bool mirror)
    {
        int m = mirror ? 8 : 0;
        return (ulong)pcs[W_QUEEN ^ m] * PRIME_WHITE_QUEEN +
               (ulong)pcs[W_ROOK ^ m] * PRIME_WHITE_ROOK +
               (ulong)pcs[W_BISHOP ^ m] * PRIME_WHITE_BISHOP +
               (ulong)pcs[W_KNIGHT ^ m] * PRIME_WHITE_KNIGHT +
               (ulong)pcs[W_PAWN ^ m] * PRIME_WHITE_PAWN +
               (ulong)pcs[B_QUEEN ^ m] * PRIME_BLACK_QUEEN +
               (ulong)pcs[B_ROOK ^ m] * PRIME_BLACK_ROOK +
               (ulong)pcs[B_BISHOP ^ m] * PRIME_BLACK_BISHOP +
               (ulong)pcs[B_KNIGHT ^ m] * PRIME_BLACK_KNIGHT +
               (ulong)pcs[B_PAWN ^ m] * PRIME_BLACK_PAWN;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static ushort make_move(int promote, int from, int to) => (ushort)(((promote & 0x7) << 12) | ((from & 0x3F) << 6) | (to & 0x3F));
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static int move_from(int move) => (move >> 6) & 0x3F;
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static int move_to(int move) => move & 0x3F;
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static int move_promotes(int move) => (move >> 12) & 0x7;

    static int type_of_piece_moved(Pos* pos, int move)
    {
        for (int i = PAWN; i <= KING; i++)
            if ((pieces_by_type(pos, pos->turn, i) & board(move_from(move))) != 0) return i;
        return 0;
    }

    static ushort* add_move(ushort* moves, bool promotes, int from, int to)
    {
        if (!promotes)
            *moves++ = make_move(TB_PROMOTES_NONE, from, to);
        else
        {
            *moves++ = make_move(TB_PROMOTES_QUEEN, from, to);
            *moves++ = make_move(TB_PROMOTES_KNIGHT, from, to);
            *moves++ = make_move(TB_PROMOTES_ROOK, from, to);
            *moves++ = make_move(TB_PROMOTES_BISHOP, from, to);
        }
        return moves;
    }

    // Generate all captures, including all underpromotions
    static ushort* gen_captures(Pos* pos, ushort* moves)
    {
        ulong occ = pos->white | pos->black;
        ulong us = pos->turn ? pos->white : pos->black, them = pos->turn ? pos->black : pos->white;
        ulong b, att;
        {
            int from = lsb(pos->kings & us);
            for (att = king_attacks(from) & them; att != 0; att = poplsb(att))
                moves = add_move(moves, false, from, lsb(att));
        }
        for (b = us & pos->queens; b != 0; b = poplsb(b))
        {
            int from = lsb(b);
            for (att = queen_attacks(from, occ) & them; att != 0; att = poplsb(att))
                moves = add_move(moves, false, from, lsb(att));
        }
        for (b = us & pos->rooks; b != 0; b = poplsb(b))
        {
            int from = lsb(b);
            for (att = rook_attacks(from, occ) & them; att != 0; att = poplsb(att))
                moves = add_move(moves, false, from, lsb(att));
        }
        for (b = us & pos->bishops; b != 0; b = poplsb(b))
        {
            int from = lsb(b);
            for (att = bishop_attacks(from, occ) & them; att != 0; att = poplsb(att))
                moves = add_move(moves, false, from, lsb(att));
        }
        for (b = us & pos->knights; b != 0; b = poplsb(b))
        {
            int from = lsb(b);
            for (att = knight_attacks(from) & them; att != 0; att = poplsb(att))
                moves = add_move(moves, false, from, lsb(att));
        }
        for (b = us & pos->pawns; b != 0; b = poplsb(b))
        {
            int from = lsb(b);
            att = pawn_attacks(from, pos->turn);
            if (pos->ep != 0 && (att & board(pos->ep)) != 0)
                moves = add_move(moves, false, from, pos->ep);
            for (att = att & them; att != 0; att = poplsb(att))
            {
                int to = lsb(att);
                moves = add_move(moves, rank(to) == 7 || rank(to) == 0, from, to);
            }
        }
        return moves;
    }

    // Generate all (pseudo-legal) moves
    static ushort* gen_moves(Pos* pos, ushort* moves)
    {
        ulong occ = pos->white | pos->black;
        ulong us = pos->turn ? pos->white : pos->black, them = pos->turn ? pos->black : pos->white;
        ulong b, att;
        {
            int from = lsb(pos->kings & us);
            for (att = king_attacks(from) & ~us; att != 0; att = poplsb(att))
                moves = add_move(moves, false, from, lsb(att));
        }
        for (b = us & pos->queens; b != 0; b = poplsb(b))
        {
            int from = lsb(b);
            for (att = queen_attacks(from, occ) & ~us; att != 0; att = poplsb(att))
                moves = add_move(moves, false, from, lsb(att));
        }
        for (b = us & pos->rooks; b != 0; b = poplsb(b))
        {
            int from = lsb(b);
            for (att = rook_attacks(from, occ) & ~us; att != 0; att = poplsb(att))
                moves = add_move(moves, false, from, lsb(att));
        }
        for (b = us & pos->bishops; b != 0; b = poplsb(b))
        {
            int from = lsb(b);
            for (att = bishop_attacks(from, occ) & ~us; att != 0; att = poplsb(att))
                moves = add_move(moves, false, from, lsb(att));
        }
        for (b = us & pos->knights; b != 0; b = poplsb(b))
        {
            int from = lsb(b);
            for (att = knight_attacks(from) & ~us; att != 0; att = poplsb(att))
                moves = add_move(moves, false, from, lsb(att));
        }
        for (b = us & pos->pawns; b != 0; b = poplsb(b))
        {
            int from = lsb(b);
            int next = from + (pos->turn ? 8 : -8);
            att = pawn_attacks(from, pos->turn);
            if (pos->ep != 0 && (att & board(pos->ep)) != 0)
                moves = add_move(moves, false, from, pos->ep);
            att &= them;
            if ((board(next) & occ) == 0)
            {
                att |= board(next);
                int next2 = from + (pos->turn ? 16 : -16);
                if ((pos->turn ? rank(from) == 1 : rank(from) == 6) && (board(next2) & occ) == 0)
                    att |= board(next2);
            }
            for (; att != 0; att = poplsb(att))
            {
                int to = lsb(att);
                moves = add_move(moves, rank(to) == 7 || rank(to) == 0, from, to);
            }
        }
        return moves;
    }

    static bool is_en_passant(Pos* pos, int move)
    {
        int from = move_from(move), to = move_to(move);
        ulong us = pos->turn ? pos->white : pos->black;
        if (pos->ep == 0) return false;
        if (to != pos->ep) return false;
        if ((board(from) & us & pos->pawns) == 0) return false;
        return true;
    }

    static bool is_capture(Pos* pos, int move)
    {
        int to = move_to(move);
        ulong them = pos->turn ? pos->black : pos->white;
        return (them & board(to)) != 0 || is_en_passant(pos, move);
    }

    // Is the position legal? (Can the king of the side that just moved be captured?)
    static bool is_legal(Pos* pos)
    {
        ulong occ = pos->white | pos->black;
        ulong us = pos->turn ? pos->black : pos->white, them = pos->turn ? pos->white : pos->black;
        ulong king = pos->kings & us;
        if (king == 0) return false;
        int sq = lsb(king);
        if ((king_attacks(sq) & (pos->kings & them)) != 0) return false;
        ulong ratt = rook_attacks(sq, occ);
        ulong batt = bishop_attacks(sq, occ);
        if ((ratt & (pos->rooks & them)) != 0) return false;
        if ((batt & (pos->bishops & them)) != 0) return false;
        if (((ratt | batt) & (pos->queens & them)) != 0) return false;
        if ((knight_attacks(sq) & (pos->knights & them)) != 0) return false;
        if ((pawn_attacks(sq, !pos->turn) & (pos->pawns & them)) != 0) return false;
        return true;
    }

    static bool is_check(Pos* pos)
    {
        ulong occ = pos->white | pos->black;
        ulong us = pos->turn ? pos->white : pos->black, them = pos->turn ? pos->black : pos->white;
        ulong king = pos->kings & us;
        int sq = lsb(king);
        ulong ratt = rook_attacks(sq, occ);
        ulong batt = bishop_attacks(sq, occ);
        if ((ratt & (pos->rooks & them)) != 0) return true;
        if ((batt & (pos->bishops & them)) != 0) return true;
        if (((ratt | batt) & (pos->queens & them)) != 0) return true;
        if ((knight_attacks(sq) & (pos->knights & them)) != 0) return true;
        if ((pawn_attacks(sq, pos->turn) & (pos->pawns & them)) != 0) return true;
        return false;
    }

    static bool is_valid(Pos* pos)
    {
        if (popcount(pos->kings) != 2) return false;
        if (popcount(pos->kings & pos->white) != 1) return false;
        if (popcount(pos->kings & pos->black) != 1) return false;
        if ((pos->white & pos->black) != 0) return false;
        if ((pos->kings & pos->queens) != 0) return false;
        if ((pos->kings & pos->rooks) != 0) return false;
        if ((pos->kings & pos->bishops) != 0) return false;
        if ((pos->kings & pos->knights) != 0) return false;
        if ((pos->kings & pos->pawns) != 0) return false;
        if ((pos->queens & pos->rooks) != 0) return false;
        if ((pos->queens & pos->bishops) != 0) return false;
        if ((pos->queens & pos->knights) != 0) return false;
        if ((pos->queens & pos->pawns) != 0) return false;
        if ((pos->rooks & pos->bishops) != 0) return false;
        if ((pos->rooks & pos->knights) != 0) return false;
        if ((pos->rooks & pos->pawns) != 0) return false;
        if ((pos->bishops & pos->knights) != 0) return false;
        if ((pos->bishops & pos->pawns) != 0) return false;
        if ((pos->knights & pos->pawns) != 0) return false;
        if ((pos->pawns & BOARD_FILE_EDGE) != 0) return false;
        if ((pos->white | pos->black) != (pos->kings | pos->queens | pos->rooks | pos->bishops | pos->knights | pos->pawns)) return false;
        return is_legal(pos);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static ulong do_bb_move(ulong b, int from, int to) => (b & ~board(to) & ~board(from)) | (((b >> from) & 0x1) << to);

    static bool do_move(Pos* pos, Pos* pos0, int move)
    {
        int from = move_from(move), to = move_to(move), promotes = move_promotes(move);
        pos->turn = !pos0->turn;
        pos->white = do_bb_move(pos0->white, from, to);
        pos->black = do_bb_move(pos0->black, from, to);
        pos->kings = do_bb_move(pos0->kings, from, to);
        pos->queens = do_bb_move(pos0->queens, from, to);
        pos->rooks = do_bb_move(pos0->rooks, from, to);
        pos->bishops = do_bb_move(pos0->bishops, from, to);
        pos->knights = do_bb_move(pos0->knights, from, to);
        pos->pawns = do_bb_move(pos0->pawns, from, to);
        pos->ep = 0;
        if (promotes != TB_PROMOTES_NONE)
        {
            pos->pawns &= ~board(to);
            switch (promotes)
            {
                case TB_PROMOTES_QUEEN: pos->queens |= board(to); break;
                case TB_PROMOTES_ROOK: pos->rooks |= board(to); break;
                case TB_PROMOTES_BISHOP: pos->bishops |= board(to); break;
                case TB_PROMOTES_KNIGHT: pos->knights |= board(to); break;
            }
            pos->rule50 = 0;
        }
        else if ((board(from) & pos0->pawns) != 0)
        {
            pos->rule50 = 0;   // pawn move
            if (rank(from) == 1 && rank(to) == 3 && (pawn_attacks(from + 8, true) & pos0->pawns & pos0->black) != 0)
                pos->ep = (byte)(from + 8);
            else if (rank(from) == 6 && rank(to) == 4 && (pawn_attacks(from - 8, false) & pos0->pawns & pos0->white) != 0)
                pos->ep = (byte)(from - 8);
            else if (to == pos0->ep)
            {
                int ep_to = pos0->turn ? to - 8 : to + 8;
                ulong ep_mask = ~board(ep_to);
                pos->white &= ep_mask;
                pos->black &= ep_mask;
                pos->pawns &= ep_mask;
            }
        }
        else if ((board(to) & (pos0->white | pos0->black)) != 0)
            pos->rule50 = 0;   // capture
        else
            pos->rule50 = (byte)(pos0->rule50 + 1);   // normal move
        return is_legal(pos);
    }

    static bool legal_move(Pos* pos, int move)
    {
        Pos pos1;
        return do_move(&pos1, pos, move);
    }

    static bool is_mate(Pos* pos)
    {
        if (!is_check(pos)) return false;
        ushort* moves = stackalloc ushort[MaxMoves];
        ushort* end = gen_moves(pos, moves);
        for (; moves < end; moves++)
        {
            Pos pos1;
            if (do_move(&pos1, pos, *moves)) return false;
        }
        return true;
    }

    static ushort* gen_legal(Pos* pos, ushort* moves)
    {
        ushort* pl_moves = stackalloc ushort[MaxMoves];
        ushort* end = gen_moves(pos, pl_moves);
        ushort* results = moves;
        for (ushort* m = pl_moves; m < end; m++)
            if (legal_move(pos, *m)) *results++ = *m;
        return results;
    }

    // ---------------------------------------------------------------- table structures (tbprobe.c)
    sealed class PairsData
    {
        public byte* indexTable;
        public ushort* sizeTable;
        public byte* data;
        public ushort* offset;     // NOT shifted by minLen (index with l - minLen)
        public byte[] symLen;
        public byte* symPat;
        public byte blockSize, idxBits, minLen;
        public byte constValue0;
        public uint numBlocks;     // entries of sizeTable (bounds for corrupt index tables)
        public ulong[] @base;      // index with l - minLen
    }

    sealed class EncInfo
    {
        public PairsData precomp;
        public readonly ulong[] factor = new ulong[TB_PIECES];
        public readonly byte[] pieces = new byte[TB_PIECES];
        public readonly byte[] norm = new byte[TB_PIECES];
    }

    sealed class BaseEntry
    {
        public ulong key;
        public string[] paths;                        // directories of the Init that created this entry
        public readonly IntPtr[] data = new IntPtr[3];
        public readonly long[] length = new long[3];  // mapped file length
        public readonly FileStream[] fs = new FileStream[3];
        public readonly MemoryMappedFile[] mmf = new MemoryMappedFile[3];
        public readonly MemoryMappedViewAccessor[] view = new MemoryMappedViewAccessor[3];
        public readonly bool[] ready = new bool[3];   // accessed with Volatile.Read/Write
        public readonly bool[] failed = new bool[3];  // the table of this type could not be loaded (Volatile)
        public bool retired;                          // replaced by a later Init (under tbMutex)
        public byte num;
        public bool symmetric, hasPawns, hasDtm, hasDtz;
        public bool kk_enc;
        public readonly byte[] pawns = new byte[2];
        // WDL: piece 2 (bside), pawn 8 (t + 4 * bside); DTZ: piece 1, pawn 4
        public EncInfo[] eiWdl, eiDtz;
        public byte* dtzMap;
        public readonly ushort[] dtzMapIdx = new ushort[16];   // [t * 4 + i]
        public readonly byte[] dtzFlags = new byte[4];
    }

    // Material hash (key -> entry). Built by Init and then never modified: a new Init publishes a new instance, so a
    // probe reads one consistent snapshot (Volatile.Read) even while the path is being changed.
    sealed class TbHash
    {
        public readonly ulong[] key = new ulong[1 << TB_HASHBITS];
        public readonly BaseEntry[] ptr = new BaseEntry[1 << TB_HASHBITS];
    }

    static readonly object tbMutex = new();
    static bool initialized;
    static string[] paths = Array.Empty<string>();
    static int numWdl, numDtm, numDtz;
    static int TB_MaxCardinality, TB_MaxCardinalityDTM;
    static int TB_LARGEST;
    static List<BaseEntry> entries = new();
    static TbHash tbHash = new();
    // Entries of earlier Inits that hold mapped files: kept for the lifetime of the process and never unmapped, as
    // a thread may still be probing them (see the port notes).
    static readonly List<BaseEntry> retired = new();

    /// <summary>Largest number of pieces (kings included) of the tables found (0: no tables).</summary>
    public static int MaxPieces => TB_LARGEST;
    public static int NumWdlFiles => numWdl;
    public static int NumDtzFiles => numDtz;
    public static int NumDtmFiles => numDtm;

    static string open_tb(string[] dirs, string str, string suffix)
    {
        foreach (var p in dirs)
        {
            string f = Path.Combine(p, str + suffix);
            if (File.Exists(f)) return f;
        }
        return null;
    }

    static bool test_tb(string str, string suffix)
    {
        string f = open_tb(paths, str, suffix);
        if (f == null) return false;
        long size;
        try { size = new FileInfo(f).Length; } catch { return false; }
        if ((size & 63) != 16)
        {
            Console.Error.WriteLine($"Incomplete tablebase file {str}{suffix}");
            return false;
        }
        return true;
    }

    static byte* map_tb(BaseEntry be, string name, string suffix, int type)
    {
        string f = open_tb(be.paths, name, suffix);
        if (f == null) return null;
        FileStream fs = null;
        MemoryMappedFile mmf = null;
        MemoryMappedViewAccessor view = null;
        bool acquired = false;
        try
        {
            fs = new FileStream(f, FileMode.Open, FileAccess.Read, FileShare.Read, 1, FileOptions.RandomAccess);
            long length = fs.Length;
            mmf = MemoryMappedFile.CreateFromFile(fs, null, 0, MemoryMappedFileAccess.Read, HandleInheritability.None, false);
            view = mmf.CreateViewAccessor(0, 0, MemoryMappedFileAccess.Read);
            byte* p = null;
            view.SafeMemoryMappedViewHandle.AcquirePointer(ref p);
            acquired = true;
            p += view.PointerOffset;
            be.fs[type] = fs;
            be.mmf[type] = mmf;
            be.view[type] = view;
            be.length[type] = length;
            if (be.retired) retired.Add(be);   // mapped by a thread still probing an old generation: never unmap
            return p;
        }
        catch (Exception e)
        {
            Console.Error.WriteLine($"Could not map {name}{suffix} into memory: {e.Message}");
            if (acquired) view.SafeMemoryMappedViewHandle.ReleasePointer();
            view?.Dispose();
            mmf?.Dispose();
            fs?.Dispose();
            return null;
        }
    }

    // Only for a table that never became ready (no other thread can hold its pointers); see retire_tb_entry.
    static void unmap_file(BaseEntry be, int type)
    {
        var view = be.view[type];
        if (view != null)
        {
            view.SafeMemoryMappedViewHandle.ReleasePointer();
            view.Dispose();
        }
        be.mmf[type]?.Dispose();
        be.fs[type]?.Dispose();
        be.view[type] = null;
        be.mmf[type] = null;
        be.fs[type] = null;
        be.data[type] = IntPtr.Zero;
        be.length[type] = 0;
    }

    static void add_to_hash(TbHash h, BaseEntry ptr, ulong key)
    {
        int idx = (int)(key >> (64 - TB_HASHBITS));
        while (h.ptr[idx] != null)
            idx = (idx + 1) & ((1 << TB_HASHBITS) - 1);
        h.key[idx] = key;
        h.ptr[idx] = ptr;
    }

    static BaseEntry find_entry(TbHash h, ulong key)
    {
        int idx = (int)(key >> (64 - TB_HASHBITS));
        while (h.key[idx] != 0 && h.key[idx] != key)
            idx = (idx + 1) & ((1 << TB_HASHBITS) - 1);
        return h.ptr[idx];
    }

    static char pchr(int i) => piece_to_char[QUEEN - i];

    // piece counts [piece code] of a table name such as "KRPvKR" (white before 'v')
    static int[] pieces_of_name(string str)
    {
        int[] pcs = new int[16];
        int color = 0;
        foreach (char s in str)
        {
            if (s == 'v') color = 8;
            else
            {
                int piece_type = char_to_piece_type(s);
                if (piece_type != 0) pcs[piece_type | color]++;
            }
        }
        return pcs;
    }

    static void init_tb(TbHash h, string str)
    {
        if (!test_tb(str, tbSuffix[WDL])) return;

        int[] pcs = pieces_of_name(str);

        ulong key = calc_key_from_pcs(pcs, false);
        ulong key2 = calc_key_from_pcs(pcs, true);
        bool hasPawns = pcs[W_PAWN] != 0 || pcs[B_PAWN] != 0;

        var be = new BaseEntry();
        entries.Add(be);
        be.paths = paths;
        be.hasPawns = hasPawns;
        be.key = key;
        be.symmetric = key == key2;
        be.num = 0;
        for (int i = 0; i < 16; i++) be.num += (byte)pcs[i];

        numWdl++;
        be.hasDtm = test_tb(str, tbSuffix[DTM]);
        be.hasDtz = test_tb(str, tbSuffix[DTZ]);
        if (be.hasDtm) numDtm++;
        if (be.hasDtz) numDtz++;

        if (be.num > TB_MaxCardinality) TB_MaxCardinality = be.num;
        if (be.hasDtm && be.num > TB_MaxCardinalityDTM) TB_MaxCardinalityDTM = be.num;

        if (!be.hasPawns)
        {
            int j = 0;
            for (int i = 0; i < 16; i++) if (pcs[i] == 1) j++;
            be.kk_enc = j == 2;
            be.eiWdl = new EncInfo[2];
            be.eiDtz = new EncInfo[1];
        }
        else
        {
            be.pawns[0] = (byte)pcs[W_PAWN];
            be.pawns[1] = (byte)pcs[B_PAWN];
            if (pcs[B_PAWN] != 0 && (pcs[W_PAWN] == 0 || pcs[W_PAWN] > pcs[B_PAWN]))
                (be.pawns[0], be.pawns[1]) = (be.pawns[1], be.pawns[0]);
            be.eiWdl = new EncInfo[8];
            be.eiDtz = new EncInfo[4];
        }
        for (int i = 0; i < be.eiWdl.Length; i++) be.eiWdl[i] = new EncInfo();
        for (int i = 0; i < be.eiDtz.Length; i++) be.eiDtz[i] = new EncInfo();

        add_to_hash(h, be, key);
        if (key != key2) add_to_hash(h, be, key2);
    }

    static int num_tables(BaseEntry be, int type) => be.hasPawns ? (type == DTM ? 6 : 4) : 1;

    // Called by Init for the entries of the previous generation: a thread may still be probing them, so nothing is
    // unmapped; entries holding a mapped file (whether or not their table became ready) are kept alive forever.
    static void retire_tb_entry(BaseEntry be)
    {
        be.retired = true;
        for (int type = 0; type < 3; type++)
            if (be.view[type] != null)
            {
                retired.Add(be);
                break;
            }
    }

    /// <summary>
    /// Initialise the tablebases from a list of directories separated by ';' or the platform's path separator (surrounding
    /// quotes of each directory are stripped). An empty path or "&lt;empty&gt;" frees everything. Safe to call while
    /// other threads probe: they finish on the previous tables, which stay mapped (see the port notes).
    /// </summary>
    public static bool Init(string path)
    {
        lock (tbMutex)
        {
            if (!initialized)
            {
                init_indices();
                initialized = true;
            }

            // retire the previous initialisation (never unmapped). TB_LARGEST keeps its value until the new
            // tables are published: probes on other threads check it (probe_ok) without the lock, and a 0 in
            // between failed every probe made during the reload (EngineBattle change).
            foreach (var e in entries) retire_tb_entry(e);
            entries = new List<BaseEntry>();
            paths = Array.Empty<string>();
            numWdl = numDtm = numDtz = 0;
            TB_MaxCardinality = TB_MaxCardinalityDTM = 0;
            var h = new TbHash();

            path ??= "";
            path = path.Trim();
            if (path.Length == 0 || path == "<empty>")
            {
                TB_LARGEST = 0;
                Volatile.Write(ref tbHash, h);
                return true;
            }

            var list = new List<string>();
            foreach (var p in path.Split(new[] { ';', Path.PathSeparator }))
            {
                string d = p.Trim().Trim('"').Trim();
                if (d.Length > 0) list.Add(d);
            }
            paths = list.ToArray();

            int i, j, k, l, m;
            for (i = 0; i < 5; i++)
                init_tb(h, $"K{pchr(i)}vK");

            for (i = 0; i < 5; i++)
                for (j = i; j < 5; j++)
                    init_tb(h, $"K{pchr(i)}vK{pchr(j)}");

            for (i = 0; i < 5; i++)
                for (j = i; j < 5; j++)
                    init_tb(h, $"K{pchr(i)}{pchr(j)}vK");

            for (i = 0; i < 5; i++)
                for (j = i; j < 5; j++)
                    for (k = 0; k < 5; k++)
                        init_tb(h, $"K{pchr(i)}{pchr(j)}vK{pchr(k)}");

            for (i = 0; i < 5; i++)
                for (j = i; j < 5; j++)
                    for (k = j; k < 5; k++)
                        init_tb(h, $"K{pchr(i)}{pchr(j)}{pchr(k)}vK");

            for (i = 0; i < 5; i++)
                for (j = i; j < 5; j++)
                    for (k = i; k < 5; k++)
                        for (l = (i == k) ? j : k; l < 5; l++)
                            init_tb(h, $"K{pchr(i)}{pchr(j)}vK{pchr(k)}{pchr(l)}");

            for (i = 0; i < 5; i++)
                for (j = i; j < 5; j++)
                    for (k = j; k < 5; k++)
                        for (l = 0; l < 5; l++)
                            init_tb(h, $"K{pchr(i)}{pchr(j)}{pchr(k)}vK{pchr(l)}");

            for (i = 0; i < 5; i++)
                for (j = i; j < 5; j++)
                    for (k = j; k < 5; k++)
                        for (l = k; l < 5; l++)
                            init_tb(h, $"K{pchr(i)}{pchr(j)}{pchr(k)}{pchr(l)}vK");

            // 7-piece tables (untested with real files)
            for (i = 0; i < 5; i++)
                for (j = i; j < 5; j++)
                    for (k = j; k < 5; k++)
                        for (l = k; l < 5; l++)
                            for (m = l; m < 5; m++)
                                init_tb(h, $"K{pchr(i)}{pchr(j)}{pchr(k)}{pchr(l)}{pchr(m)}vK");

            for (i = 0; i < 5; i++)
                for (j = i; j < 5; j++)
                    for (k = j; k < 5; k++)
                        for (l = k; l < 5; l++)
                            for (m = 0; m < 5; m++)
                                init_tb(h, $"K{pchr(i)}{pchr(j)}{pchr(k)}{pchr(l)}vK{pchr(m)}");

            for (i = 0; i < 5; i++)
                for (j = i; j < 5; j++)
                    for (k = j; k < 5; k++)
                        for (l = 0; l < 5; l++)
                            for (m = l; m < 5; m++)
                                init_tb(h, $"K{pchr(i)}{pchr(j)}{pchr(k)}vK{pchr(l)}{pchr(m)}");

            Volatile.Write(ref tbHash, h);
            // DTM is not ported, so only WDL determines the probe-able size
            TB_LARGEST = TB_MaxCardinality;
            return true;
        }
    }

    /// <summary>Drop all tables (they stay mapped for threads still probing them; see Init).</summary>
    public static void Free() => Init("");

    // ---------------------------------------------------------------- indexing
    static readonly sbyte[] OffDiag = {
        0,-1,-1,-1,-1,-1,-1,-1,
        1, 0,-1,-1,-1,-1,-1,-1,
        1, 1, 0,-1,-1,-1,-1,-1,
        1, 1, 1, 0,-1,-1,-1,-1,
        1, 1, 1, 1, 0,-1,-1,-1,
        1, 1, 1, 1, 1, 0,-1,-1,
        1, 1, 1, 1, 1, 1, 0,-1,
        1, 1, 1, 1, 1, 1, 1, 0
    };

    static readonly byte[] Triangle = {
        6, 0, 1, 2, 2, 1, 0, 6,
        0, 7, 3, 4, 4, 3, 7, 0,
        1, 3, 8, 5, 5, 8, 3, 1,
        2, 4, 5, 9, 9, 5, 4, 2,
        2, 4, 5, 9, 9, 5, 4, 2,
        1, 3, 8, 5, 5, 8, 3, 1,
        0, 7, 3, 4, 4, 3, 7, 0,
        6, 0, 1, 2, 2, 1, 0, 6
    };

    static readonly byte[] FlipDiag = {
         0,  8, 16, 24, 32, 40, 48, 56,
         1,  9, 17, 25, 33, 41, 49, 57,
         2, 10, 18, 26, 34, 42, 50, 58,
         3, 11, 19, 27, 35, 43, 51, 59,
         4, 12, 20, 28, 36, 44, 52, 60,
         5, 13, 21, 29, 37, 45, 53, 61,
         6, 14, 22, 30, 38, 46, 54, 62,
         7, 15, 23, 31, 39, 47, 55, 63
    };

    static readonly byte[] Lower = {
        28,  0,  1,  2,  3,  4,  5,  6,
         0, 29,  7,  8,  9, 10, 11, 12,
         1,  7, 30, 13, 14, 15, 16, 17,
         2,  8, 13, 31, 18, 19, 20, 21,
         3,  9, 14, 18, 32, 22, 23, 24,
         4, 10, 15, 19, 22, 33, 25, 26,
         5, 11, 16, 20, 23, 25, 34, 27,
         6, 12, 17, 21, 24, 26, 27, 35
    };

    static readonly byte[] Diag = {
         0,  0,  0,  0,  0,  0,  0,  8,
         0,  1,  0,  0,  0,  0,  9,  0,
         0,  0,  2,  0,  0, 10,  0,  0,
         0,  0,  0,  3, 11,  0,  0,  0,
         0,  0,  0, 12,  4,  0,  0,  0,
         0,  0, 13,  0,  0,  5,  0,  0,
         0, 14,  0,  0,  0,  0,  6,  0,
        15,  0,  0,  0,  0,  0,  0,  7
    };

    static readonly byte[][] Flap = {
        new byte[] {
            0,  0,  0,  0,  0,  0,  0,  0,
            0,  6, 12, 18, 18, 12,  6,  0,
            1,  7, 13, 19, 19, 13,  7,  1,
            2,  8, 14, 20, 20, 14,  8,  2,
            3,  9, 15, 21, 21, 15,  9,  3,
            4, 10, 16, 22, 22, 16, 10,  4,
            5, 11, 17, 23, 23, 17, 11,  5,
            0,  0,  0,  0,  0,  0,  0,  0 },
        new byte[] {
            0,  0,  0,  0,  0,  0,  0,  0,
            0,  1,  2,  3,  3,  2,  1,  0,
            4,  5,  6,  7,  7,  6,  5,  4,
            8,  9, 10, 11, 11, 10,  9,  8,
           12, 13, 14, 15, 15, 14, 13, 12,
           16, 17, 18, 19, 19, 18, 17, 16,
           20, 21, 22, 23, 23, 22, 21, 20,
            0,  0,  0,  0,  0,  0,  0,  0 }
    };

    static readonly byte[][] PawnTwist = {
        new byte[] {
            0,  0,  0,  0,  0,  0,  0,  0,
           47, 35, 23, 11, 10, 22, 34, 46,
           45, 33, 21,  9,  8, 20, 32, 44,
           43, 31, 19,  7,  6, 18, 30, 42,
           41, 29, 17,  5,  4, 16, 28, 40,
           39, 27, 15,  3,  2, 14, 26, 38,
           37, 25, 13,  1,  0, 12, 24, 36,
            0,  0,  0,  0,  0,  0,  0,  0 },
        new byte[] {
            0,  0,  0,  0,  0,  0,  0,  0,
           47, 45, 43, 41, 40, 42, 44, 46,
           39, 37, 35, 33, 32, 34, 36, 38,
           31, 29, 27, 25, 24, 26, 28, 30,
           23, 21, 19, 17, 16, 18, 20, 22,
           15, 13, 11,  9,  8, 10, 12, 14,
            7,  5,  3,  1,  0,  2,  4,  6,
            0,  0,  0,  0,  0,  0,  0,  0 }
    };

    static readonly short[] KKIdx = {
        -1, -1, -1,  0,  1,  2,  3,  4,
        -1, -1, -1,  5,  6,  7,  8,  9,
        10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25,
        26, 27, 28, 29, 30, 31, 32, 33,
        34, 35, 36, 37, 38, 39, 40, 41,
        42, 43, 44, 45, 46, 47, 48, 49,
        50, 51, 52, 53, 54, 55, 56, 57,

        58, -1, -1, -1, 59, 60, 61, 62,
        63, -1, -1, -1, 64, 65, 66, 67,
        68, 69, 70, 71, 72, 73, 74, 75,
        76, 77, 78, 79, 80, 81, 82, 83,
        84, 85, 86, 87, 88, 89, 90, 91,
        92, 93, 94, 95, 96, 97, 98, 99,
       100,101,102,103,104,105,106,107,
       108,109,110,111,112,113,114,115,

       116,117, -1, -1, -1,118,119,120,
       121,122, -1, -1, -1,123,124,125,
       126,127,128,129,130,131,132,133,
       134,135,136,137,138,139,140,141,
       142,143,144,145,146,147,148,149,
       150,151,152,153,154,155,156,157,
       158,159,160,161,162,163,164,165,
       166,167,168,169,170,171,172,173,

       174, -1, -1, -1,175,176,177,178,
       179, -1, -1, -1,180,181,182,183,
       184, -1, -1, -1,185,186,187,188,
       189,190,191,192,193,194,195,196,
       197,198,199,200,201,202,203,204,
       205,206,207,208,209,210,211,212,
       213,214,215,216,217,218,219,220,
       221,222,223,224,225,226,227,228,

       229,230, -1, -1, -1,231,232,233,
       234,235, -1, -1, -1,236,237,238,
       239,240, -1, -1, -1,241,242,243,
       244,245,246,247,248,249,250,251,
       252,253,254,255,256,257,258,259,
       260,261,262,263,264,265,266,267,
       268,269,270,271,272,273,274,275,
       276,277,278,279,280,281,282,283,

       284,285,286,287,288,289,290,291,
       292,293, -1, -1, -1,294,295,296,
       297,298, -1, -1, -1,299,300,301,
       302,303, -1, -1, -1,304,305,306,
       307,308,309,310,311,312,313,314,
       315,316,317,318,319,320,321,322,
       323,324,325,326,327,328,329,330,
       331,332,333,334,335,336,337,338,

        -1, -1,339,340,341,342,343,344,
        -1, -1,345,346,347,348,349,350,
        -1, -1,441,351,352,353,354,355,
        -1, -1, -1,442,356,357,358,359,
        -1, -1, -1, -1,443,360,361,362,
        -1, -1, -1, -1, -1,444,363,364,
        -1, -1, -1, -1, -1, -1,445,365,
        -1, -1, -1, -1, -1, -1, -1,446,

        -1, -1, -1,366,367,368,369,370,
        -1, -1, -1,371,372,373,374,375,
        -1, -1, -1,376,377,378,379,380,
        -1, -1, -1,447,381,382,383,384,
        -1, -1, -1, -1,448,385,386,387,
        -1, -1, -1, -1, -1,449,388,389,
        -1, -1, -1, -1, -1, -1,450,390,
        -1, -1, -1, -1, -1, -1, -1,451,

       452,391,392,393,394,395,396,397,
        -1, -1, -1, -1,398,399,400,401,
        -1, -1, -1, -1,402,403,404,405,
        -1, -1, -1, -1,406,407,408,409,
        -1, -1, -1, -1,453,410,411,412,
        -1, -1, -1, -1, -1,454,413,414,
        -1, -1, -1, -1, -1, -1,455,415,
        -1, -1, -1, -1, -1, -1, -1,456,

       457,416,417,418,419,420,421,422,
        -1,458,423,424,425,426,427,428,
        -1, -1, -1, -1, -1,429,430,431,
        -1, -1, -1, -1, -1,432,433,434,
        -1, -1, -1, -1, -1,435,436,437,
        -1, -1, -1, -1, -1,459,438,439,
        -1, -1, -1, -1, -1, -1,460,440,
        -1, -1, -1, -1, -1, -1, -1,461
    };

    static readonly byte[] FileToFile = { 0, 1, 2, 3, 3, 2, 1, 0 };
    static readonly int[] WdlToMap = { 1, 3, 0, 2, 0 };
    static readonly byte[] PAFlags = { 8, 0, 0, 0, 4 };

    static readonly ulong[] Binomial = new ulong[7 * 64];          // [k * 64 + n] = Bin(n, k)
    static readonly ulong[] PawnIdx = new ulong[2 * 6 * 24];       // [(enc * 6 + i) * 24 + j]
    static readonly ulong[] PawnFactorFile = new ulong[6 * 4];     // [i * 4 + j]
    static readonly ulong[] PawnFactorRank = new ulong[6 * 6];     // [i * 6 + j]

    static void init_indices()
    {
        int i, j, k;
        for (i = 0; i < 7; i++)
            for (j = 0; j < 64; j++)
            {
                ulong f = 1, l = 1;
                for (k = 0; k < i; k++)
                {
                    f *= unchecked((ulong)(long)(j - k));
                    l *= (ulong)(k + 1);
                }
                Binomial[i * 64 + j] = f / l;
            }

        for (i = 0; i < 6; i++)
        {
            ulong s = 0;
            for (j = 0; j < 24; j++)
            {
                PawnIdx[(0 * 6 + i) * 24 + j] = s;
                s += Binomial[i * 64 + PawnTwist[0][(1 + (j % 6)) * 8 + (j / 6)]];
                if ((j + 1) % 6 == 0)
                {
                    PawnFactorFile[i * 4 + j / 6] = s;
                    s = 0;
                }
            }
        }

        for (i = 0; i < 6; i++)
        {
            ulong s = 0;
            for (j = 0; j < 24; j++)
            {
                PawnIdx[(1 * 6 + i) * 24 + j] = s;
                s += Binomial[i * 64 + PawnTwist[1][(1 + (j / 4)) * 8 + (j % 4)]];
                if ((j + 1) % 4 == 0)
                {
                    PawnFactorRank[i * 6 + j / 4] = s;
                    s = 0;
                }
            }
        }
    }

    static int leading_pawn(int* p, BaseEntry be, int enc)
    {
        byte[] flap = Flap[enc - 1];
        for (int i = 1; i < be.pawns[0]; i++)
            if (flap[p[0]] > flap[p[i]])
                (p[0], p[i]) = (p[i], p[0]);
        return enc == FILE_ENC ? FileToFile[p[0] & 7] : (p[0] - 8) >> 3;
    }

    static ulong encode(int* p, EncInfo ei, BaseEntry be, int enc)
    {
        int n = be.num;
        ulong idx;
        int k;

        if ((p[0] & 0x04) != 0)
            for (int i = 0; i < n; i++)
                p[i] ^= 0x07;

        if (enc == PIECE_ENC)
        {
            if ((p[0] & 0x20) != 0)
                for (int i = 0; i < n; i++)
                    p[i] ^= 0x38;

            for (int i = 0; i < n; i++)
                if (OffDiag[p[i]] != 0)
                {
                    if (OffDiag[p[i]] > 0 && i < (be.kk_enc ? 2 : 3))
                        for (int j = 0; j < n; j++)
                            p[j] = FlipDiag[p[j]];
                    break;
                }

            if (be.kk_enc)
            {
                idx = unchecked((ulong)(long)KKIdx[Triangle[p[0]] * 64 + p[1]]);
                k = 2;
            }
            else
            {
                int s1 = p[1] > p[0] ? 1 : 0;
                int s2 = (p[2] > p[0] ? 1 : 0) + (p[2] > p[1] ? 1 : 0);

                if (OffDiag[p[0]] != 0)
                    idx = (ulong)(Triangle[p[0]] * 63 * 62 + (p[1] - s1) * 62 + (p[2] - s2));
                else if (OffDiag[p[1]] != 0)
                    idx = (ulong)(6 * 63 * 62 + Diag[p[0]] * 28 * 62 + Lower[p[1]] * 62 + p[2] - s2);
                else if (OffDiag[p[2]] != 0)
                    idx = (ulong)(6 * 63 * 62 + 4 * 28 * 62 + Diag[p[0]] * 7 * 28 + (Diag[p[1]] - s1) * 28 + Lower[p[2]]);
                else
                    idx = (ulong)(6 * 63 * 62 + 4 * 28 * 62 + 4 * 7 * 28 + Diag[p[0]] * 7 * 6 + (Diag[p[1]] - s1) * 6 + (Diag[p[2]] - s2));
                k = 3;
            }
            idx *= ei.factor[0];
        }
        else
        {
            byte[] twist = PawnTwist[enc - 1];
            for (int i = 1; i < be.pawns[0]; i++)
                for (int j = i + 1; j < be.pawns[0]; j++)
                    if (twist[p[i]] < twist[p[j]])
                        (p[i], p[j]) = (p[j], p[i]);

            k = be.pawns[0];
            idx = PawnIdx[((enc - 1) * 6 + (k - 1)) * 24 + Flap[enc - 1][p[0]]];
            for (int i = 1; i < k; i++)
                idx += Binomial[(k - i) * 64 + twist[p[i]]];
            idx *= ei.factor[0];

            // pawns of the other colour
            if (be.pawns[1] != 0)
            {
                int t = k + be.pawns[1];
                for (int i = k; i < t; i++)
                    for (int j = i + 1; j < t; j++)
                        if (p[i] > p[j]) (p[i], p[j]) = (p[j], p[i]);
                ulong s = 0;
                for (int i = k; i < t; i++)
                {
                    int sq = p[i];
                    int skips = 0;
                    for (int j = 0; j < k; j++)
                        skips += sq > p[j] ? 1 : 0;
                    s += Binomial[(i - k + 1) * 64 + (sq - skips - 8)];
                }
                idx += s * ei.factor[k];
                k = t;
            }
        }

        for (; k < n;)
        {
            int t = k + ei.norm[k];
            for (int i = k; i < t; i++)
                for (int j = i + 1; j < t; j++)
                    if (p[i] > p[j]) (p[i], p[j]) = (p[j], p[i]);
            ulong s = 0;
            for (int i = k; i < t; i++)
            {
                int sq = p[i];
                int skips = 0;
                for (int j = 0; j < k; j++)
                    skips += sq > p[j] ? 1 : 0;
                s += Binomial[(i - k + 1) * 64 + (sq - skips)];
            }
            idx += s * ei.factor[k];
            k = t;
        }

        return idx;
    }

    // number of placements of k like pieces on n squares
    static ulong subfactor(ulong k, ulong n)
    {
        ulong f = n, l = 1;
        for (ulong i = 1; i < k; i++)
        {
            f *= n - i;
            l *= i + 1;
        }
        return f / l;
    }

    // A corrupt or truncated table file: thrown during table setup / decompression and turned into a failed probe.
    static void corrupt(string what) => throw new InvalidDataException("corrupted table (" + what + ")");

    // Throws unless n bytes from p lie inside the mapped file [.., end).
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static void need(byte* p, ulong n, byte* end)
    {
        if (p > end || n > (ulong)(end - p)) corrupt("truncated");
    }

    // pcs: piece counts of the table (from its file name) to validate the piece list read from the file
    static ulong init_enc_info(EncInfo ei, BaseEntry be, byte* tb, int shift, int t, int enc, int[] pcs)
    {
        bool morePawns = enc != PIECE_ENC && be.pawns[1] > 0;
        int mp = morePawns ? 1 : 0;

        for (int i = 0; i < be.num; i++)
        {
            ei.pieces[i] = (byte)((tb[i + 1 + mp] >> shift) & 0x0f);
            ei.norm[i] = 0;
        }

        // the piece list must be the table's material (fill_squares writes one square per piece of each listed
        // type into a TB_PIECES buffer), with the leading pawns first for pawn tables
        Span<int> cnt = stackalloc int[16];
        cnt.Clear();   // SkipLocalsInit: stackalloc is not zeroed
        for (int i = 0; i < be.num; i++) cnt[ei.pieces[i]]++;
        for (int i = 0; i < 16; i++)
            if (cnt[i] != pcs[i]) corrupt("piece list");
        if (enc != PIECE_ENC)
            for (int i = 0; i < be.pawns[0] + (morePawns ? be.pawns[1] : 0); i++)
                if (TypeOfPiece(ei.pieces[i]) != PAWN) corrupt("pawn list");

        int order = (tb[0] >> shift) & 0x0f;
        int order2 = morePawns ? (tb[1] >> shift) & 0x0f : 0x0f;

        int k = ei.norm[0] = (byte)(enc != PIECE_ENC ? be.pawns[0] : be.kk_enc ? 2 : 3);

        if (morePawns)
        {
            ei.norm[k] = be.pawns[1];
            k += ei.norm[k];
        }

        for (int i = k; i < be.num; i += ei.norm[i])
            for (int j = i; j < be.num && ei.pieces[j] == ei.pieces[i]; j++)
                ei.norm[i]++;

        int n = 64 - k;
        ulong f = 1;

        for (int i = 0; k < be.num || i == order || i == order2; i++)
        {
            if (i == order)
            {
                ei.factor[0] = f;
                f *= enc == FILE_ENC ? PawnFactorFile[(ei.norm[0] - 1) * 4 + t]
                   : enc == RANK_ENC ? PawnFactorRank[(ei.norm[0] - 1) * 6 + t]
                   : be.kk_enc ? 462UL : 31332UL;
            }
            else if (i == order2)
            {
                ei.factor[ei.norm[0]] = f;
                f *= subfactor(ei.norm[ei.norm[0]], (ulong)(48 - ei.norm[0]));
            }
            else
            {
                ei.factor[k] = f;
                f *= subfactor(ei.norm[k], (ulong)n);
                n -= ei.norm[k];
                k += ei.norm[k];
            }
        }

        return f;
    }

    // tmp[s]: 0 = not visited, 1 = done, 2 = in progress (a cycle in a corrupt file would recurse forever)
    static void calc_symLen(PairsData d, uint s, byte[] tmp, uint numSyms)
    {
        tmp[s] = 2;
        byte* w = d.symPat + 3 * s;
        uint s2 = (uint)((w[2] << 4) | (w[1] >> 4));
        if (s2 == 0x0fff)
            d.symLen[s] = 0;
        else
        {
            uint s1 = (uint)(((w[1] & 0xf) << 8) | w[0]);
            if (s1 >= numSyms || s2 >= numSyms) corrupt("symbol id");
            if (tmp[s1] == 0) calc_symLen(d, s1, tmp, numSyms);
            else if (tmp[s1] == 2) corrupt("symbol cycle");
            if (tmp[s2] == 0) calc_symLen(d, s2, tmp, numSyms);
            else if (tmp[s2] == 2) corrupt("symbol cycle");
            d.symLen[s] = (byte)(d.symLen[s1] + d.symLen[s2] + 1);
        }
        tmp[s] = 1;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)] static uint read_le_u32(byte* p) => *(uint*)p;   // little-endian host
    [MethodImpl(MethodImplOptions.AggressiveInlining)] static ushort read_le_u16(byte* p) => *(ushort*)p;

    static PairsData setup_pairs(ref byte* ptr, byte* end, ulong tb_size, ulong* size, out byte flags, int type)
    {
        PairsData d;
        byte* data = ptr;

        need(data, 1, end);
        flags = data[0];
        if ((data[0] & 0x80) != 0)
        {
            need(data, 2, end);
            d = new PairsData();
            d.idxBits = 0;
            d.constValue0 = type == WDL ? data[1] : (byte)0;
            ptr = data + 2;
            size[0] = size[1] = size[2] = 0;
            return d;
        }

        need(data, 10, end);
        byte blockSize = data[1];
        byte idxBits = data[2];
        uint realNumBlocks = read_le_u32(data + 4);
        uint numBlocks = realNumBlocks + data[3];
        int maxLen = data[8];
        int minLen = data[9];
        int h = maxLen - minLen + 1;
        if (blockSize >= 32 || idxBits == 0 || idxBits >= 64 || minLen < 1 || maxLen > 64 || h < 1 || numBlocks < realNumBlocks)
            corrupt("pairs header");
        need(data, 12 + 2 * (ulong)h, end);
        uint numSyms = read_le_u16(data + 10 + 2 * h);
        need(data, 12 + 2 * (ulong)h + 3 * (ulong)numSyms + (numSyms & 1), end);
        d = new PairsData();
        d.blockSize = blockSize;
        d.idxBits = idxBits;
        d.numBlocks = numBlocks;
        d.offset = (ushort*)(data + 10);
        d.symLen = new byte[numSyms];
        d.symPat = data + 12 + 2 * h;
        d.minLen = (byte)minLen;
        ptr = data + 12 + 2 * h + 3 * numSyms + (numSyms & 1);

        ulong num_indices = (tb_size + (1UL << idxBits) - 1) >> idxBits;
        size[0] = 6UL * num_indices;
        size[1] = 2UL * numBlocks;
        size[2] = (ulong)realNumBlocks << blockSize;

        var tmp = new byte[numSyms];
        for (uint s = 0; s < numSyms; s++)
            if (tmp[s] == 0)
                calc_symLen(d, s, tmp, numSyms);

        d.@base = new ulong[h];
        d.@base[h - 1] = 0;
        for (int i = h - 2; i >= 0; i--)
            d.@base[i] = unchecked(d.@base[i + 1] + read_le_u16((byte*)(d.offset + i)) - read_le_u16((byte*)(d.offset + i + 1))) / 2;
        for (int i = 0; i < h; i++)
            d.@base[i] <<= 64 - (minLen + i);

        return d;
    }

    // Maps and sets up one table file (under tbMutex). A missing, truncated or corrupt file returns false with the
    // file unmapped again (nobody else can hold its pointers: the table never became ready).
    static bool init_table(BaseEntry be, string str, int type)
    {
        byte* data = map_tb(be, str, tbSuffix[type], type);
        if (data == null) return false;
        try
        {
            setup_table(be, str, type, data, data + be.length[type]);
            return true;
        }
        catch (Exception e)
        {
            Console.Error.WriteLine($"Could not load {str}{tbSuffix[type]}: {e.Message}");
            unmap_file(be, type);
            return false;
        }
    }

    static void setup_table(BaseEntry be, string str, int type, byte* data, byte* end)
    {
        need(data, 5, end);
        if (read_le_u32(data) != tbMagic[type])
            corrupt("magic");

        be.data[type] = (IntPtr)data;

        bool split = type != DTZ && (data[4] & 0x01) != 0;

        data += 5;

        int[] pcs = pieces_of_name(str);
        ulong* tb_size = stackalloc ulong[6 * 2];
        int num = num_tables(be, type);
        EncInfo[] ei = type == WDL ? be.eiWdl : be.eiDtz;
        int enc = !be.hasPawns ? PIECE_ENC : type != DTM ? FILE_ENC : RANK_ENC;
        int step = be.num + 1 + (be.hasPawns && be.pawns[1] != 0 ? 1 : 0);

        for (int t = 0; t < num; t++)
        {
            need(data, (ulong)step, end);
            tb_size[t * 2 + 0] = init_enc_info(ei[t], be, data, 0, t, enc, pcs);
            if (split)
                tb_size[t * 2 + 1] = init_enc_info(ei[num + t], be, data, 4, t, enc, pcs);
            data += step;
        }
        data += (nuint)data & 1;

        ulong* size = stackalloc ulong[6 * 2 * 3];   // [(t * 2 + s) * 3 + i]
        for (int t = 0; t < num; t++)
        {
            ei[t].precomp = setup_pairs(ref data, end, tb_size[t * 2 + 0], size + (t * 2 + 0) * 3, out byte flags, type);
            if (type == DTZ)
                be.dtzFlags[t] = flags;
            if (split)
                ei[num + t].precomp = setup_pairs(ref data, end, tb_size[t * 2 + 1], size + (t * 2 + 1) * 3, out flags, type);
            else if (type != DTZ)
                ei[num + t].precomp = null;
        }

        if (type == DTZ)
        {
            byte* map = data;
            be.dtzMap = map;
            for (int t = 0; t < num; t++)
            {
                if ((be.dtzFlags[t] & 2) != 0)
                {
                    if ((be.dtzFlags[t] & 16) == 0)
                    {
                        for (int i = 0; i < 4; i++)
                        {
                            need(data, 1, end);
                            if (data + 1 - map > ushort.MaxValue) corrupt("dtz map");
                            be.dtzMapIdx[t * 4 + i] = (ushort)(data + 1 - map);
                            need(data, 1 + (ulong)data[0], end);
                            data += 1 + data[0];
                        }
                    }
                    else
                    {
                        data += (nuint)data & 0x01;
                        for (int i = 0; i < 4; i++)
                        {
                            need(data, 2, end);
                            if ((ushort*)data + 1 - (ushort*)map > ushort.MaxValue) corrupt("dtz map");
                            be.dtzMapIdx[t * 4 + i] = (ushort)((ushort*)data + 1 - (ushort*)map);
                            need(data, 2 + 2 * (ulong)read_le_u16(data), end);
                            data += 2 + 2 * read_le_u16(data);
                        }
                    }
                }
            }
            data += (nuint)data & 0x01;
        }

        for (int t = 0; t < num; t++)
        {
            ei[t].precomp.indexTable = data;
            need(data, size[(t * 2 + 0) * 3 + 0], end);
            data += size[(t * 2 + 0) * 3 + 0];
            if (split)
            {
                ei[num + t].precomp.indexTable = data;
                need(data, size[(t * 2 + 1) * 3 + 0], end);
                data += size[(t * 2 + 1) * 3 + 0];
            }
        }

        for (int t = 0; t < num; t++)
        {
            ei[t].precomp.sizeTable = (ushort*)data;
            need(data, size[(t * 2 + 0) * 3 + 1], end);
            data += size[(t * 2 + 0) * 3 + 1];
            if (split)
            {
                ei[num + t].precomp.sizeTable = (ushort*)data;
                need(data, size[(t * 2 + 1) * 3 + 1], end);
                data += size[(t * 2 + 1) * 3 + 1];
            }
        }

        for (int t = 0; t < num; t++)
        {
            data = (byte*)(((nuint)data + 0x3f) & ~(nuint)0x3f);
            ei[t].precomp.data = data;
            need(data, size[(t * 2 + 0) * 3 + 2], end);
            data += size[(t * 2 + 0) * 3 + 2];
            if (split)
            {
                data = (byte*)(((nuint)data + 0x3f) & ~(nuint)0x3f);
                ei[num + t].precomp.data = data;
                need(data, size[(t * 2 + 1) * 3 + 2], end);
                data += size[(t * 2 + 1) * 3 + 2];
            }
        }
        need(data, 0, end);   // the final data pointer lies within the file
    }

    // Returns the first two bytes of the decompressed symbol (w[0] | w[1] << 8).
    static int decompress_pairs(PairsData d, ulong idx)
    {
        if (d.idxBits == 0)
            return d.constValue0;

        uint mainIdx = (uint)(idx >> d.idxBits);
        int litIdx = unchecked((int)((idx & ((1UL << d.idxBits) - 1)) - (1UL << (d.idxBits - 1))));
        uint block = *(uint*)(d.indexTable + 6 * (ulong)mainIdx);

        ushort idxOffset = *(ushort*)(d.indexTable + 6 * (ulong)mainIdx + 4);
        litIdx += idxOffset;

        // block bounds: only a corrupt index table can leave them (cheap next to the decompression itself)
        uint numBlocks = d.numBlocks;
        if (block >= numBlocks) corrupt("block index");
        if (litIdx < 0)
            while (litIdx < 0)
            {
                if (block == 0) corrupt("block index");
                litIdx += d.sizeTable[--block] + 1;
            }
        else
            while (litIdx > d.sizeTable[block])
            {
                litIdx -= d.sizeTable[block++] + 1;
                if (block >= numBlocks) corrupt("block index");
            }

        uint* ptr = (uint*)(d.data + ((ulong)block << d.blockSize));

        int m = d.minLen;
        ushort* offset = d.offset;
        ulong[] @base = d.@base;
        byte[] symLen = d.symLen;
        uint sym, bitCnt;

        ulong code = BinaryPrimitives.ReverseEndianness(*(ulong*)ptr);

        ptr += 2;
        bitCnt = 0;   // number of "empty bits" in code
        for (; ; )
        {
            int l = m;
            while (code < @base[l - m]) l++;
            sym = offset[l - m];
            sym += (uint)((code - @base[l - m]) >> (64 - l));
            if (litIdx < symLen[sym] + 1) break;
            litIdx -= symLen[sym] + 1;
            code <<= l;
            bitCnt += (uint)l;
            if (bitCnt >= 32)
            {
                bitCnt -= 32;
                uint tmp = BinaryPrimitives.ReverseEndianness(*ptr++);
                code |= (ulong)tmp << (int)bitCnt;
            }
        }

        byte* symPat = d.symPat;
        while (symLen[sym] != 0)
        {
            byte* w = symPat + 3 * sym;
            int s1 = ((w[1] & 0xf) << 8) | w[0];
            if (litIdx < symLen[s1] + 1)
                sym = (uint)s1;
            else
            {
                litIdx -= symLen[s1] + 1;
                sym = (uint)((w[2] << 4) | (w[1] >> 4));
            }
        }

        byte* r = symPat + 3 * sym;
        return r[0] | (r[1] << 8);
    }

    // p[i] is to contain the square 0-63 (A1-H8) for a piece of type pc[i] ^ flip, where 1 = white pawn, ...,
    // 14 = black king and pc ^ flip flips between white and black if flip == true.
    // Pieces of the same type are guaranteed to be consecutive.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static int fill_squares(Pos* pos, byte[] pc, bool flip, int mirror, int* p, int i)
    {
        bool color = ColorOfPiece(pc[i]);
        if (flip) color = !color;
        ulong bb = pieces_by_type(pos, color, TypeOfPiece(pc[i]));
        do
        {
            p[i++] = lsb(bb) ^ mirror;
            bb = poplsb(bb);
        } while (bb != 0);
        return i;
    }

    static string prt_str(Pos* pos, bool flip)
    {
        var sb = new System.Text.StringBuilder(16);
        bool color = !flip;   // true = white
        for (int pt = KING; pt >= PAWN; pt--)
            for (int i = popcount(pieces_by_type(pos, color, pt)); i > 0; i--)
                sb.Append(piece_to_char[pt]);
        sb.Append('v');
        color = !color;
        for (int pt = KING; pt >= PAWN; pt--)
            for (int i = popcount(pieces_by_type(pos, color, pt)); i > 0; i--)
                sb.Append(piece_to_char[pt]);
        return sb.ToString();
    }

    static int probe_table(Pos* pos, int s, int* success, int type)
    {
        // Obtain the position's material-signature key
        ulong key = calc_key(pos, false);

        // Test for KvK
        if (type == WDL && key == 0UL)
            return 0;

        BaseEntry be = find_entry(Volatile.Read(ref tbHash), key);
        if (be == null)
        {
            *success = 0;
            return 0;
        }

        if ((type == DTM && !be.hasDtm) || (type == DTZ && !be.hasDtz) || type == DTM)
        {
            *success = 0;
            return 0;
        }

        // double-checked locking; a table that failed to load is flagged per type (a bad DTZ file leaves the WDL
        // probes of the same material working, and both hash slots of the entry see the flag)
        if (!Volatile.Read(ref be.ready[type]))
        {
            if (Volatile.Read(ref be.failed[type]))
            {
                *success = 0;
                return 0;
            }
            lock (tbMutex)
            {
                if (!Volatile.Read(ref be.ready[type]))
                {
                    if (Volatile.Read(ref be.failed[type]))
                    {
                        *success = 0;
                        return 0;
                    }
                    string str = prt_str(pos, be.key != key);
                    if (!init_table(be, str, type))
                    {
                        Volatile.Write(ref be.failed[type], true);
                        *success = 0;
                        return 0;
                    }
                    Volatile.Write(ref be.ready[type], true);
                }
            }
        }

        bool bside, flip;
        if (!be.symmetric)
        {
            flip = key != be.key;
            bside = pos->turn == flip;
        }
        else
        {
            flip = !pos->turn;
            bside = false;
        }

        int* p = stackalloc int[TB_PIECES];
        ulong idx;
        int t = 0;
        byte flags = 0;
        EncInfo ei;

        if (!be.hasPawns)
        {
            if (type == DTZ)
            {
                flags = be.dtzFlags[0];
                if ((flags & 1) != (bside ? 1 : 0) && !be.symmetric)
                {
                    *success = -1;
                    return 0;
                }
            }
            ei = type != DTZ ? be.eiWdl[bside ? 1 : 0] : be.eiDtz[0];
            for (int i = 0; i < be.num;)
                i = fill_squares(pos, ei.pieces, flip, 0, p, i);
            idx = encode(p, ei, be, PIECE_ENC);
        }
        else
        {
            ei = type == WDL ? be.eiWdl[0] : be.eiDtz[0];
            int i = fill_squares(pos, ei.pieces, flip, flip ? 0x38 : 0, p, 0);
            t = leading_pawn(p, be, FILE_ENC);
            if (type == DTZ)
            {
                flags = be.dtzFlags[t];
                if ((flags & 1) != (bside ? 1 : 0) && !be.symmetric)
                {
                    *success = -1;
                    return 0;
                }
            }
            ei = type == WDL ? be.eiWdl[t + 4 * (bside ? 1 : 0)] : be.eiDtz[t];
            while (i < be.num)
                i = fill_squares(pos, ei.pieces, flip, flip ? 0x38 : 0, p, i);
            idx = encode(p, ei, be, FILE_ENC);
        }

        int w = decompress_pairs(ei.precomp, idx);

        if (type == WDL)
            return (w & 0xff) - 2;

        int v = (w & 0xff) + (((w >> 8) & 0x0f) << 8);

        if ((flags & 2) != 0)
        {
            int m = WdlToMap[s + 2];
            if ((flags & 16) == 0)
                v = be.dtzMap[be.dtzMapIdx[t * 4 + m] + v];
            else
                v = ((ushort*)be.dtzMap)[be.dtzMapIdx[t * 4 + m] + v];
        }
        if ((flags & PAFlags[s + 2]) == 0 || (s & 1) != 0)
            v *= 2;

        return v;
    }

    static int probe_wdl_table(Pos* pos, int* success) => probe_table(pos, 0, success, WDL);
    static int probe_dtz_table(Pos* pos, int wdl, int* success) => probe_table(pos, wdl, success, DTZ);

    // probe_ab() is not called for positions with en passant captures.
    static int probe_ab(Pos* pos, int alpha, int beta, int* success)
    {
        ushort* moves0 = stackalloc ushort[MaxCaptures];
        ushort* m = moves0;
        // Generate (at least) all legal captures including (under)promotions.
        ushort* end = gen_captures(pos, m);
        for (; m < end; m++)
        {
            Pos pos1;
            int move = *m;
            if (!is_capture(pos, move))
                continue;
            if (!do_move(&pos1, pos, move))
                continue;   // illegal move
            int vv = -probe_ab(&pos1, -beta, -alpha, success);
            if (*success == 0) return 0;
            if (vv > alpha)
            {
                if (vv >= beta)
                    return vv;
                alpha = vv;
            }
        }

        int v = probe_wdl_table(pos, success);

        return alpha >= v ? alpha : v;
    }

    // Probe the WDL table for a particular position.
    // If *success != 0, the probe was successful.
    // If *success == 2, the position has a winning capture, or the position is a cursed win and has a cursed
    // winning capture, or the position has an ep capture as only best move. This is used in probe_dtz().
    // The return value is from the point of view of the side to move:
    // -2 : loss, -1 : loss but draw under 50-move rule, 0 : draw, 1 : win but draw under 50-move rule, 2 : win
    static int probe_wdl(Pos* pos, int* success)
    {
        *success = 1;

        // Fail early (before the capture resolution) when there is no table for this material (KvK needs none).
        ulong key = calc_key(pos, false);
        if (key != 0)
        {
            BaseEntry be = find_entry(Volatile.Read(ref tbHash), key);
            if (be == null || Volatile.Read(ref be.failed[WDL]))
            {
                *success = 0;
                return 0;
            }
        }

        // Generate (at least) all legal captures including (under)promotions.
        ushort* moves0 = stackalloc ushort[MaxCaptures];
        ushort* m = moves0;
        ushort* end = gen_captures(pos, m);
        int bestCap = -3, bestEp = -3;

        // We do capture resolution, letting bestCap keep track of the best capture without ep rights and letting
        // bestEp keep track of still better ep captures if they exist.
        for (; m < end; m++)
        {
            Pos pos1;
            int move = *m;
            if (!is_capture(pos, move))
                continue;
            if (!do_move(&pos1, pos, move))
                continue;   // illegal move
            int vv = -probe_ab(&pos1, -2, -bestCap, success);
            if (*success == 0) return 0;
            if (vv > bestCap)
            {
                if (vv == 2)
                {
                    *success = 2;
                    return 2;
                }
                if (!is_en_passant(pos, move))
                    bestCap = vv;
                else if (vv > bestEp)
                    bestEp = vv;
            }
        }

        int v = probe_wdl_table(pos, success);
        if (*success == 0) return 0;

        // Now max(v, bestCap) is the WDL value of the position without ep rights. If the position without ep
        // rights is not stalemate or no ep captures exist, then the value of the position is
        // max(v, bestCap, bestEp). If the position without ep rights is stalemate and bestEp > -3, then the value
        // of the position is bestEp (and we will have v == 0).
        if (bestEp > bestCap)
        {
            if (bestEp > v)
            {   // ep capture (possibly cursed losing) is best.
                *success = 2;
                return bestEp;
            }
            bestCap = bestEp;
        }

        // Now max(v, bestCap) is the WDL value of the position unless the position without ep rights is
        // stalemate and bestEp > -3.
        if (bestCap >= v)
        {
            // No need to test for the stalemate case here: either there are non-ep captures, or
            // bestCap == bestEp >= v anyway.
            *success = 1 + (bestCap > 0 ? 1 : 0);
            return bestCap;
        }

        // Now handle the stalemate case.
        if (bestEp > -3 && v == 0)
        {
            ushort* moves = stackalloc ushort[MaxMoves];
            ushort* end2 = gen_moves(pos, moves);
            // Check for stalemate in the position with ep captures.
            for (m = moves; m < end2; m++)
                if (!is_en_passant(pos, *m) && legal_move(pos, *m)) break;
            if (m == end2 && !is_check(pos))
            {
                // stalemate score from tb (w/o e.p.), but an en-passant capture is possible.
                *success = 2;
                return bestEp;
            }
        }
        // Stalemate / en passant not an issue, so v is the correct value.
        return v;
    }

    static readonly int[] WdlToDtz = { -1, -101, 0, 101, 1 };

    // Probe the DTZ table for a particular position.
    // If *success != 0, the probe was successful.
    // The return value is from the point of view of the side to move:
    //         n < -100 : loss, but draw under 50-move rule
    // -100 <= n < -1   : loss in n ply (assuming 50-move counter == 0)
    //         0        : draw
    //     1 < n <= 100 : win in n ply (assuming 50-move counter == 0)
    //   100 < n        : win, but draw under 50-move rule
    // If the position is mate, -1 is returned instead of 0.
    // The return value n can be off by 1: a return value -n can mean a loss in n+1 ply and a return value +n can
    // mean a win in n+1 ply. This cannot happen for tables with positions exactly on the "edge" of the 50-move rule.
    // This means that if dtz > 0 is returned, the position is certainly a win if dtz + 50-move-counter <= 99.
    // Care must be taken that the engine picks moves that preserve dtz + 50-move-counter <= 99.
    // If n = 100 immediately after a capture or pawn move, then the position is also certainly a win, and during
    // the whole phase until the next capture or pawn move, the inequality to be preserved is
    // dtz + 50-movecounter <= 100.
    // In short, if a move is available resulting in dtz + 50-move-counter <= 99, then do not accept moves leading
    // to dtz + 50-move-counter == 100.
    static int probe_dtz(Pos* pos, int* success)
    {
        int wdl = probe_wdl(pos, success);
        if (*success == 0) return 0;

        // If draw, then dtz = 0.
        if (wdl == 0) return 0;

        // Check for winning capture or en passant capture as only best move.
        if (*success == 2)
            return WdlToDtz[wdl + 2];

        ushort* moves = stackalloc ushort[MaxMoves];
        ushort* m = moves, end = null;
        Pos pos1;

        // If winning, check for a winning pawn move.
        if (wdl > 0)
        {
            // Generate at least all legal non-capturing pawn moves including non-capturing promotions.
            // (The following call in fact generates all moves.)
            end = gen_legal(pos, moves);

            for (m = moves; m < end; m++)
            {
                int move = *m;
                if (type_of_piece_moved(pos, move) != PAWN || is_capture(pos, move))
                    continue;
                if (!do_move(&pos1, pos, move))
                    continue;   // not legal
                int vv = -probe_wdl(&pos1, success);
                if (*success == 0) return 0;
                if (vv == wdl)
                    return WdlToDtz[wdl + 2];
            }
        }

        // If we are here, we know that the best move is not an ep capture. In other words, the value of wdl
        // corresponds to the WDL value of the position without ep rights. It is therefore safe to probe the DTZ
        // table with the current value of wdl.
        int dtz = probe_dtz_table(pos, wdl, success);
        if (*success >= 0)
            return WdlToDtz[wdl + 2] + (wdl > 0 ? dtz : -dtz);

        // *success < 0 means we need to probe DTZ for the other side to move.
        int best;
        if (wdl > 0)
        {
            best = int.MaxValue;
        }
        else
        {
            // If (cursed) loss, the worst case is a losing capture or pawn move as the "best" move, leading to
            // dtz of -1 or -101. In case of mate, this will cause -1 to be returned.
            best = WdlToDtz[wdl + 2];
            // If wdl < 0, we still have to generate all moves.
            end = gen_moves(pos, m);
        }

        for (m = moves; m < end; m++)
        {
            int move = *m;
            // We can skip pawn moves and captures. If wdl > 0, we already caught them. If wdl < 0, the initial
            // value of best already takes account of them.
            if (is_capture(pos, move) || type_of_piece_moved(pos, move) == PAWN)
                continue;
            if (!do_move(&pos1, pos, move))
                continue;   // move was not legal
            int vv = -probe_dtz(&pos1, success);
            // Check for the case of mate in 1
            if (vv == 1 && is_mate(&pos1))
                best = 1;
            else if (wdl > 0)
            {
                if (vv > 0 && vv + 1 < best)
                    best = vv + 1;
            }
            else
            {
                if (vv - 1 < best)
                    best = vv - 1;
            }
            if (*success == 0) return 0;
        }
        return best;
    }

    // Use the DTZ tables to rank and score all root moves in the list.
    // A return value of false means that not all probes were successful.
    static bool root_probe_dtz(Pos* pos, bool hasRepeated, bool useRule50, int* rmMoves, int* rmRank, int* rmScore, out int count)
    {
        int v, success;
        count = 0;

        // Obtain 50-move counter for the root position.
        int cnt50 = pos->rule50;

        // The border between draw and win lies at rank 1 or rank 900, depending on whether the 50-move rule is used.
        int bound = useRule50 ? 900 : 1;

        // Probe, rank and score each move.
        ushort* rootMoves = stackalloc ushort[MaxMoves];
        ushort* end = gen_legal(pos, rootMoves);
        int size = (int)(end - rootMoves);
        Pos pos1;
        for (int i = 0; i < size; i++)
        {
            int move = rootMoves[i];
            rmMoves[i] = move;
            do_move(&pos1, pos, move);

            // Calculate dtz for the current move counting from the root position.
            if (pos1.rule50 == 0)
            {
                // If the move resets the 50-move counter, dtz is -101/-1/0/1/101.
                v = -probe_wdl(&pos1, &success);
                v = WdlToDtz[v + 2];
            }
            else
            {
                // Otherwise, take dtz for the new position and correct by 1 ply.
                v = -probe_dtz(&pos1, &success);
                if (v > 0) v++;
                else if (v < 0) v--;
            }
            // Make sure that a mating move gets value 1.
            if (v == 2 && is_mate(&pos1))
                v = 1;

            if (success == 0) return false;

            // Better moves are ranked higher. Guaranteed wins are ranked equally. Losing moves are ranked equally
            // unless a 50-move draw is in sight. Note that moves ranked 900 have dtz + cnt50 == 100, which in rare
            // cases may be insufficient to win as dtz may be one off (see the comments before probe_dtz()).
            int r = v > 0 ? (v + cnt50 <= 99 && !hasRepeated ? 1000 : 1000 - (v + cnt50))
                  : v < 0 ? (-v * 2 + cnt50 < 100 ? -1000 : -1000 + (-v + cnt50))
                  : 0;
            rmRank[i] = r;

            // Determine the score to be displayed for this move. Assign at least 1 cp to cursed wins and let it
            // grow to 49 cp as the position gets closer to a real win.
            // NOTE: this reproduces Fathom exactly, whose unparenthesised max/min macros make the expression
            //   r > 0 ? (3 > r - 800 ? 3 : r - 800 * TB_VALUE_PAWN / 200) : ...
            rmScore[i] = r >= bound ? TB_VALUE_MATE - TB_MAX_MATE_PLY - 1
                       : r > 0 ? (3 > r - 800 ? 3 : r - 800 * TB_VALUE_PAWN / 200)
                       : r == 0 ? TB_VALUE_DRAW
                       : r > -bound ? (-3 < r + 800 ? -3 : r + 800 * TB_VALUE_PAWN / 200)
                       : -TB_VALUE_MATE + TB_MAX_MATE_PLY + 1;
        }
        count = size;
        return true;
    }

    static readonly int[] WdlToRank = { -1000, -899, 0, 899, 1000 };
    static readonly int[] WdlToValue = {
        -TB_VALUE_MATE + TB_MAX_MATE_PLY + 1,
        TB_VALUE_DRAW - 2,
        TB_VALUE_DRAW,
        TB_VALUE_DRAW + 2,
        TB_VALUE_MATE - TB_MAX_MATE_PLY - 1
    };

    // Use the WDL tables to rank all root moves in the list. This is a fallback for the case that some or all
    // DTZ tables are missing. A return value of false means that not all probes were successful.
    static bool root_probe_wdl(Pos* pos, bool useRule50, int* rmMoves, int* rmRank, int* rmScore, out int count)
    {
        int v, success;
        count = 0;
        ushort* moves = stackalloc ushort[MaxMoves];
        ushort* end = gen_legal(pos, moves);
        int size = (int)(end - moves);
        Pos pos1;
        for (int i = 0; i < size; i++)
        {
            rmMoves[i] = moves[i];
            do_move(&pos1, pos, moves[i]);
            v = -probe_wdl(&pos1, &success);
            if (success == 0) return false;
            if (!useRule50)
                v = v > 0 ? 2 : v < 0 ? -2 : 0;
            rmRank[i] = WdlToRank[v + 2];
            rmScore[i] = WdlToValue[v + 2];
        }
        count = size;
        return true;
    }

    static int dtz_to_wdl(int cnt50, int dtz)
    {
        int wdl = 0;
        if (dtz > 0)
            wdl = dtz + cnt50 <= 100 ? 2 : 1;
        else if (dtz < 0)
            wdl = -dtz + cnt50 <= 100 ? -2 : -1;
        return wdl + 2;
    }

    // The original Fathom root probe (tb_probe_root). Returns the Fathom move (0 = failure, MOVE_CHECKMATE,
    // MOVE_STALEMATE); fills per-move results (move, wdl, dtz) for the legal moves in generation order.
    static int probe_root(Pos* pos, out int score, int* resMove, int* resWdl, int* resDtz, out int resCount)
    {
        score = 0;
        resCount = 0;
        int success;
        int dtz = probe_dtz(pos, &success);
        if (success == 0)
            return 0;

        short* scores = stackalloc short[MaxMoves];
        ushort* moves = stackalloc ushort[MaxMoves];
        ushort* end = gen_moves(pos, moves);
        int len = (int)(end - moves);
        int num_draw = 0;
        int j = 0;
        for (int i = 0; i < len; i++)
        {
            Pos pos1;
            if (!do_move(&pos1, pos, moves[i]))
            {
                scores[i] = SCORE_ILLEGAL;
                continue;
            }
            int v = 0;
            if (dtz > 0 && is_mate(&pos1))
                v = 1;
            else
            {
                if (pos1.rule50 != 0)
                {
                    v = -probe_dtz(&pos1, &success);
                    if (v > 0) v++;
                    else if (v < 0) v--;
                }
                else
                {
                    v = -probe_wdl(&pos1, &success);
                    v = WdlToDtz[v + 2];
                }
            }
            num_draw += v == 0 ? 1 : 0;
            if (success == 0)
                return 0;
            scores[i] = (short)v;
            resMove[j] = moves[i];
            resWdl[j] = dtz_to_wdl(pos->rule50, v);
            resDtz[j] = (v < 0 ? -v : v) & 0xFFF;   // TB_SET_DTZ keeps 12 bits
            j++;
        }
        resCount = j;
        score = dtz;

        // Now be a bit smart about filtering out moves.
        if (dtz > 0)   // winning (or 50-move rule draw)
        {
            int best = BEST_NONE;
            int best_move = 0;
            for (int i = 0; i < len; i++)
            {
                int v = scores[i];
                if (v == SCORE_ILLEGAL)
                    continue;
                if (v > 0 && v < best)
                {
                    best = v;
                    best_move = moves[i];
                }
            }
            return best == BEST_NONE ? 0 : best_move;
        }
        else if (dtz < 0)   // losing (or 50-move rule draw)
        {
            int best = 0;
            int best_move = 0;
            for (int i = 0; i < len; i++)
            {
                int v = scores[i];
                if (v == SCORE_ILLEGAL)
                    continue;
                if (v < best)
                {
                    best = v;
                    best_move = moves[i];
                }
            }
            return best == 0 ? MOVE_CHECKMATE : best_move;
        }
        else   // drawing
        {
            // Check for stalemate:
            if (num_draw == 0)
                return MOVE_STALEMATE;

            // Select a "random" move that preserves the draw. Uses calc_key as the PRNG.
            ulong count = calc_key(pos, !pos->turn) % (ulong)num_draw;
            for (int i = 0; i < len; i++)
            {
                int v = scores[i];
                if (v == SCORE_ILLEGAL)
                    continue;
                if (v == 0)
                {
                    if (count == 0)
                        return moves[i];
                    count--;
                }
            }
            return 0;
        }
    }

    // ---------------------------------------------------------------- public API (FEN)
    static string SquareName(int s) => $"{(char)('a' + file(s))}{(char)('1' + rank(s))}";

    /// <summary>A Fathom move (promotes&lt;&lt;12 | from&lt;&lt;6 | to) in UCI.</summary>
    static string ToUci(int tbMove)
    {
        string promo = move_promotes(tbMove) switch
        {
            TB_PROMOTES_QUEEN => "q", TB_PROMOTES_ROOK => "r", TB_PROMOTES_BISHOP => "b", TB_PROMOTES_KNIGHT => "n", _ => ""
        };
        return SquareName(move_from(tbMove)) + SquareName(move_to(tbMove)) + promo;
    }

    /// <summary>
    /// The position of a FEN, as Fathom's apps/fathom.c reads it (placement, side, castling, en passant, halfmove
    /// clock); false for a malformed FEN or one with castling rights (tablebases have none).
    /// </summary>
    static bool ParseFen(string fen, out Pos pos)
    {
        pos = default;
        if (string.IsNullOrWhiteSpace(fen)) return false;
        string[] f = fen.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries);
        if (f.Length < 2) return false;
        int r = 7, c = 0;
        foreach (char ch in f[0])
        {
            if (ch == '/') { if (c != 8 || r == 0) return false; r--; c = 0; continue; }
            if (ch >= '1' && ch <= '8') { c += ch - '0'; if (c > 8) return false; continue; }
            if (c >= 8) return false;
            ulong b = board(r * 8 + c);
            switch (char.ToLowerInvariant(ch))
            {
                case 'k': pos.kings |= b; break;
                case 'q': pos.queens |= b; break;
                case 'r': pos.rooks |= b; break;
                case 'b': pos.bishops |= b; break;
                case 'n': pos.knights |= b; break;
                case 'p': pos.pawns |= b; break;
                default: return false;
            }
            if (char.IsUpper(ch)) pos.white |= b; else pos.black |= b;
            c++;
        }
        if (r != 0 || c != 8) return false;
        if (f[1] != "w" && f[1] != "b") return false;
        pos.turn = f[1] == "w";
        if (f.Length > 2 && f[2] != "-") return false;
        if (f.Length > 3 && f[3] != "-")
        {
            string ep = f[3];
            if (ep.Length != 2 || ep[0] < 'a' || ep[0] > 'h' || ep[1] < '1' || ep[1] > '8') return false;
            pos.ep = (byte)((ep[1] - '1') * 8 + (ep[0] - 'a'));
        }
        if (f.Length > 4)
        {
            if (!int.TryParse(f[4], out int rule50) || rule50 < 0) return false;
            pos.rule50 = (byte)Math.Min(rule50, 255);
        }
        return true;
    }

    // The en passant square (0 = none) must be on the 6th rank (from the mover's view), empty, with an enemy pawn
    // just past it and its origin square empty, as after a double push.
    static bool ep_ok(Pos* pos)
    {
        int ep = pos->ep;
        if (ep == 0) return true;
        if (ep >= 64) return false;
        ulong occ = pos->white | pos->black;
        if ((occ & board(ep)) != 0) return false;
        if (pos->turn)
            return rank(ep) == 5 && (pos->pawns & pos->black & board(ep - 8)) != 0 && (occ & board(ep + 8)) == 0;
        return rank(ep) == 2 && (pos->pawns & pos->white & board(ep + 8)) != 0 && (occ & board(ep - 8)) == 0;
    }

    // Cheap validity guard of every public probe, before any table access: exactly one king per side, consistent
    // bitboards, no pawns on the back ranks, side not to move not in check (is_valid), a sane ep square and a piece
    // count the tables cover. The probe code indexes the raw attack tables with lsb() of the king bitboards, so
    // this must reject a bad position up front (lsb(0) = 64 would read out of bounds: not catchable).
    static bool probe_ok(Pos* pos)
    {
        int n = popcount(pos->white | pos->black);
        return n <= TB_LARGEST && n <= TB_PIECES && is_valid(pos) && ep_ok(pos);
    }

    /// <summary>
    /// WDL probe of a FEN (Fathom's probe_wdl). The result ASSUMES THE 50-MOVE COUNTER IS 0 (the FEN's halfmove clock
    /// is ignored): exact only right after a capture or pawn move. Fails with castling rights, more pieces than
    /// MaxPieces, an invalid position, a missing or corrupt table. Thread-safe.
    /// </summary>
    public static TbWdl ProbeWdl(string fen)
    {
        try
        {
            if (!ParseFen(fen, out Pos pos)) return TbWdl.Failed;
            pos.rule50 = 0;
            if (!probe_ok(&pos)) return TbWdl.Failed;
            int success;
            int v = probe_wdl(&pos, &success);
            return success == 0 ? TbWdl.Failed : (TbWdl)(v + 2);
        }
        catch (Exception) { return TbWdl.Failed; }
    }

    /// <summary>
    /// The original Fathom root probe (tb_probe_root), as fathom.exe prints it: the root WDL (rule50 taken into
    /// account), |DTZ|, the best move (fastest win / slowest loss / a drawing move) and every legal move's WDL and
    /// DTZ. A checkmated root gives Checkmate with Wdl = Loss (Fathom reports TB_WIN there). Thread-safe.
    /// </summary>
    public static TbRootResult ProbeRoot(string fen)
    {
        var r = new TbRootResult();
        try
        {
            if (!ParseFen(fen, out Pos pos)) return r;
            if (!probe_ok(&pos)) return r;
            int* rm = stackalloc int[MaxMoves];
            int* rw = stackalloc int[MaxMoves];
            int* rd = stackalloc int[MaxMoves];
            int move = probe_root(&pos, out int dtz, rm, rw, rd, out int n);
            if (move == 0) return r;
            var moves = new TbMoveResult[n];
            for (int i = 0; i < n; i++)
                moves[i] = new TbMoveResult { Uci = ToUci(rm[i]), Wdl = (TbWdl)rw[i], Dtz = rd[i] };
            r.Moves = moves;
            r.Ok = true;
            if (move == MOVE_CHECKMATE) { r.Checkmate = true; r.Wdl = TbWdl.Loss; return r; }
            if (move == MOVE_STALEMATE) { r.Stalemate = true; r.Wdl = TbWdl.Draw; return r; }
            r.Wdl = (TbWdl)dtz_to_wdl(pos.rule50, dtz);
            r.Dtz = (dtz < 0 ? -dtz : dtz) & 0xFFF;
            r.BestMove = ToUci(move);
            return r;
        }
        catch (Exception)
        {
            return new TbRootResult();
        }
    }
}
