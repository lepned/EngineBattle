namespace EngineBattle.Tablebases;

/// <summary>
/// The attack tables Syzygy.cs probes with (Fathom's TB_*_ATTACKS hooks), a1 = 0. Engines often build their slider attacks with
/// BMI2 (PEXT), which is x86 only; EngineBattle also runs on ARM (Apple Silicon, Linux), so sliders here walk the rays.
/// A probe makes a few hundred lookups at most, so this costs nothing that shows.
/// </summary>
static class Attacks
{
    public static readonly ulong[] KnightAtt = new ulong[64];
    public static readonly ulong[] KingAtt = new ulong[64];
    /// <summary>[0 * 64 + sq] white pawn attacks (towards rank 8), [1 * 64 + sq] black.</summary>
    public static readonly ulong[] PawnAtt = new ulong[2 * 64];

    static readonly (int dr, int df)[] RookDirs = { (1, 0), (-1, 0), (0, 1), (0, -1) };
    static readonly (int dr, int df)[] BishopDirs = { (1, 1), (1, -1), (-1, 1), (-1, -1) };

    static Attacks()
    {
        (int, int)[] knight = { (1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2) };
        for (int sq = 0; sq < 64; sq++)
        {
            int r = sq >> 3, f = sq & 7;
            ulong n = 0, k = 0;
            foreach (var (dr, df) in knight)
                if (On(r + dr, f + df)) n |= 1UL << ((r + dr) * 8 + f + df);
            for (int dr = -1; dr <= 1; dr++)
                for (int df = -1; df <= 1; df++)
                    if ((dr != 0 || df != 0) && On(r + dr, f + df)) k |= 1UL << ((r + dr) * 8 + f + df);
            KnightAtt[sq] = n;
            KingAtt[sq] = k;
            ulong wp = 0, bp = 0;
            if (r < 7) { if (f > 0) wp |= 1UL << (sq + 7); if (f < 7) wp |= 1UL << (sq + 9); }
            if (r > 0) { if (f > 0) bp |= 1UL << (sq - 9); if (f < 7) bp |= 1UL << (sq - 7); }
            PawnAtt[sq] = wp;
            PawnAtt[64 + sq] = bp;
        }
    }

    static bool On(int r, int f) => r >= 0 && r < 8 && f >= 0 && f < 8;

    static ulong Slide(int sq, ulong occ, (int dr, int df)[] dirs)
    {
        ulong a = 0;
        int r0 = sq >> 3, f0 = sq & 7;
        foreach (var (dr, df) in dirs)
            for (int r = r0 + dr, f = f0 + df; On(r, f); r += dr, f += df)
            {
                ulong b = 1UL << (r * 8 + f);
                a |= b;
                if ((occ & b) != 0) break;
            }
        return a;
    }

    public static ulong Rook(int sq, ulong occ) => Slide(sq, occ, RookDirs);
    public static ulong Bishop(int sq, ulong occ) => Slide(sq, occ, BishopDirs);
}
