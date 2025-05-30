
module QBBOpsTests

open Xunit
open ChessLibrary.QBBOperations

module QBBOperationsTests =

    // --- Bitboard Helpers Tests ---

    [<Fact>]
    let ``RevBB reverses bitboard correctly`` () =
        // Reverse the bits of 0x0000000000000001.
        // Expected: the least-significant byte becomes the most-significant.
        let input = 0x0000000000000001UL
        let expected = 0x0100000000000000UL
        let result = RevBB input
        Assert.Equal(expected, result)

    [<Fact>]
    let ``MSB returns the index of the most significant bit`` () =
        // For a bitboard with only bit0 set, MSB should be 0.
        let result1 = MSB 0x1UL 
        Assert.Equal(0UL, result1)
        // For a bitboard with bit7 set (0x80) MSB should be 7.
        let result2 = MSB 0x80UL 
        Assert.Equal(7UL, result2)

    [<Fact>]
    let ``LSB returns the index of the least significant bit`` () =
        // For a bitboard with only bit0 set, LSB should be 0.
        let result1 = LSB 0x1UL
        Assert.Equal(0UL, result1)
        // For bitboard 0x8 (binary 1000), the LSB is at index 3.
        let result2 = LSB 0x8UL
        Assert.Equal(3UL, result2)

    [<Fact>]
    let ``ExtractLSB returns the lowest set bit`` () =
        // For bitboard 0xA (binary 1010) the lowest set bit is 0x2.
        let result = ExtractLSB 0xAUL
        Assert.Equal(0x2UL, result)

    [<Fact>]
    let ``ClearLSB clears only the lowest set bit`` () =
        // For bitboard 0xA (1010) clearing the least-significant set bit gives 0x8 (1000).
        let result = ClearLSB 0xAUL
        Assert.Equal(0x8UL, result)

    [<Fact>]
    let ``Pop returns the correct population count`` () =
        // For bitboard 0xA (binary 1010) there are 2 bits set.
        let result = Pop 0xAUL
        Assert.Equal(2UL, result)

    // --- Dictionary Lookups Tests ---

    [<Fact>]
    let ``Square name dictionaries work correctly for White and Black`` () =
        // Testing a few known squares.
        Assert.Equal(0, squareNameToNumberDictWhite.["a1"])
        Assert.Equal(7, squareNameToNumberDictWhite.["h1"])
        Assert.Equal(0, squareNameToNumberDictBlack.["a8"])
        // For Black dictionary, "h1" should be at index 63.
        Assert.Equal(63, squareNameToNumberDictBlack.["h1"])

    [<Fact>]
    let ``Square number to name dictionaries work correctly for White and Black`` () =
        Assert.Equal("a1", squareNumberToNameDictWhite.[0])
        Assert.Equal("h8", squareNumberToNameDictWhite.[63])
        Assert.Equal("a8", squareNumberToNameDictBlack.[0])
        Assert.Equal("h1", squareNumberToNameDictBlack.[63])

    // --- File Mask Functions Tests ---

    [<Fact>]
    let ``getAFileKingAttack returns expected bit using AFile mask`` () =
        // When passing the AFile constant as occupation, the function extracts the lowest set bit.
        let occupation = AFile
        let result = getAFileKingAttack occupation
        let expected = ExtractLSB (AFile &&& occupation)
        Assert.Equal(expected, result)

    // --- Knight Mask Tests ---

    [<Fact>]
    let ``getKnightMask returns correct mask for same start and end square`` () =
        // When start and end squares are equal, the mask should equal the knight destination for that square.
        let fromSq = 0
        let toSq = 0
        let expected = KnightDest.[fromSq]
        let result = getKnightMask fromSq toSq
        Assert.Equal(expected, result)

    // --- Bishop Mask Tests ---

    [<Fact>]
    let ``getBishopMask returns correct mask for same start and end square`` () =
        // When fromSq equals toSq the mask is simply the bishop attack mask for that square.
        let fromSq = 0
        let toSq = 0
        let expected = BishopAttacks.[fromSq]
        let result = getBishopMask fromSq toSq
        Assert.Equal(expected, result)

    // --- Utility Function Tests ---

    [<Fact>]
    let ``setBitsInRange sets bits correctly`` () =
        // For a range from 3 to 5, bits 3, 4, and 5 should be set.
        let result = setBitsInRange 3 5
        let expected = (1UL <<< 3) ||| (1UL <<< 4) ||| (1UL <<< 5)
        Assert.Equal(expected, result)

    // --- Pawn and King Mask Tests ---

    [<Fact>]
    let ``getPawnMask returns correct mask for same start and end square`` () =
        // For pawn mask with fromSq = 0 and toSq = 0, the mask should include bits at positions (0+7) and (0+9).
        let result = getPawnMask 0 0
        let expected = (1UL <<< (0 + 7)) ||| (1UL <<< (0 + 9))
        Assert.Equal(expected, result)

    [<Fact>]
    let ``getKingMask returns 0 for same start and end square`` () =
        // When start and end squares are the same, the king mask should return 0.
        let result = getKingMask 3 3
        Assert.Equal(0UL, result)

    // --- RookPlacementInfo Tests (Chess960) ---

    [<Fact>]
    let ``RookPlacementInfo defaults and property set/get work correctly`` () =
        // New instances default to rookInitPlacementBits = 0xFFFFus, which decodes to 14 for all properties.
        let mutable rpi = RookPlacementInfo()
        Assert.Equal(14uy, rpi.WhiteKRInitPlacement)
        Assert.Equal(14uy, rpi.WhiteQRInitPlacement)
        Assert.Equal(14uy, rpi.BlackKRInitPlacement)
        Assert.Equal(14uy, rpi.BlackQRInitPlacement)
        // Set new values and verify that get returns the same.
        rpi.WhiteKRInitPlacement <- 5uy
        rpi.WhiteQRInitPlacement <- 3uy
        rpi.BlackKRInitPlacement <- 7uy
        rpi.BlackQRInitPlacement <- 2uy
        Assert.Equal(5uy, rpi.WhiteKRInitPlacement)
        Assert.Equal(3uy, rpi.WhiteQRInitPlacement)
        Assert.Equal(7uy, rpi.BlackKRInitPlacement)
        Assert.Equal(2uy, rpi.BlackQRInitPlacement)

