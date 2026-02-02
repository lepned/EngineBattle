namespace ChessLibrary

open System
open System.IO

module TestsTypes =
  type ChessRecord =
    { Corr: float
      Ceres: float
      BT4: float
      SF: float
      V1: float
      Unc: float
      QDn: float
      QUp: float
      FEN: string }
    override this.ToString() =
      sprintf "Corr: %f Ceres: %f BT4: %f SF: %f v1: %f Unc: %f Qdn: %f Qup: %f" this.Corr this.Ceres this.BT4 this.SF this.V1 this.Unc this.QDn this.QUp

  let parseLine (line: string) =
      let parts = line.Split([|' '; ':'|], StringSplitOptions.RemoveEmptyEntries)
      { ChessRecord.Corr = float parts.[0]
        Ceres = float parts.[2]
        BT4 = float parts.[4]
        SF = float parts.[6]
        V1 = float parts.[7]
        Unc = float parts.[8]
        QDn = float parts.[9]
        QUp = float parts.[10]
        FEN = String.Join(" ", parts.[11..]) }

  let text = """
    1.00  Ceres:  0.99   BT4: 0.97   SF: 1.00      0.95   0.03   0.09   0.04    5rk1/1bqN2bp/p3p1p1/6B1/1Qpr4/2N4P/5PP1/R3R1K1 b - - 0 25
    0.00  Ceres:  0.96   BT4: 0.94   SF: 1.00      0.90   0.04   0.12   0.09    5rk1/1bqr2bp/p3R1p1/6B1/1Qp5/2N4P/5PP1/R5K1 b - - 0 26
    -1.00  Ceres:  0.86   BT4: 0.44   SF: 0.74      0.71   0.10   0.39   0.05    rnbqkb1r/pp1p1ppp/5n2/2pPp3/2P5/8/PP2PPPP/RNBQKBNR w KQkq e6 0 4
    -0.99  Ceres:  0.91   BT4: 0.40   SF: 0.81      0.78   0.07   0.35   0.03    rnbqkb1r/pp3p1p/3p1np1/2pPp3/2P1P3/2N5/PP3PPP/R1BQKBNR w KQkq - 0 6
    """

  let records filePath =
      File.ReadAllLines filePath
      |> Array.map parseLine
      |> Array.toList
