module GpuMonitorTests

open Xunit
open ChessLibrary.GpuMonitor

let private gpu index temp = { Index = index; Name = "RTX"; TempC = temp; MarginC = Some(90 - temp); LoadPercent = Some 98; PowerW = Some 412.4 }

[<Fact>]
let ``heat turns warm within 5 degrees of the card's limit and hot at it`` () =
    let at margin = heat { gpu 0 60 with MarginC = Some margin }
    Assert.Equal(Normal, at 6)
    Assert.Equal(Warm, at 5)
    Assert.Equal(Warm, at 1)
    Assert.Equal(Hot, at 0)
    Assert.Equal(Hot, at -2)

[<Fact>]
let ``a card that does not give its limit is taken to have it at 90`` () =
    let at temp = heat { gpu 0 temp with MarginC = None }
    Assert.Equal(Normal, at 84)
    Assert.Equal(Warm, at 85)
    Assert.Equal(Hot, at 90)

[<Fact>]
let ``the banner text lists every GPU and the colour follows the hottest`` () =
    Assert.Equal("67 °C", tempText [ gpu 0 67 ])
    Assert.Equal("67 · 90 °C", tempText [ gpu 0 67; gpu 1 90 ])
    Assert.Equal(Hot, hottest [ gpu 0 67; gpu 1 90 ])
    Assert.Equal(Warm, hottest [ gpu 0 67; gpu 1 88 ])
    Assert.Equal("", tempText [])
    Assert.Equal(Normal, hottest [])

[<Fact>]
let ``the tooltip names each GPU with its load and power, leaving out what the card does not say`` () =
    Assert.Equal("GPU 0 RTX: 67 °C (23 °C below its limit), 98 %, 412 W", detailText [ gpu 0 67 ])
    Assert.Equal("GPU 0 RTX: 91 °C (at its limit), 98 %, 412 W", detailText [ { gpu 0 91 with MarginC = Some -1 } ])
    Assert.Equal("GPU 1 RTX: 54 °C", detailText [ { gpu 1 54 with MarginC = None; LoadPercent = None; PowerW = None } ])
