# EngineDef.json for LC0 example - copy this file as template

```
{
      "Name": "Lc0 BT4 Base 6147K",
      "Alias":"LC0",
      "Version": "Lc0 v0.31.2",
      "TimeControlID": 1,
      "Rating" : 3580,
      "Dev": "by The LCZero Authors.",
      "LogoPath": "img/lc0.png",
      "Protocol": "UCI",
      "ContemptEnabled": false,
      "NegativeContemptAllowed": false,
      "Path": "C:/Dev/Chess/Engines/lc0/lc0.exe",
      "NetworkPath": "C:/Dev/Chess/Networks",
      "Args": "--show-hidden",
      "Options": {        
        "Threads": 2,
        "Backend": "demux",
        "BackendOptions": "backend=cuda-fp16,(gpu=0,policy_head=optimistic, value_head=winner)",
        "WeightsFile": "C:/Dev/Chess/Networks/BT4/BT4-1024x15x32h-swa-6147500.pb.gz",        
        "SmartPruningFactor":2,        
        "MoveOverheadMs": 250,
        "MinibatchSize": 340,
        "CPuct": 2.897,
        "CPuctBase": 45669,
        "CPuctFactor": 3.973,
        "FpuValue": 0.98416,
        "PolicyTemperature": 1.4,    
        "ScoreType": "WDL_mu",
        "StrictTiming": true,
        "TimeManager": "legacy(book-ply-bonus=0.4)",
        "SyzygyPath": "C:/Dev/Chess/TableBases/sygyzy",
        "VerboseMoveStats": true,        
        "LogLiveStats": true,
        "UCI_ShowWDL": true,        
        "Ponder": false
      }
    }

```

