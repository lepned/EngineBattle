namespace WebGUI.Services
{
  using Microsoft.JSInterop;
  using System;
  using System.Threading.Tasks;

  public class JavaScriptInteropService
  {
    private IJSObjectReference _module;
    public IJSObjectReference Module { get; set; }

    public async Task<IJSObjectReference> ImportModuleAsync(IJSRuntime jsRuntime)
    {
      if (_module == null)
      {
        _module = await jsRuntime.InvokeAsync<IJSObjectReference>("import", "./js/chessInterop.js?v=1.87.0");
      }
      Module = _module;
      return _module;
    }
  }

  public class ClipboardService
  {
    private readonly JavaScriptInteropService _interopService;

    public ClipboardService(JavaScriptInteropService interopService)
    {
      _interopService = interopService;
    }

    public async Task<string> ReadTextAsync()
    {
      // Module is only set after some component ran ImportModuleAsync — a paste that
      // races the first page's OnAfterRenderAsync must not NullReference
      var mod = _interopService.Module;
      if (mod == null) return string.Empty;
      return await mod.InvokeAsync<string>("readTextFromClipboard");
    }
  }

}
