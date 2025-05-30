using System.Collections.Concurrent;
using WebGUI.Services;

public class OverlaySetting
{
  private readonly ConcurrentDictionary<string, OverlaySettings> _overlaySettings = new();

  public double MinPolicyThreshold { get; set; } = 0.01;
  // Add or update overlay settings for a component by ID
  public void SetOverlaySetting(string componentId, OverlaySettings setting)
  {
    _overlaySettings[componentId] = setting;
  }

  //create a thread safe property to check if overlay settings are enabled
  public bool IsOverlaySettingsEnabled => _overlaySettings.Any(e => e.Value != OverlaySettings.None);

  // Get the overlay setting for a specific component by ID
  public OverlaySettings GetOverlaySetting(string componentId)
  {
    return _overlaySettings.TryGetValue(componentId, out var setting) ? setting : OverlaySettings.None;
  }

  // Check if a specific component has overlay settings enabled
  public bool HasOverlaySetting(string componentId)
  {
    return _overlaySettings.TryGetValue(componentId, out var setting) && setting != OverlaySettings.None;
  }

  // Remove overlay settings for a specific component by ID
  public void RemoveOverlaySetting(string componentId)
  {
    _overlaySettings.TryRemove(componentId, out _);
  }

  // Get all components with their overlay settings
  public IReadOnlyDictionary<string, OverlaySettings> GetAllOverlaySettings()
  {
    return _overlaySettings;
  }

  // create a method that checks for all overlay settings that are enabled
  public bool BothHasEnabledOverlaySettings()
  {
    return _overlaySettings.Values.All(setting => setting != OverlaySettings.None);
  }
}
