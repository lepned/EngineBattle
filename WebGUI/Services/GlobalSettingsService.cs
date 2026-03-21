using System.Text.Json;

namespace WebGUI.Services;

public class GlobalSettingsService
{
    private GlobalSettings _settings;
    private readonly string _filePath;
    private readonly object _lock = new();
    private static readonly JsonSerializerOptions JsonOptions = new() { WriteIndented = true };

#nullable enable
    public event Action? OnSettingsChanged;
#nullable restore

    // Captures OnSettingsChanged under lock, invokes outside lock to avoid deadlock

    public GlobalSettings Settings
    {
        get { lock (_lock) return _settings; }
    }

    public GlobalSettingsService()
    {
        _filePath = Path.Combine(AppPaths.BaseDir, "Data", "globalSettings.json");
        _settings = Load();
    }

    public void Save(GlobalSettings settings)
    {
#nullable enable
        Action? handler;
#nullable restore
        lock (_lock)
        {
            _settings = settings;
            var json = JsonSerializer.Serialize(settings, JsonOptions);
            var dir = Path.GetDirectoryName(_filePath)!;
            if (!Directory.Exists(dir))
                Directory.CreateDirectory(dir);
            File.WriteAllText(_filePath, json);
            handler = OnSettingsChanged;
        }
        handler?.Invoke();
    }

    private GlobalSettings Load()
    {
        if (!File.Exists(_filePath))
            return new GlobalSettings();

        try
        {
            var json = File.ReadAllText(_filePath);
            return JsonSerializer.Deserialize<GlobalSettings>(json, JsonOptions) ?? new GlobalSettings();
        }
        catch
        {
            return new GlobalSettings();
        }
    }
}
