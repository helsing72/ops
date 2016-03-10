using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

namespace Ops
{
  /// <summary>
  /// This logger will log to a text file.
  /// </summary>
  public class TextFileLogger : LoggerImplementation
  {

    string logFilename = Path.GetDirectoryName(Application.ExecutablePath) + "\\Log.txt";

    public TextFileLogger(string logFile)
    {
        if (logFile != "")
        {
            logFilename = logFile;
        }
    }

    /// <summary>Logs the specified error.</summary>
    /// <param name="error">The error to log.</param>
    public override void LogError(string error)
    {

      List<string> data = new List<string>();

      lock(this)
      {
        if (File.Exists(logFilename))
        {
          using (StreamReader reader = new StreamReader(logFilename))
          {
            string line = null;
            do
            {
              line = reader.ReadLine();
              data.Add(line);
            }
            while (line != null);
          }
        }

        // truncate the file if it's too long
        int writeStart = 0;
        if (data.Count > 500)
          writeStart = data.Count - 500;

        using (StreamWriter stream = new StreamWriter(logFilename, false))
        {
          for (int i = writeStart; i < data.Count; i++)
          {
            stream.WriteLine(data[i]);
          }

          stream.Write(error);
        }
      }
    }
  }
}
