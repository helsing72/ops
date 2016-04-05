using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Runtime.Remoting.Messaging;
using System.Text;
using System.Threading;
using System.Windows.Forms;

namespace Ops
{
    /// <summary>Enumerated type that defines how users will be notified of exceptions</summary>
    public enum NotificationType
    {
        /// <summary>Users will not be notified, exceptions will be automatically logged to the registered loggers</summary>
        Silent,

        /// <summary>Users will be notified an exception has occurred, exceptions will be automatically logged to the registered loggers</summary>
        Inform,

        /// <summary>Users will be notified an exception has occurred and will be asked if they want the exception logged</summary>
        Ask
    }

    /// <summary>
    /// Abstract class for logging errors to different output devices, primarily for use in Windows Forms applications
    /// </summary>
    public abstract class LoggerImplementation
    {
        /// <summary>Logs the specified error.</summary>
        /// <param name="error">The error to log.</param>
        public abstract void LogError(string error);
    }

    /// <summary>
    /// Class to log unhandled exceptions
    /// </summary>
    public class ExceptionLogger
    {
        private static volatile ExceptionLogger instance;
        private static object syncRoot = new object();
        private List<LoggerImplementation> loggers;
        private NotificationType notificationType = NotificationType.Silent;

        /// <summary>
        /// Creates a new instance of the ExceptionLogger class
        /// </summary>
        public ExceptionLogger()
        {
            Application.ThreadException += new System.Threading.ThreadExceptionEventHandler(this.OnThreadException);
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(this.OnUnhandledException);
            this.loggers = new List<LoggerImplementation>();
        }

        private delegate void LogExceptionDelegate(Exception e);

        public bool LogLoadedModules { get; set; }

        /// <summary>
        /// Gets or sets the type of the notification shown to the end user.
        /// </summary>
        public NotificationType NotificationType
        {
            get { return this.notificationType; }
            set { this.notificationType = value; }
        }

        public static ExceptionLogger Inst()
        {
            if (instance == null)
            {
                // Ensure thread safe construction of the singleton instance.
                lock (syncRoot)
                {
                    if (instance == null)
                    {
                        instance = new ExceptionLogger();
                    }
                }
            }

            return instance;
        }

        /// <summary>
        /// Adds a logger implementation to the list of used loggers.
        /// </summary>
        /// <param name="logger">The logger to add.</param>
        public void AddLogger(LoggerImplementation logger)
        {
            this.loggers.Add(logger);
        }

        /// <summary>writes exception details to the registered loggers</summary>
        /// <param name="exception">The exception to log.</param>
        public void LogException(Exception exception)
        {
            StringBuilder error = new StringBuilder();

            error.AppendLine("Application:       " + Application.ProductName);
            error.AppendLine("Version:           " + Application.ProductVersion);
            error.AppendLine("Date:              " + DateTime.Now.ToString("dd/MM/yyyy HH:mm:ss"));
            error.AppendLine("Computer name:     " + SystemInformation.ComputerName);
            error.AppendLine("User name:         " + SystemInformation.UserName);
            error.AppendLine("OS:                " + Environment.OSVersion.ToString());
            error.AppendLine("Culture:           " + CultureInfo.CurrentCulture.Name);
            error.AppendLine("Resolution:        " + SystemInformation.PrimaryMonitorSize.ToString());
            error.AppendLine("System up time:    " + GetSystemUpTime());
            error.AppendLine("App up time:       " + (DateTime.Now - Process.GetCurrentProcess().StartTime).ToString());
            error.AppendLine("IP address:        " + GetLocalIPAddress());

            MEMORYSTATUSEX memStatus = new MEMORYSTATUSEX();
            if (GlobalMemoryStatusEx(memStatus))
            {
                error.AppendLine("Total memory:      " + (memStatus.TotalPhys / (1024 * 1024)) + "Mb");
                error.AppendLine("Available memory:  " + (memStatus.AvailPhys / (1024 * 1024)) + "Mb");
            }

            error.AppendLine(string.Empty);

            error.AppendLine("Exception classes:   ");
            error.Append(this.GetExceptionTypeStack(exception));
            error.AppendLine(string.Empty);
            error.AppendLine("Exception messages: ");
            error.Append(this.GetExceptionMessageStack(exception));

            error.AppendLine(string.Empty);
            error.AppendLine("Stack Traces:");
            error.Append(this.GetExceptionCallStack(exception));

            if (this.LogLoadedModules)
            {
                error.AppendLine(string.Empty);
                error.AppendLine("Loaded Modules:");
                Process thisProcess = Process.GetCurrentProcess();
                foreach (ProcessModule module in thisProcess.Modules)
                {
                    error.AppendLine(module.FileName + " " + module.FileVersionInfo.FileVersion);
                }
            }

            for (int i = 0; i < this.loggers.Count; i++)
            {
                this.loggers[i].LogError(error.ToString());
            }
        }

        /// <summary>writes whatever you want to the registered loggers</summary>
        /// <param name="logMessage">The message to log.</param>
        public void LogMessage(string logMessage)
        {
            for (int i = 0; i < this.loggers.Count; i++)
            {
                this.loggers[i].LogError(logMessage);
            }
        }

        private static TimeSpan GetSystemUpTime()
        {
            PerformanceCounter systemUpTime = new PerformanceCounter("System", "System Up Time");
            systemUpTime.NextValue();
            return TimeSpan.FromSeconds(systemUpTime.NextValue());
        }

        private static string GetLocalIPAddress()
        {
            string result = string.Empty;
            System.Net.IPHostEntry host = System.Net.Dns.GetHostEntry(System.Net.Dns.GetHostName());

            foreach (System.Net.IPAddress ip in host.AddressList)
            {
                if (ip.AddressFamily == System.Net.Sockets.AddressFamily.InterNetwork)
                {
                    result = ip.ToString();
                }
            }

            return result;
        }

        [return: MarshalAs(UnmanagedType.Bool)]
        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        private static extern bool GlobalMemoryStatusEx([In, Out] MEMORYSTATUSEX longpointerBuffer);

        private void HandleException(Exception e)
        {
            switch (this.notificationType)
            {
                case NotificationType.Ask:
                    if (MessageBox.Show("An unexpected error occurred - " + e.Message + ". Do you wish to log the error?", "Error", MessageBoxButtons.YesNo) == DialogResult.No)
                    {
                        return;
                    }

                    break;

                case NotificationType.Inform:
                    MessageBox.Show("An unexpected error occurred - " + e.Message);
                    break;

                case NotificationType.Silent:
                    break;
            }

            LogExceptionDelegate logDelegate = new LogExceptionDelegate(this.LogException);
            logDelegate.BeginInvoke(e, new AsyncCallback(this.LogCallBack), null);
        }

        // Event handler that will be called when an unhandled
        // exception is caught
        private void OnThreadException(object sender, ThreadExceptionEventArgs e)
        {
            this.HandleException(e.Exception);
        }

        private void OnUnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            this.HandleException((Exception)e.ExceptionObject);
        }

        private void LogCallBack(IAsyncResult result)
        {
            AsyncResult asyncResult = (AsyncResult)result;
            LogExceptionDelegate logDelegate = (LogExceptionDelegate)asyncResult.AsyncDelegate;
            if (!asyncResult.EndInvokeCalled)
            {
                logDelegate.EndInvoke(result);
            }
        }

        private string GetExceptionTypeStack(Exception e)
        {
            if (e.InnerException != null)
            {
                StringBuilder message = new StringBuilder();
                message.AppendLine(this.GetExceptionTypeStack(e.InnerException));
                message.AppendLine("   " + e.GetType().ToString());
                return message.ToString();
            }
            else
            {
                return "   " + e.GetType().ToString();
            }
        }

        private string GetExceptionMessageStack(Exception e)
        {
            if (e.InnerException != null)
            {
                StringBuilder message = new StringBuilder();
                message.AppendLine(this.GetExceptionMessageStack(e.InnerException));
                message.AppendLine("   " + e.Message);
                return message.ToString();
            }
            else
            {
                return "   " + e.Message;
            }
        }

        private string GetExceptionCallStack(Exception e)
        {
            if (e.InnerException != null)
            {
                StringBuilder message = new StringBuilder();
                message.AppendLine(this.GetExceptionCallStack(e.InnerException));
                message.AppendLine("--- Next Call Stack:");
                message.AppendLine(e.StackTrace);
                return message.ToString();
            }
            else
            {
                return e.StackTrace;
            }
        }

        // use to get memory available
        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
        private class MEMORYSTATUSEX
        {
            private uint length;
            private uint memoryLoad;
            private ulong totalPhys;
            private ulong availPhys;
            private ulong totalPageFile;
            private ulong availPageFile;
            private ulong totalVirtual;
            private ulong availVirtual;
            private ulong availExtendedVirtual;

            public MEMORYSTATUSEX()
            {
                this.length = (uint)Marshal.SizeOf(typeof(MEMORYSTATUSEX));
            }

            public ulong TotalPhys
            {
                get { return this.totalPhys; }
                set { this.totalPhys = value; }
            }

            public ulong AvailPhys
            {
                get { return this.availPhys; }
                set { this.availPhys = value; }
            }
        }
    }
}
