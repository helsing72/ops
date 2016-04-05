// EasyThread provides a facade to inheriting from a Thread class.
// Override the perform work method to perform your tasks.
// just inherit from the EasyThread class and override the Run method

using System;
using System.Threading;

namespace Ops
{
    public class EasyThread : IDisposable
    {
        Thread WorkerThread;

        // This one is important to ensure that this thread is terminated 
        // when all foreground threads has been terminated.
        bool isBackgroundThread = true;

        public EasyThread()
        {
            this.WorkerThread = new Thread(new ThreadStart(Run));
        }

        public void Start()
        {
            this.WorkerThread.IsBackground = isBackgroundThread;

            if (!this.WorkerThread.IsAlive)
                this.WorkerThread.Start();
        }

        /// <summary>
        /// EasyThread provides a facade to inheriting from a Thread class.
        /// Override the Run method to perform your tasks.
        /// </summary>
        protected virtual void Run()
        {

        }

        public void Abort()
        {
            this.WorkerThread.Abort();
        }

        private void Cleanup()
        {
            this.WorkerThread.Join(0);
            this.WorkerThread = null;
        }

        public virtual void Dispose()
        {
            Cleanup();
        }

        public bool IsBackground
        {
            get
            {
                return this.WorkerThread.IsBackground;
            }
            set
            {
                // Check whether the thread has previously been named
                // to avoid a possible InvalidOperationException.
                if (this.WorkerThread.IsAlive)
                {
                    Logger.ExceptionLogger.LogMessage("'IsBackground' shall be set before the thread is started.");
                }
                else
                {
                    this.WorkerThread.IsBackground = value;
                }
            }
        }

        public string Name
        {
            get
            {
                if (this.WorkerThread.Name == null)
                {
                    return "";
                }
                else
                {
                    return this.WorkerThread.Name;
                }
            }
            set
            {
                // Check whether the thread has previously been named
                // to avoid a possible InvalidOperationException.
                if (this.WorkerThread.Name == null)
                {
                    this.WorkerThread.Name = value;
                }
                else
                {
                    Logger.ExceptionLogger.LogMessage("Unable to name a previously named thread.");
                }
            }
        }

        public void Quit()
        {
            Cleanup();
        }

    }

}