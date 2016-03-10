using System.Collections;

namespace Ops 
{
    // Interface the all observer classes should implement
    public interface IObserver
    {
        void Update(IObservable o, object arg); 
    }

    // Interface that all observable classes should implement
    public interface IObservable
    {
        void AddObserver(IObserver anObserver);
        void DeleteObserver(IObserver anObserver);
    }

        
    // Helper class that implements observable interface
    public class Observable : IObservable 
    {
        // Container to store the observer instance
        private Hashtable _observerContainer = new Hashtable();
        private System.Object _lock = new System.Object();
        private bool _useLock = false;

        public Observable()
        {
        }

        public Observable(bool useLock)
        {
            _useLock = useLock;
        }
       
        // Add the observer
        public void AddObserver(IObserver anObserver)
        {
            if (_useLock)
            {
                lock (_lock)
                {
                    _observerContainer.Add(anObserver, anObserver);
                }
            }
            else
            {
                _observerContainer.Add(anObserver, anObserver);
            }
        }
          
        // Remove the observer
        public void DeleteObserver(IObserver anObserver)
        {
            if (_useLock)
            {
                lock (_lock)
                {
                    _observerContainer.Remove(anObserver);
                }
            }
            else
            {
                _observerContainer.Remove(anObserver);
            }
        }

        // Common method to notify all the observers
        public void NotifyObservers(object anObject) 
        {
            if (_useLock)
            {
                lock (_lock)
                {
                    // Enumeration the observers and invoke their notify method
                    foreach (IObserver anObserver in _observerContainer.Keys)
                    {
                        anObserver.Update(this, anObject);
                    }
                }
            }
            else
            {
                // Enumeration the observers and invoke their notify method
                foreach (IObserver anObserver in _observerContainer.Keys)
                {
                    anObserver.Update(this, anObject);
                }
            }
        }

    }
}