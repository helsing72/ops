///////////////////////////////////////////////////////////
//  InProcessTransport.cs
//  Implementation of the Class InProcessTransport
//  Created on:      12-nov-2011 09:25:30
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;

namespace Ops
{
    // This BlockingQueue comes from:
    // http://element533.blogspot.com/2010/01/stoppable-blocking-queue-for-net.html
    // Take() returns null if the queue has been stopped.
    public class BlockingQueue<T>
    {
        private readonly Queue<T> _queue = new Queue<T>();
        private bool _stopped;

        public bool Put(T item)
        {
            if (_stopped)
                return false;
            lock (_queue)
            {
                if (_stopped)
                    return false;
                _queue.Enqueue(item);
                Monitor.Pulse(_queue);
            }
            return true;
        }

        public T Take()
        {
            if (_stopped)
                return default(T);
            lock (_queue)
            {
                if (_stopped)
                    return default(T);
                while (_queue.Count == 0)
                {
                    Monitor.Wait(_queue);
                    if (_stopped)
                        return default(T);
                }
                return _queue.Dequeue();
            }
        }

        public void Stop()
        {
            if (_stopped)
                return;
            lock (_queue)
            {
                if (_stopped)
                    return;
                _stopped = true;
                Monitor.PulseAll(_queue);
            }
        }
    }
    
    // This BlockingQueue comes from:
    // http://blogs.msdn.com/b/toub/archive/2006/04/12/blocking-queues.aspx
    // and can be used like this:
    //
    // BlockingQueue<int> _queue = new BlockingQueue<int>();
    // ...
    // foreach(int item in _queue) { ... }
    //
    //class BlockingQueue<T> : IEnumerable<T>
    //{
    //    private int _count = 0;
    //    private Queue<T> _queue = new Queue<T>();

    //    public T Take()
    //    {
    //        lock (_queue)
    //        {
    //            while (_count <= 0) Monitor.Wait(_queue);
    //            _count--;
    //            return _queue.Dequeue();
    //        }
    //    }

    //    public void Put(T data)
    //    {
    //        if (data == null) throw new ArgumentNullException("data");
    //        lock (_queue)
    //        {
    //            _queue.Enqueue(data);
    //            _count++;
    //            Monitor.Pulse(_queue);
    //        }
    //    }

    //    IEnumerator<T> IEnumerable<T>.GetEnumerator()
    //    {
    //        while (true) yield return Take();
    //    }

    //    IEnumerator IEnumerable.GetEnumerator()
    //    {
    //        return ((IEnumerable<T>)this).GetEnumerator();
    //    }
    //}

}