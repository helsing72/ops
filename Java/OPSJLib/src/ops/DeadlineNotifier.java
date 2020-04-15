/**
*
* Copyright (C) 2006-2009 Anton Gravestam.
* Copyright (C) 2020 Lennart Andersson.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.

* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*/
package ops;

import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Anton
 */
public class DeadlineNotifier extends Thread
{

    private static DeadlineNotifier theInstance;

    public static synchronized DeadlineNotifier getInstance()
    {
        if (theInstance == null)
        {
            theInstance = new DeadlineNotifier();
            theInstance.start();
            //System.out.println("Created DeadlineNotifier()");
        }
        return theInstance;
    }

    private Vector<Subscriber> subscribers = new Vector<Subscriber>();
    private volatile boolean keepRunning;

    private DeadlineNotifier()
    {
        setName("DeadlineNotifierThread");
        keepRunning = true;
    }

    public synchronized boolean remove(Subscriber o)
    {

        boolean result = subscribers.remove(o);
        if (subscribers.size() == 0)
        {
            ///We don't stop the thread for now since that results in deadlines stop working
            ///TODO fix the thread start/stop handling
            ///stopRunning();
            ///theInstance = null;
        }
        return result;
    }

    public synchronized boolean add(Subscriber e)
    {
        boolean result = subscribers.add(e);
        //System.out.println("Deadline.add() for topic: " + e.getTopic().getName() + ". Num subs: " + subscribers.size());
        return result;
    }

    public synchronized boolean contains(Subscriber o)
    {
        return subscribers.contains(o);
    }

    public void stopRunning()
    {
        keepRunning = false;
    }

    @Override
    public void run()
    {
        //System.out.println("DeadlineNotifier thread started");
        while (keepRunning)
        {
            try
            {
                synchronized (this)
                {
                    for (Subscriber subscriber : subscribers)
                    {
                        subscriber.checkDeadline();
                    }
                }
                Thread.sleep(5);
            } catch (InterruptedException ex)
            {
                Logger.getLogger(DeadlineNotifier.class.getName()).log(Level.SEVERE, null, ex);
            }

        }
        //System.out.println("DeadlineNotifier thread exit");
    }
}
