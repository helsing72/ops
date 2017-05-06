/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package ops.IOService;

import java.io.IOException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author angr
 */
public class IOService implements Runnable
{
    private Selector selector;
    private HashMap<SelectionKey, AsynchReceiveWork> callbackHandlersMap = new HashMap<SelectionKey, AsynchReceiveWork>();
    private Thread thread;
    private String name;
    private boolean keepRunning;

    public IOService(String name) throws IOException
    {
        Selector.open();
        this.name = name;
        thread = new Thread("ops.IOService " + name);
    }

    public void start()
    {
        keepRunning = true;
        thread.start();
    }
    public void stop()
    {
        keepRunning = false;
        selector.wakeup();
    }


    public void asynchReceive(AsynchReceiveWork work)
    {
        callbackHandlersMap.put(work.selectionKey, work);
    }
    public SelectionKey registerChannel(DatagramChannel channel) throws IOException
    {
        channel.configureBlocking(false);
        return channel.register(selector, SelectionKey.OP_READ);
    }

    public void run()
    {
        while(keepRunning)
        {
            try
            {
                int selectResult = selector.select(100);
                if(selectResult < 0)
                {
                    Set<SelectionKey> keys = selector.keys();
                    for (SelectionKey selectionKey : keys)
                    {
                        try
                        {
                            AsynchReceiveWork work = callbackHandlersMap.get(selectionKey);
                            if (work != null)
                            {
                                notifyAsynchReceive(work);
                            }
                        } catch (IOException ex)
                        {
                            //TODO: call error event handler
                            Logger.getLogger(IOService.class.getName()).log(Level.SEVERE, null, ex);
                        }
                    }
                }
            }
            catch (IOException ex)
            {
                Logger.getLogger(IOService.class.getName()).log(Level.SEVERE, null, ex);
            }


        }
    }

    private void notifyAsynchReceive(AsynchReceiveWork work) throws IOException
    {
        SocketChannel sc = (SocketChannel) work.selectionKey.channel();
        sc.read(work.buf);
        work.callback.handle(this, work.buf);

    }

}
