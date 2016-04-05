/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ops;

import java.io.IOException;
import java.net.InetAddress;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Anton Gravestam
 */
public class McSendDataHandler extends SendDataHandlerBase
{
    private final InetAddress sinkIP;

    public McSendDataHandler(Topic t, String localInterface, int ttl) throws IOException
    {
        sender = new MulticastSender(0, localInterface, ttl, t.getOutSocketBufferSize());
        sinkIP = InetAddress.getByName(t.getDomainAddress());
    }

    public synchronized boolean sendData(byte[] bytes, int size, Topic t)
    {
        return sendData(bytes, size, sinkIP, t.getPort());
    }

}
