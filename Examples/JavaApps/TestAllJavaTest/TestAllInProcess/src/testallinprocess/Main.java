/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package testallinprocess;

import TestAll.ChildData;
import TestAll.ChildDataPublisher;
import TestAll.ChildDataSubscriber;
import java.util.Observable;
import java.util.Observer;
import java.util.logging.Level;
import java.util.logging.Logger;
import ops.ConfigurationException;
import ops.Participant;
import ops.Topic;
import ops.protocol.OPSMessage;

/**
 * Example showing how to subscribe to data on OPS from Java
 * @author angr
 */
public class Main {
    private static ChildDataSubscriber sub;

    public static void main(String[] args)
    {
        // TODO code application logic here
        //OPSObjectFactory.getInstance().add(new TestAll.TestAllTypeFactory());

        try
        {
            Participant participant = Participant.getInstance("TestAllDomain");
            participant.addTypeSupport(new TestAll.TestAllTypeFactory());

            Topic topic = participant.createTopic("ChildTopic");

            sub = new ChildDataSubscriber(topic);

            sub.addObserver(new Observer() { public void update(Observable o, Object arg){onNewChildData((ChildData)arg);} });

            sub.start();

            ChildDataPublisher publisher = new ChildDataPublisher(topic);

            ChildData data = new ChildData();
            data.baseText = "Hello from in process transport test";


            while (true)
            {
                sleep(1000);
                publisher.write(data);
            }
        }
        catch (ops.ConfigurationException ex)
        {
           Logger.getLogger(Main.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    private static void onNewChildData(ChildData childData)
    {
        System.out.println("Wohooo!, New ChildData!" + childData.baseText);
        OPSMessage message = sub.getMessage();
        if(sub.getMessage().getQosMask() == 1)
        {
            System.out.println("Publication came over InProcessTransport");
        }
        else
        {
            System.out.println("Publication came over Multicast");
        }
        System.out.println("Publication " + message.getPublicationID() + " from " + message.getPublisherName());

    }

    private static void sleep(int i)
    {
        try
        {
            Thread.sleep(i);
        }
        catch (InterruptedException ex)
        {
        }
    }

}
