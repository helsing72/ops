/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package testalljavatestsubscriber;

import TestAll.ChildData;
import TestAll.ChildDataSubscriber;
import java.util.Observable;
import java.util.Observer;
import java.util.logging.Level;
import java.util.logging.Logger;
import ops.KeyFilterQoSPolicy;
import ops.Participant;
import ops.Topic;
import ops.ConfigurationException;
import ops.protocol.OPSMessage;

/**
 * Example showing how to subscribe to data on OPS from Java
 * @author angr
 */
public class Main {

    private static ChildDataSubscriber sub;

    public static void main(String[] args)
    {
      try {
        Participant participant = Participant.getInstance("TestAllDomain");
        participant.addTypeSupport(new TestAll.TestAllTypeFactory());

        Topic topic = participant.createTopic("ChildTopic");

        sub = new ChildDataSubscriber(topic);

        sub.addObserver(new Observer() { public void update(Observable o, Object arg){onNewChildData((ChildData)arg);} });

        sub.start();

        while (true)
        {
            sleep(1000);
        }
      } catch (ConfigurationException e)
      {
          System.out.println("Exception: " + e.getMessage());
      }

    }
    private static void onNewChildData(ChildData childData)
    {
        System.out.println("Wohooo!, New ChildData!" + childData.i);
        OPSMessage message = sub.getMessage();
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
