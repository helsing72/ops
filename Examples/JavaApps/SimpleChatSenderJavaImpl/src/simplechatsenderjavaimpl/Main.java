/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package simplechatsenderjavaimpl;

import chat.ChatData;
import chat.ChatDataPublisher;
import chat.ChatDataSubscriber;
import java.util.Observable;
import java.util.Observer;
import java.util.Scanner;
import ops.Participant;
import ops.ConfigurationException;

/**
 *
 * @author Anton
 */
public class Main
{

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args)
    {
      try {
        // TODO code application logic here
        System.out.println("Please enter your name.");
        String name;
        Scanner in = new Scanner(System.in);
        name = in.nextLine();
        System.out.println("Name set to " + name);
        Participant participant = Participant.getInstance("ChatDomain");
        participant.addTypeSupport(new ChatExample.ChatExampleTypeFactory());
        ChatDataPublisher publisher = new ChatDataPublisher(participant.createTopic("ChatTopic"));

        ChatDataSubscriber subscriber = new ChatDataSubscriber(participant.createTopic("ChatTopic"));
        subscriber.addObserver(new Observer()
        {
            public void update(Observable o, Object arg)
            {
                onNewChatData((ChatData) arg);
            }
        });

        subscriber.setDeadlineQoS(1000);
        subscriber.deadlineEvent.addObserver(new Observer()
        {
            public void update(Observable o, Object arg)
            {
                System.out.println("Deadline Missed");
            }
        });
        subscriber.start();

        ChatData chatData = new ChatData();
        chatData.sender.name = name;
        while (true)
        {
            chatData.messageData.message = in.nextLine();
            publisher.write(chatData);
        }
      } catch (ConfigurationException e)
      {
          System.out.println("Exception: " + e.getMessage());
      }
    }

    private static void onNewChatData(ChatData chatData)
    {
        System.out.println("" + chatData.sender.name + " wrote: " + chatData.messageData.message);
    }
}
