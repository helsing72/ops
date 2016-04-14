/*
 * opstestmain.java
 */

package opstestmain;

import java.util.Vector;

import opstestapp.IOpsHelperListener;
import opstestapp.COpsHelper;

import ops.Participant;
import ops.OPSConfig;
import ops.Domain;
import ops.Topic;
import ops.KeyFilterQoSPolicy;

import pizza.PizzaData;
import pizza.PizzaDataSubscriber;
import pizza.PizzaDataPublisher;
import pizza.VessuvioData;
import pizza.VessuvioDataSubscriber;
import pizza.VessuvioDataPublisher;
import PizzaProject.PizzaProjectTypeFactory;

import ops.ConfigurationException;
import ops.Listener;
import ops.OPSObject;

public class opstestmain implements IOpsHelperListener, ops.Listener<ops.Error> {

  private String defaultDomain = "PizzaDomain";
  private String OtherDomain = "OtherPizzaDomain";

  private Participant participant = null;
  private Participant otherParticipant = null;

  private class MyTopicInfo {
      public String domain;
      public String topName;
      public String typeName;

      public COpsHelper helper;
      public Participant part;

      public boolean selected;

      public MyTopicInfo(String dom, String top, String typ)
      {
          this.domain = dom;
          this.topName = top;
          this.typeName = typ;
          this.selected = false;
      }
  }
  Vector<MyTopicInfo> MyTopicInfoList = new Vector<MyTopicInfo>();

  private pizza.PizzaData pizzaData = new PizzaData();
  private pizza.VessuvioData vessuvioData = new VessuvioData();

  private long deadLineEventIntervall = 0;
  private int Counter = 0;

  public opstestmain() {

    MyTopicInfoList.add(new MyTopicInfo("PizzaDomain",      "PizzaTopic",         "pizza.PizzaData"));
    MyTopicInfoList.add(new MyTopicInfo("PizzaDomain",      "VessuvioTopic",      "pizza.VessuvioData"));
    MyTopicInfoList.add(new MyTopicInfo("PizzaDomain",      "PizzaTopic2",        "pizza.PizzaData"));
    MyTopicInfoList.add(new MyTopicInfo("PizzaDomain",      "VessuvioTopic2",     "pizza.VessuvioData"));
    MyTopicInfoList.add(new MyTopicInfo("PizzaDomain",      "TcpPizzaTopic",      "pizza.PizzaData"));
    MyTopicInfoList.add(new MyTopicInfo("PizzaDomain",      "TcpVessuvioTopic",   "pizza.VessuvioData"));
    MyTopicInfoList.add(new MyTopicInfo("PizzaDomain",      "TcpPizzaTopic2",     "pizza.PizzaData"));
    MyTopicInfoList.add(new MyTopicInfo("PizzaDomain",      "TcpVessuvioTopic2",  "pizza.VessuvioData"));
    MyTopicInfoList.add(new MyTopicInfo("OtherPizzaDomain", "OtherPizzaTopic",    "pizza.PizzaData"));
    MyTopicInfoList.add(new MyTopicInfo("OtherPizzaDomain", "OtherVessuvioTopic", "pizza.VessuvioData"));
    MyTopicInfoList.add(new MyTopicInfo("PizzaDomain",      "UdpPizzaTopic",      "pizza.PizzaData"));
    MyTopicInfoList.add(new MyTopicInfo("PizzaDomain",      "UdpVessuvioTopic",   "pizza.VessuvioData"));

    for(int i = 0; i < MyTopicInfoList.size(); i++) {
        MyTopicInfo info = MyTopicInfoList.elementAt(i);
        info.helper = new COpsHelper(this);
    }

    try
    {
      //Create Participant
      participant = Participant.getInstance(defaultDomain, defaultDomain);
      participant.addTypeSupport(new PizzaProjectTypeFactory());
      participant.addListener(this);
      otherParticipant = Participant.getInstance(OtherDomain, OtherDomain);
      otherParticipant.addTypeSupport(new PizzaProjectTypeFactory());
      otherParticipant.addListener(this);


      OnLog("Created Participants\n");
      OnLog("DomainID: " + participant.getDomain().getDomainID());
      OnLog("DomainAddress: " + participant.getDomain().getDomainAddress());
      OnLog("InSocketBufferSize: " + participant.getDomain().GetInSocketBufferSize());
      OnLog("OutSocketBufferSize: " + participant.getDomain().GetOutSocketBufferSize());
      OnLog("MetaDataMcPort: " + participant.getDomain().GetMetaDataMcPort());
      OnLog("LocalInterface: " + participant.getDomain().getLocalInterface());
      OnLog("TimeToLive: " + participant.getDomain().getTimeToLive() + "\n");

      OnLog("DomainID: " + otherParticipant.getDomain().getDomainID());
      OnLog("DomainAddress: " + otherParticipant.getDomain().getDomainAddress());
      OnLog("InSocketBufferSize: " + otherParticipant.getDomain().GetInSocketBufferSize());
      OnLog("OutSocketBufferSize: " + otherParticipant.getDomain().GetOutSocketBufferSize());
      OnLog("MetaDataMcPort: " + otherParticipant.getDomain().GetMetaDataMcPort());
      OnLog("LocalInterface: " + otherParticipant.getDomain().getLocalInterface());
      OnLog("TimeToLive: " + otherParticipant.getDomain().getTimeToLive() + "\n");

      for(int i = 0; i < MyTopicInfoList.size(); i++) {
        MyTopicInfo info = MyTopicInfoList.elementAt(i);
        if (info.domain.equals(defaultDomain)) {
          info.part = participant;
        } else if (info.domain.equals(OtherDomain)) {
          info.part = otherParticipant;
        }
      }
    }
    catch (ConfigurationException e)
    {
      OnLog("Exception: " + e.getMessage());
    }
  }

  public void OnLog(final String str) {
    System.out.println (str);
  }

  public void onNewEvent(ops.Notifier<ops.Error> notifier, ops.Error arg) {
      OnLog("###!!! " + arg.getErrorMessage());
  }

  public void OnData(final String topName, final pizza.PizzaData Data) {
      OnLog("[ " + topName + " ] New PizzaData: " + Data.cheese + "\n");
  }

  public void OnData(final String topName, final pizza.VessuvioData Data) {
      OnLog("[ " + topName + " ] New VessuvioData: " + Data.cheese + ", Ham length: " + Data.ham.length() + "\n");
  }

  public void OnData(final String topName, final OPSObject Data) {
      if (Data instanceof VessuvioData) {
          OnData(topName, (VessuvioData)Data);
      } else if (Data instanceof PizzaData) {
          OnData(topName, (PizzaData)Data);
      }
  }

  public void ChangeSelect(int index, boolean value) {
    MyTopicInfo info = MyTopicInfoList.elementAt(index);
    info.selected = value;
  }

  public void CreateSubscriber() {
      for(int i = 0; i < MyTopicInfoList.size(); i++) {
          MyTopicInfo info = MyTopicInfoList.elementAt(i);
          if (info.selected) {
              info.helper.CreateSubscriber(info.part, info.topName);
          }
      }
  }

  public void CreatePublisher() {
      for(int i = 0; i < MyTopicInfoList.size(); i++) {
          MyTopicInfo info = MyTopicInfoList.elementAt(i);
          if (info.selected) {
              info.helper.CreatePublisher(info.part, info.topName);
          }
      }
  }

  public void DeleteSubscriber() {
      for(int i = 0; i < MyTopicInfoList.size(); i++) {
          MyTopicInfo info = MyTopicInfoList.elementAt(i);
          if (info.selected) {
              info.helper.DeleteSubscriber();
          }
      }
  }

  public void DeletePublisher() {
      for(int i = 0; i < MyTopicInfoList.size(); i++) {
          MyTopicInfo info = MyTopicInfoList.elementAt(i);
          if (info.selected) {
              info.helper.DeletePublisher();
          }
      }
  }

  public void StartSubscriber() {
      for(int i = 0; i < MyTopicInfoList.size(); i++) {
          MyTopicInfo info = MyTopicInfoList.elementAt(i);
          if (info.selected) {
              info.helper.StartSubscriber(deadLineEventIntervall);
          }
      }
  }

  public void StopSubscriber() {
      for(int i = 0; i < MyTopicInfoList.size(); i++) {
          MyTopicInfo info = MyTopicInfoList.elementAt(i);
          if (info.selected) {
              info.helper.StopSubscriber(true);
          }
      }
  }

  public void WriteToAll(int NumExtraBytes) {
      Counter++;
      pizzaData.cheese = "Java " + Counter;

      vessuvioData.cheese = "Java " + Counter;

      StringBuilder sb = new StringBuilder();
      sb.setLength(NumExtraBytes);
      vessuvioData.ham = sb.toString();

      for(int i = 0; i < MyTopicInfoList.size(); i++) {
          MyTopicInfo info = MyTopicInfoList.elementAt(i);
          if (info.selected) {
              if (info.typeName.equals(PizzaData.getTypeName())) {
                  info.helper.Write(pizzaData);
              }
              if (info.typeName.equals(VessuvioData.getTypeName())) {
                  info.helper.Write(vessuvioData);
              }
          }
      }
  }

  public void menu()
	{
		for (int i = 0; i < MyTopicInfoList.size(); i++) {
      MyTopicInfo info = MyTopicInfoList.elementAt(i);
			System.out.print("\t " + i);
      if (info.helper.HasPublisher()) {
        System.out.print(" P");
      } else {
        System.out.print("  ");
      }
      if (info.helper.HasSubscriber()) {
        System.out.print("S");
      } else {
        System.out.print(" ");
      }
      if (info.selected) {
        System.out.print("*");
      } else {
        System.out.print(" ");
      }
			System.out.println(info.domain + "::" + info.topName);
		}

    System.out.println("");
  	System.out.println("\t PC    Create Publishers");
  	System.out.println("\t PD    Delete Publishers");
  	//System.out.println("\t PS    Start Publishers");
  	//System.out.println("\t PT    Stop Publishers");
  	System.out.println("\t SC    Create Subscriber");
  	System.out.println("\t SD    Delete Subscriber");
  	System.out.println("\t SS    Start Subscriber");
  	System.out.println("\t ST    Stop Subscriber");
  	//System.out.println("\t L num Set num Vessuvio Bytes [" << NumVessuvioBytes << "]" << std::endl;
  	//System.out.println("\t T ms  Set deadline timeout [ms]");
  	//System.out.println("\t V ms  Set send period [ms] [" << sendPeriod << "]" << std::endl;
  	//System.out.println("\t A     Start/Stop periodical Write with set period");
  	System.out.println("\t W     Write data");
  	//System.out.println("\t Q     Quite (minimize program output)");
  	System.out.println("\t X     Exit program");
	}

  public void doRun()
  {
    menu();
    for (; ; ) {
			System.out.print(" command > ");
			String input = System.console().readLine();
			//char tmp = (char) System.in.read();

			//System.out.printf("Input was: '%s'\n", input);

			if (input.length() == 0) continue;

			if ((input.charAt(0) >= '0') && (input.charAt(0) <= '9')) {
				try {
					int i = Integer.parseInt(input);
					//System.out.printf("Integer was: %d\n", i);
					ChangeSelect(i, true);
          menu();
				} catch(NumberFormatException nfe) {
					System.err.println("Invalid Format!");
				}
			}

      if (input.startsWith("?", 0)) { menu(); }

			if (input.startsWith("pc", 0)) { CreatePublisher(); }
			if (input.startsWith("pd", 0)) { DeletePublisher(); }
			//if (input == "ps") { app.StartPublisher(); }
			//if (input == "pt") { app.StopPublisher(); }

			if (input.startsWith("sc", 0)) { CreateSubscriber(); }
			if (input.startsWith("sd", 0)) { DeleteSubscriber(); }
			if (input.startsWith("ss", 0)) { StartSubscriber(); }
			if (input.startsWith("st", 0)) { StopSubscriber(); }

			if (input.startsWith("w", 0)) { WriteToAll(0); }
			if (input.startsWith("x", 0)) break;
		}

    // Clean up
    for (int i = 0; i < MyTopicInfoList.size(); i++) {
      MyTopicInfo info = MyTopicInfoList.elementAt(i);
      if (info.helper.HasSubscriber()) info.helper.DeleteSubscriber();
      if (info.helper.HasPublisher()) info.helper.DeletePublisher();
      info.helper = null;
		}

    participant = null;
    otherParticipant = null;
  }

}
