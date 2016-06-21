/*
 * XMLConfigLoader.java
 *
 * Created on den 10 september 2007, 08:28
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package idlcompiler.topicconfigcreators;

import java.io.FileReader;
import java.io.IOException;
import java.util.Vector;
import javax.xml.parsers.SAXParser;
import ops.Topic;
import org.xml.sax.SAXException;

import org.xml.sax.XMLReader;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.helpers.DefaultHandler;
import parsing.TopicInfo;


public class XMLConfigLoader extends DefaultHandler
{

    
    private Vector<TopicInfo> topics = new Vector<TopicInfo>();
    private Vector<RequestReplyInfo> requestReplyInfos = new Vector<RequestReplyInfo>();
    private String baseAddress = null;
    public XMLConfigLoader(String fileName) throws SAXException, IOException
    {
	super();
        
        XMLReader xr = XMLReaderFactory.createXMLReader();
	//SAXParser xr = SAXParser();

        //com.sun.org.apache.xerces.internal.parsers.SAXParser xr = new com.sun.org.apache.xerces.internal.parsers.SAXParser();
	xr.setContentHandler(this);
	xr.setErrorHandler(this);
        
        xr.parse(new InputSource(fileName));
    }

    public String getBaseAddress()
    {
        return baseAddress;
    }

    public Vector<TopicInfo> getTopics()
    {

        return topics;
    }
    public Vector<RequestReplyInfo> getRequestReplyInfos()
    {

        return requestReplyInfos;
    }
    public String getTypeForTopic(String topicName)
    {
        for (TopicInfo topicInfo : topics)
        {
            if(topicInfo.name.equals(topicName))
            {
                return topicInfo.type;
            }

        }
        return null;

    }


    ////////////////////////////////////////////////////////////////////
    // Event handlers.
    ////////////////////////////////////////////////////////////////////

    public void startElement (String uri, String name,
			      String qName, Attributes atts)
    {
        if(name.equals("manager"))
        {
            baseAddress = atts.getValue("domainAddress");
        }
        else if(name.equals("requestreply"))
        {
            RequestReplyInfo rrInfo = new RequestReplyInfo();
            rrInfo.className = atts.getValue("classname");
            rrInfo.replyTopic = atts.getValue("replytopic");
            rrInfo.requestTopic = atts.getValue("requesttopic");
            
            requestReplyInfos.add(rrInfo);
        }
        
        else if(name.equals("topic"))
        {
            TopicInfo t = new TopicInfo();
            t.name = (atts.getValue("name"));
            t.type = (atts.getValue("type"));
            //t.port = (Integer.parseInt(atts.getValue("portID")));
            //t.address = (baseAddress);
            topics.add(t);
        }
    }
    
    public void startDocument ()
    {
	
    }


    public void endDocument ()
    {
	
    }


    


    public void endElement (String uri, String name, String qName)
    {
	
    }


    public void characters (char ch[], int start, int length)
    {
	
    }

}