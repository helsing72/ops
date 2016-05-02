package configlib;

import com.sun.org.apache.xerces.internal.parsers.DOMParser;
import configlib.Deserializable;
import configlib.exception.FormatException;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 *
 * @author Anton Gravestam
 */
public class XMLArchiverIn implements ArchiverInOut
{

    private InputStream is;
    private Document doc;
    private Node currentNode;
    private int currentElement = -1;
    private Stack<Node> nodeStack = new Stack<Node>();
    private SerializableCompositeFactory factory = new SerializableCompositeFactory();

    public XMLArchiverIn()
    {

    }

    public XMLArchiverIn(InputStream is) throws FormatException
    {
        try
        {
            this.is = is;
            DOMParser parser = new DOMParser();

            // Parse the document.
            parser.parse(new InputSource(is));
            // Obtain the document.
            doc = parser.getDocument();

            currentNode = doc;
        } catch (SAXException ex)
        {
            throw new FormatException("Caused by underlying SAXException: " + ex.getMessage());
        } catch (IOException ex)
        {
            throw new FormatException("Caused by underlying IOException: " + ex.getMessage());
        }
    }

    public XMLArchiverIn(InputStream is, String rootNode) throws FormatException
    {
        try
        {
            this.is = is;
            DOMParser parser = new DOMParser();

            // Parse the document.
            parser.parse(new InputSource(is));
            // Obtain the document.
            doc = parser.getDocument();

            currentNode = doc;
            currentNode = getNode(rootNode);
        } catch (SAXException ex)
        {
            throw new FormatException("Caused by underlying SAXException: " + ex.getMessage());
        } catch (IOException ex)
        {
            throw new FormatException("Caused by underlying IOException: " + ex.getMessage());
        }
    }

    public void setInputStream(InputStream is) throws FormatException
    {
        try
        {
            this.is = is;
            DOMParser parser = new DOMParser();

            // Parse the document.
            parser.parse(new InputSource(is));
            // Obtain the document.
            doc = parser.getDocument();

            currentNode = doc;
        } catch (SAXException ex)
        {
            throw new FormatException("Caused by underlying SAXException: " + ex.getMessage());
        } catch (IOException ex)
        {
            throw new FormatException("Caused by underlying IOException: " + ex.getMessage());
        }
    }



    public byte getByte(String name)
    {
        return Byte.parseByte(getValue(name));
    }

    public int getInt(String name)
    {
        return Integer.parseInt(getValue(name));
    }

    public short getShort(String name)
    {
        return Short.parseShort(getValue(name));
    }

    public long getLong(String name)
    {
        return Long.parseLong(getValue(name));
    }

    public float getFloat(String name)
    {
        return Float.parseFloat(getValue(name));
    }

    public double getDouble(String name)
    {
        return Double.parseDouble(getValue(name));
    }

    public String getString(String name)
    {
        String ret = getValue(name);
        if (ret == null)
        {
            ret = "";
        }
        return ret;
    }

//    public void getDeserializable(String name, Deserializable des)
//    {
//        nodeStack.push(currentNode);
//        currentNode = getNode(name);
//        des.desirialize(this);
//        currentNode = nodeStack.pop();
//    }
    public int getNrElements(String name)
    {

        nodeStack.push(currentNode);
        try
        {
            currentNode = getNode(name);
            if(currentNode == null)
            {
                return 0;
            }
            NodeList nodes = currentNode.getChildNodes();
            int size = 0;
            for (int i = 0; i < nodes.getLength(); i++)
            {
                if (nodes.item(i).getNodeName().equals("element"))
                {
                    size++;
                }
            }
            return size;
        }
        finally
        {
            currentNode = nodeStack.pop();
        }


    }
//    public Deserializable getElement(String name, int i, Deserializable deserializable)
//    {
//        try
//        {
//            nodeStack.push(currentNode);
//            currentNode = getNode(name);
//            nodeStack.push(currentNode);
//            currentNode = getCurrentElement(i);
//
//            //Deserializable deserializable = (Deserializable) cls.getConstructor(null).newInstance();
//
//            deserializable.desirialize(this);
//
//            currentNode = nodeStack.pop();
//            currentNode = nodeStack.pop();
//
//            return deserializable;
//        }
//        catch (Exception ex)
//        {
//           return null;
//        }
//
//
//
//
//
//    }

    private Node getCurrentElement(int n)
    {

        int ittElement = 0;
        NodeList nodes = currentNode.getChildNodes();
        for (int i = 0; i < nodes.getLength(); i++)
        {
            if (nodes.item(i).getNodeName().equals("element") && n == ittElement)
            {
                return nodes.item(i);
            } else
            {
                if (nodes.item(i).getNodeName().equals("element"))
                {
                    ittElement++;
                }
            }
        }

        return null;

    }

    private Node getNode(String name)
    {
        NodeList nodes = currentNode.getChildNodes();
        for (int i = 0; i < nodes.getLength(); i++)
        {
            if (nodes.item(i).getNodeName().equals(name))
            {
                return nodes.item(i);
            }
        }
        return null;

    }

    private String getValue(String name)
    {
        NodeList nodes = currentNode.getChildNodes();
        for (int i = 0; i < nodes.getLength(); i++)
        {
            if (nodes.item(i).getNodeName().equals(name))
            {
                return nodes.item(i).getTextContent();
            }

        }
        return null;
    }

//    public void getList(String name, AddElementCallback addElementCallback, Class<? extends Deserializable> cls)
//    {
//        nodeStack.push(currentNode);
//        currentNode = getNode(name);
//        NodeList nodes = currentNode.getChildNodes();
//        for(int i = 0 ; i < nodes.getLength(); i++)
//        {
//            if(nodes.item(i).getNodeName().equals("element"))
//            {
//                try
//                {
//                    Deserializable deserializable = (Deserializable) cls.getConstructor(null).newInstance();
//                    getDeserializable("element", deserializable);
//                    addElementCallback.addElement(deserializable);
//
//                }
//                catch (NoSuchMethodException ex)
//                {
//                    Logger.getLogger(XMLArchiverIn.class.getName()).log(Level.SEVERE, null, ex);
//                }
//                catch (SecurityException ex)
//                {
//                    Logger.getLogger(XMLArchiverIn.class.getName()).log(Level.SEVERE, null, ex);
//                }
//                catch (InstantiationException ex)
//                {
//                    Logger.getLogger(XMLArchiverIn.class.getName()).log(Level.SEVERE, null, ex);
//                }
//                catch (IllegalAccessException ex)
//                {
//                    Logger.getLogger(XMLArchiverIn.class.getName()).log(Level.SEVERE, null, ex);
//                }
//                catch (IllegalArgumentException ex)
//                {
//                    Logger.getLogger(XMLArchiverIn.class.getName()).log(Level.SEVERE, null, ex);
//                }
//                catch (InvocationTargetException ex)
//                {
//                    Logger.getLogger(XMLArchiverIn.class.getName()).log(Level.SEVERE, null, ex);
//                }
//            }
//        }
//
//
//        currentNode = nodeStack.pop();
//
//    }
    public boolean getBoolean(String name)
    {
        return Boolean.parseBoolean(getValue(name));
    }

    public int inout(String name, int v) throws IOException
    {
        try
        {
            return getInt(name);
        } catch (NumberFormatException e)
        {
            return v;
        }
    }

    public long inout(String name, long v) throws IOException
    {
        try
        {
            return getLong(name);
        } catch (NumberFormatException e)
        {
            return v;
        }
    }

    public byte inout(String name, byte v) throws IOException
    {
        try
        {
            return getByte(name);
        } catch (NumberFormatException e)
        {
            return v;
        }
    }

    public short inout(String name, short v) throws IOException
    {
        try
        {
            return getShort(name);
        } catch (NumberFormatException e)
        {
            return v;
        }
    }

    public float inout(String name, float v) throws IOException
    {
        try
        {
            return getFloat(name);
        } catch (NumberFormatException e)
        {
            return v;
        }
    }

    public boolean inout(String name, boolean v) throws IOException
    {
        try
        {
            return getBoolean(name);
        } catch (NumberFormatException e)
        {
            return v;
        }
    }

    public String inout(String name, String v) throws IOException
    {
        return getString(name);

    }

    public double inout(String name, double v) throws IOException
    {
        try
        {
            return getDouble(name);
        } catch (NumberFormatException e)
        {
            return v;
        }
    }

    public Serializable inout(String name, Serializable v) throws IOException
    {
        nodeStack.push(currentNode);
        currentNode = getNode(name);
        String type = currentNode.getAttributes().getNamedItem("type").getNodeValue();
        Serializable newElem = factory.create(type);
        newElem.serialize(this);
        currentNode = nodeStack.pop();
        return newElem;
    }

    public List inout(String name, List v) throws IOException
    {
        int size = getNrElements(name);
        for (int i = 0; i < size; i++)
        {
            v.add(getElement(name, i));
        }
        return v;
    }

    public Serializable getElement(String name, int i)
    {
        try
        {
            nodeStack.push(currentNode);
            currentNode = getNode(name);
            nodeStack.push(currentNode);
            currentNode = getCurrentElement(i);
            String type = currentNode.getAttributes().getNamedItem("type").getNodeValue();

            Serializable newElem = factory.create(type);

            newElem.serialize(this);

            currentNode = nodeStack.pop();
            currentNode = nodeStack.pop();

            return newElem;
        } catch (Exception ex)
        {
            return null;
        }
    }

    public boolean remove(Object o)
    {
        return factory.remove(o);
    }

    public boolean add(SerializableFactory e)
    {
        return factory.add(e);
    }

    public List<Integer> inoutIntegerList(String name, List<Integer> v) throws IOException
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public List<Long> inoutLongList(String name, List<Long> v) throws IOException
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public List<Byte> inoutByteList(String name, List<Byte> v) throws IOException
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public List<Short> inoutShortList(String name, List<Short> v) throws IOException
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public List<Float> inoutFloatList(String name, List<Float> v) throws IOException
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public List<Boolean> inoutBooleanList(String name, List<Boolean> v) throws IOException
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public List<String> inoutStringList(String name, List<String> v) throws IOException
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public List<Double> inoutDoubleList(String name, List<Double> v) throws IOException
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public List inoutSerializableList(String name, List v) throws IOException
    {
        int size = getNrElements(name);
        for (int i = 0; i < size; i++)
        {
            v.add(getElement(name, i));
        }
        return v;
    }
}
