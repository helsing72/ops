package configlib;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

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
public class XMLArchiverIn extends ArchiverInOut
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
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            doc = db.parse(is);

            currentNode = doc;
        } catch (ParserConfigurationException ex)
        {
            throw new FormatException("Caused by underlying ParserConfigurationException: " + ex.getMessage());
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
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            doc = db.parse(is);

            currentNode = doc;
            currentNode = getNode(rootNode);
        } catch (ParserConfigurationException ex)
        {
            throw new FormatException("Caused by underlying ParserConfigurationException: " + ex.getMessage());
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
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            doc = db.parse(is);

            currentNode = doc;
        } catch (ParserConfigurationException ex)
        {
            throw new FormatException("Caused by underlying ParserConfigurationException: " + ex.getMessage());
        } catch (SAXException ex)
        {
            throw new FormatException("Caused by underlying SAXException: " + ex.getMessage());
        } catch (IOException ex)
        {
            throw new FormatException("Caused by underlying IOException: " + ex.getMessage());
        }
    }

    public boolean isOut() { return false; }

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

    interface Converter<T> {
        T operation(String inp);
    }

    private <T> List<T> inoutCoretypeList(String name, List<T> v, Converter<T> conv)
    {
        int size = getNrElements(name);

        nodeStack.push(currentNode);
        currentNode = getNode(name);

        v.clear();

        for (int i = 0; i < size; i++)
        {
            Node node = getCurrentElement(i);
            v.add(conv.operation(node.getTextContent().trim()));
        }

        currentNode = nodeStack.pop();

        return v;
    }

    public List<Integer> inoutIntegerList(String name, List<Integer> v) throws IOException
    {
        return inoutCoretypeList(name, v, (x) -> { return Integer.parseInt(x); });
    }

    public List<Long> inoutLongList(String name, List<Long> v) throws IOException
    {
        return inoutCoretypeList(name, v, (x) -> { return Long.parseLong(x); });
    }

    public List<Byte> inoutByteList(String name, List<Byte> v) throws IOException
    {
        return inoutCoretypeList(name, v, (x) -> { return Byte.parseByte(x); });
    }

    public List<Short> inoutShortList(String name, List<Short> v) throws IOException
    {
        return inoutCoretypeList(name, v, (x) -> { return Short.parseShort(x); });
    }

    public List<Float> inoutFloatList(String name, List<Float> v) throws IOException
    {
      return inoutCoretypeList(name, v, (x) -> { return Float.parseFloat(x); });
    }

    public List<Boolean> inoutBooleanList(String name, List<Boolean> v) throws IOException
    {
        return inoutCoretypeList(name, v, (x) -> { return Boolean.parseBoolean(x); });
    }

    public List<String> inoutStringList(String name, List<String> v) throws IOException
    {
        return inoutCoretypeList(name, v, (x) -> { return x; });
    }

    public List<Double> inoutDoubleList(String name, List<Double> v) throws IOException
    {
        return inoutCoretypeList(name, v, (x) -> { return Double.parseDouble(x); });
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
