//Auto generated OPS-code. DO NOT MODIFY!

package pizza;

import ops.OPSObject;
import configlib.ArchiverInOut;
import configlib.SerializableFactory;
import configlib.Serializable;
import java.io.IOException;

public class CapricosaData extends VessuvioData
{
    public String mushrooms = "";


    private static SerializableFactory factory = new TypeFactory();

    public static String getTypeName(){return "pizza.CapricosaData";}

    public static SerializableFactory getTypeFactory()
    {
        return factory;
    }

    public CapricosaData()
    {
        super();
        appendType(getTypeName());

    }
    public void serialize(ArchiverInOut archive) throws IOException
    {
        super.serialize(archive);
        mushrooms = archive.inout("mushrooms", mushrooms);

    }
    @Override
    public Object clone()
    {
        CapricosaData cloneResult = new CapricosaData();
        fillClone(cloneResult);
        return cloneResult;
    }

    @Override
    public void fillClone(OPSObject cloneO)
    {
        super.fillClone(cloneO);
        CapricosaData cloneResult = (CapricosaData)cloneO;
                cloneResult.mushrooms = this.mushrooms;

    }

    private static class TypeFactory implements SerializableFactory
    {
        public Serializable create(String type)
        {
            if (type.equals(CapricosaData.getTypeName()))
            {
                return new CapricosaData();
            }
            else
            {
                return null;
            }
        }
    }
}

