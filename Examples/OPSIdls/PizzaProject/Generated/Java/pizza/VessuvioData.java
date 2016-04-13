//Auto generated OPS-code. DO NOT MODIFY!

package pizza;

import ops.OPSObject;
import configlib.ArchiverInOut;
import configlib.SerializableFactory;
import configlib.Serializable;
import java.io.IOException;

public class VessuvioData extends PizzaData
{
    public String ham = "";


    private static SerializableFactory factory = new TypeFactory();

    public static String getTypeName(){return "pizza.VessuvioData";}

    public static SerializableFactory getTypeFactory()
    {
        return factory;
    }

    public VessuvioData()
    {
        super();
        appendType(getTypeName());

    }
    public void serialize(ArchiverInOut archive) throws IOException
    {
        super.serialize(archive);
        ham = archive.inout("ham", ham);

    }
    @Override
    public Object clone()
    {
        VessuvioData cloneResult = new VessuvioData();
        fillClone(cloneResult);
        return cloneResult;
    }

    @Override
    public void fillClone(OPSObject cloneO)
    {
        super.fillClone(cloneO);
        VessuvioData cloneResult = (VessuvioData)cloneO;
                cloneResult.ham = this.ham;

    }

    private static class TypeFactory implements SerializableFactory
    {
        public Serializable create(String type)
        {
            if (type.equals(VessuvioData.getTypeName()))
            {
                return new VessuvioData();
            }
            else
            {
                return null;
            }
        }
    }
}

