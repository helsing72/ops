//Auto generated OPS-code. DO NOT MODIFY!

package pizza;

import ops.OPSObject;
import configlib.ArchiverInOut;
import configlib.SerializableFactory;
import configlib.Serializable;
import java.io.IOException;

public class PizzaData extends OPSObject
{
    public String cheese = "";
    public String tomatoSauce = "";


    private static SerializableFactory factory = new TypeFactory();

    public static String getTypeName(){return "pizza.PizzaData";}

    public static SerializableFactory getTypeFactory()
    {
        return factory;
    }

    public PizzaData()
    {
        super();
        appendType(getTypeName());

    }
    public void serialize(ArchiverInOut archive) throws IOException
    {
        super.serialize(archive);
        cheese = archive.inout("cheese", cheese);
        tomatoSauce = archive.inout("tomatoSauce", tomatoSauce);

    }
    @Override
    public Object clone()
    {
        PizzaData cloneResult = new PizzaData();
        fillClone(cloneResult);
        return cloneResult;
    }

    @Override
    public void fillClone(OPSObject cloneO)
    {
        super.fillClone(cloneO);
        PizzaData cloneResult = (PizzaData)cloneO;
                cloneResult.cheese = this.cheese;
        cloneResult.tomatoSauce = this.tomatoSauce;

    }

    private static class TypeFactory implements SerializableFactory
    {
        public Serializable create(String type)
        {
            if (type.equals(PizzaData.getTypeName()))
            {
                return new PizzaData();
            }
            else
            {
                return null;
            }
        }
    }
}

