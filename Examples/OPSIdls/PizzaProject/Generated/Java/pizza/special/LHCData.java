//Auto generated OPS-code. DO NOT MODIFY!

package pizza.special;

import ops.OPSObject;
import configlib.ArchiverInOut;
import configlib.SerializableFactory;
import configlib.Serializable;
import java.io.IOException;

public class LHCData extends pizza.CapricosaData
{
    public String bearnaise = "";
    public String beef = "";
    public java.util.Vector<pizza.PizzaData> p = new java.util.Vector<pizza.PizzaData>();


    private static SerializableFactory factory = new TypeFactory();

    public static String getTypeName(){return "pizza.special.LHCData";}

    public static SerializableFactory getTypeFactory()
    {
        return factory;
    }

    public LHCData()
    {
        super();
        appendType(getTypeName());

    }
    public void serialize(ArchiverInOut archive) throws IOException
    {
        super.serialize(archive);
        bearnaise = archive.inout("bearnaise", bearnaise);
        beef = archive.inout("beef", beef);
        p = (java.util.Vector<pizza.PizzaData>) archive.inoutSerializableList("p", p);

    }
    @Override
    public Object clone()
    {
        LHCData cloneResult = new LHCData();
        fillClone(cloneResult);
        return cloneResult;
    }

    @Override
    public void fillClone(OPSObject cloneO)
    {
        super.fillClone(cloneO);
        LHCData cloneResult = (LHCData)cloneO;
                cloneResult.bearnaise = this.bearnaise;
        cloneResult.beef = this.beef;
        java.util.Collections.copy(cloneResult.p, this.p);

    }

    private static class TypeFactory implements SerializableFactory
    {
        public Serializable create(String type)
        {
            if (type.equals(LHCData.getTypeName()))
            {
                return new LHCData();
            }
            else
            {
                return null;
            }
        }
    }
}

