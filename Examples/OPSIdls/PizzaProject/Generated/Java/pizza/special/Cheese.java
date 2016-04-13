//Auto generated OPS-code. DO NOT MODIFY!

package pizza.special;

import ops.OPSObject;
import configlib.ArchiverInOut;
import configlib.SerializableFactory;
import configlib.Serializable;
import java.io.IOException;

public class Cheese extends OPSObject
{
    public String name = "";
    public double age;


    private static SerializableFactory factory = new TypeFactory();

    public static String getTypeName(){return "pizza.special.Cheese";}

    public static SerializableFactory getTypeFactory()
    {
        return factory;
    }

    public Cheese()
    {
        super();
        appendType(getTypeName());

    }
    public void serialize(ArchiverInOut archive) throws IOException
    {
        super.serialize(archive);
        name = archive.inout("name", name);
        age = archive.inout("age", age);

    }
    @Override
    public Object clone()
    {
        Cheese cloneResult = new Cheese();
        fillClone(cloneResult);
        return cloneResult;
    }

    @Override
    public void fillClone(OPSObject cloneO)
    {
        super.fillClone(cloneO);
        Cheese cloneResult = (Cheese)cloneO;
                cloneResult.name = this.name;
        cloneResult.age = this.age;

    }

    private static class TypeFactory implements SerializableFactory
    {
        public Serializable create(String type)
        {
            if (type.equals(Cheese.getTypeName()))
            {
                return new Cheese();
            }
            else
            {
                return null;
            }
        }
    }
}

