/**
 *
 * OPS generated code, DO NOT MODIFY!
 */
package PizzaProject;

import configlib.Serializable;
import configlib.SerializableFactory;


public class PizzaProjectTypeFactory implements SerializableFactory
{
    public Serializable create(String type)
    {
        if(type.equals("pizza.CapricosaData"))
        {
            return new pizza.CapricosaData();
        }
        if(type.equals("pizza.PizzaData"))
        {
            return new pizza.PizzaData();
        }
        if(type.equals("pizza.special.Cheese"))
        {
            return new pizza.special.Cheese();
        }
        if(type.equals("pizza.special.ExtraAllt"))
        {
            return new pizza.special.ExtraAllt();
        }
        if(type.equals("pizza.special.LHCData"))
        {
            return new pizza.special.LHCData();
        }
        if(type.equals("pizza.VessuvioData"))
        {
            return new pizza.VessuvioData();
        }
        return null;

    }
}
