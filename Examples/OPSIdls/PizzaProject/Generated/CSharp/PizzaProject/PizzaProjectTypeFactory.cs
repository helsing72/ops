/**
 *
 * OPS generated code, DO NOT MODIFY!
 */

using Ops;

namespace PizzaProject
{
    public class PizzaProjectTypeFactory : ISerializableFactory
    {
        public ISerializable Create(string type)
        {
            if (type.Equals("pizza.CapricosaData"))
            {
                return new pizza.CapricosaData();
            }
            if (type.Equals("pizza.PizzaData"))
            {
                return new pizza.PizzaData();
            }
            if (type.Equals("pizza.special.Cheese"))
            {
                return new pizza.special.Cheese();
            }
            if (type.Equals("pizza.special.ExtraAllt"))
            {
                return new pizza.special.ExtraAllt();
            }
            if (type.Equals("pizza.special.LHCData"))
            {
                return new pizza.special.LHCData();
            }
            if (type.Equals("pizza.VessuvioData"))
            {
                return new pizza.VessuvioData();
            }
            return null;

        }

        public string Create(object obj)
        {
            if (obj is pizza.CapricosaData)
            {
                return "pizza.CapricosaData";
            }
            if (obj is pizza.PizzaData)
            {
                return "pizza.PizzaData";
            }
            if (obj is pizza.special.Cheese)
            {
                return "pizza.special.Cheese";
            }
            if (obj is pizza.special.ExtraAllt)
            {
                return "pizza.special.ExtraAllt";
            }
            if (obj is pizza.special.LHCData)
            {
                return "pizza.special.LHCData";
            }
            if (obj is pizza.VessuvioData)
            {
                return "pizza.VessuvioData";
            }
            return null;

        }

    }
}
