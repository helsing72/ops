//Auto generated OPS-code. DO NOT MODIFY!

using Ops;
using System;
using System.ComponentModel;

namespace __packageName
{
    [Serializable]
    public class __className : Ops.OPSObject
    {
        public enum Value {__declarations UNDEFINED};

        public Value value { get; set; }

        public static new string GetTypeName() { return "__packageName.__className"; }

        public __className() : base()
        {
            AppendType(GetTypeName());
        }

        public override void Serialize(IArchiverInOut archive)
        {
            base.Serialize(archive);
            value = FromInt(archive.Inout("value", ToInt(value)));
        }

        public override object Clone()
        {
            __className cloneResult = new __className();
            cloneResult.value = this.value;
            return cloneResult;
        }

        private Value FromInt(int i)
        {
            return (Value)i;
            //return Value.UNDEFINED;
        }

        private int ToInt(Value value)
        {
            return (int)value;
        }
    }
}
