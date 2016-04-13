//Auto generated OPS-code. DO NOT MODIFY!

using Ops;
using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace pizza.special
{
    // Message

    [Serializable]
    public class Cheese : OPSObject
    {
        public string name { get; set; }

        public double age { get; set; }



        public static new string GetTypeName() { return "pizza.special.Cheese"; }

        public Cheese() : base()
        {
            AppendType(GetTypeName());

        }

        public override void Serialize(IArchiverInOut archive)
        {
            base.Serialize(archive);
            name = archive.Inout("name", name);
            age = archive.Inout("age", age);

        }
    
        public override object Clone()
        {
            Cheese cloneResult = new Cheese();
            FillClone(cloneResult);
            return cloneResult;
        }

        public override void FillClone(OPSObject cloneO)
        {
            base.FillClone(cloneO);
            Cheese cloneResult = (Cheese)cloneO;
            cloneResult.name = this.name;
            cloneResult.age = this.age;

        }
    }

    // Publisher

    public class CheesePublisher : Publisher
    {
        public CheesePublisher(Topic t) : base(t)
        {
            CheckTypeString(Cheese.GetTypeName());
        }

        public void Write(Cheese o)
        {
            base.Write(o);
        }
    }

    // Subscriber

    public delegate void CheeseEventHandler(CheeseSubscriber sender, Cheese data);

    public class CheeseSubscriber : Subscriber
    {
        public event CheeseEventHandler newData;

        public CheeseSubscriber(Topic t) : base(t)
        {
            CheckTypeString(Cheese.GetTypeName());
        }

        protected override void NewDataArrived(OPSObject o)
        {
            if (newData != null) newData(this, (Cheese)o);
        }

        public Cheese WaitForNextData(long millis)
        {
            return (Cheese)WaitForNextOpsObjectData(millis);
        }
    }

}
