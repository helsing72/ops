//Auto generated OPS-code. DO NOT MODIFY!

using Ops;
using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace pizza
{
    // Message

    [Serializable]
    public class PizzaData : OPSObject
    {
		public string cheese { get; set; }

		public string tomatoSauce { get; set; }



        public static new string GetTypeName() { return "pizza.PizzaData"; }

        public PizzaData() : base()
        {
            AppendType(GetTypeName());

        }

        public override void Serialize(IArchiverInOut archive)
        {
            base.Serialize(archive);
			cheese = archive.Inout("cheese", cheese);
			tomatoSauce = archive.Inout("tomatoSauce", tomatoSauce);

        }
    
        public override object Clone()
        {
            PizzaData cloneResult = new PizzaData();
            FillClone(cloneResult);
            return cloneResult;
        }

        public override void FillClone(OPSObject cloneO)
        {
            base.FillClone(cloneO);
            PizzaData cloneResult = (PizzaData)cloneO;
			cloneResult.cheese = this.cheese;
			cloneResult.tomatoSauce = this.tomatoSauce;

        }
    }

    // Publisher

    public class PizzaDataPublisher : Publisher
    {
        public PizzaDataPublisher(Topic t) : base(t)
        {
            CheckTypeString(PizzaData.GetTypeName());
        }

        public void Write(PizzaData o)
        {
            base.Write(o);
        }
    }

    // Subscriber

    public delegate void PizzaDataEventHandler(PizzaDataSubscriber sender, PizzaData data);

    public class PizzaDataSubscriber : Subscriber
    {
        public event PizzaDataEventHandler newData;

        public PizzaDataSubscriber(Topic t) : base(t)
        {
            CheckTypeString(PizzaData.GetTypeName());
        }

        protected override void NewDataArrived(OPSObject o)
        {
            if (newData != null) newData(this, (PizzaData)o);
        }

        public PizzaData WaitForNextData(long millis)
        {
            return (PizzaData)WaitForNextOpsObjectData(millis);
        }
    }

}
