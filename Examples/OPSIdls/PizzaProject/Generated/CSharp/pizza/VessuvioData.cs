//Auto generated OPS-code. DO NOT MODIFY!

using Ops;
using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace pizza
{
    // Message

    [Serializable]
    public class VessuvioData : PizzaData
    {
        public string ham { get; set; }



        public static new string GetTypeName() { return "pizza.VessuvioData"; }

        public VessuvioData() : base()
        {
            AppendType(GetTypeName());

        }

        public override void Serialize(IArchiverInOut archive)
        {
            base.Serialize(archive);
            ham = archive.Inout("ham", ham);

        }
    
        public override object Clone()
        {
            VessuvioData cloneResult = new VessuvioData();
            FillClone(cloneResult);
            return cloneResult;
        }

        public override void FillClone(OPSObject cloneO)
        {
            base.FillClone(cloneO);
            VessuvioData cloneResult = (VessuvioData)cloneO;
            cloneResult.ham = this.ham;

        }
    }

    // Publisher

    public class VessuvioDataPublisher : Publisher
    {
        public VessuvioDataPublisher(Topic t) : base(t)
        {
            CheckTypeString(VessuvioData.GetTypeName());
        }

        public void Write(VessuvioData o)
        {
            base.Write(o);
        }
    }

    // Subscriber

    public delegate void VessuvioDataEventHandler(VessuvioDataSubscriber sender, VessuvioData data);

    public class VessuvioDataSubscriber : Subscriber
    {
        public event VessuvioDataEventHandler newData;

        public VessuvioDataSubscriber(Topic t) : base(t)
        {
            CheckTypeString(VessuvioData.GetTypeName());
        }

        protected override void NewDataArrived(OPSObject o)
        {
            if (newData != null) newData(this, (VessuvioData)o);
        }

        public VessuvioData WaitForNextData(long millis)
        {
            return (VessuvioData)WaitForNextOpsObjectData(millis);
        }
    }

}
