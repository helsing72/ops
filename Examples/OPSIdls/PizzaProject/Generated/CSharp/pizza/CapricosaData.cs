//Auto generated OPS-code. DO NOT MODIFY!

using Ops;
using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace pizza
{
    // Message

    [Serializable]
    public class CapricosaData : VessuvioData
    {
		public string mushrooms { get; set; }



        public static new string GetTypeName() { return "pizza.CapricosaData"; }

        public CapricosaData() : base()
        {
            AppendType(GetTypeName());

        }

        public override void Serialize(IArchiverInOut archive)
        {
            base.Serialize(archive);
			mushrooms = archive.Inout("mushrooms", mushrooms);

        }
    
        public override object Clone()
        {
            CapricosaData cloneResult = new CapricosaData();
            FillClone(cloneResult);
            return cloneResult;
        }

        public override void FillClone(OPSObject cloneO)
        {
            base.FillClone(cloneO);
            CapricosaData cloneResult = (CapricosaData)cloneO;
			cloneResult.mushrooms = this.mushrooms;

        }
    }

    // Publisher

    public class CapricosaDataPublisher : Publisher
    {
        public CapricosaDataPublisher(Topic t) : base(t)
        {
            CheckTypeString(CapricosaData.GetTypeName());
        }

        public void Write(CapricosaData o)
        {
            base.Write(o);
        }
    }

    // Subscriber

    public delegate void CapricosaDataEventHandler(CapricosaDataSubscriber sender, CapricosaData data);

    public class CapricosaDataSubscriber : Subscriber
    {
        public event CapricosaDataEventHandler newData;

        public CapricosaDataSubscriber(Topic t) : base(t)
        {
            CheckTypeString(CapricosaData.GetTypeName());
        }

        protected override void NewDataArrived(OPSObject o)
        {
            if (newData != null) newData(this, (CapricosaData)o);
        }

        public CapricosaData WaitForNextData(long millis)
        {
            return (CapricosaData)WaitForNextOpsObjectData(millis);
        }
    }

}
