//Auto generated OPS-code. DO NOT MODIFY!

using Ops;
using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace pizza.special
{
    // Message

    [Serializable]
    public class LHCData : pizza.CapricosaData
    {
        public string bearnaise { get; set; }

        public string beef { get; set; }

        private List<pizza.PizzaData> _p = new List<pizza.PizzaData>();
        public List<pizza.PizzaData> p { get { return  _p; } set { _p = value; } } 



        public static new string GetTypeName() { return "pizza.special.LHCData"; }

        public LHCData() : base()
        {
            AppendType(GetTypeName());

        }

        public override void Serialize(IArchiverInOut archive)
        {
            base.Serialize(archive);
            bearnaise = archive.Inout("bearnaise", bearnaise);
            beef = archive.Inout("beef", beef);
            _p = (List<pizza.PizzaData>) archive.InoutSerializableList("p", _p);

        }
    
        public override object Clone()
        {
            LHCData cloneResult = new LHCData();
            FillClone(cloneResult);
            return cloneResult;
        }

        public override void FillClone(OPSObject cloneO)
        {
            base.FillClone(cloneO);
            LHCData cloneResult = (LHCData)cloneO;
            cloneResult.bearnaise = this.bearnaise;
            cloneResult.beef = this.beef;
            cloneResult.p = new List<pizza.PizzaData>(this.p.Count);
            this.p.ForEach((item) => { cloneResult.p.Add((pizza.PizzaData)item.Clone()); });

        }
    }

    // Publisher

    public class LHCDataPublisher : Publisher
    {
        public LHCDataPublisher(Topic t) : base(t)
        {
            CheckTypeString(LHCData.GetTypeName());
        }

        public void Write(LHCData o)
        {
            base.Write(o);
        }
    }

    // Subscriber

    public delegate void LHCDataEventHandler(LHCDataSubscriber sender, LHCData data);

    public class LHCDataSubscriber : Subscriber
    {
        public event LHCDataEventHandler newData;

        public LHCDataSubscriber(Topic t) : base(t)
        {
            CheckTypeString(LHCData.GetTypeName());
        }

        protected override void NewDataArrived(OPSObject o)
        {
            if (newData != null) newData(this, (LHCData)o);
        }

        public LHCData WaitForNextData(long millis)
        {
            return (LHCData)WaitForNextOpsObjectData(millis);
        }
    }

}
