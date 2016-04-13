//Auto generated OPS-code. DO NOT MODIFY!

using Ops;
using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace pizza.special
{
    // Message

    [Serializable]
    public class ExtraAllt : LHCData
    {
		///Does the order include extra cheese???
		public bool extraCheese { get; set; }

		///@limits(0,INFINITY)
		public byte nrOfMushRooms { get; set; }

		public int meetQuality { get; set; }

		public long timestamp { get; set; }

		public float timeBakedHours { get; set; }

		public double timeBakedSeconds { get; set; }

		public string description { get; set; }

		private pizza.special.Cheese _cheese_ = new pizza.special.Cheese();
		[System.ComponentModel.TypeConverter(typeof(System.ComponentModel.ExpandableObjectConverter))]
		public pizza.special.Cheese cheese_ { get { return  _cheese_; } set { _cheese_ = value; } } 

		private List<bool> _bools = new List<bool>();
		public List<bool> bools { get { return  _bools; } set { _bools = value; } } 

		private List<byte> _bytes = new List<byte>();
		public List<byte> bytes { get { return  _bytes; } set { _bytes = value; } } 

		private List<int> _ints = new List<int>();
		public List<int> ints { get { return  _ints; } set { _ints = value; } } 

		private List<long> _longs = new List<long>();
		public List<long> longs { get { return  _longs; } set { _longs = value; } } 

		private List<float> _floats = new List<float>();
		public List<float> floats { get { return  _floats; } set { _floats = value; } } 

		private List<double> _doubles = new List<double>();
		public List<double> doubles { get { return  _doubles; } set { _doubles = value; } } 

		private List<string> _strings = new List<string>();
		[Editor(@"System.Windows.Forms.Design.StringCollectionEditor," +
			"System.Design, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a",
			typeof(System.Drawing.Design.UITypeEditor))]
		public List<string> strings { get { return  _strings; } set { _strings = value; } } 

		private List<pizza.special.Cheese> _cheeses = new List<pizza.special.Cheese>();
		public List<pizza.special.Cheese> cheeses { get { return  _cheeses; } set { _cheeses = value; } } 



        public static new string GetTypeName() { return "pizza.special.ExtraAllt"; }

        public ExtraAllt() : base()
        {
            AppendType(GetTypeName());

        }

        public override void Serialize(IArchiverInOut archive)
        {
            base.Serialize(archive);
			extraCheese = archive.Inout("extraCheese", extraCheese);
			nrOfMushRooms = archive.Inout("nrOfMushRooms", nrOfMushRooms);
			meetQuality = archive.Inout("meetQuality", meetQuality);
			timestamp = archive.Inout("timestamp", timestamp);
			timeBakedHours = archive.Inout("timeBakedHours", timeBakedHours);
			timeBakedSeconds = archive.Inout("timeBakedSeconds", timeBakedSeconds);
			description = archive.Inout("description", description);
			_cheese_ = (pizza.special.Cheese) archive.Inout("cheese_", _cheese_);
			_bools = (List<bool>) archive.InoutBooleanList("bools", _bools);
			_bytes = (List<byte>) archive.InoutByteList("bytes", _bytes);
			_ints = (List<int>) archive.InoutIntegerList("ints", _ints);
			_longs = (List<long>) archive.InoutLongList("longs", _longs);
			_floats = (List<float>) archive.InoutFloatList("floats", _floats);
			_doubles = (List<double>) archive.InoutDoubleList("doubles", _doubles);
			_strings = (List<string>) archive.InoutStringList("strings", _strings);
			_cheeses = (List<pizza.special.Cheese>) archive.InoutSerializableList("cheeses", _cheeses);

        }
    
        public override object Clone()
        {
            ExtraAllt cloneResult = new ExtraAllt();
            FillClone(cloneResult);
            return cloneResult;
        }

        public override void FillClone(OPSObject cloneO)
        {
            base.FillClone(cloneO);
            ExtraAllt cloneResult = (ExtraAllt)cloneO;
			cloneResult.extraCheese = this.extraCheese;
			cloneResult.nrOfMushRooms = this.nrOfMushRooms;
			cloneResult.meetQuality = this.meetQuality;
			cloneResult.timestamp = this.timestamp;
			cloneResult.timeBakedHours = this.timeBakedHours;
			cloneResult.timeBakedSeconds = this.timeBakedSeconds;
			cloneResult.description = this.description;
			cloneResult.cheese_ = (pizza.special.Cheese)this.cheese_.Clone();
			cloneResult.bools = new List<bool>(this.bools);
			cloneResult.bytes = new List<byte>(this.bytes);
			cloneResult.ints = new List<int>(this.ints);
			cloneResult.longs = new List<long>(this.longs);
			cloneResult.floats = new List<float>(this.floats);
			cloneResult.doubles = new List<double>(this.doubles);
			cloneResult.strings = new List<string>(this.strings);
			cloneResult.cheeses = new List<pizza.special.Cheese>(this.cheeses.Count);
			this.cheeses.ForEach((item) => { cloneResult.cheeses.Add((pizza.special.Cheese)item.Clone()); });

        }
    }

    // Publisher

    public class ExtraAlltPublisher : Publisher
    {
        public ExtraAlltPublisher(Topic t) : base(t)
        {
            CheckTypeString(ExtraAllt.GetTypeName());
        }

        public void Write(ExtraAllt o)
        {
            base.Write(o);
        }
    }

    // Subscriber

    public delegate void ExtraAlltEventHandler(ExtraAlltSubscriber sender, ExtraAllt data);

    public class ExtraAlltSubscriber : Subscriber
    {
        public event ExtraAlltEventHandler newData;

        public ExtraAlltSubscriber(Topic t) : base(t)
        {
            CheckTypeString(ExtraAllt.GetTypeName());
        }

        protected override void NewDataArrived(OPSObject o)
        {
            if (newData != null) newData(this, (ExtraAllt)o);
        }

        public ExtraAllt WaitForNextData(long millis)
        {
            return (ExtraAllt)WaitForNextOpsObjectData(millis);
        }
    }

}
