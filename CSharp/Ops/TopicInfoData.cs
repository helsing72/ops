///////////////////////////////////////////////////////////
//  TopicInfoData.cs
//  Implementation of the Class TopicInfoData
//  Created on:      01-jan-2013 
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace Ops
{
    // NOTE. Must be kept in sync with C++
	public class TopicInfoData : OPSObject
	{
        public string name { get; set; }
        public string type { get; set; }
        public string transport { get; set; }
        public string address { get; set; }
        public int port { get; set; }

        // C++ version: std::vector<string> keys;
        private List<string> _keys = new List<string>();
        [Editor(@"System.Windows.Forms.Design.StringCollectionEditor," +
            "System.Design, Version=2.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a",
            typeof(System.Drawing.Design.UITypeEditor))]
        public List<string> keys { get { return _keys; } set { _keys = value; } }

        // C++ version: //std::vector<OPSObject*> filters;
		

		public TopicInfoData()
		{
			AppendType("TopicInfoData");
		}

		public TopicInfoData(Topic topic)
		{
			AppendType("TopicInfoData");
			name = topic.GetName();
			type = topic.GetTypeID();
			transport = topic.GetTransport();
			address = topic.GetDomainAddress();
			port = topic.GetPort();
			//keys;
		}

        public override void Serialize(IArchiverInOut archive)
		{
			base.Serialize(archive);

			name = archive.Inout("name", name);
            type = archive.Inout("type", type);
            transport = archive.Inout("transport", transport);
            address = archive.Inout("address", address);
            port = archive.Inout("port", port);
            _keys = (List<string>)archive.InoutStringList("keys", _keys);

            //archiver->inout(std::string("filters"), filters);
		}
		
	};

}
