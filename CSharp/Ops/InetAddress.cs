using System;
using System.Net;

namespace Ops
{
	public class InetAddress
	{
		internal IPAddress addr;

        public InetAddress(string addr)
		{
			this.addr = IPAddress.Parse(addr);
		}

        public InetAddress(IPAddress addr)
		{
			this.addr = addr;
		}

		public bool IsAnyLocalAddress()
		{
			return IPAddress.IsLoopback(addr);
		}

		public bool Equals(InetAddress addr)
		{
			return addr.ToString().Equals( addr.ToString());
		}

		public bool Equals(string addr)
		{
			return addr.ToString().Equals( addr.ToString());
		}

		public override string ToString()
		{
			return addr.ToString ();
		}

		public override bool Equals(object obj)
		{
			return Equals (obj.ToString());
		}

        public IPAddress GetIPAddress()
        {
            return this.addr;
        }

        public string GetHostAddress()
		{
			return ToString();
		}

		public override int GetHashCode()
		{
			return base.GetHashCode();
		}

		public static InetAddress GetByName(string name)
		{
#pragma warning disable 618
			return new InetAddress( Dns.GetHostByName(name).AddressList[0] );
#pragma warning restore 618
        }
	}
}
