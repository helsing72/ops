/**
*
* Copyright (C) 2010-2012 Saab Dynamics AB
*   author Lennart Andersson <nnnn@saabgroup.com>
*
* Copyright (C) 2018-2019 Lennart Andersson.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.

* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*/

#pragma once

#include "CSocketTransport.h"

namespace opsbridge {

	class CTcpTransportServer : public CSocketTransport
	{
	public:
		CTcpTransportServer(uint16_t const localPort);
		~CTcpTransportServer();

		void Terminate();

	protected:
		void Run();

		void CloseServer();

	private:
		uint16_t m_localPort;
		SOCKET m_listenSocket;
#ifdef _WIN32
		bool m_WsaInitialized;
#endif
	};

}
