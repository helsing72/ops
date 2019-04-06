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

#ifdef _WIN32
#define _WINSOCK_DEPRECATED_NO_WARNINGS
#endif

#include "CTcpTransportClient.h"
#include "BridgeLog.h"

namespace opsbridge {

CTcpTransportClient::CTcpTransportClient(std::string remoteHost, uint16_t remotePort):
	m_remoteHost(remoteHost), m_remotePort(remotePort)
#ifdef _WIN32
	, m_WsaInitialized(false)
#endif
{
#ifdef _WIN32
	WSADATA wsaData;
	// Initialize Winsock
    int iResult = WSAStartup(MAKEWORD(2,2), &wsaData);
    m_WsaInitialized = (iResult == NO_ERROR);
    if (!m_WsaInitialized)  {
		BL_ERROR("# [ TcpClient ] WSAStartup failed with error: %d\n", iResult); 
    }
#endif
}

CTcpTransportClient::~CTcpTransportClient()
{
	Terminate();
	WaitFor();
#ifdef _WIN32
	if (m_WsaInitialized) WSACleanup();
#endif
}

void CTcpTransportClient::Run()
{
	sockaddr_in		saClient;
	int				iResult;

	// The sockaddr_in structure specifies the address family,
	// IP address, and port of the server to be connected to.
	saClient.sin_family = AF_INET;
	saClient.sin_addr.s_addr = inet_addr(m_remoteHost.c_str());
	saClient.sin_port = htons(m_remotePort);

	//
	while (!terminated()) {
		// Connect phase
		while (!terminated()) {
			// Create socket
			m_socketCom = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
			if (m_socketCom == INVALID_SOCKET) {
				BL_ERROR("# [ TcpClient ] socket() creation failed with error: %d\n", GetError());

			} else {
			    // Connect to server.
				BL_TRACE("# [ TcpClient ] connecting to: %s, Port: %d\n", m_remoteHost.c_str(), m_remotePort);
			    
				iResult = connect(m_socketCom, (sockaddr*)&saClient, sizeof(saClient));
			    if (iResult != SOCKET_ERROR) break;

				BL_ERROR("# [ TcpClient ] connect() failed with error: %d\n", GetError());
				
				Close();
			}

			// Wait a short while and then try again
			std::this_thread::sleep_for(std::chrono::seconds(1));
		}

		if (!terminated()) {
			// Connected
			BL_INFO("# [ TcpClient ] CONNECTED\n"); 
				
			int optVal = 1;
			int optLen = sizeof(optVal);
			setsockopt(m_socketCom, IPPROTO_TCP, TCP_NODELAY, (char*)&optVal, optLen);

			m_Connected = true;
			if (m_user) m_user->onConnect(this);

			// Handle data from other side while connected
			HandleData();

			m_Connected = false;
			if (m_user) m_user->onDisconnect(this);

			BL_INFO("# [ TcpClient ] DISCONNECTED\n"); 
		}

		// Disconnect
		Close();
	}
}

}
