/**
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

#include <thread>

#include "ops.h"

#include "BridgeConfig.h"
#include "BridgeLog.h"
#include "RawMcUdp.h"
#include "CBridge.h"
#include "CTcpTransportClient.h"
#include "CTcpTransportServer.h"

// Default values
int BL_log_level = 0;
std::string cfgfile = "opsbridge_config.xml";
int bridgeNumber = 0;

void menu()
{
	std::cout << "\n";
	std::cout << "Usage: OPSBridge <commands>\n";
	std::cout << "\n";
	std::cout << "  -b n            Bridge number to use from configuration file (" << bridgeNumber << ")\n";
	std::cout << "  -c cfgfile      OPSBridge configuration file to use (" << cfgfile << ")\n";
	std::cout << "  -l n            Log level to use 0..9 (" << BL_log_level << ")(higher value --> less logging)\n";
	std::cout << "  -h | -?         Show this help\n";
	std::cout << "\n";
}

int main(int argc, char* argv[])
{
	bool show_menu = false;

	for (int i = 1; i < argc; ) {
		std::string arg = argv[i++];

		std::string* stringp = nullptr;
		int* intp = nullptr;

		if (arg == "-b") {
			intp = &bridgeNumber;
		}
		if (arg == "-c") {
			stringp = &cfgfile;
		}
		if (arg == "-l") {
			intp = &BL_log_level;
		}
		if ((arg == "-?") || (arg == "-h")) {
			show_menu = true;
		}
		if (i < argc) {
			if (stringp) *stringp = argv[i++];
			if (intp) *intp = atoi(argv[i++]);
		}
	}

	if (show_menu || (argc <= 1)) {
		menu();
		exit(0);
	}

	opsbridge::BridgeConfig cfg(cfgfile);

	// Setup the OPS static error service (common for all participants, reports errors during participant creation)
	ops::ErrorWriter* errorWriterStatic = new ops::ErrorWriter(std::cout);
	ops::Participant::getStaticErrorService()->addListener(errorWriterStatic);

	if ((int)cfg.vBridges.size() <= bridgeNumber) return 2;

	opsbridge::BridgeConfig::TBridgeConfig bc = cfg.vBridges[bridgeNumber];

	BL_INFO("Started bridge: %s\n", bc.sBridgeName.c_str());

	opsbridge::CTransport* transport = nullptr;
	switch (bc.endpoint.eType) {
	case opsbridge::BridgeConfig::local: 
		return 3;
		break;
	case opsbridge::BridgeConfig::tcpClient: 
		transport = new opsbridge::CTcpTransportClient(bc.endpoint.remoteHost, bc.endpoint.remotePort);
		break;
	case opsbridge::BridgeConfig::tcpServer: 
		transport = new opsbridge::CTcpTransportServer(bc.endpoint.localPort);
		BL_INFO("Running as server on port: %d\n", bc.endpoint.localPort);
		break;
	}

	opsbridge::CBridge br(bc.sBridgeName, bc.iBufferSize, bc.iMinPubTime_ms, transport);

	for (size_t i = 0; i < bc.vRawReceives.size(); i++) {
		br.getRef().AddReceiver(bc.vRawReceives[i].sIp, bc.vRawReceives[i].port, bc.vRawReceives[i].sIfc);
	}
	
	for (size_t i = 0; i < bc.vRawSends.size(); i++) {
		br.getRef().AddTranslation(
			bc.vRawSends[i].rec.sIp, bc.vRawSends[i].rec.port,
			bc.vRawSends[i].sNewIp, bc.vRawSends[i].newPort,
			bc.vRawSends[i].sIfc, bc.vRawSends[i].ttl
		);
	}

	br.Start();

	BL_INFO("\n\nPress ^C to stop bridge ...\n\n");

	for (;;) { std::this_thread::sleep_for(std::chrono::milliseconds(1000)); }

	return 0;
}
