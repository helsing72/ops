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

namespace opsbridge {

#pragma pack(push, 1)

	// Message types
	typedef enum : uint8_t {mtOps, mtAckNak, mtCommand, mtStatus, mtUdpMc} TMessageType;

	typedef struct {
		uint32_t Length;
		TMessageType Type;	
	} THeader;

	// Will be followed by:
	//   * TopicName (incl. in Head.Length)
	//   * PublisherName (incl. in Head.Length)
	//   * OPS Message excl. sparebytes  (incl. in Head.Length)
	//   * Sparebytes from OPS Object
	typedef struct {
		THeader Head;
		uint16_t PartNo;
		uint16_t TotalParts;
		uint32_t DataLength;		// Sparebytes length
		uint64_t AckCounter;
	} TOpsMessage;

	constexpr auto EC_NO_ERROR = 0;
	constexpr auto EC_MISSING_PUB = 1;
	constexpr auto EC_PUBLISH_FAILED = 2;

	typedef struct {
		THeader Head;
		uint64_t AckCounter;
		uint32_t ErrorCode;		// 0 = No error, others = error code
	} TAckNakMessage;

	// Commands
	typedef enum : uint8_t {ctPause, ctContinue, ctPreparePub} TCommandType;

	typedef struct {
		THeader Head;
		TCommandType Command;
		ops::ObjectName_T DestTopicName;	// Only valid for ctPreparePub
		ops::TypeId_T DataType;				// Only valid for ctPreparePub
		uint64_t AckCounter;				// Only valid for ctPreparePub
	} TCommandMessage;

	typedef struct {
		THeader Head;
		uint32_t NumQueuedMessages;
		bool Paused;
	} TStatusMessage;

	// Will be followed by:
	//   * Message Data
	typedef struct {
		THeader Head;
		uint32_t SrcIP;
		uint32_t DstMcIP;
		uint16_t SrcPort;
		uint16_t DstMcPort;
		uint32_t DataLength;		// Message length
	} TUdpMcMessage;

#pragma pack(pop)
}
