/**
 *
 * Copyright (C) 2006-2009 Anton Gravestam.
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

#include "OPSTypeDefs.h"
#include "ReceiveDataChannel.h"
#include "OPSArchiverIn.h"
#include "BasicError.h" 
#include "Domain.h"
#include "Participant.h"
#include "ReceiverFactory.h"
#include "CommException.h"
#include "DataSegmentPool.h"

namespace ops
{

	ReceiveDataChannel::ReceiveDataChannel(Topic top, Participant& part, Receiver* recv) :
		_client(nullptr),
		receiver(recv),
		memMap(top.getSampleMaxSize() / OPSConstants::PACKET_MAX_SIZE + 1, OPSConstants::PACKET_MAX_SIZE, &DataSegmentAllocator::Instance()),
		sampleMaxSize(top.getSampleMaxSize()),
		participant(part),
		currentMessageSize(0),
		expectedSegment(0),
		firstReceived(false)
    {
		if (receiver == nullptr) { receiver = ReceiverFactory::getReceiver(top, participant); }

        if (receiver == nullptr) {
            throw exceptions::CommException("Could not create receiver");
        }

        receiver->addListener(this);
    }

    ReceiveDataChannel::~ReceiveDataChannel()
    {
        delete receiver;
    }

	void ReceiveDataChannel::start()
    {
		receiver->start();
		expectedSegment = 0;
		currentMessageSize = 0;
        receiver->asynchWait(memMap.getSegment(expectedSegment), memMap.getSegmentSize());
	}

    void ReceiveDataChannel::stop()
    {
		receiver->stop();
	}

	static void ReportError(Participant& participant, ErrorMessage_T message, Address_T addr, uint16_t port)
	{
		message += " [";
		message += addr;
		message += "::";
		message += NumberToString(port);
		message += ']';
		BasicError err("ReceiveDataChannel", "onNewEvent", message);
		participant.reportError(&err);
	}

    ///Override from Listener
    ///Called whenever the receiver has new data.
    void ReceiveDataChannel::onNewEvent(Notifier<BytesSizePair>* sender, BytesSizePair byteSizePair)
    {
		UNUSED(sender);
        if (byteSizePair.size <= 0)
        {
            //Inform participant that we had an error waiting for data,
            //this means the underlying socket is down but hopefully it will reconnect, so no need to do anything.
            //Only happens with tcp connections so far.

            if (byteSizePair.size == -5)
            {
                BasicError err("ReceiveDataChannel", "onNewEvent", "Connection was lost but is now reconnected.");
                participant.reportError(&err);
            }
            else
            {
                BasicError err("ReceiveDataChannel", "onNewEvent", "Empty message or error.");
                participant.reportError(&err);
            }

            receiver->asynchWait(memMap.getSegment(expectedSegment), memMap.getSegmentSize());
            return;
        }

		///TODO Check that all segments come from the same source (IP and port)
		Address_T addr;
		uint16_t port;
		receiver->getSource(addr, port);

        //Create a temporay map and buf to peek data before putting it in to memMap
        MemoryMap tMap(memMap.getSegment(expectedSegment), memMap.getSegmentSize());
        ByteBuffer tBuf(tMap);

        //Check protocol
        if (tBuf.checkProtocol())
        {
            int nrOfFragments = tBuf.ReadInt();
            int currentFragment = tBuf.ReadInt();

            if (currentFragment != (nrOfFragments - 1) && byteSizePair.size != OPSConstants::PACKET_MAX_SIZE)
            {
				ReportError(participant, "Debug: Received broken package.", addr, port);
            }

            currentMessageSize += byteSizePair.size; // - tBuf.GetSize();

            if (currentFragment != expectedSegment)
            {//For testing only...
                if (firstReceived)
                {
					ReportError(participant, "Segment Error, sample will be lost.", addr, port);
                    firstReceived = false;
                }
                expectedSegment = 0;
                currentMessageSize = 0;
                receiver->asynchWait(memMap.getSegment(expectedSegment), memMap.getSegmentSize());
                return;
            }

            if (currentFragment == (nrOfFragments - 1))
            {
                firstReceived = true;
                expectedSegment = 0;
                ByteBuffer buf(memMap);

                buf.checkProtocol();
                int i1 = buf.ReadInt();
                int i2 = buf.ReadInt();
                UNUSED(i1)
                UNUSED(i2)
                int segmentPaddingSize = buf.GetSize();

                //Read of the actual OPSMessage
                OPSArchiverIn archiver(buf, participant.getObjectFactory());

                OPSMessage* message = nullptr;

				try {
					message = dynamic_cast<OPSMessage*> (archiver.inout("message", message));
				} catch (ops::ArchiverException& e) {
					ErrorMessage_T msg("Invalid data on network. Exception: ");
					msg += e.what();
					ReportError(participant, msg, addr, port);
				} catch (std::exception& e) {
					ErrorMessage_T msg("Invalid data on network. Exception: ");
					msg += e.what();
					ReportError(participant, msg, addr, port);
				}
				if (message != nullptr)
                {
					//Check that we succeded in creating the actual data message
					if (message->getData() != nullptr) 
					{
						//Put spare bytes in data of message
						if (!calculateAndSetSpareBytes(buf, message->getData(), segmentPaddingSize)) {
							//Message has consumed more bytes than received
							ReportError(participant, "Invalid data on network. Wrong message length.", addr, port);
						}

						// Add IP and port for source as meta data into OPSMessage
						message->setSource(addr, port);

						//Send it to ReceiveDataHandler
						if (_client != nullptr) {
							_client->onMessage(*this, message);
						}

						currentMessageSize = 0;
					}
					else
					{
						ReportError(participant, "Failed to deserialize message. Check added Factories.", addr, port);
						delete message;
					}
                }
                else
                {
                    //Inform participant that invalid data is on the network.
					ReportError(participant, "Unexpected type received. Type creation failed.", addr, port);
                }
            }
            else
            {
                expectedSegment++;

				if (expectedSegment >= memMap.getNrOfSegments()) {
					ReportError(participant, "Buffer too small for received message.", addr, port);
					expectedSegment = 0;
					currentMessageSize = 0;
				}
            }
            receiver->asynchWait(memMap.getSegment(expectedSegment), memMap.getSegmentSize());
        }
        else
        {
            //Inform participant that invalid data is on the network.
			ReportError(participant, "Protocol ERROR.", addr, port);
            receiver->asynchWait(memMap.getSegment(expectedSegment), memMap.getSegmentSize());
        }
    }

	// Called when there are no more listeners and we are about to be put on the garbage-list for later removal
    void ReceiveDataChannel::clear()
    {
        receiver->removeListener(this);
    }

    bool ReceiveDataChannel::calculateAndSetSpareBytes(ByteBuffer &buf, OPSObject* obj, int segmentPaddingSize)
    {
        //We must calculate how many unserialized segment headers we have and substract that total header size from the size of spareBytes.
        int nrOfSerializedBytes = buf.GetSize();
        int totalNrOfSegments = (int) (currentMessageSize / memMap.getSegmentSize());
        int nrOfSerializedSegements = (int) (nrOfSerializedBytes / memMap.getSegmentSize());
        int nrOfUnserializedSegments = totalNrOfSegments - nrOfSerializedSegements;

        int nrOfSpareBytes = currentMessageSize - buf.GetSize() - (nrOfUnserializedSegments * segmentPaddingSize);

        if (nrOfSpareBytes > 0) {
            obj->spareBytes.reserve(nrOfSpareBytes);
            obj->spareBytes.resize(nrOfSpareBytes, 0);

            //This will read the rest of the bytes as raw bytes and put them into spareBytes field of data.
            buf.ReadChars(&(obj->spareBytes[0]), nrOfSpareBytes);
		}

		//Return false if message has consumed more bytes than received (but still within our buffer size)
		return (nrOfSpareBytes >= 0);
    }

}
