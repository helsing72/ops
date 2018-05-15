import time
import re
import sys
import os
import platform

from ops import Participant

import OpsDebugCommands

# default key (CSCI) to send messages to
Key = "Pizza"

def menu():
	print ""
	print "\t key name                Set key to given name [" + Key + "]"
	print "\t li                      List publishers and subscribers"
	print "\t en [pub | sub] topic    Enable Publisher/Subscriber for topic"
	print "\t di [pub | sub] topic    Disable Publisher/Subscriber for topic"
	print "\t st [pub | sub] topic    Get status from Publisher/Subscriber for topic"
	print "\t inc num topic           Increment PubId for Publisher for topic"
	print "\t x                       Exit program"
	print ""

def usage():
	print ""
	print "  Usage: run <ops_config-file> <domain-name>"


print ""
print '  Number of arguments:', len(sys.argv)
print '  Argument List:', str(sys.argv)

if len(sys.argv) < 3:
	usage()
	sys.exit(-1)

ops_config_filename = sys.argv[1]
ops_domainname = sys.argv[2]

participant = Participant.Participant.getInstance(ops_domainname, ops_domainname, ops_config_filename)
if participant == None:
	print ""
	print "Failed to create Participant. Missing ops_config.xml or domain ??"
	usage()
	sys.exit(-1)

dbgHandler = OpsDebugCommands.OpsDebugCommands(participant)

doExit = False
menu()

while not doExit:
	commands = re.split(" |\t", raw_input(" (? = menu) > "))

	while len(commands)>0:
		if (commands[0]=="?"):
			menu()
			del commands[0]

		elif (commands[0].lower()=="key"):
			Key = commands[1]
			del commands[0]
			del commands[0]

		elif (commands[0].lower()=="li"):
			dbgHandler.ListPublishers(Key)
			dbgHandler.ListSubscribers(Key)
			del commands[0]

		elif (commands[0].lower()=="en"):
			# en [pub | sub] topic
			if commands[1].lower()=="pub":
				dbgHandler.EnablePublisher(key = Key, topic = commands[2])
			if commands[1].lower()=="sub":
				dbgHandler.EnableSubscriber(key = Key, topic = commands[2])
			del commands[0]
			del commands[0]
			del commands[0]

		elif (commands[0].lower()=="di"):
			# di [pub | sub] topic
			if commands[1].lower()=="pub":
				dbgHandler.DisablePublisher(key = Key, topic = commands[2])
			if commands[1].lower()=="sub":
				dbgHandler.DisableSubscriber(key = Key, topic = commands[2])
			del commands[0]
			del commands[0]
			del commands[0]

		elif (commands[0].lower()=="st"):
			# st [pub | sub] topic
			if commands[1].lower()=="pub":
				dbgHandler.PublisherStatus(key = Key, topic = commands[2])
			if commands[1].lower()=="sub":
				dbgHandler.SubscriberStatus(key = Key, topic = commands[2])
			del commands[0]
			del commands[0]
			del commands[0]

		elif (commands[0].lower()=="inc"):
			# inc num topic
			if commands[1].isdigit():
				num = int(commands[1])
			else:
				num = 1
			dbgHandler.IncrementPubId(key = Key, topic = commands[2], amount = num)
			del commands[0]
			del commands[0]
			del commands[0]

		elif (commands[0].lower()=="x"):
			doExit = True
			del commands[0]

		else:
			print "unknown command: '" + commands[0] + "'"
			del commands[0]
