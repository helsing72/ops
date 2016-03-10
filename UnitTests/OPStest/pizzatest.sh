#!/bin/bash

cd $1

build/UnitTests/test-serialize-and-deserialize

build/UnitTests/test-subscribe &
build/UnitTests/test-subscribeTCP &
build/UnitTests/test-subscribeUDP &
sleep 1
build/src/publishBin


