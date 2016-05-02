#!/bin/bash

cd UnitTests
python TestSerializeAndDeserialize.py
python SubscribeDataAndTest.py &
sleep 1
python PublishData.py

