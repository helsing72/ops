#!/bin/bash

cd UnitTests
python SubscribeDataAndTest.py &
sleep 1
python PublishData.py

