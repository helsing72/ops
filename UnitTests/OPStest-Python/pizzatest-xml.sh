#!/bin/bash


export PYTHONPATH=$PYTHONPATH:$PWD/Pizzas
export PYTHONPATH=$PYTHONPATH:"../../Python"
echo $PYTHONPATH

echo $PWD
python -m pytest -rw UnitTests/test_serializeAndDeserialize.py --junitxml="../../unittest-python-ser-deser-result.xml"

python -m pytest -rw UnitTests/test_subscribe.py --junitxml="../../unittest-python-pub-sub-result.xml" &

sleep 1
python  UnitTests/PublishData.py
