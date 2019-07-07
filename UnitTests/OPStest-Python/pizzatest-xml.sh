#!/bin/bash
if [ -z "$1" ]
then
  PYTHON=python
else
  PYTHON=python3
fi

export PYTHONPATH=$PYTHONPATH:$PWD/Pizzas
export PYTHONPATH=$PYTHONPATH:"../../Python"
echo $PYTHONPATH

echo $PWD
$PYTHON -m pytest -rw UnitTests/test_serializeAndDeserialize.py --junitxml="../../unittest-python-ser-deser-result.xml"

$PYTHON -m pytest -rw UnitTests/test_subscribe.py --junitxml="../../unittest-python-pub-sub-result.xml" &

sleep 1
$PYTHON UnitTests/PublishData.py
