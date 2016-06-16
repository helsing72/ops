#!/bin/bash

JENK_DIR="/tmp/jenkins/workspace/ops_pizzatest"

cd OPStest-Python/

export PYTHONPATH=$PYTHONPATH:$PWD/Pizzas
export PYTHONPATH=$PYTHONPATH:"../../Python"
echo $PYTHONPATH

python -m pytest -rw UnitTests/test_serializeAndDeserialize.py --junitxml="$JENK_DIR/unittest-python-ser-deser-result.xml"

python -m pytest -rw UnitTests/test_subscribe.py --junitxml="$JENK_DIR/unittest-python-pub-sub-result.xml" &


sleep 1
python  UnitTests/PublishData.py

