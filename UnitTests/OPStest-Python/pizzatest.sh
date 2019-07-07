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

$PYTHON -m pytest -rw UnitTests/test_serializeAndDeserialize.py
exit_des_ser=$?

$PYTHON -m pytest -rw UnitTests/test_subscribe.py &

pid_pub_sub=$!

sleep 1
$PYTHON UnitTests/PublishData.py

wait $pid_pub_sub
exit_pub_sub=$?

exit_count=$(( $exit_des_ser + $exit_pub_sub ))

if [ $exit_count == 0 ]
then
	printf "\n\n   ALL TEST PASSED  \n\n\n"
else
	printf "\n\n   $exit_count TEST FAILED  \n\n\n"
fi
