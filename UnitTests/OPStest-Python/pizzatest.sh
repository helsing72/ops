#!/bin/bash

#cd UnitTests

export PYTHONPATH=$PYTHONPATH:$PWD/Pizzas
export PYTHONPATH=$PYTHONPATH:"/home/pane/slask/jali-ops/ops-development/Python"
echo $PYTHONPATH

#python -m pytest -rw test_extraAllt
python -m pytest -rw UnitTests/test_serializeAndDeserialize.py
exit_des_ser=$?

#python -m pytest -rw test_normal
python -m pytest -rw UnitTests/test_subscribe.py

pid_pub_sub=$!

sleep 1
python  UnitTests/PublishData.py

wait $pid_pub_sub
exit_pub_sub=$?


exit_count=$(( $exit_des_ser + $exit_pub_sub ))

if [ $exit_count == 0 ]
then
	printf "\n\n   ALL TEST PASSED  \n\n\n"
else
	printf "\n\n   $exit_count TEST FAILED  \n\n\n"
fi

