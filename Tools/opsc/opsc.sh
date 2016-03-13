#!/bin/bash
#
# test script by helm

# exit on errors
set -e

# find out where this is script is so we can invoke jar in ../jars
#SCRIPT_PATH=`dirname "$0"`; SCRIPT_PATH=`eval "cd \"$SCRIPT_PATH\" && pwd"`
SCRIPT_PATH=`readlink -f "$0"`; SCRIPT_PATH=`dirname "$SCRIPT_PATH"`; SCRIPT_PATH=`eval "cd \"$SCRIPT_PATH\" && pwd"`

# test
echo "invoking script at: $SCRIPT_PATH"
CWD=$(pwd)
echo "current working directory $CWD"

java -jar ${SCRIPT_PATH}/opsc.jar $@
