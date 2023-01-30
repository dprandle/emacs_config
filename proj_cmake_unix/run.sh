#!/bin/bash
config=$1
exec_name=$2
args=""
for (( i=2; i <= "$#"; i++ )); do
    args="${args} ${!i}"
done
echo "Running: build/${config}/bin/${exec_name} ${args}"
build/${config}/bin/${exec_name} ${args}
