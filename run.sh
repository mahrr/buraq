#!/usr/bin/bash

if [ $# -ne 1 ]; then
    echo "usage: ./run.sh <file-path>"
    exit 1
fi

# extract the path base and the file name
path_base=$(basename -- $1)
file_name="${path_base%.*}"

# invoke the compilation script
./compile.sh $1

# run the resulted executable, if compilation not failed
if [ $? == 0 ]; then
    ./build/$file_name
fi