#!/usr/bin/bash

if [ $# -ne 1 ]; then
    echo "usage: ./compile.sh <file-path>"
    exit 1
fi

# extract the path base and the file name
path_base=$(basename -- $1)
file_name="${path_base%.*}"

# make a build dir
rm -rf build/
mkdir build/

# run the buraq compile to output assembly file
cargo run $1 > "build/$file_name.s"

# check if if compilation failed
if [ $? -ne 0 ]; then
    echo "[compile.sh]: compilation failed!"
    exit 1
fi

# convert the assembly file to an object file
nasm -f elf64 build/$file_name.s -o build/$file_name.o

# check if the assembler failed
if [ $? -ne 0 ]; then
    echo "[compile.sh]: assembler failed!"
    exit 1
fi

# link the object file with buraq runtime
gcc -no-pie -g -mstackrealign -o build/$file_name src/runtime.c build/$file_name.o

# check if linking failed
if [ $? -ne 0 ]; then
    echo "[compile.sh]: linking failed!"
    exit 1
fi