#!/bin/bash
python3 build.py $1
./a.out
echo $?
