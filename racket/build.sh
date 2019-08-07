#!/bin/bash

rm test.s a.out
./compiler.rkt 1> test.s 2> /dev/null
gcc test.s 
./a.out 
echo $?
