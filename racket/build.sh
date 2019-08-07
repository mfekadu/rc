#!/bin/bash

rm test.s a.out
./compiler.rkt > test.s 
gcc test.s 
./a.out 
echo $?
