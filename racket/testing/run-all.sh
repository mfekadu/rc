#!/bin/bash

echo -e "\nRunning assign-homes tests..."
./assign-homes-tests.rkt

echo -e "\nRunning ec-tests..."
./ec-tests.rkt

echo -e "\nRunning patch instructions tests..."
./patch-instructions-tests.rkt

echo -e "\nRunning print-x86 tests..."
./print-x86-tests.rkt

echo -e "\nRunning rco tests..."
./rco-tests.rkt

echo -e "\nRunning select instructions tests..."
./select-instructions-tests.rkt

echo -e "\nRunning uncover locals tests..."
./uncover-locals-tests.rkt

echo -e "\nRunning uniquify tests..."
./uniquify-tests.rkt
