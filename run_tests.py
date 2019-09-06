#!/usr/bin/env python3

# script for running tests bc i hate scripting with bash
import subprocess
import os
import argparse

curr_path = os.path.dirname(os.path.realpath(__file__))

# runs all the scripts in testing directory
def unit_tests():
    test_path = os.path.join(curr_path, 'testing')

    # hack to filter out utilities.rkt and random .swp files vim generates
    test_files = list(filter(lambda x: 'test' in x and x.endswith('.rkt'), os.listdir(test_path)))
    for test in test_files:
        print('\n{} running...'.format(test))
        curr_test_path = os.path.join(test_path, test)
        subprocess.run([curr_test_path])

# compiles and runs everything in ../programs and compares the return code
# to the output from the racket interpreter
def full_tests():
    prog_path = os.path.join(curr_path, 'programs')
    prog_files = list(filter(lambda x: x.endswith('.rc'), os.listdir(prog_path)))
    print(prog_files)

if __name__ == '__main__':
    
#unit_tests()
full_tests()
