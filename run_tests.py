#!/usr/bin/env python3

# script for running tests bc i hate scripting with bash
import subprocess
import os
import argparse
import re
from build import compile_rc

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

def full_test_success():
    pass

def full_test_fail():
    prog_path = os.path.join(curr_path, 'programs', 'fail')
    prog_files = list(filter(lambda x: x.endswith('.rc'), os.listdir(prog_path)))
    outfile = 'tmp'
    rv = 0
    for prog in prog_files:
        print('\nCompiling {}...'.format(prog))
        curr_prog = os.path.join(prog_path, prog)
        if compile_rc(curr_prog, outfile) == 0:
            print("ERROR: program {} should have failed to compile but succeeded".format(prog))
            rv = -1
        else:
            print("Program {} correctly failed to compile".format(prog))

    return rv

def full_test_pass():
    
    # another great hack to remove the dumb (program  () shit
    def strip_eval_string(string):
        return "'" + string[string.find('()') + 2:len(string)-2] + "'"
    
    prog_path = os.path.join(curr_path, 'programs', 'pass')
    prog_files = list(filter(lambda x: x.endswith('.rc'), os.listdir(prog_path)))
    outfile = 'tmp'
    rv = 0
    for prog in prog_files:
        print('\nCompiling {}...'.format(prog))
        curr_prog = os.path.join(prog_path, prog)
        if compile_rc(curr_prog, outfile) != 0:
            rv = -1
            continue

        # now compare the output to what the racket interpreter says
        rc_result = subprocess.run(['./' + outfile])
        
        with open(curr_prog, 'r') as prog_file:
            eval_string = strip_eval_string(re.sub(' +', ' ', prog_file.read().replace('\n', ' ')))
            racket_result = subprocess.run('racket -e {}'.format(eval_string), shell=True)
            print(racket_result.returncode)

    if os.path.exists(outfile):
        os.remove(outfile)
    return rv


# compiles and runs everything in ../programs and compares the return code
# to the output from the racket interpreter
def full_tests():
    full_test_fail()
    full_test_pass()
    return 0
       
def main():
    parser = argparse.ArgumentParser(description='Script to run unit/integration tests')
    parser.add_argument('-u', '--unit', action='store_true')
    parser.add_argument('-f', '--full', action='store_true')

    args = parser.parse_args()
    # if the user says test --unit, it means that only the unit test should run 
    if args.unit:
        unit_tests()
        return 0

    if args.full:
        full_tests()
        return 0

    # default - run both
    print("\n*************** RUNNING UNIT TESTS ***************\n")
    unit_tests()

    print("\n*************** RUNNING FULL TESTS ***************\n")
    full_tests()

if __name__ == '__main__':
    main() 
