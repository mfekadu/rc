#!/usr/bin/env python3
import argparse
import subprocess

def do_stuff():
    parser = argparse.ArgumentParser(description='Wrapper on top of compiler.rkt')
    parser.add_argument('filename', help='File that will be compiled by compiler.rkt. Must end with ".rc" suffix', type=str)
    parser.add_argument('-o', '--output', help='Name of output assembly file')

    args = parser.parse_args()

    if not args.output:
        args.output = 'a.out'

    if not args.filename.endswith('.rc'):
        print('ERROR: bad file input to compiler.py. Input filename must end with ".rc" suffix')
        exit(1)

    asm_file = args.filename.replace('.rc', '.s')

    compile_args = ['./compiler.rkt', args.filename]
    with open(asm_file, 'w+') as output:
        # call './compiler.rkt filename' and redirect output to asm_file, and check the output
        subprocess.run(compile_args, stdout=output, check=True)

    assemble_args = ['gcc', asm_file, '-o', args.output]
    subprocess.run(assemble_args)


if __name__ == '__main__':
    do_stuff()
