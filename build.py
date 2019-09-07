#!/usr/bin/env python3
import argparse
import subprocess

def compile_rc(infile, outfile, output_asm=True):
    if not infile.endswith('.rc'):
        print('ERROR: bad file input to compiler.py. Input filename must end with ".rc" suffix')
        return -1

    asm_file = infile.replace('.rc', '.s')

    compile_args = ['./compiler.rkt', infile]
    with open(asm_file, 'w+') as output:
        # call './compiler.rkt filename' and redirect output to asm_file, and check the output
        result = subprocess.run(compile_args, stdout=output)
        if result.returncode != 0:
            print('Compilation of file {} failed'.format(infile))
            return -1

    assemble_args = ['gcc', asm_file, '-o', outfile]
    subprocess.run(assemble_args)
    return 0

def main():
    parser = argparse.ArgumentParser(description='Wrapper on top of compiler.rkt')
    parser.add_argument('filename', help='File that will be compiled by compiler.rkt. Must end with ".rc" suffix', type=str)
    parser.add_argument('-o', '--output', help='Name of output executable')

    args = parser.parse_args()

    if not args.output:
        args.output = 'a.out'

    compile_rc(args.filename, args.output)

if __name__ == '__main__':
    main()
