# Description
This is a racket to x86 compiler written in racket. It follows the approach outlined by the book `Essentials of Compilation` by Jeremy Siek and Ryan Newton.

# Compiling a file 
```
usage: python3 build.py [-h] [-o OUTPUT] filename
```

### Example with checking output
```
> cat programs/test-should-exit-code-4.rc
(program () (let ([x 2]) (+ x 2)))
> python3 build.py programs/test
> ./a.out
> echo $?
4
```

###### `./build.py` works on linux
```
> ./build.py programs/test-should-exit-code-4.rc -o two_plus_two_executable_on_linux
> cat programs/test-should-exit-code-4.s
      .global main
main:
      movq %rsp, %rbp
      movq $2, -8(%rbp)
      movq -8(%rbp), %rax
      movq %rax, -16(%rbp)
      addq $2, -16(%rbp)
      movq -16(%rbp), %rax
      retq

> ./two_plus_two_executable_on_linux
> echo $?
4
```

###### `./macos-build.py` works on mac
```
> ./macos-build.py programs/test-should-exit-code-4.rc -o two_plus_two_executable_on_mac
> cat programs/test-should-exit-code-4.s
      .global _main
_main:
      movq %rsp, %rbp
      movq $2, -8(%rbp)
      movq -8(%rbp), %rax
      movq %rax, -16(%rbp)
      addq $2, -16(%rbp)
      movq -16(%rbp), %rax
      retq

> ./two_plus_two_executable_on_mac
> echo $?
4
```

# Running tests
All tests are in the `testing` directory.

```
./run_tests.sh
```
