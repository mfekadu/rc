# Description
This is a racket to x86 compiler written in racket. It follows the approach outlined by the book `Essentials of Compilation` by Jeremy Siek and Ryan Newton.

# Compiling a file 
### On Linux
```
usage: build.py [-h] [-o OUTPUT] filename
```
### On MacOS
###### optionally modify the first line of `build.py` to point to the location of your python3 discovered via `which python3` in terminal
```
usage: python3 build.py [-h] [-o OUTPUT] filename
```

# Running tests
All tests are in the `testing` directory.

```
./run_tests.sh
```
