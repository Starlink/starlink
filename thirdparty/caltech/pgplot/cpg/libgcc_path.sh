#!/bin/sh

# Create a dummy C file.

echo 'int main() {return 0;}' > dummy.c

# Compile and link the dummy file with verbose mode turned on and
# capture -L* library paths.

gcc -v -o dummy dummy.c 2>&1 | tr ' ' '\012' | egrep '^-L'

\rm -f dummy dummy.o dummy.c
