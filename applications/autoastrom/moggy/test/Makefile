# Test directory makefile
#
# This doesn't express _all_ the dependencies within this directory:
# test scripts which have dependencies, such as t3 on t3.run, run make
# internally.

test:
	./runtests

t3.run: t3.C ../util.o
	g++ -o $@ t3.C ../util.o

clean:
	rm -f *~ *.run *.stderr *.stdout
