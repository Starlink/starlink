# Some of the programs in here require specific compiler options.  
# The following definitions are for the Sun f77 compiler;
# replace them with suitable ones for the compiler you use.
#
# Sun's f77:
PROFILING=-pg
NOUNDERSCORES=-ext_names=plain
#
# gcc/g77:
# not yet!

EXECS=timewaster fpdemo fpp mixed crash

all: $(EXECS)

#
# Floating-point demo
#
# Intel and Alpha chips are little-endian.
# Sparc, Motorola 68k and PowerPC chips are big-endian.
# Compile with -DBIGENDIAN on the latter.
#
#BIGENDIAN=
BIGENDIAN=-DBIGENDIAN

# Simple link of the FP programs
fpdemo: fpdemo.c
	$(CC) $(BIGENDIAN) -o $@ fpdemo.c

fpp: fpp.o
	$(LINK.C) $(BIGENDIAN) -o $@ fpp.o $(LDLIBS)

#
# Compile all three profiled programs with the $(PROFILING) flag, 
# to enable us to use the profiler.
# 
p1.o: p1.f
	$(F77) $(PROFILING) -c p1.f

p2.o: p2.f
	$(F77) $(PROFILING) -c p2.f

p3.o: p3.f
	$(F77) $(PROFILING) -c p3.f

timewaster: p1.o p2.o p3.o
	$(F77) -o $@ $(PROFILING) p1.o p2.o p3.o

#
# Compile and link the mixed-language demo.
#
mixed: mixed-f.o mixed-c.o
	$(LINK.F) -o mixed mixed-c.o mixed-f.o

# Compile the Fortran code with no trailing underscores
mixed-f.o: mixed-f.f
	$(F77) -c mixed-f.f $(NOUNDERSCORES)

mixed-c.o: mixed-c.c
	$(CC) -c mixed-c.c


# Tar up the current directory
examples.tar: clean
	(THISDIR=`basename \`pwd\``; cd .. ; tar cf $@ $$THISDIR)
	mv ../$@ .

%.gz: %
	gzip -9 $<

tidy:
	rm -f *~ core gmon.out timewaster.gprof *.gz

clean: tidy
	rm -f *.o $(EXECS)
