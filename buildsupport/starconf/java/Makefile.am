## Process this file with automake to produce Makefile.in

java_DATA = starconf.jar
# The following must match what's returned by
# `starconf --show buildsupportjar`
javadir = $(pkgdatadir)

# Default target.  We must be able to cope with Java not being
# present.  Test the value of the JAVAC variable, and if it is
# NOJAVAC, do nothing (successfully).  The rest of the buildsupport
# system is (well, should be) written to cope with this absence.  If
# JAVAC is available, we assume JAR is, too.
all:
	if test $(JAVAC) = NOJAVAC; then \
	  echo "Warning: No Java available -- not building starconf.jar"; \
	else \
	  $(MAKE) starconf.jar; \
	fi

starconf.jar: GenerateDependencies.class Transformer.class
	$(JAR) cf $@ *.class

SUFFIXES = .java .class
.java.class:
	$(JAVAC) -source 1.4 $<

check: GenerateDependencies.class testdeps.xml testdeps.correct
	rm -f testdeps.stdout testdeps.stderr
	$(JAVA) GenerateDependencies --test testdeps.xml \
	    --makefile=testdeps.stdout 2>testdeps.stderr
	diff testdeps.correct testdeps.stdout
	rm -f testdeps.stdout testdeps.stderr

