## Process this file with automake to produce Makefile.in

java_DATA = starconf.jar
# The following must match what's returned by
# `starconf --show buildsupportjar`
javadir = $(pkgdatadir)

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

