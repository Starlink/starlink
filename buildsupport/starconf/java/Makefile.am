## Process this file with automake to produce Makefile.in

if BUILDJAVA
java_DATA = starconf.jar
else !BUILDJAVA
# Can't build Java on this platform
java_DATA = 
endif !BUILDJAVA

# The following must match what's returned by
# `starconf --show buildsupportjar`
javadir = $(pkgdatadir)

starconf.jar: GenerateDependencies.class Transformer.class
	$(JAR) cf $@ *.class

SUFFIXES = .java .class
if BUILDJAVA
.java.class:
	$(JAVAC) $<
else !BUILDJAVA
.java.class:
	echo "Can't build Java on this platform" >&2
	false
endif !BUILDJAVA

check: GenerateDependencies.class testdeps.xml testdeps.correct
	rm -f testdeps.stdout testdeps.stderr
	$(JAVA) GenerateDependencies --test testdeps.xml \
	    --makefile=testdeps.stdout 2>testdeps.stderr
	diff testdeps.correct testdeps.stdout
	rm -f testdeps.stdout testdeps.stderr
