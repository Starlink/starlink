# $Id: Makefile,v 1.6 2004/03/18 08:16:03 olpa Exp $

all: tests

clean:
	rm -rf *~ */*~ tmp

good_xml_tests:=$(notdir $(wildcard data/*xml))
good_test_stamps=$(addprefix tmp/data/,$(good_xml_tests:.xml=.stamp))
fail_xml_tests:=$(notdir $(wildcard faildata/*xml))
fail_test_stamps=$(addprefix tmp/faildata/,$(fail_xml_tests:.xml=.stamp))

#
# Individual test parameters to converter
#
params-mixed   = -e ascii
params-koi8    = -e koi8-r
params-koi8bad = -e koi8-r
params-latex   = -e koi8-r
params-cmdnest = -e ascii

tests: $(good_test_stamps) $(fail_test_stamps)

#
# Initialization, creation of folders
#
init=tmp/data/.stamp tmp/faildata/.stamp

tmp/data/.stamp tmp/faildata/.stamp:
	mkdir -p $(dir $@)
	touch $@

# Need a line after "FORCE"
FORCE:

# Need a line after "FORCE"

.SECONDARY:

#
# Running "good" tests
#
tmp/data/%.out: data/%.xml ../bin/texml.py ../bin/*py $(init)
	python $(word 2,$+) $(call params-$*) $< $@

tmp/data/%.stamp: tmp/data/%.out FORCE
	diff -qs $(basename $@).out data/$(notdir $*).out

#
# Running "bad" tests: if fact, we make grep of error message
#
tmp/faildata/%.err: faildata/%.xml ../bin/texml.py ../bin/*py $(init)
	python $(word 2,$+) $< $(basename $@).out 2>$@ || true

tmp/faildata/%.stamp: tmp/faildata/%.err FORCE
	grep -q -f faildata/$(notdir $*).grep $<
