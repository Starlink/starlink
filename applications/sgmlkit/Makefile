DISTDIRS=doc dsssl dtd lib

sgml.tar: #clean
	(cd ..; tar cf sgml.tar `for i in $(DISTDIRS); do echo sgml/$$i; done` )
	mv ../sgml.tar .

clean:
	for i in $(DISTDIRS); do \
		(cd $$i; if [ -f Makefile ]; then make clean; fi); \
	done
	rm -f *~
