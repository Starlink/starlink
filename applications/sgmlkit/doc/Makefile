
# Recurse into subdirectories, except current one
clean:
	for i in `find . -type d | sed 1d`; do \
		if [ -f $$i/Makefile ]; then (cd $$i; make clean); \
			else rm $$i/*~; \
		fi \
	done
