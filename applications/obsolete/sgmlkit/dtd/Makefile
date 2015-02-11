# The list of all the installed files is maintained in ../Makefile.am.
# May need to worry about starlink.decl{,.linux} -- platform dependency?

# CATALOG-override is identical to CATALOG, except that it has 
# `OVERRIDE YES' at the beginning of it.  See CATALOG-override.header
# for discussion.
CATALOG-override: CATALOG CATALOG-override.header
	rm -f CATALOG-override
	cat CATALOG-override.header CATALOG >CATALOG-override

maintainer-clean distclean: clean
clean:
	rm -f CATALOG-override
