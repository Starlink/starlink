# Make a list of all the files which are to be installed.
# May need to worry about starlink.decl{,.linux} -- platform dependency?
# Create a manifest by listing all the files, below here, removing
# certain directories, CVS directories, backup files, and then
# removing all files which are present in the file manifest-omit.
manifest: CATALOG-override manifest-omit
	rm -f manifest
	find . -type f | \
	    sed -e 's,^\./,,' | \
	    sed -e /^html/d -e /^html4/d -e /CVS/d -e '/~$$/d' | \
	    cat - manifest-omit | sort | uniq -u | \
	    sed 's,^,dtd/,' >manifest

# CATALOG-override is identical to CATALOG, except that it has 
# `OVERRIDE YES' at the beginning of it.  See CATALOG-override.header
# for discussion.
CATALOG-override: CATALOG CATALOG-override.header
	rm -f CATALOG-override
	cat CATALOG-override.header CATALOG >CATALOG-override

clean:
	rm -f CATALOG-override manifest
