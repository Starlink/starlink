# Make a list of all the files which are to be installed.
# May need to worry about starlink.decl{,.linux} -- platform dependency?
# Create a manifest by listing all the files, below here, removing
# certain directories, CVS directories, backup files, and then
# removing all files which are present in the file manifest-omit.
manifest: CATALOG-override
	rm -f manifest
	find . -type f | \
	    sed -e 's,^\./,,' | \
	    sed -e /^html/d -e /^html4/d -e /CVS/d -e '/~$$/d' | \
	    cat - manifest-omit | sort | uniq -u | \
	    sed 's,^,dtd/,' >manifest

# CATALOG-override is identical to CATALOG, except that it has 
# `OVERRIDE YES' at the beginning of it.  This means that public IDs
# within the catalog override system IDs, which makes it more usable for
# XML documents, which require a system ID for (for example) DTDs, even when
# a public ID would be more useful and portable.
# See <http://www.oasis-open.org/specs/a401.htm> for full discussion.
CATALOG-override: CATALOG
	rm -f CATALOG-override
	echo 'OVERRIDE YES' >CATALOG-override
	cat CATALOG >>CATALOG-override

clean:
	rm -f CATALOG-override manifest
