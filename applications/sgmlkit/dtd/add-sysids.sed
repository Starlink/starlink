# Script to replace DTD lines including string %CATSYSID with a URL
# based on last PUBLIC identifier seen.
#
# Usage: sed -f add-sysids.sed starlink-0.6.dtd
#
# $Id$
/PUBLIC/h
/^[ 	]*%CATSYSID/{
	g
	s/^[^"]*//
	s/[ 	]*$//
	s/  */%20/g
	s+\"+	%CATSYSID; \"http://goedel.astro.gla.ac.uk/catalogue/public/+
	s/\" *$/\" %CATSYSID;/
}
