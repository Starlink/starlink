# Script to edit URL sysids.
#
# Whenever the script finds a public identifier with a string
# (delimited by " or ') on the next line, it replaces that next line
# with a URL which points to a catalogue server, and which contains an
# HTTP-escaped and normalised version of the public identifier.
#
# The public identifier must be on a single line, and the string must be
# on the immediately following line.  That line is replaced completely,
# so it mustn't have anything else on the line.
#
# Usage: sed -f add-sysids.sed starlink-0.6.dtd
#
# $Id$
#
/PUBLIC/{
	h
	n
	s/["']//
	t subs
	b
	: subs
	g
	s/^[^"']*["'][ 	]*//
	s/[ 	]*["'].*$//
	s/  */%20/g
	s+^+	\"http://goedel.astro.gla.ac.uk/catalogue/public/+
	s/$/\"/
}
