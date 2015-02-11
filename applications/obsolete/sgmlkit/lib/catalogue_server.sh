#! /bin/sh -

#+
#  Name:
#    search-catalogue.sh
#
#  Type of module:
#    Shell script.
#
#  Purpose:
#    Provide HTTP access to SGML catalogs.
#
#  Usage:
#    To be installed as a CGI script.  See below.
#
#  Description:
#    The script is invoked as a CGI script, through a URL like
#    http://server/catalogue/public/-//Starlink//DTD%20Starlink%200.6//EN
#    (that is, a URL with any spaces encoded as % escapes, as
#    described in the URI spec (RFC 2396, section 2.4.1).
#    The CGI specification is managed at <http://www.w3.org/CGI/>.
#
#    The script wraps the two applications ospcat and ospent, which come
#    with the OpenSP distribution.  It obtains its argument (the
#    identifier which is to be resolved) by examining the environment
#    variable PATH_INFO, which the web server has set.  It normalises
#    this by removing leading and trailing spaces, and collapsing
#    multiple whitespace.  If it finds a catalogue entry and
#    corresponding system entity, it returns the entity.  If it can't
#    find the entry, or it does find it but can't find the
#    corresponding entity, it returns an HTTP 404 code.
#
#    Different types of searches can be done on the catalogue.  The
#    script examines its own name, and searches for entries which
#    match it.  Therefore, if the script is installed as `public' (the
#    usual case), then it will search for public identifiers; if it is
#    installed as `system', it will search for system identifiers.
#    Such a scheme doesn't make sense for all of the entry types in
#    the catalogue, only for those (including public, system and
#    entity) which make a mapping from one identifier to another.
#
#  Installation:
#    It should be installed in the web server tree, in such as way
#    that it's recognised as a CGI script.  As described above, the
#    script's action depends on the name used to invoke it, so that it
#    should be installed as a link named `public' (in the usual case).
#
#    A suitable set of configuration options (for the Apache server)
#    is therefore
#
#      access.conf:
#          <Directory /local1/www/scripts/catalogue>
#          AllowOverride None
#          Options ExecCGI FollowSymLinks
#          </Directory>
#
#      srm.conf:
#          ScriptAlias /catalogue/ /local1/www/scripts/catalogue/
#
#      and with the link
#
#          /local1/www/scripts/catalogue/public ->
#             /home/norman/s/src/sgml/w/sgml/lib/catalogue_server.pl
#
#    You will need to edit the paths and the definition of
#    SGML_CATALOG_FILES at the top of the script.
#
#    Note that this script requires the `ospcat' application, which is
#    present in the OpenSP-1.4 distribution, but not in openjade-1.3.
#    The current jade distribution included in the Starlink SGML Kit
#    is based on the latter, so the ospcat application will not be
#    present in the Kit, and this script will not work.  That is,
#    this script is currently designed to work only on selected
#    servers, and not as an integral part of the Starlink SGML Kit.
#
#  Copyright:
#    Copyright 2000, Central Laboratory of the Research Councils
#
#  Authors:
#    NG: Norman Gray (Starlink, Glasgow)
#
#  History:
#    01-Feb-2000 (NG):
#      Initial version
#
#  RCS:
#    $Id$
#-

# Catalog path, and paths to binaries, are hard-coded, but may be overridden.
test -n "$OSPCAT" || OSPCAT=/home/norman/s/star/bin/sgml-goedel/OpenSP-1.4/bin/ospcat
test -n "$OSPENT" || OSPENT=/home/norman/s/star/bin/sgml-goedel/OpenSP-1.4/bin/ospent

SGML_CATALOG_FILES=/home/norman/s/src/sgml/w/sgml/dtd/CATALOG:/usr/local/lib/sgml/CATALOG
export SGML_CATALOG_FILES

server_name="Catalogue server 0.1"


if test -z "$SERVER_SOFTWARE" -o -z "REQUEST_METHOD" -o -z "$PATH_INFO"; then
    cat <<EOF
Status: 400 Malformed request
Content-type: text/html

<title>$server_name: Error</title>
<p>Missing environment variables.  The server may be misconfigured.
EOF
    exit 0
fi

ofile=

# Remove any temporary files on exit
trap 'test -n "$ofile" && rm -f $ofile' 0 1 2 9 11 15


# First check that this is the result of a GET request -- the only
# type supported.
if test $REQUEST_METHOD != "GET"; then
    cat <<EOF
Status: 400 Unsupported request method $REQUEST_METHOD
Server: $SERVER_SOFTWARE ($server_name)
Content-type: text/html

<title>$server_name: Error</title>
<p>Unsupported request method <code>$REQUEST_METHOD</code>
EOF
    exit 0;
fi

# The action we perform is determined by the name by which this script
# was invoked.  Test this, and return an error code if it's not
# something we recognise.

case $0 in
    *public) spcatopt="-P"
	;;
    *system) spcatopt="-S"
	;;
    *) spcatopt="XXX"
	;;
esac

if test $spcatopt = "XXX"; then
    cat <<EOF
Status: 400 Unrecognised request
Server: $SERVER_SOFTWARE ($server_name)
Content-type: text/html

<title>$server_name: Error</title>
<p>Error: unrecognised request $0
EOF
    exit 0
fi


# The public identifier we are to resolve is passed in the environment
# variable PATH_INFO, so check that this was actually provided.

if test -z "$PATH_INFO"; then
    cat <<EOF
Status: 400 No Path
Server: $SERVER_SOFTWARE ($server_name)
Content-type: text/html

<title>$server_name: Error</title>
<p>Error: no entity reference supplied
EOF
    exit 0
fi


# We're about to pass user-supplied data as an argument to echo.  I
# don't think there's too much to go wrong here, but just to be on the
# safe side, check there aren't any odd characters in PATH_INFO.  The
# following check is possibly over-cautious, but at least it catches
# the obvious nasties of `;' and the quote characters.  There are more
# characters here than may be in a public identifier, but that's
# because we have to include characters in system identifiers which
# may be either filenames or URLs.
#
# The allowed characters in `minimum literal data' are SPACE, LC
# Letter, UC Letter, Digit and Special (8879, production 78), where
# `Special' is the characters "'()+,-./:=?" (ie, 39-41, 43-47, 58, 61, 63)

if expr "$PATH_INFO" : "[ a-zA-Z0-9'()+,./:=?-]*$" >/dev/null; then
    # That's OK
    :
else
    cat <<EOF
Status: 400 Mysterious characters
Server: $SERVER_SOFTWARE ($server_name)
Content-type: text/html

<title>$server_name: Error</title>
<p>Error: There are some odd characters in the request:
<blockquote>
$PATH_INFO
</blockquote>
Are you sure that's a real identifier?
EOF
    exit 0
fi


# PATH_INFO contains the part of the URL path after the command name.
# This therefore has a leading slash, which must be removed.  Do this
# at the same time as we normalise the public identifier in the
# correct way for a `minimum literal', namely stripping leading and
# trailing blanks, and compressing multiple spaces to single ones.
pubid=`echo "$PATH_INFO" | sed -e 's+^/ *++' -e 's/ *$//' -e 's/  */ /g'`


# Call ospcat, saving the returned system identifier
sysid=`$OSPCAT "$spcatopt$pubid"`


# If this failed, then report that we can't resolve the public identifier
if test $? != 0; then
    cat <<EOF
Status: 404 Unrecognised Public Identifier
Server: $SERVER_SOFTWARE ($server_name)
Content-type: text/html

<title>$server_name: Error</title>
<p>Error: can't find system identifier corresponding to
public identifier <code>$pubid</code>
EOF
    exit 0
fi


# We managed to get a system identifier.  Now turn this into a file
# using ospent.  Send the result of this to a temporary file.  We
# can't simply cat it directly to the output, since that would mean we
# couldn't return the correct HTTP error code if this step failed
# (which would happen if the system identifier returned from the
# catalogue doesn't actually correspond to a file on disk).

ofile=/tmp/search-catalogue.$$.output
$OSPENT "$sysid" >$ofile 2>/dev/null

if test $? != 0; then
    cat <<EOF
Status: 404 No system identifier
Server: $SERVER_SOFTWARE ($server_name)
Content-type: text/html

<title>$server_name: Error</title>
<p>Error: The public identifier <code>$pubid</code> resolves to a
system identifier which could not be found.
EOF
    exit 0
fi


# Everything's OK!  Find the size of the file and cat it to stdout.
# Is parsing the output of `ls' really the best way of getting a
# file's size?

fsize=`ls -l $ofile | awk '{print $5}'`

cat <<EOF
Status: 200 OK
Server: $SERVER_SOFTWARE ($server_name)
Content-length: $fsize
Content-type: text/plain

EOF


# We've sent out the right headers.  All that remains is to provide
# the file itself.
cat $ofile


exit 0
