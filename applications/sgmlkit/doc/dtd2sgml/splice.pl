#! /usr/bin/perl -w

#+
#  Name:
#     splice.pl
#
#  Type of module:
#     Perl script
#
#  Purpose:
#     Splice files into a parent file.
#
#  Description:
#
#     This is a very simple little script.  The `input file' is
#     a file conforming to some DTD.  At appropriate points in this
#     file, are comments of the form `<!--splice:element-name-->', on
#     a line by themselves.
#
#     This script takes three arguments: the name of the input and
#     resulting files, and the name of a directory, in
#     which are fragments to be inserted in the destination file.
#     When it finds a comment of the above form, it will search in the
#     directory for files named `element-name', as found in the
#     comment; if it finds one, it inserts it in the output file
#     before the comment and prints the file's name on stdout.
#
#     It is not an error if no fragment file exists.  It does no
#     parsing of the files other than to search for the comment -- it
#     is the user's responsibility to ensure that the file will remain
#     valid after the insertion of the commentary files.
#
#  Arguments:
#
#     Takes exactly three arguments:
#       1: input file, with magic comments;
#       2: output file, with any insertions, still with the magic comments;
#       3: name of directory, containing fragments to be inserted in
#          the parent file.
#
#  Authors:
#     NG: Norman Gray (Starlink, Glasgow)
#
#  History:
#     28-Mar-2000 (NG):
#       Initial version
#
#  RCS Id:
#     $Id$
#-

$Usage = "$0 input-file new-file fragment-directory";

$#ARGV == 2 || die $Usage;

$infile = $ARGV[0];
$newfile = $ARGV[1];
$fragments = $ARGV[2];

open (DESC, "$infile") || die "Can't open $infile to read";
open (NEWDESC, ">$newfile") || die "Can't open $newfile to write";

while ($line = <DESC>) {
    if ($line =~ /<!--splice:([a-zA-Z-]+)-->/) {
	$infile = $1;
	if (open (INSERTION, "$fragments/$infile")) {
	    while (<INSERTION>) {
		print NEWDESC;
	    }
	    close (INSERTION);
	    print "$fragments/$infile\n"; # print on stdout
	}
	# not being able to open the file is not an error
    }
    print NEWDESC $line;
}

close (DESC);
close (NEWDESC);

exit 0;
