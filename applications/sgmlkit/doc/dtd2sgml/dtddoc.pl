#! /usr/bin/perl -w

#+
#  Name:
#     dtddoc.pl
#
#  Type of module:
#     Perl script
#
#  Purpose:
#     Transform email messages to XML.
#
#  Description:
#     This takes email messages on stdin, and writes them out as a
#     fragment of XML, corresponding to the `commentary' part of the
#     dtddescription DTD.  It expects to receive mail addressed to
#     something like `prefix+docxref@...', possibly more than one,
#     possibly mixed in with an arbitrary number of other addresses.
#     It takes such mail, and appends it to a file `prefix/docxref' in
#     the target directory (which is hard-coded).
#
#  Arguments:
#     addressprefix (required): Prefix to be stripped from target addresses.
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

($dir = $0) =~ s+/[^/]*$++;


$#ARGV == 0 || die "Usage: $0 addressprefix\n";

$addressroot = shift (@ARGV);

@otherheader = ();
@othervalue = ();
@toaddresses = ();

# Parse the header.  To, Cc and Bcc addresses are split up and put
# into a single array @toaddresses; Date and Subject lines are put into
# corresponding variables; everything else is put into an
# @otherheader/@othervalue array pair.
#
# Jump out on a blank line.
while (<>) {
    if (/^ *$/) {
	last;
    } elsif (/^(to|cc|bcc): *(.*)$/i) {
	push (@toaddresses, split(/ *, */, $2));
    } elsif (/^From: *(.*)$/) {
	$fromline = $1;
	if ($fromline =~ /^(.*)<([^>]+)>(.*)$/) {
	    $email = $2;
	    ($from = "$1$3") =~ s/^ +//;
	    $from =~ s/ +$//;
	} elsif ($fromline =~ /(.*)\"(.+)\"(.*)$/) {
	    $from = $2;
	    ($email = "$1$3") =~ s/ //g;
	} else {
	    $email = $fromline;
	}
    } elsif (/^Date: *(.*)$/) {
	$date = $1;
    } elsif (/^Subject: *(.*)$/) {
	$subject = $1;
    } else {
	if (/^([A-Za-z-]+) +(.*)/) {
	    push (@otherheader, "$1 ");
	    push (@othervalue, $2);
	} elsif (/^([A-Za-z-]+): *(.*)/) {
	    push (@otherheader, $1);
	    push (@othervalue, $2);
	} else {
	    chop;
	    s/^[ \t]+/ /;
	    if ($#othervalue >= 0) {
		if (defined ($othervalue[$#othervalue])) {
		    $othervalue[$#othervalue] .= $_;
		} else {
		    $othervalue[$#othervalue] = $_;
		}
	    }
	    # a bit odd... But just discard it (probably not important anyway!)
	}
    }
}

# gobble the rest of the message body, and escape problematic characters
@body = <>;
foreach $line (@body) {
    $line =~ s/&/&amp;/g;
    $line =~ s/</&lt;/g;
}

# Now go through the list of destination addresses, extracting
# addresses in any of the forms `address', `"name" address', and `name
# <address>' (these aren't quite the same as RFC822 comments, but
# they're pretty common).  

@destfiles = ();
while ($#toaddresses >= 0) {
    $addr = shift (@toaddresses);
    if ($addr =~ /^(.*)<([^>]+)>(.*)$/) {
	$targetaddr = $2;
    } elsif ($addr =~ /(.*)\"(.+)\"(.*)$/) {
	($targetaddr = "$1$3") =~ s/ //g;
    } else {
	$targetaddr = $addr;
    }
    if ($targetaddr =~ /^$addressroot\+([a-zA-Z0-9-]+)@/) {
	push (@destfiles, "$1");
    } elsif ($targetaddr =~ /^$addressroot/) {
	push (@destfiles, "general-comments");
    }
    #print "addr=$addr --> $targetaddr\n";
}
if ($#destfiles < 0) {
    # Can't think of where else to put it: just bung it in general-comments
    push (@destfiles, "general-comments");
}
#print "destfiles=@destfiles\n";

# Make the destination directory, if it doesn't already exist
# Remember that this script will generally be run as the mail user,
# which will probably _not_ have permission to create files in
# the target directory.  This might very well fail, therefore, unless
# the directory is created appropriately beforehand.
(-d "$dir/$addressroot") ||
    mkdir ("$dir/$addressroot", 0755) ||
    die "Can't create directory $addressroot\n";

# Dump the headers and body to each of the files in the array @destfiles
while ($#destfiles >= 0) {
    $destfile = shift (@destfiles);
    $destfilename = "$dir/$addressroot/$destfile";
    open (OP, ">>$destfilename") || die "Can't open $destfilename";

    print OP "<commentary>\n";
    print OP "<from>$from</from>\n" if (defined($from));
    print OP "<email>$email</email>\n" if (defined($email));
    print OP "<date>$date</date>\n" if (defined($date));
    print OP "<subject>$subject</subject>\n" if (defined ($subject));
    while ($#otherheader >= 0) {
	$t = shift (@othervalue);
	$t =~ s/&/&amp;/g;
	$t =~ s/</&lt;/g;
	print OP "<otherheader name='",
	shift (@otherheader), "'>$t</otherheader>\n";
    }
    print OP "<body>\n";
    print OP @body;
    print OP "</body>\n</commentary>\n";

    close OP;
    #chmod (0666, "$destfilename");
}

	
exit 0;
