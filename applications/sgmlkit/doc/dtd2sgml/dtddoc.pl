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
#     something like `addressprefix+suffix@...', possibly more than one,
#     possibly mixed in with an arbitrary number of other addresses.
#
#     The script appends the fragments to files `addressprefix/suffix' in
#     the target directory.
#
#  Arguments:
#     addressprefix (required): Prefix to be recognised in, and
#     stripped from, target addresses.
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

$addressprefix = shift (@ARGV);

$debug = 0;

@otherheader = ();
@othervalue = ();
@toaddresses = ();
@body = ();

$inheader = 1;

# Parse the mailfile.  To, Cc and Bcc addresses are split up and put
# into a single array @toaddresses; Date and Subject lines are put into
# corresponding variables; everything else is put into an
# @otherheader/@othervalue array pair.
#
while (<>) {
    if ($inheader) {
	if (/^ *$/) {
	    # blank line -- end of header, read the body
	    $inheader = 0;
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
    else {
	# A further message: process the current one and reinitialise
	if (/^From /) {
	    # beginning of the next message -- process the body and reset
	    process_body();
	    
	    @otherheader = ();
	    @othervalue = ();
	    @toaddresses = ();
	    @body = ();

	    $inheader = 1;
	} else {
	    push (@body, $_);
	}
    }
}

process_body ();

exit 0;


sub process_body {

    # process the message body, and escape problematic characters
    $bodytext = join ('',@body);
    $bodytext =~ s/&/&amp;/g;
    $bodytext =~ s/</&lt;/g;
    $bodytext =~ s/>/&gt;/g;

    # Pick out URLs in the body text
    $bodytext =~ s=(http://\S+)=<url>$1</url>=g;

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
	if ($targetaddr =~ /^$addressprefix\+([a-zA-Z0-9-]+)@/) {
	    push (@destfiles, "$1");
	} elsif ($targetaddr =~ /^$addressprefix/) {
	    push (@destfiles, "general-comments");
	}
	print "addr=$addr --> $targetaddr\n" if $debug;
    }
    if ($#destfiles < 0) {
	# Can't think of where else to put it: just bung it in general-comments
	push (@destfiles, "general-comments");
    }
    print "destfiles=@destfiles\n" if $debug;

    # Make the destination directory, if it doesn't already exist
    # Remember that this script will generally be run as the mail user,
    # which will probably _not_ have permission to create files in
    # the target directory.  This might very well fail, therefore, unless
    # the directory is created appropriately beforehand.
    (-d "$dir/$addressprefix") ||
	mkdir ("$dir/$addressprefix", 0755) ||
	    die "Can't create directory $addressprefix\n";

    # Dump the headers and body to each of the files in the array @destfiles
    while ($#destfiles >= 0) {
	$destfile = shift (@destfiles);
	$destfilename = "$dir/$addressprefix/$destfile";
	print "destfilename=$destfilename\n" if $debug;
	open (OP, ">>$destfilename") || die "Can't open $destfilename";

	print OP "<commentary>\n";
	print OP "<from>$from</from>\n" if (defined($from));
	print OP "<email>$email</email>\n" if (defined($email));
	print OP "<date>$date</date>\n" if (defined($date));
	$subject =~ s/&/&amp;/g;
	$subject =~ s/</&lt;/g;
	$subject =~ s/>/&gt;/g;
	print OP "<subject>$subject</subject>\n" if (defined ($subject));
	while ($#otherheader >= 0) {
	    $t = shift (@othervalue);
	    $t =~ s/&/&amp;/g;
	    $t =~ s/</&lt;/g;
	    $t =~ s/>/&gt;/g;
	    print OP "<otherheader name='",
	    shift (@otherheader), "'>$t</otherheader>\n";
	}

	print OP "<body>\n$bodytext\n</body>\n</commentary>\n";
	
	close OP;
    }
}

	
exit 0;
