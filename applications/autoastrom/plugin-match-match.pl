# Plugin for autoastrom, which uses match
# <http://acd188a-005.rit.edu/match/> to do the matching.
#
# To work as an autoastrom plugin, we have to define a routine which
# takes the correct arguments, and store a reference to it in
# $helpers{plugin-match-xxx}, where `xxx' is the name which will be
# used to refer to it.
#
# The arguments are:
#
#    my $helpers = shift;	# Reference to hash of helper programs.
#    my $cat1 = shift;		# First position catalogue
#    my $cat2 = shift;		# Second position catalogue
#    my $matchopts = shift;	# Reference to hash of options
#				# (may include {poserr}, {objsize},
#				# {area}).  Crucially may also contain
#				# element {method}, which can be
#				# `findoff', the default, or the name
#				# of some other program loaded in a plugin.
#    my $tempfn = shift;	# Temporary filename prefix
#
# The catalogues are arrays of hashes with fields {id}, {x} and {y} at
# least. 
#
# Return an array containing the two catalogues, in the same order as
# the corresponding input files, plus a flag (1=ok, 0=error)
# indicating whether the match succeeded or not.  The returned
# catalogues must have the same fields as the corresponding input
# catalogues.
#
# RCS: $Id$
#
# This file is
# Copyright 2001, Council for the Central Laboratory of the Research Councils.
# Licenced under the GNU General Public Licence.

use autoastrom qw ( decompose_transform );

sub match_positions_match ($$$$$) {
    my ($helpers, $cat1, $cat2, $matchopts, $tempfn) = @_;

    my $cat1file = "$tempfn-match-in-1";
    my $cat2file = "$tempfn-match-in-2";
    my $matchoutstem = "$tempfn-match-out";
    my $matchprog = $ENV{AUTOASTROM_DIR} . '/match';

    my $myname = 'match_positions_match';

    # Check the input catalogues have the right form
    (defined($cat1->[0]->{x})
     && defined($cat1->[0]->{y})
     && defined($cat1->[0]->{id})
     && defined($cat1->[0]->{mag}))
      || wmessage ('fatal', "$myname: cat1 does not have required fields");
    (defined($cat2->[0]->{x})
     && defined($cat2->[0]->{y})
     && defined($cat2->[0]->{id})
     && defined($cat2->[0]->{mag}))
      || wmessage ('fatal', "$myname: cat2 does not have required fields");

    # Check the program is there
    (-x $matchprog)
      || wmessage ('fatal',
	   "$myname: can't find match program $matchprog");

#     # Get the list of keys which are in each of the hashes, and then
#     # write the catalogues to files in the correct format for match
#     # (just plain columns of id, x, y, mag).
#     my @cat1keys = keys(%{$cat1->[0]});
#     my %cat1idx = ();
#     for (my $i=0; $i<=$#cat1keys; $i++) {
# 	$cat1idx{$cat1keys[$i]} = $i;
#     }
#     open (CAT, ">$cat1file")
#       || wmessage ('fatal', "$myname: can't open file $cat1file to write");
#     foreach my $r (@$cat1) {
# 	foreach my $e (@cat1keys) {
# 	    printf CAT "%f ", $r->{$e};
# 	}
# 	printf CAT "\n";
#     }
#     close (CAT);
#     my @cat2keys = keys(%{$cat2->[0]});
#     my %cat2idx = ();
#     for (my $i=0; $i<=$#cat2keys; $i++) {
# 	$cat2idx{$cat2keys[$i]} = $i;
#     }
#     open (CAT, ">$cat2file")
#       || wmessage ('fatal', "$myname: can't open file $cat2file to write");
#     foreach my $r (@$cat2) {
# 	foreach my $e (@cat2keys) {
# 	    printf CAT "%f ", $r->{$e};
# 	}
# 	printf CAT "\n";
#     }
#     close (CAT);

    # Use the array indexes as IDs, to make post-processing easy.
    open (CAT, ">$cat1file")
      || wmessage ('fatal', "$myname: can't open file $cat1file to write");
    my $idnum = 0;
    foreach my $r (@$cat1) {
	printf CAT ("%5d  %12.2f  %12.2f  %f\n",
		    $idnum++, $r->{x}, $r->{y}, $r->{mag});
    }
    close (CAT);
    open (CAT, ">$cat2file")
      || wmessage ('fatal', "$myname: can't open file $cat2file to write");
    $idnum = 0;
    foreach my $r (@$cat2) {
	printf CAT ("%5d  %12.2f  %12.2f  %f\n",
		    $idnum++, $r->{x}, $r->{y}, $r->{mag});
    }
    close (CAT);

    # Let's go: build the command and arguments
#    my @matchargs = ($matchprog,
#		     $cat1file, $cat1idx{x}, $cat1idx{y}, $cat1idx{mag},
#		     $cat2file, $cat2idx{x}, $cat2idx{y}, $cat2idx{mag},
#		     'id1='.$cat1idx{id}, 'id2='.$cat2idx{id},
#		     "outfile=$matchoutstem",
#		     "linear",	# if this changes, also change analysis below
#		    );
    my @matchargs = ($matchprog,
		     $cat1file, '1', '2', '3',
		     $cat2file, '1', '2', '3',
		     'id1=0', 'id2=0',
		     "outfile=$matchoutstem",
		     "linear",	# if this changes, also change analysis below
		    );
    if ($verbose) {
	print STDERR "Starting match";
	foreach my $e (@matchargs) {
	    printf STDERR ("\t\\\n\t%s", $e);
	}
	print STDERR "\n";
    }

    # Run the command at the end of a pipe, and redirect the command's
    # stdout to stream MATCHER.  This form of the open command does an
    # implicit fork and returns pid zero in the child and non-zero in
    # the parent.  If the fork fails for any reason, then pid is returned
    # undefined.
    #
    # We don't have to worry about flushing bufferes prior to the
    # fork.  It doesn't matter, since we capture the stdout from the
    # child, and the match program doesn't write anything to stderr.
    my $matchresponse = '';
    my $line;
    my $pid = open (MATCHER, "-|");
    my $successfulmatch = 0;	# initialise false
    if ($pid) {
	# The implicit fork worked, and
	# we're in the parent -- read output of $matchprog from
	# MATCHER.  The output may in principle be multi-line,
	# though a successful output is just a single line.
	while (defined($line = <MATCHER>)) {
	    chomp ($line);
	    $matchresponse .= "$line "; # add extra space instead
	    # of newline
	}
	close (MATCHER);
	# Parse the output of $matchprog.  A successful return
	# consists of a line starting `TRANS:'
	if ($matchresponse =~ /^TRANS/) {
	    printf STDERR ("%s: %s successful: response=%s\n",
			   $myname, $matchprog, $matchresponse)
	      if $verbose;
	    # Format is `TRANS: a=nnn b=nnn...'
	    my @valstrings = split (' ', $matchresponse);
	    my @vals = ();
	    shift (@valstrings); # shift off `TRANS:'
	    foreach my $v (@valstrings) {
		($v =~ /^[a-z]=([-+.eE0-9]*)$/)
		  || wmessage ('fatal',
			       "match response malformed: $matchresponse");
		push (@vals, $1);
	    }
	    if ($verbose) {
		print STDERR "$myname: $matchprog returned...\n";
		my $coef = 'a';
		foreach my $vn (@vals) {
		    print STDERR "\t$coef = $vn\n";
		    $coef++;
		}
	    }
	    # Decompose the transform into scales and angles.
	    my @transcpts
	      = decompose_transform ($vals[0], $vals[1], $vals[2],
				     $vals[3], $vals[4], $vals[5]);
	    printf STDERR ("decompose_transform: xz=%f yz=%f sx=%f, sy=%f perp=%f orient=%f\n",
			   $transcpts[0],
			   $transcpts[1],
			   $transcpts[2],
			   $transcpts[3],
			   $transcpts[4],
			   $transcpts[5])
	      if $verbose;
	    wmessage ('info',
		      sprintf ("%s: transform scale %.1g, non-perpendicularity %.0f",
			       $myname,
			       sqrt(abs($transcpts[2]*$transcpts[3])),
			       $transcpts[4]));
	    # Check the coefficients: the non-perpendicularity
	    # should be `small'.  How small is small?  I'm not
	    # sure, but if it's more than 10 degrees, we should
	    # probably at least warn about it.
	    if (abs($transcpts[4]) > 10) {
		wmessage ('warning',
			  sprintf ("%s: match results skew (%.0f deg); but I'll use the matches anyway",
				   $myname, $transcpts[4]));
	    }
	    
	    $successfulmatch = 1;
	} else {
	    # Output of $matchprog wasn't what we were expecting
	    printf STDERR ("%s: %s: response = %s\n",
			   $myname, $matchprog, $matchresponse)
	      if $verbose;
	}
    } elsif (defined($pid)) {
	# $pid is defined but zero: we're in the child -- exec $matchprog
	exec (@matchargs);
	# We shouldn't get here
	wmessage ('fatal',
		  "plugin-match-match: Failed to exec $matchprog");
	exit 1;			# belt _and_ braces,
				# just in case wmessage is not defined to exit
    } else {
	# $pid was undefined -- we couldn't fork
	wmessage ('fatal',
		  "plugin-match-match: Couldn't start MATCHER=$matchprog");
    }

    if (! $successfulmatch) {
	# Indicate failure
	return (undef, undef, 0); # JUMP OUT
    }

    # That seemed to go OK...

    # Read in the results files, creating hashes to be returned.
    # Although the match documentation is rather vague about it,
    # it seems that the object described on line n of one match
    # file has been matched with the object on line n of the
    # other. 
    #
    # The output files have just four columns: id, x, y, mag.  The
    # id corresponds to the ID in the input file.  Therefore the
    # only column we need to examine in these result files is the
    # first, and use this to select entries from the input
    # catalogues. 
    open (RES, "<$matchoutstem.mtA")
      || wmessage ('fatal',
		   sprintf ("%s: Can't open match results file %s",
			    $myname, "$matchoutstem.mtA"));
    my @res1;
    while (<RES>) {
	push (@res1, /^ *([0-9]+)/);
    }
    close (RES);
    # Same again, for the B results file
    open (RES, "<$matchoutstem.mtB")
      || wmessage ('fatal',
		   sprintf ("%s: Can't open match results file %s",
			    $myname, "$matchoutstem.mtB"));
    my @res2;
    while (<RES>) {
	push (@res2, /^ *([0-9]+)/);
    }
    close (RES);

    # These two results arrays should be the same length.  Check that,
    # and bomb out if it's not true.
    ($#res1 == $#res2)
      || wmessage ('fatal',
		   sprintf ("%s: match results files are not same length (%s:%d, %s:%d)",
			    $myname,
			    "$matchoutstem.mtA", $#res1,
			    "$matchoutstem.mtB", $#res2));

    # The IDs were set up to be the array indexes of the input
    # arrays. Thus we construct the output arrays by assembling
    # $cat1->[$res1[i]] for i in (0..$#res1)
    my @rescat1;
    my @rescat2;
    for (my $i=0; $i<=$#res1; $i++) {
	push (@rescat1, $cat1->[$res1[$i]]);
	push (@rescat2, $cat2->[$res2[$i]]);
    }
    
    # Return with success
    return (\@rescat1, \@rescat2, 1);
}

$helpers{'plugin-match-match'} = \&match_positions_match;

1;
