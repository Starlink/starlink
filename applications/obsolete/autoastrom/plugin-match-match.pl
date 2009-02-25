# Plugin for autoastrom, which uses match
# <http://spiff.rit.edu/match/> to do the matching.
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
# The catalogues are hash references with entries {catalogue},
# containing an array of hashes with fields {id}, {x} and {y} at
# least; and {provenance}, containing a string which reports where it
# came from.
#
# Return an array containing the two catalogues, in the same order as
# the corresponding input files, plus a flag (1=ok, 0=error)
# indicating whether the match succeeded or not.  The returned
# catalogues must have the same fields as the corresponding input
# catalogues.
#
# RCS: $Id$
#
# This file is Copyright 2001, 2003, Council for the Central
# Laboratory of the Research Councils.  Licenced under the GNU
# General Public Licence.

use autoastrom qw ( decompose_transform run_command_pipe );

my $matchprog = $ENV{AUTOASTROM_DIR} . '/match';

sub match_positions_match ($$$$$) {
    my ($helpers, $cat1, $cat2, $matchopts, $tempfn) = @_;

    my $cat1file = "$tempfn-match-in-1";
    my $cat2file = "$tempfn-match-in-2";
    my $matchoutstem = "$tempfn-match-out";

    my $myname = 'match_positions_match';

    my $successfulmatch = 1;
    my @rescat1;
    my @rescat2;

  DOMATCH: {

        my @cat1cat = @{$cat1->{catalogue}};
        my @cat2cat = @{$cat2->{catalogue}};

        # Check the input catalogues have the right form
        (defined($cat1cat[0]->{x})
         && defined($cat1cat[0]->{y})
         && defined($cat1cat[0]->{id})
         && defined($cat1cat[0]->{mag}))
          || do {
              wmessage ('warning',
                        "$myname: cat1 does not have required fields");
              $successfulmatch = 0;
              last DOMATCH; };
        (defined($cat2cat[0]->{x})
         && defined($cat2cat[0]->{y})
         && defined($cat2cat[0]->{id})
         && defined($cat2cat[0]->{mag}))
          || do {
              wmessage ('warning',
                        "$myname: cat2 does not have required fields");
              $successfulmatch = 0;
              last DOMATCH; };

        # Check the program is there
        (-x $matchprog)
          || do {
              wmessage ('warning',
                        "$myname: can't find match program $matchprog");
              $successfulmatch = 0;
              last DOMATCH; };

        # Write the input catalogues into the correct form for match.
        # (just plain columns of id, x, y, mag).
        # Use the array indexes as IDs, to make post-processing easy.
        open (CAT, ">$cat1file")
          || do {
              wmessage ('warning',
                        "$myname: can't open file $cat1file to write");
              $successfulmatch = 0;
              last DOMATCH; };
        my $idnum = 0;
        printf CAT ("# Provenance: %s\n# Cols: my-id x y mag orig-id\n",
                    $cat1->{provenance});
        foreach my $r (@cat1cat) {
            printf CAT ("%5d  %12.2f  %12.2f  %f  %d\n",
                        $idnum++, $r->{x}, $r->{y}, $r->{mag}, $r->{id});
        }
        close (CAT);
        open (CAT, ">$cat2file")
          || do {
              wmessage ('warning',
                        "$myname: can't open file $cat2file to write");
              $successfulmatch = 0;
              last DOMATCH; };
        printf CAT ("# Provenance: %s\n# Cols: my-id x y mag orig-id\n",
                    $cat2->{provenance});
        $idnum = 0;
        foreach my $r (@cat2cat) {
            printf CAT ("%5d  %12.2f  %12.2f  %f  %d\n",
                        $idnum++, $r->{x}, $r->{y}, $r->{mag}, $r->{id});
        }
        close (CAT);

        # Set the number of objects to match.  The default is 20, but
        # since the magnitude order produced by Extractor's FLUX_ISO
        # output isn't necessarily the same as that from the
        # catalogue, we can end up with the top nobj objects having
        # too few matches.  The `match' documentation warns that
        # the algorithm scales as nobj**6, so we can't go wild, here.
        my $nobj = 40;
        if (defined($matchopts->{looseness})) {
            $nobj *= $matchopts->{looseness};
        }

        # Let's go: build the command and arguments
        my @matchargs = ($matchprog,
                         $cat1file, '1', '2', '3',
                         $cat2file, '1', '2', '3',
                         'id1=0', 'id2=0',
                         "outfile=$matchoutstem",
                         "nobj=$nobj",
                         "linear", # if this changes, also change analysis below
                        );
        if ($verbose) {
            print STDERR "Starting match";
            foreach my $e (@matchargs) {
                printf STDERR ("\t\\\n\t%s", $e);
            }
            print STDERR "\n";
        }

        my $matchretref = run_command_pipe(@matchargs);
        defined($matchretref) || do {
            #wmessage('warning', "Match program ".$matchargs[0]." failed");
            $successfulmatch = 0;
            last DOMATCH; };

        my @matchret = @$matchretref;

        if ($#matchret < 0) {
            wmessage('warning', "Match program ".$matchargs[0]." returned no lines");
            $successfulmatch = 0;
            last DOMATCH; };

        # The output of match may in principle be multi-line,
        # though a successful output is just a single line.
        if ($#matchret != 0) {
            wmessage('warning',
                     "Match program ".$matchargs[0].
                     " returned multiple lines: ignoring all but first");
        }
        my $matchresponse = $matchret[0];

        # Parse the output of $matchprog.  A successful return
        # consists of a line starting `TRANS:'
        if ($matchresponse =~ /^TRANS/) {
            printf STDERR ("%s: %s successful: response=<%s>\n",
                           $myname, $matchprog, $matchresponse)
              if $verbose;
            # Format is `TRANS: a=nnn b=nnn...'
            my @valstrings = split (' ', $matchresponse);
            my %vals = ();
            shift (@valstrings); # shift off `TRANS:'
            foreach my $v (@valstrings) {
                ($v =~ /^(\w+)=([-+.eE0-9]+)$/)
                  || do {
                      wmessage ('warning',
                                "match response malformed: $matchresponse");
                      $successfulmatch = 0;
                      last DOMATCH; };
                $vals{$1} = $2;
            }
            if ($verbose) {
                print STDERR "$myname: $matchprog returned...\n";
                foreach my $vn (sort(keys(%vals))) {
                    print STDERR "\t$vn = $vals{$vn}\n";
                }
            }
            # Decompose the transform into scales and angles.
            my @transcpts
              = decompose_transform ($vals{a}, $vals{b}, $vals{c},
                                     $vals{d}, $vals{e}, $vals{f});
            printf STDERR ("decompose_transform: xz=%f yz=%f sx=%f, sy=%f perp=%f orient=%f (scale=%f)\n",
                           $transcpts[0],
                           $transcpts[1],
                           $transcpts[2],
                           $transcpts[3],
                           $transcpts[4],
                           $transcpts[5],
                           sqrt(abs($transcpts[2]*$transcpts[3])))
              if $verbose;
            # Check the coefficients: the non-perpendicularity
            # should be `small'.  How small is small?  I'm not
            # sure, but if it's more than 10 degrees, we should
            # probably at least warn about it.
            if (abs($transcpts[4]) > 10) {
                wmessage ('warning',
                          sprintf ("%s: match results skew (%.0f deg); but I'll use the matches anyway",
                                   $myname, $transcpts[4]));
            }
	} else {
	    # Output of $matchprog wasn't what we were expecting
	    printf STDERR ("%s: %s: response = <%s>\n",
			   $myname, $matchprog, $matchresponse)
	      if $verbose;
            $successfulmatch = 0;
            last DOMATCH;
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
          || do {
              wmessage ('warning',
                        sprintf ("%s: Can't open match results file %s",
                                 $myname, "$matchoutstem.mtA"));
              $successfulmatch = 0;
              last DOMATCH; };
        my @res1;
        while (<RES>) {
            push (@res1, /^ *([0-9]+)/);
        }
        close (RES);
        # Same again, for the B results file
        open (RES, "<$matchoutstem.mtB")
          || do {
              wmessage ('warning',
                        sprintf ("%s: Can't open match results file %s",
                                 $myname, "$matchoutstem.mtB"));
              $successfulmatch = 0;
              last DOMATCH; };
        my @res2;
        while (<RES>) {
            push (@res2, /^ *([0-9]+)/);
        }
        close (RES);

        # These two results arrays should be the same length.  Check that,
        # and bomb out if it's not true.
        ($#res1 == $#res2)
          || do {
              wmessage ('warning',
                        sprintf ("%s: match results files are not same length (%s:%d, %s:%d)",
                                 $myname,
                                 "$matchoutstem.mtA", $#res1,
                                 "$matchoutstem.mtB", $#res2));
              $successfulmatch = 0;
              last DOMATCH; };

        # The IDs were set up to be the array indexes of the input
        # arrays. Thus we construct the output arrays by assembling
        # $cat1cat[$res1[i]] for i in (0..$#res1)
        for (my $i=0; $i<=$#res1; $i++) {
            push (@rescat1, $cat1cat[$res1[$i]]);
            push (@rescat2, $cat2cat[$res2[$i]]);
        }
    }

    printf STDERR ("plugin-match-match: #cat1=%d, #cat2=%d, success=%d\n",
                   $#rescat1, $#rescat2, $successfulmatch)
      if $verbose;

    if ($successfulmatch) {
        return (\@rescat1, \@rescat2, 1); # success
    } else {
	return (undef, undef, 0);
    }
}

# Check that the match program is actually present
if (-x $matchprog) {
    # It is, so install the plugin.
    $helpers{'plugin-match-match'} = \&match_positions_match;
    $helpers{'plugin-status'} = 1;
} else {
    # It's not, so give a warning and don't install the plugin.
    wmessage ('warning',
      "Match program $matchprog not found: match plugin not installed");
    $helpers{'plugin-status'} = 0;
}

1;
