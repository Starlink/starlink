#! /usr/bin/perl -w
#
# Testing dvi2bitmap specials
#
# Create a TeX file with strut and mark specials in it, use
# --query=bitmaps, and check that the correct stuff is reported.

$prefix = 'temp-t8';
$texfile = "$prefix.tex";
$m = 2;				# magnification factor

open (TEX, ">$texfile") || die "Can't open $texfile for output";
$newpage = "\n\n\\vfill\\eject\n\n";

$nerrors = 0;

@expected = ();

print TEX <<'EOD';
\hoffset=0pt
\voffset=0pt
\leftskip=0pt % default
\topskip=0pt
\parindent=0pt
\parskip=0pt
\nopagenumbers


EOD

print TEX "\\special{dvi2bitmap default unit bp}\n\n";

print TEX '\vrule width 20bp height 10bp';
print TEX $newpage;
push(@expected, [20*$m, 10*$m]);


print TEX '\vrule width 20bp height 10bp \special{dvi2bitmap strut 10 4 5 6}';
print TEX $newpage;
push(@expected, [24*$m, 16*$m]);

print TEX '\vrule width 10bp height 10bp \special{dvi2bitmap strut 12 0 15 0}';
print TEX $newpage;
push(@expected, [12*$m, 15*$m]);

print TEX '\vrule width 20bp height 10bp \special{dvi2bitmap mark}';
print TEX $newpage;
push(@expected, [20*$m, 10*$m, 20*$m, 10*$m]);

print TEX "\\bye\n";

close(TEX);

system("tex $prefix") == 0
    || die "Command 'tex $prefix' failed";

$cmd = sprintf("../dvi2bitmap --output-type=gif --output=%s-%%d --query=bitmaps --resolution=%d --verbose=quiet %s.dvi",
	       $prefix, 72*$m, $prefix);

open (DB, "$cmd |") || die "Can't open $cmd in a pipe";

$ntests = 0;
while (<DB>) {
    $ntests++;
    chomp;
    s/^Qbitmaps \S*\s*//;
    my @a = split;

    if ($#expected < 0) {
	print STDERR "More actual than expected\n";
	$nerrors++;
	last;
    }
    my @e = @{shift(@expected)};

    my $isok = 1;
    my $i = 0;
    while (1) {
	if (! (defined($e[$i]) && defined($a[$i]))) { # one list has ended
	    $isok = !(defined($e[$i]) || defined($a[$i]));
            # false unless both undef
	    last;
	}
	if ($e[$i] != $a[$i]) {
	    $isok = 0;
	    last;
	}
	$i++;
    }
    if (!$isok) {
	print STDERR "Test $ntests: expected [";
	foreach $i (0..$#e) {
	    printf STDERR " %.1f", $e[$i];
	}
	print STDERR " ], got [";
	foreach $i (0..$#a) {
	    printf STDERR " %.1f", $a[$i];
	}
	print " ]\n";
	$nerrors++;
    }
}

close(DB);

exit($nerrors);
