#! /usr/bin/perl -w
#
# Process the file produced by the img.maths mode of .../slmaths.dsl
#
# Format is 
#
#   %%imgmath type1 label1
#   ...LaTeX maths code...
#   %%imgmath type2 label2
#   ...
#
# In future, deal with image reuse

($#ARGV eq 0) || Usage ();

$infile = $ARGV[0];
($filenameroot = $infile) =~ s/\..*$//;

$eqcount = '000';

%eqtypes = ( 'start-inline' => '\(',
	     'end-inline' => '\)',
	     'start-equation' => '\[',
	     'end-equation' => '\]',
	     'start-eqnarray' => '\begin{eqnarray*}',
	     'end-eqnarray' => '\end{eqnarray*}',
	     );

open (EQIN, "$infile")
    || die "Can't open $infile to read";
open (SGMLOUT, ">$filenameroot.imgeq-sgml")
    || die "Can't open $filenameroot.imgeq-sgml to write";
open (LATEXOUT, ">$filenameroot.imgeq.tex")
    || die "Can't open $filenameroot.imgeq.tex to write";

print LATEXOUT <<'EOT';
\documentclass{minimal}
\pagestyle{empty}
\begin{document}
EOT

while (defined($line = <EQIN>)) {
    if ($line =~ /^%%imgmath/) {
	chop($line);
	print LATEXOUT $eqtypes{'end-'.$eqtype} . "\n\\newpage\n"
	    if ($eqcount > 0);
	$eqcount++;
	($dummy,$eqtype,$label) = split (/ /, $line);
	print LATEXOUT $eqtypes{'start-'.$eqtype} . "\n";
	print SGMLOUT "<img-eqn label='$label' sysid='$filenameroot.imgeq$eqcount.gif'>\n";
    } else {
	print LATEXOUT $line;
    }
}
print LATEXOUT $eqtypes{'end-'.$eqtype} . "\n\n\\end{document}\n";

close (SGMLOUT);
close (LATEXOUT);
close (EQIN);

exit 0;


sub Usage {
    die "Usage: $0 filename\n";
}
