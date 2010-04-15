#! /usr/bin/perl -w

$RCSID = '$Id$';

#+
# <routinename id=abstract-star2html>abstract-star2html.pl
#
# <purpose>Obtain an SGML summary of a Star2HTML document
#
# <description>
# This reads a file marked up using the Star2HTML
# extensions, plus an HTX index file, and produces a summary of the
# document marked up using the DocumentSummary DTD.  That includes the
# sub*section structure, the cross-references, and most of the header.
# It emits a suitable CATALOG line on STDOUT.
#
# <p>The aim of this tool is only secondarily to produce an accurate,
# or complete, summary of the target document.  It is primarily
# required simply to complete whilst running unattended (within the
# package's installation script), producing a valid SGML document,
# which can be used as a cross-reference target in the General DTD's
# DOCXREF element without being grossly misleading.
#
# <p>Usage:
# <verbatim>
#    abstract-star2html.pl \
#          --prefix=sun180.htx/ --output=sun180.summary \
#          /star/docs/sun180.tex /star/docs/sun180.htx/htx.index \
#          >>CATALOG 2>abstract-warnings
# </verbatim>
#
# <p>The parsing can cope with <code>\xlabel</code> commands either outside
# section headings or inside them, and copes with multiple
# <code>\xlabel</code> commands within a heading by
# emitting <code>&lt;label></>
# elements after the heading.  It logs a message to stderr when it
# discovers this.
#
# <p>If the parser discovers LaTeX markup in the section headings (which is
# true at some point for almost every file), then it logs a message to STDERR.
#
# <p>The parsing respects <code>\begin{htmlonly}...\end{htmlonly}</code>
#
# <p>The parsing copes with the arguments to each of the commands it matches
# (<code>\newcommand{\star...}</code> and <code>\sub*section{...}</code>)
# being on more than one line.
# It concatenates the lines before analysing them.
#
# <p>Emits a warning if an <code>\xlabel</code> doesn't appear in the
# HTX index file.
#
#
# <diytopic>Limitations
#
# <p>The result could benefit from a little editing, to insert the
# attribute values of the AUTHOR element such as email address, which
# aren't included in the Star2HTML file, but it should be valid
# without it.  However, these attributes aren't actually used in the
# cross reference, so there's no great loss at present.
#
# <p>It also
# assumes that the things it matches are at the beginning of the line, possibly
# preceded by whitespace (this isn't just to speed it up, but also to avoid
# matching any reference to the <code>\section</code> command within the
# body of the text).
#
# <p>The parsing doesn't attempt to deal with markup in section titles, but it
# does attempt to detect and warn about it, logging a message to stderr.
#
# <p>There's not a lot of point in working hard to make this code do much
# better than this, since that would essentially require the sophistication
# of a full document conversion.
#
# <p>Because folk can do arbitrarily clever things with newcommands, I've had
# to dumb down the parsing of them.  The code here will successfully extract
# document number, author, etc, as long as the corresponding newcommand is
# all on one line.  I've had to do the same with
# <code>\(sub)*section</> parsing.
# This will fail sometimes, but the result will be a
# thin but <em>valid</> SGML document, and will not cause this code to spin
# its wheels indefinitely.  If you include the option <code>--force</>, then
# even if there's some fatal error, such as an input file not being present,
# then the script will still return with a zero exit status.
#
# <returnvalue type=file>
# A file conforming to the DocumentSummary DTD
#
# <argumentlist>
#  <parameter>input-file-name
#    <type>Star2HTML file
#    <description>A file marked up using the Star2HTML extensions to LaTeX2HTML
#
#  <parameter>index-file-name
#    <type>HTX index file
#    <description>The index file produced by HTX (see SUN/188)
#
#  <parameter>--prefix=url-prefix
#    <type>option
#    <description>The prefix is added to each of the filenames in the
#      HTX index, to make the generated URLs relative to the appropriate
#      root of the document server, which is set in the DSSSL variable
#      <code>%starlink-document-server%</code>, and might have the value
#      <code>`file:///star/docs/'</code>
#
#  <parameter>--output=outfile
#    <type>option
#    <description>The name of the file to receive the generated output.
#    If omitted, the result goes to STDOUT.
#
#  <parameter>--force
#    <type>option
#    <description>If present, then any errors which emerge after this will
#      terminate the program, but return with a zero exit status.
#
#  <parameter>--version
#    <type>option
#    <description>Print the version number and exit.
#
#  <authorlist>
#  <author id=ng webpage='http://www.astro.gla.ac.uk/users/norman/'
#    affiliation='Glasgow'>Norman Gray
#-

$ident_string = "Starlink SGML system, release ((PKG_VERS))";

$dontfail = 0;
$programname = $0;
$urlprefix = '';
while ($#ARGV >= 0)
{
    if ($ARGV[0] =~ /^--prefix=(\S*)/) {
	$urlprefix = $1;
    } elsif ($ARGV[0] =~ /^--output=(\S*)/) {
	$outputfile = $1;
    } elsif ($ARGV[0] eq '--force') { # fail quietly (ie, zero exit status)
	$dontfail = 1;
    } elsif ($ARGV[0] eq '--version') {
	print "$ident_string\n$RCSID\n";
	exit (0);
    } elsif (!defined ($inputfilename)) {
	$inputfilename = $ARGV[0];
    } elsif (!defined ($indexfilename)) {
	$indexfilename = $ARGV[0];
    } else {
	Usage ();
    }
    shift;
}

defined ($indexfilename) || Usage ();

# Arguments parsed...

open (TF, $inputfilename)
    || Error ("Can't open TeX file $inputfilename to read");

open (IDX, $indexfilename)
    || Error ("Can't open index file $indexfilename to read");

if (defined $outputfile) {
    open (OUTFILE, ">$outputfile")
	|| Error ("Can't open output file $outputfile to write");
} else {
    open (OUTFILE, ">&STDOUT");
}

# Write an identifying line on STDERR
# This should be redirected by the caller.
# This script produces so many warnings that a bit of excess chatter
# won't matter.
print STDERR "### Processing $inputfilename...\n";

while (defined($idxline = <IDX>))
{
    #next if $idxline =~ /^>/;
    chop $idxline;
    if (($node, $key) = ($idxline =~ /^<\s*(\S+)\s*(\S*)/))
    {
	#if ($key !~ /^\s*$/)
	if ($key ne '')
	{
	    $labels{$key} = $node;
	}
    } elsif (($node) = ($idxline =~ /^T\s*(\S+)/)) {
	$labels{TOP} = $node;
    }
}
close (IDX);

# We've read the HTX index, so now work through the main document.

$htmlonly = 0;

while (defined($line = <TF>))
{
    # Respect \begin{htmlonly}
    if ($line =~ /\\end{htmlonly}/) {
	$htmlonly = 0;
	next;
    }
    next if $htmlonly;

    if ($line =~ /\\begin{htmlonly}/) {
	$htmlonly = 1;
	next;
    }

    # leap out, unless there's useful information on the line
    next unless $line =~ /^\s*\\(begin\{document\}|newcommand\s*\{\\star|section|subsection|subsubsection|xlabel)/;


    $line =~ /(\\(sub)*section.*)/ && do {
	#    # Append new lines as long as the line-so-far doesn't match
	#    # the following regexp, which matches only lines with
	#    # A SINGLE LEVEL of balanced braces
	#    while ($line !~ /\\(sub)*section{([^{]*{[^{}]*})*[^{}]*}/) {
	#	$newline = <TF>;
	#	defined ($newline) || die "Unexpected EOF";
	#	$line .= $newline;
	#	# compress spaces, also replacing newlines
	#	$line =~ s/\s+/ /g;
	#    }
	#    # Re-match the line, so we can use $1.  We can't just bracket
	#    # the appropriate bit of the !~ regexp - $1 isn't set, and I'm
	#    # not quite sure why (something to do with the test failing?)
	#    $line =~ /(\\(sub)*section{([^{]*{[^{}]*})*[^{}]*})/;
	#    my $ptitle = parse_section ($1);
	    my $ptitle = parse_section ($line);
	    check_markup ('section title', $ptitle);
	    print OUTFILE $ptitle;
	    next;
	};
    $line =~ /\\xlabel{(.*)}/ && do {
	    if (defined($labels{$1})) {
		print OUTFILE "<label id=\"$1\" export urlpath=\"$urlprefix$labels{$1}\">\n";
	    } else {
	    # This xlabel wasn't in the htx.index.
	    # Log a remark to STDERR, and print nothing
	    print STDERR "Unknown \\xlabel{$1}\n";
	    }
	    next;
	};

    # Header lines
    $line =~ /newcommand/ && do {
	# Don't even attempt to do anything clever with newcommands -
	# nothing short of magic will do here, so dumb it down to make it
	# more robust.  The following will only
	# work if \newcommand{\stardocnumber}{...} is all on one line.
	# This'll fail miserably for the abstract, but I DON'T CARE!

	# As above...
	#while ($line !~ /\\newcommand\s*{([^}]*)}\s*{(([^{]*{[^{}]*})*[^{}]*)}/) {
	#    $newline = <TF>;
	#    defined ($newline) || die "Unexpected EOF";
	#    $line .= $newline;
	#    $line =~ s/\s+/ /g;
	#    print STDERR "$line";
	#}
	#my ($cmd,$arg) =
	#    ($line =~ /\\newcommand\s*{([^}]*)}\s*{(([^{]*{[^{}]*})*[^{}]*)}/);

	my ($cmd,$arg) = ($line =~ /\\newcommand\s*{(.*?)}(.*)$/);
	$arg =~ s/(^\s+|\s+$|{|})//g;

	$cmd eq '\stardocinitials' && do {
	    check_markup ('\stardocinitials', $arg);
	    $documenttype = $arg;
	    next; };
	$cmd eq '\stardocnumber' && do {
	    check_markup ('\stardocnumber', $arg);
	    $documentnumber = $arg;
	    next; };
	$cmd eq '\stardocauthors' && do {
	    @documentauthlist = split (/\\\\/, $arg);
	    for $a (@documentauthlist) {
		check_markup ('author', $a);
	    }
	    next;
	};
	$cmd eq '\stardoctitle' && do {
	    check_markup ('\stardoctitle', $arg);
	    $documenttitle = $arg;
	    next; };
	$cmd eq '\stardocabstract' && do {
	    # don't bother to warn about markup in the abstract
	    $documentabstract = $arg;
	    next; };
        next;
    };

    $line =~ /\\begin\{document\}/ &&
	do {
	    print OUTFILE "<!doctype documentsummary public '-//Starlink//DTD Document Summary 0.2//EN'>
<documentsummary urlpath='$urlprefix$labels{TOP}' urllinkpolicy='explicit'>
<docinfo>
";
	    print OUTFILE "<title>$documenttitle</title>\n<authorlist>\n";
	    foreach $a (@documentauthlist)
	    {
#		print OUTFILE '<author email="???"
#  webpage="???"
#  affiliation="???"
#  id="???">';
		print OUTFILE "<author>$a</author>\n";
		print OUTFILE "<!-- Insert email, webpage, affiliation attributes -->\n";
	    }
	    print OUTFILE "</authorlist>
<docnumber documenttype=\"$documenttype\">$documentnumber</docnumber>
</docinfo>
<docbody>
";
	    # Strip any document version number
	    $documentnumber =~ s/^(\d*).*$/$1/;
	    $catalogueline = "PUBLIC \"-//Starlink//DOCUMENT Summary $documenttype/$documentnumber//EN\" $outputfile\n";

	    print OUTFILE "<abstract>$documentabstract</abstract>\n"
		if (defined($documentabstract));
	    next;
	};
   print STDERR "Unmatched line: $line";
}

print OUTFILE "</docbody>\n</documentsummary>\n";
print $catalogueline;
exit 0;

# Argument is of form
# \(sub)*section{blah \label{something} blah \xlabel{something else} blah}
# (assumed on one line).  Return a string
# "<(sub*)sect><title>blah blah blah</title>" if \xlabel is empty
# including ID, EXPORT and URL attributes if \xlabel is specified.
# Dumbed-down version: don't assume braces match
sub parse_section {
    $sectheading = shift;
    my ($level, $dummy, $secttitle, @xlabel, $nextxlabel);

    ($level, $dummy, $secttitle) =
	($sectheading =~ /^\\((sub)*sect)ion{(.*)$/);

    # discard \label commands
    $secttitle =~ s/\\label{[^}]*}//g;
    # extract \xlabel commands, and put them in an array @xlabel
    while ($secttitle =~ s/\\xlabel{([^}]*)}//) {
	push (@xlabel, $1);
    }

    #print "#sectheading=<$sectheading>\n";
    #print "#level=<$level>  secttitle=<$secttitle>  xlabel=<@xlabel>\n";

    # Trim leading and trailing whitespace, and remaining {} from section title
    #$secttitle =~ s/(^ +)|( +$)//g;
    $secttitle =~ s/(^\s+|\s+$|{|})//g;

    $nextxlabel = shift (@xlabel);
    if (! defined($nextxlabel))
    {
	# There were no \xlabel commands...
	return "<$level><title>$secttitle</title>\n";
    } else {
	my $retval;
	if (defined($labels{$nextxlabel})) {
	    $retval = "<$level id=\"$nextxlabel\" export urlpath=\"$urlprefix$labels{$nextxlabel}\"><title>$secttitle</title>\n";
	} else {
	    # This xlabel wasn't in the htx.index.
	    # Log a remark to STDERR, and return unadorned heading
	    print STDERR "Unknown \\xlabel{$nextxlabel}\n";
	    $retval = "<$level><title>$secttitle</title>\n";
	}

	while (defined ($nextxlabel = shift (@xlabel))) {
	    print STDERR "Multiple labels in $sectheading\n";
	    # We found _several_ xlabels in the title.  Emit <LABEL> elements
	    if (defined($labels{$nextxlabel})) {
		$retval .= "<label id=\"$nextxlabel\" export urlpath=\"$urlprefix$labels{$nextxlabel}\">\n";
	    } else {
		# This xlabel wasn't in the htx.index.
		# Log a remark to STDERR, and do nothing
		print STDERR "Unknown \\xlabel{$nextxlabel}\n";
	    }
	}
	return $retval;
    }
}

# Check to see if there's any LaTeX markup in the argument, and warn if
# there is.  It doesn't do anything with any it finds at present, but it could
# be adapted to remove or transform it.
sub check_markup {
    my ($context,$arg) = @_;
    if ($arg =~ /\\|~/) {
	$arg =~ s/\s*$//;	# delete any trailing whitespace
	print STDERR "Warning: markup in $context: $arg\n";
    }
}


sub Usage {
    Error ("$ident_string\nUsage: $programname [--prefix=url-prefix] [--output=outfile] [--force] input-file-name index-file-name\n");
}

sub Error {
    my $msg = shift;
    if ($dontfail) {
	print STDERR $msg;
	exit (0);
    } else {
	die $msg;
    }
}
