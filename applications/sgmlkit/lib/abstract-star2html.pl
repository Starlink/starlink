#! /usr/bin/perl -w 
#
# RCS $Id$
#+
#<func>
#<routinename>abstract-star2html.pl
#<purpose>Obtain an SGML summary of a Star2HTML document
#<description>
# This reads a file marked up using the Star2HTML
# extensions, plus an HTX index file, and produces a summary of the
# document marked up using the DocumentSummary DTD.  That includes the
# sub*section structure, the cross-references, and most of the header.
#
# <p>Usage:
# <verbatim>
#    abstract-star2html.pl --prefix=sun180.htx/ \
#          /star/docs/sun180.tex /star/docs/sun180.htx/htx.index \
#          > sun180.summary
# </verbatim>
#
# <p>The parsing can cope with <code>\xlabel</code> commands either outside 
# section headings or inside them, and copes with multiple
# <code>\xlabel</code> commands within a heading by emitting <LABEL>
# elements after the heading.  It logs a message to stderr when it
# discovers this. 
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
# <p>Limitations:
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
#<returnvalue none>
#<parameter>input-file-name<type>Star2HTML file
#  <description>A file marked up using the Star2HTML extensions to LaTeX2HTML
#<parameter>index-file-name<type>SGML file
#  <description>A file conforming to the DocumentSummary DTD
#<parameter>--prefix=url-prefix<type>option
#  <description>The prefix is added to each of the filenames in the
# HTX index, to make the generated URLs relative to the appropriate
# root of the document server, which is set in the DSSSL variable 
# <code>%starlink-document-server%<code>, and might have the value
# <code>`file:///star/docs/'</code>
#<author id=ng webpage='http://www.astro.gla.ac.uk/users/norman/'
#  affiliation='Glasgow'>Norman Gray

$ident_string = "Starlink SGML system, release ((PKG_VERS))";

$programname = $0;
$urlprefix = '';
while ($#ARGV >= 0)
{
    if ($ARGV[0] =~ /^--prefix=(\S*)/) {
	$urlprefix = $1;
    } elsif (!defined ($inputfilename)) {
	$inputfilename = $ARGV[0];
    } elsif (!defined ($indexfilename)) {
	$indexfilename = $ARGV[0];
    } else {
	Usage ();
    }
    shift;
}
#$inputfilename = ':sun188.tex';
#$indexfilename = ':sun188.htx:htx.index';

defined ($indexfilename) || Usage ();

# Arguments parsed...

open (TF, $inputfilename)
    || die "Can't open TeX file $inputfilename to read";

open (IDX, $indexfilename)
    || die "Can't open index file $indexfilename to read";
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
	    # Append new lines as long as the line-so-far doesn't match
	    # the following regexp, which matches only lines with 
	    # A SINGLE LEVEL of balanced braces
	    while ($line !~ /\\(sub)*section{([^{]*{[^{}]*})*[^{}]*}/) {
		$newline = <TF>;
		defined ($newline) || die "Unexpected EOF";
		$line .= $newline;
		# compress spaces, also replacing newlines
		$line =~ s/\s+/ /g;
	    }
	    # Re-match the line, so we can use $1.  We can't just bracket
	    # the appropriate bit of the !~ regexp - $1 isn't set, and I'm
	    # not quite sure why (something to do with the test failing?)
	    $line =~ /(\\(sub)*section{([^{]*{[^{}]*})*[^{}]*})/;
	    my $ptitle = parse_section ($1);
	    check_markup ('section title', $ptitle);
	    print $ptitle;
	    next;
	};
    $line =~ /\\xlabel{(.*)}/ && do {
	    if (defined($labels{$1})) {
		print "<label id=\"$1\" export urlpath=\"$urlprefix$labels{$1}\">\n";
	    } else {
	    # This xlabel wasn't in the htx.index.  
	    # Log a remark to STDERR, and print nothing
	    print STDERR "Unknown \\xlabel{$1}\n";
	    }
	    next;
	};

    # Header lines
    $line =~ /newcommand/ && do {
	# As above...
	while ($line !~ /\\newcommand\s*{([^}]*)}\s*{(([^{]*{[^{}]*})*[^{}]*)}/) {
	    $newline = <TF>;
	    defined ($newline) || die "Unexpected EOF";
	    $line .= $newline;
	    $line =~ s/\s+/ /g;
	}
	my ($cmd,$arg) = 
	    ($line =~ /\\newcommand\s*{([^}]*)}\s*{(([^{]*{[^{}]*})*[^{}]*)}/);

	#print STDERR "Warning: markup in $cmd $arg\n"
	#	if ($arg =~ /(\\|~)/ && $cmd !~ /\\stardoc(authors|abstract)/);

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
	    print "<!doctype documentsummary public '-//Starlink//DTD Document Summary 0.1//EN'>
<documentsummary urlpath='$urlprefix$labels{TOP}' urllinkpolicy='explicit'>
<docinfo>
";
	    print "<title>$documenttitle</title>\n<authorlist>\n";
	    foreach $a (@documentauthlist)
	    {
#		print '<author email="???"
#  webpage="???"
#  affiliation="???"
#  id="???">';
		print "<author>$a</author>\n";
		print "<!-- Insert email, webpage, affiliation attributes -->\n";
	    }
	    print "</authorlist>
<docnumber documenttype=\"$documenttype\">$documentnumber</docnumber>
</docinfo>
<docbody>
";
	    print "<abstract>$documentabstract</abstract>\n"
		if (defined($documentabstract));
	    next; };
   print STDERR "Unmatched line: $line";
}

print "</docbody>\n</documentsummary>\n";

exit 0;

# Argument is of form 
# \(sub)*section{blah \label{something} blah \xlabel{something else} blah}
# (assumed on one line).  Return a string 
# "<(sub*)sect><title>blah blah blah</title>" if \xlabel is empty
# including ID, EXPORT and URL attributes if \xlabel is specified.
sub parse_section {
    $sectheading = shift;
    my ($level, $dummy, $secttitle, @xlabel, $nextxlabel);

    ($level, $dummy, $secttitle) = 
	($sectheading =~ /^\\((sub)*sect)ion{(.*)}$/);

    # discard \label commands
    $secttitle =~ s/\\label{[^}]*}//g;
    # extract \xlabel commands, and put them in an array @xlabel
    while ($secttitle =~ s/\\xlabel{([^}]*)}//) {
	push (@xlabel, $1);
    }

    #print "#sectheading=<$sectheading>\n";
    #print "#level=<$level>  secttitle=<$secttitle>  xlabel=<@xlabel>\n";

    # Trim leading and trailing whitespace from section title
    $secttitle =~ s/(^ +)|( +$)//g;

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
    die "$ident_string\nUsage: $programname [--prefix=url-prefix] input-file-name index-file-name\n";
}
