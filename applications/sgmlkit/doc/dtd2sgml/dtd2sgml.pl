#! /usr/bin/perl
#
# Parse a DTD and an element descriptions file, and produce a fragment
# suitable for inclusion in a document marked up using the Starlink
# General DTD.  The descriptions file has the format
#
#    element-description elementname
#    <p>This is an element description, which refers to another
#    element by the name of *otherelement*.
#
# Additionally, you can use `element-description elementname*attributename'.
# Avoid using any other markup in the description, at present, as I
# want to leave open the possibility of transforming the descriptions
# file to other formats.  Perhaps it would be better to have the
# descriptions file in SGML - that's nice, but it means that we
# couldn't do the complete merge of the parsed DTD and the
# descriptions just with Perl.
#
# The DTD parsing is due to Earl Hood's perlSGML
# (http://www.oac.uci.edu/indiv/ehood/perlSGML.html), and the format of
# the description file follows the description file for perlSGML's
# dtd2html program.  The one can easily be converted into the other.
#    

$#ARGV == 1 || die "Usage: $0 dtd-file descriptions-file\n";

$dtdfilename = $ARGV[0];
$descripfile = $ARGV[1];

use SGML::DTD;

open (DESCRIP, $descripfile) || die "Can't open file $descripfile to read\n";
$entry = '';
$currelem = '';
while (defined($line = <DESCRIP>)) {
    if ($line =~ /^#/) {
	next;
    } elsif ($line =~ /^element-description\s(\S+)/) {
	if ($currelem ne '' && $entry ne '') {
	    $description{$currelem} = $entry;
	}
	$currelem = $1;
	$entry = '';
    } else {
	$entry .= $line;
    }
}
if ($currelem ne '' && $entry ne '') {
    $description{$currelem} = $entry;
}
close(DESCRIP);

# Now parse the DTD and write the output
open (DTDFILE, $dtdfilename) || die "Can't open DTD $dtdfilename to read\n";

$dtd = new SGML::DTD;
$dtd->read_dtd(\*DTDFILE);

@elements = $dtd->get_elements(0);

foreach $e (@elements) {
    print "<subsubsect id='elementlist.$e'>$e\n";

    $eldesc = $description{$e};
    if (defined($eldesc)) {
	$eldesc =~ s/\*(\S+)\*/<ref id='elementlist.$1'>/g;
	print $eldesc;
    }

    @content = $dtd->get_base_children ($e, 1);
    print "<p>Content: <code>@content</code>\n";
    %atts = $dtd->get_elem_attr ($e);
    $printattheader = 1;
    foreach $a (keys(%atts)) {
	if ($printattheader) {
	    print "<p>Attributes:\n<dl>\n";
	    $printattheader = 0;
	}
	$ar = $atts{$a};
	print "<dt>$a<dd><px><code>@$ar</code></px>\n";
	$attdesc = $description{$e.'*'.$a};
	if (defined($attdesc)) {
	    $attdesc =~ s/<p>/<px>/g;
	    $attdesc =~ s/\*(\S+)\*/<ref id='elementlist.$1'>/g;
	    print $attdesc;
	}
    }
    print "</dl>\n" unless $printattheader;
}
