#! /usr/bin/perl
#
# Parse a DTD, and produce a
# representation of the DTD as an SGML document.
#
# $Id$
#
# The DTD parsing is done by Earl Hood's perlSGML package
# (http://www.oac.uci.edu/indiv/ehood/perlSGML.html).
#    
# A _much_ nicer way to do this whole thing would be to root around in a
# document's property set.  Unfortunately, Jade doesn't support the
# modules which are necessary (module "prolog sgml document string"
# for class DocumentTypeDecl and friends).  Pity....

$Usage = "Usage: $0 [--descrip descrip-file] dtd-file\n";

while ($#ARGV >= 0) {
  Switch: {
      ($ARGV[0] eq '--descrip') && do {
	  ($#ARGV > 0) || die $Usage;
	  shift (@ARGV);
	  $descripfilename = $ARGV[0];
	  last Switch;
      };
      $dtdfilename = $ARGV[0];
  }
    shift (@ARGV);
}

defined ($dtdfilename) || die $Usage;
defined ($descripfilename) || do {
    ($descripfilename = $dtdfilename) =~ s+^.*/++;
    $descripfilename =~ s/\.[^.]*$/\.description/;
};

use SGML::DTD;

# Now parse the DTD and write the output
open (DTDFILE, $dtdfilename) || die "Can't open DTD $dtdfilename to read\n";

$dtd = new SGML::DTD;
$dtd->read_dtd(\*DTDFILE);

@elements = $dtd->get_elements(0);

@top_element = $dtd->get_top_elements();

print "<!doctype dtdelementlist system 'dtdelementlist.dtd'>\n<dtdelementlist sysid='$dtdfilename' top='$top_element[0]' description='$descripfilename'>\n";

foreach $e (@elements) {
    print "<dtdelement gi='$e'>\n";

    @parents = $dtd->get_parents ($e);
    print "<dtdparents>\n";
    foreach $pe (@parents) {
	print "<dtdelemref gi='", lc($pe), "'>\n";
    }
    print "</dtdparents>\n";

    @content = $dtd->get_base_children ($e, 1);
    print "<dtdcontent>";
    foreach $ce (@content) {
	$ce = lc($ce);
	if ($ce =~ /[a-z]+/) {
	    if (SGML::DTD->is_elem_keyword($ce)) {
		print "$ce";
	    } else {
		print "<dtdelemref gi='$ce'>";
	    }
	} else {
	    print "$ce ";
	}
    }
    print "</dtdcontent>\n";

    print "<dtdtree>\n";
    $dtd->print_tree($e, 2);
    print "</dtdtree>\n";

    # FIXME -- following doesn't deal with notations properly
    %atts = $dtd->get_elem_attr ($e);
    foreach $a (keys(%atts)) {
	$ar = $atts{$a};
	@values = @$ar;
	$def = lc($values[0]);
	shift (@values);
	print "<dtdattribute name='$a' default='$def'";
	if ($def eq '#fixed') {
	    $type = $values[$#values];
	    pop(@values);
	    print " type='$type'";
	} else {
	    if (SGML::DTD->is_attr_keyword ($values[0])) {
		print " type='", lc($values[0]), "'";
		shift (@values);
	    }
	}
	print ">@values</dtdattribute>\n";
    }
    print "</dtdelement>\n";
}
print "</dtdelementlist>\n";
