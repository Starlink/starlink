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

$#ARGV == 0 || die "Usage: $0 dtd-file\n";

$dtdfilename = $ARGV[0];

$dtdecl = '
<!element elementlist - - (element+)>
<!element element - - (parents, content, tree?, attribute*)>
<!element parents - - (elemref*)>
<!element content - - (#pcdata | elemref)*>
<!element tree - - (#pcdata)>
<!element attribute - - (#pcdata)>
<!element elemref - o empty>
<!attlist elementlist sysid cdata #required>
<!attlist element gi id #required>
<!attlist attribute 
  name cdata #required
  default cdata #required
  type cdata #implied>
<!attlist elemref gi idref #required>
';

use SGML::DTD;

# Now parse the DTD and write the output
open (DTDFILE, $dtdfilename) || die "Can't open DTD $dtdfilename to read\n";

$dtd = new SGML::DTD;
$dtd->read_dtd(\*DTDFILE);

@elements = $dtd->get_elements(0);

print "<!doctype elementlist [$dtdecl]>\n<elementlist sysid='$dtdfilename'>\n";

foreach $e (@elements) {
    print "<element gi='$e'>\n";

    @parents = $dtd->get_parents ($e);
    print "<parents>\n";
    foreach $pe (@parents) {
	print "<elemref gi='", lc($pe), "'>\n";
    }
    print "</parents>\n";

    @content = $dtd->get_base_children ($e, 1);
    print "<content>";
    foreach $ce (@content) {
	$ce = lc($ce);
	if ($ce =~ /[a-z]+/) {
	    print "<elemref gi='$ce'>";
	} else {
	    print "$ce ";
	}
    }
    print "</content>\n";

    print "<tree>\n";
    $dtd->print_tree($e, 2);
    print "</tree>\n";

    # FIXME -- following doesn't deal with notations properly
    %atts = $dtd->get_elem_attr ($e);
    foreach $a (keys(%atts)) {
	$ar = $atts{$a};
	@values = @$ar;
	$def = lc($values[0]);
	shift (@values);
	print "<attribute name='$a' default='$def'";
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
	print ">@values</attribute>\n";
    }
    print "</element>\n";
}
print "</elementlist>\n";
