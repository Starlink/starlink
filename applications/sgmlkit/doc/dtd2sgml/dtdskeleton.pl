#! /usr/bin/perl
#
# Parse a DTD, and produce a skeleton dtddescription document
# $Id$

$Usage = "Usage: $0 [--catalog catalogue] dtd-file\n";

$catalogfile = './CATALOG';	# probably not-very-useful default

while ($#ARGV >= 0) {
  Switch: {
      ($ARGV[0] eq '--catalog') && do {
	  ($#ARGV > 0) || die $Usage;
	  shift (@ARGV);
	  $catalogfile = $ARGV[0];
	  last Switch;
      };
      ($ARGV[0] =~ /^--/) && die $Usage;
      $dtdfilename = $ARGV[0];
  }
    shift (@ARGV);
}

defined ($dtdfilename) || die $Usage;

use SGML::DTD;
use SGML::EntMan;

# Open the DTD file
open (DTDFILE, $dtdfilename) || die "Can't open DTD $dtdfilename to read\n";

if (defined($catalogfile)) {
    $entman = new SGML::EntMan;
    $entman->read_catalog($catalogfile)
	|| die "Can't read catalog $catalogfile\n";

    $dtd = new SGML::DTD \*DTDFILE, $entman;
} else {
    $dtd = new SGML::DTD;
    $dtd->read_dtd(\*DTDFILE);
}

@elements = $dtd->get_elements(0);

@top_element = $dtd->get_top_elements();

print "<!DOCTYPE dtddescription SYSTEM 'dtddescription.dtd'>\n<dtddescription docelem=$top_element[0]>\n";

foreach $e (@elements) {
    print "<element gi='$e'>\n";

    %attributes = $dtd->get_elem_attr($e);
    foreach $a (keys %attributes) {
	my $ar = $attributes{$a};
	my @values = @$ar;
	if (lc($values[0]) ne '#fixed') {
	    print "<attribute name='$a'>\n";
	}
    }
}
