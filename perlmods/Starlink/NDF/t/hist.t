#!perl -w

use strict;
no strict "vars";

use NDF;

# ================================================================
#   Test NDF calls to read HISTORY information
#   from test.sdf
# ================================================================

# Test file
$oldfile = "test";
$file = "twrite";

system("cp $oldfile.sdf $file.sdf");


$n=1; # number of tests
print "1..$n\n";

# initialise global status
$status = &NDF::SAI__OK;

# For -w
$place = 0;
$val = 0;



# Initialise NDF
ndf_begin;

# Open up the test file
die "Couldn't find test file: $file\n" unless (-e "$file.sdf");

ndf_happn("Perl test", $status);
ndf_open(&NDF::DAT__ROOT,$file,'UPDATE','OLD',$indf,$place,$status);

ndf_hcre($indf, $status);

ndf_hnrec($indf, $nnrec, $status);
print "Number of records: $nnrec\n";

@date = (1996, 8, 8, 20, 00);
ndf_hfind($indf, @date, 0.0, 0, $irec, $status);
print "Record is $irec\n";
ndf_hdef($indf,' ',$status);

@text = ("This is a test of history. (Last word should be HI)", 
	 "Text should be formatted.", "HI");
ndf_hput('NORMAL', '',0, 3, @text, 1, 1, 1, $indf, $status);
ndf_hend($status);


ndf_hnrec($indf, $nrec, $status);
ndf_hout($indf, $nrec, $status);
ndf_hinfo($indf, 'APPLICATION', $nrec, $val, $status);



ndf_annul($indf, $status);
ndf_end($status);

unlink("$file.sdf");


($status != &NDF::SAI__OK) && do {print "not ok\n"; exit;};

if (($nrec - $nnrec) == 1) {
  print "ok\n";
} else {
  print "not ok\n";
}
