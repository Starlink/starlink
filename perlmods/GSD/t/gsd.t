#!perl -w

# Test the pure library interface
use strict;
no strict 'refs';
use Test::More tests => 6;

BEGIN { use_ok( "GSD" ); }

my ($version, $label, $no_items, $fptr, $file_dsc, $item_dsc, $data_ptr);

my $filename = "t/obs_das_0141";

print "# Trying gsdOpenRead...\n";

my $status = gsdOpenRead($filename,$version,$label,$no_items,$fptr,$file_dsc,
			 $item_dsc,$data_ptr);

$label =~ s/\s+$//g;

if ($status != 0) {
  BAIL_OUT( "Failed to open the file so can not continue testing" );
} else {
  is($status, 0, "Open status");
}

like($label, qr/JCMT/, "Is this a JCMT file");

my $nm = "c1long";
my ($unit, $type, $array, $item);
$status = gsdFind($file_dsc,$item_dsc,$nm,$item,$unit,$type,$array);

my $newdata;
my $call = "gsdGet$array\l$type";
$status = &{$call}($file_dsc,$item_dsc,$data_ptr,$item,$newdata);

ok($newdata - 155.479721069336  < 1e-10, "Compare C1LONG");

my $i  = 10;
$status = gsdItem($file_dsc,$item_dsc,$i,$nm,$unit,$type,$array);
$call = "gsdGet$array\l$type";
$status = &{$call}($file_dsc,$item_dsc,$data_ptr,$i,$newdata);


like($newdata, qr/BESSELIAN/, "Besselian epoch?");

$status = gsdClose($fptr,$file_dsc,$item_dsc,$data_ptr);
print "# Close status: $status\n";
is($status, 0, "Closing file");

exit;

