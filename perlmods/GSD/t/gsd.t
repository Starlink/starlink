#!perl -w

# Test the pure library interface
use strict;
no strict 'refs';
use Test;

BEGIN { plan tests => 6 }

use GSD;

ok(1);

my ($version, $label, $no_items, $fptr, $file_dsc, $item_dsc, $data_ptr);

my $filename = "t/obs_das_0141";

print "# Trying gsdOpenRead...\n";

my $status = gsdOpenRead($filename,$version,$label,$no_items,$fptr,$file_dsc,
			 $item_dsc,$data_ptr);

$label =~ s/\s+$//g;

if ($status != 0) {
  ok(0);
  exit;
} else {
  ok(1);
}

ok($label =~ /JCMT/);

#($label =~ /JCMT/) && (print "ok\n") || (print "not ok\n");


my $nm = "c1long";
my ($unit, $type, $array, $item);
$status = gsdFind($file_dsc,$item_dsc,$nm,$item,$unit,$type,$array);

my $newdata;
my $call = "gsdGet$array\l$type";
$status = &{$call}($file_dsc,$item_dsc,$data_ptr,$item,$newdata);

ok($newdata - 155.479721069336  < 1e-10);

my $i  = 10;
$status = gsdItem($file_dsc,$item_dsc,$i,$nm,$unit,$type,$array);
$call = "gsdGet$array\l$type";
$status = &{$call}($file_dsc,$item_dsc,$data_ptr,$i,$newdata);


ok($newdata =~ /BESSELIAN/);

$status = gsdClose($fptr,$file_dsc,$item_dsc,$data_ptr);
print "# Close status: $status\n";
ok($status == 0);

exit;

