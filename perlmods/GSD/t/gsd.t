#!perl -w

use GSD;

my ($filename, $status, $version, $label, $no_items, $fptr, $file_dsc);
my ($item_dsc, $data_ptr, $nm, $item, $unit, $array, $call, $i);
my ($newdata, $type);

print "1..3\n";   # 3 tests

$filename = "t/obs_das_0141";

print "Trying gsdOpenRead...\n";

$status = gsdOpenRead($filename,$version,$label,$no_items,$fptr,$file_dsc,
	$item_dsc,$data_ptr);

$label =~ s/\s+$//g;


($status != 0) && do {print "not ok\n"; exit;};

($label =~ /JCMT/) && (print "ok\n") || (print "not ok\n");


$nm = "c1long";
$status = gsdFind($file_dsc,$item_dsc,$nm,$item,$unit,$type,$array);

$call = "gsdGet$array\l$type";
$status = &{$call}($file_dsc,$item_dsc,$data_ptr,$item,$newdata);

($newdata - 155.479721069336  < 1e-10) && (print "ok\n") 
	|| (print "not ok\n");

$i  = 10;
$status = gsdItem($file_dsc,$item_dsc,$i,$nm,$unit,$type,$array);
$call = "gsdGet$array\l$type";
$status = &{$call}($file_dsc,$item_dsc,$data_ptr,$i,$newdata);

($newdata =~ /BESSELIAN/) && (print "ok\n") || (print "not ok\n");

$status = gsdClose($fptr,$file_dsc,$item_dsc,$data_ptr);

exit;

