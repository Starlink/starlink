BEGIN { $| = 1; print "1..1\n"; }
END {print "not ok 1\n" unless $loaded;}
use Starlink::Config;
$loaded = 1;
print "ok 1\n";

foreach (keys %StarConfig) {
  print "$_\t=> $StarConfig{$_}\n"; 
}
