# $Id$

BEGIN { $| = 1; print "1..2\n"; }
END { print "not ok 1\n" unless $loaded; }

use File::Find;
use File::Temp;
use Test::AutoBuild::Lib;

$loaded = 1;
use strict;

print "ok 1\n";

my $modules;
$modules->{test1} = Test::AutoBuild::Module->new(name => "test1", label => 'Test1', paths => '', repository => '');
$modules->{test2} = Test::AutoBuild::Module->new(name => "test2", label => 'Test2', paths => '', repository => '');
$modules->{test3} = Test::AutoBuild::Module->new(name => "test3", label => 'Test3', paths => '', repository => '');

my $order = Test::AutoBuild::Lib::sort_modules($modules);
my @mod_names = keys %{$modules};
if ($#{$order} == $#mod_names) {
    print "ok 2\n";
} else {
    print "expected length: " . scalar(@mod_names) . "\n";
    print "got length: " . scalar(@{$order}) . "\n";

    print "not ok 2\n";
}
