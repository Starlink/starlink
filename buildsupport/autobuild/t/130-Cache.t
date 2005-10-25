# $Id$

BEGIN { $| = 1; print "1..5\n"; }
END { print "not ok 1\n" unless $loaded; }

use File::Find;
use File::Temp;
use Test::AutoBuild::Cache;

$loaded = 1;
print "ok 1\n";

my $tempdir = File::Temp::tempdir();
my $tempdir2 = File::Temp::tempdir();
my $file1 = "$tempdir/file1";
my $file2 = "$tempdir/file2";
my $timestamp_file = "$tempdir2/timestamp";
my $timestamp = time();

my $cache = Test::AutoBuild::Cache->new(cache_root => $tempdir);

open (FILE1, "> $file1");
print FILE1 "foo";
close FILE1;

open (TIMESTAMP, "> $timestamp_file");
print TIMESTAMP "bar";
close TIMESTAMP;

my $newer;

$newer = Test::AutoBuild::Cache::newer_than_timestamp($timestamp_file, $tempdir);
if ( $newer == 0 ) {
    print "ok 2\n";
} else {
    print "not ok 2\n";
}

sleep 1;

open (FILE2, "> $file2");
print FILE2 "bar";
close FILE2;

$newer = Test::AutoBuild::Cache::newer_than_timestamp($timestamp_file, $tempdir);
if ( $newer == 1 ) {
    print "ok 3\n";
} else {
    print "not ok 3\n";
}

$cache->write_timestamp(module_name, $timestamp);
my $read_timestamp = $cache->read_timestamp(module_name);

if ( $timestamp == $read_timestamp ) {
    print "ok 4\n";
} else {
    print "cache_root: $tempdir\n";
    print "timestamp: $timestamp\n";
    print "read_timestamp: $read_timestamp\n";
    print "not ok 4\n";
}

$cache->build_log(module_name, "log");
if ( "log" eq $cache->build_log(module_name) ) {
    print "ok 5\n";
} else {
    print "not ok 5\n";
}

sub delete_directory {
    my $dir = shift;

    if (! -d $dir) {
        return 0;
    }

    find ( sub { -f $_ && unlink }, $dir);
    finddepth ( sub { -d $_ && rmdir }, $dir);

    rmdir $dir || die "can't rmdir $dir: $!";

    return 1;
}

delete_directory ($tempdir);
delete_directory ($tempdir2);

