#! /usr/bin/perl -w

$keepgoing = 1;
$| = 1;

open (LOGFILE, ">dummy-slave.log")
  || die "Can't open dummy-slave.log to write";

while ($keepgoing) {
    $line = <STDIN>;
    defined ($line) || die "Unexpected EOF";

    $line =~ s/[ \t\r\n]*$//;

    print LOGFILE ">$line\n";

    @words = split (' ',uc($line));

    if ($words[0] =~ /^QUIT/) {
	$keepgoing = 0;
	$response = "200 quitting";
    } elsif ($words[0] =~ /^AST/) {
	print "300 Bring it on...\r\n";
	while (defined($line = <STDIN>) && $line !~ /^\./) {
	    $line =~ s{[ \t\r\n]*$}{};
	    print LOGFILE ">>$line\n";
	}
	$response = "200 Gottit!";
    } else {
	$response = "200 OK";
    }

    print $response, "\r\n";
}

exit 0;
