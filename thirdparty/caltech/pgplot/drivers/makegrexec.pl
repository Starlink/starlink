#!/usr/bin/perl

# Perl equivalent of grexec.awk script. Does not use drivers.list.
# Tries to build up grexec.f file from the list of fortran files being
# used for the build

use strict;

my @files = @ARGV;

# List of driver variants. Each ??driv file is called either once
# (zero) or as a variant for eg landscape or portrait
my %counts = (
	      BCDRIV => 0,
	      CADRIV => 0,
	      CCDRIV => 0,
	      CGDRIV => 2,
	      CWDRIV => 0,
	      EPDRIV => 0,
	      EXDRIV => 2,
	      GCDRIV => 0,
	      GIDRIV => 2,
	      GLDRIV => 2,
	      GODRIV => 0,
	      GVDRIV => 0,
	      HGDRIV => 0,
	      HIDRIV => 0,
	      HJDRIV => 0,
	      HPDRIV => 0,
	      LADRIV => 0,
	      LJDRIV => 0,
	      LSDRIV => 2,
	      LNDRIV => 0,
	      LVDRIV => 0,
	      LXDRIV => 0,
	      MFDRIV => 0,
	      NEDRIV => 0,
	      NUDRIV => 0,
	      PGDRIV => 0,
	      PNDRIV => 2,
	      PPDRIV => 2,
	      PSDRIV => 4,
	      PXDRIV => 0,
	      QMDRIV => 2,
	      TFDRIV => 0,
	      TODRIV => 0,
	      TTDRIV => 10,
	      TXDRIV => 0,
	      VADRIV => 0,
	      VBDRIV => 0,
	      VTDRIV => 0,
	      WDDRIV => 2,
	      WSDRIV => 0,
	      X2DRIV => 0,
	      XWDRIV => 2,
	      ZEDRIV => 0,
	      LHDRIV => 0,
	      MSDRIV => 0,
	      SSDRIV => 0,
	      ACDRIV => 0,
	      XMDRIV => 0,
	      XADRIV => 0,
	      TKDRIV => 0,
	      RVDRIV => 0,
	     );

# First need to find all the driv files
my @entries;
my $ndev = 0;
for my $file (@files) {
  if ($file =~ /^(.*driv)\./) {
    my $thisdriv = uc($1);
    if (exists $counts{$thisdriv}) {
      print "Found driver $thisdriv\n";
      push(@entries, $thisdriv);
      my $nvar = $counts{$thisdriv};
      $nvar++ if $nvar == 0;  # 0 means one variant
      $ndev += $nvar;
    }
  }
}


# Standard header
my @lines = <DATA>;
push(@lines, "      PARAMETER (NDEV=$ndev)\n");
push(@lines, "      CHARACTER*10 MSG\n");
push(@lines, "C----\n");
push(@lines, "      IF (IDEV .EQ. 0) THEN\n");
push(@lines, "          RBUF(1) = NDEV\n");
push(@lines, "          NBUF = 1\n");


# Now go through the drivers
my $devnum = 0;
for my $drv (@entries) {
  my $max = $counts{$drv};
  $max = 1 if $max == 0;
  for my $i (1..$max) {
    $devnum++;
    my $index = ",$i";
    $index = '' if $max == 1;
    push(@lines, "      ELSE IF (IDEV .EQ. $devnum) THEN\n");
    push(@lines, "        CALL $drv(IFUNC,RBUF,NBUF,CHR,LCHR$index)\n");
  }
}

push(@lines, "      ELSE\n");
push(@lines, "        WRITE(MSG,'(I10)') IDEV\n");
push(@lines, "        CALL GRWARN('Unknown device code in GREXEC:'//MSG)\n");
push(@lines, "      END IF\n");
push(@lines, "      RETURN\n");
push(@lines, "      END\n");


# Now write the grexec.f
open(my $GR, ">grexec.f") or die "Error opening file grexec.f for write: $!";
print $GR @lines;
close($GR);

__DATA__
C*GREXEC -- PGPLOT device handler dispatch routine
C+
      SUBROUTINE GREXEC(IDEV,IFUNC,RBUF,NBUF,CHR,LCHR)
      INTEGER IDEV, IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C---
      INTEGER NDEV
