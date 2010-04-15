#!/usr/bin/perl

open INFILE, "documented_files";
while( $file =<INFILE> ){
   chomp( $file );
   $files = $files." ".$file;
}
close INFILE;

foreach $file (split /\s+/, $files) {
   if( ! $file =~ /~$/ ) {
      open INFILE, $file;
      $purpose = "";
      $state = 0;
      while( $line =<INFILE> ){
         chomp( $line );

         if( $state == 0 ) {
            if( $line =~ /^\* +Name: *$/ ) {
               $state = 1;
            }

         } elsif( $state == 1 ) {
            if( $line =~ /^\* +(\w.*\S) *$/ ) {
               $name = $1;
               $state = 2;
            } else {
               printf "Cannot find the \"Name:\" prologue section in $ARGV[0]/$file !!!!\n";
               exit;
            }

         } elsif( $state == 2 ) {
            if( $line =~ /^\* +Purpose: *$/ ) {
               $state = 3;
            }

         } elsif( $state == 3 ) {
            if( $line =~ /^\* +(\w.*\S) *$/ ) {
               $purpose = "$purpose$1 ";
            } else {
               $state = 4;
               last;
            }
         }
      }

      if( $state != 4 ) {
         printf "Cannot find the \"Purpose:\" prologue section in $ARGV[0]/$file !!!!\n";
         exit;
      }

      $name = uc( $name );
      $purps{$name} = $purpose;

      close $INFILE;
   }
}


foreach $key (keys %purps) {
   $text = "\\noteroutine{$key}{\n   $purps{$key}\n}\n";
   $text =~ s/_/\\_/g;
   print "$text\n";
}


