#!/star/Perl/bin/perl
   while( $line = <STDIN> ) {
      chomp( $line );
      if( $line =~ /^\/\*(.*)\*\/\s*$/ ) {
         print "\n/*";
         $len = 0;
         foreach $word ( split '\s+', $1 ) {
            $len += length( $word ) + 1;
            if( $len > 70 ) {
               print "\n   ";
               $len = length( $word ) + 1;
            }
            print " $word";
         }
         print " */\n";

      } else {
         print "$line\n";
      }

   }

