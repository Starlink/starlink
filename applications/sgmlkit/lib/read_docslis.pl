   sub read_docslis {
# $Id$
#+
#  Name:
#     read_docslis
#
#  Invocation:
#     ( $r_docs, $r_authors ) = read_docslis( $docslisfile )
#
#  Purpose:
#     Read and parse the docs_lis file.
#
#  Description:
#     This routine reads the docs_lis file which usually lives in
#     /star/docs.  It obviously relies heavily on that file having the
#     right format.  It returns data structures containing the useful
#     information from that file.
#
#  Arguments:
#     $docslisfile = string:
#        Pathname of the docs_lis file.
#
#  Return values:
#     \%docs = ref to hash:
#        The keys of the hash are document names in the form 'sun666' etc.
#        The values are refs to a hash with keys 'version', 'date',
#        'authinit', 'pages' and 'title'.  As in the docs_lis file,
#        author initials (authinit) entries in lower case indicate that
#        the named author is first, rather than sole, author.
#     \%authors = ref to hash:
#        The keys of the hash are the author's initials, as per the
#        'authint' entries of the docs hash, in upper case.  The values
#        are refs to a hash with keys 'name' and 'site'.
#-
      my( $docslisfile ) = @_;
      open( DOCSLIS, $docslisfile ) or die( "Failed to open $docslisfile\n" );
      my( %docs, $authors );

      while ( <DOCSLIS> ) {
         next unless ( /^([A-Z]+)/ );
         my( $heading ) = $1;
         if ( $heading =~ /^(SC|SG|SGP|SSN|SUN|SUG)$/ ) {
            while ( <DOCSLIS> ) {
               last unless ( /^(\d\d\d|SUG)\.(\d\d) \s+
                               (\d\d)\.(\d\d)\.(\d\d) \s+
                               ([A-Za-z]+) \s+
                               (\d+) \s+
                               (.*) $/x );
               my( $num, $vers, $y, $m, $d, $init, $pg, $title )
                  = ( $1, $2, $3, $4, $5, $6, $7, $8 );

#  Normalize the format of document number.
               if ( $num eq 'SUG' ) {
                  $num = '';
               }
               else {
                  $num += 0;   # remove leading 0's
               }

#  Some adhoc tidying of the title.
               $title =~ s/[,:][\sA-Z]*$//;
               $title =~ s/\s*[vV]?\d\.\d.*//;

               my( $doc ) = lc( $heading ) . $num;
               $docs{ $doc } = { version => $vers,
                                 date => "$y.$d.$m",
                                 authinit => $init,
                                 pages => $pg,
                                 title => $title,
                               };
            }
         }
         elsif ( $heading eq 'AUTHORS' ) {
            while ( <DOCSLIS> ) {
               next if ( /\(Site code\)/ );
               last unless ( /\s* ([A-Z]+) \s+
                              (.*?) \s*
                              \((.*)\) /x );
               my( $init, $name, $site ) = ( $1, $2, $3 );
               $authors{ $init } = { name => $name, site => $site };
               $site =~ s/-\s*left\s*$//;
            }
         }
      }
      close( DOCSLIS );
      return( \%docs, \%authors );
   }

   1;

# $Id$
