#!/star/Perl/bin/perl -w
# $Id$

#+
#  Name:
#     minsumm.pl
#
#  Purpose:
#     Write minimal catalogue entries for flat text Starlink documents.
#
#  Description:
#     This program generates a minimal SGML summary file for each document
#     entry in the docs_lis file (which usually lives in /star/docs).
#     The resulting summaries contain no information concerning the
#     internal structure of the documents (i.e. the DOCBODY element is
#     empty), but do conform to the Starlink 'Summary' DTD and therefore
#     are sufficient for use in defining SUBDOC entities within Starlink
#     documents which need to refer to them from a DOCXREF element.
#     Because there is no internal structure in the summaries, such
#     DOCXREF references can only refer to the document as a whole, i.e.
#     will have to leave the LOC attribute #implied.
#
#     The program writes summary files into the the current directory,
#     with filenames like 'sun123.summary'.
#     By default it only writes summaries for documents which do not
#     already have summary files in this directory.  If --overwrite
#     is set however, it can be forced to write such files always.
#
#  Arguments:
#     None.
#
#  Flags:
#     --overwrite
#        If this flag is given, then summary files will be written for
#        every document referred to in the docs_lis file.  Otherwise
#        (the default) only those files which do not have existing
#        .summary files will be written.
#
#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#
#  History:
#     25-AUG-1999 (MBT):
#        Initial version.
#-

#  Safe programming.
   use strict;

#  External dependencies.
   require( "read_docslis.pl" );

#  Set script name.
   my( $self ) = $0;
   $self =~ s%.*/%%;

#  Set up default file locations and configuration options.
   my( $overwrite ) = 0;
   my( $verbose ) = 1;
   my( $starlink ) = $ENV{ 'STARLINK' } || '/star';
   my( $docslisfile ) = "$starlink/docs/docs_lis";
   my( $dtdname ) = "-//Starlink//DTD Document Summary 0.7//EN";

#  Read flags.
   while ( $_ = shift( @ARGV ) ) {
      if ( /^--overwrite/ ) {
         $overwrite = 1;
      }
   }

#  Read the docs_lis file.
   my( $r_docs, $r_authors ) = read_docslis( $docslisfile );
   print( STDERR "$self: Document info read from `$docslisfile'\n" );
   my( %docs ) = %{ $r_docs };
   my( %authors ) = %{ $r_authors };

#  Write summaries.  Loop through each document we found in docs_lis.
   my( $doc, %docatts );
   for $doc ( sort( keys( %docs ) ) ) {

#  Get the information about each document.
      %docatts = %{ $docs{ $doc } };
      $doc =~ /([a-z]*)(\d*)/i;
      my( $doctype ) = lc( $1 );
      my( $docnum ) = $2;
      my( $version, $date, $authinit, $pages, $title )
         = @docatts{ qw( version date authinit pages title ) };
      my( $docnumber ) = $docnum;
      $docnumber = "" if ( $doctype eq 'SUG' );
      my( $authkey ) = uc( $authinit );
      my( $etal ) = $authkey ne $authinit;
      my( %authatts ) = %{$authors{ $authkey }};
      my( $authname, $authsite ) = @authatts{ qw( name site ) };

#  Open summary file and write the information to it.
      my( $summfile ) = "$doc.summary";
      next if ( -e $summfile && ! $overwrite );
      print( "   $doc\n" ) if ( $verbose );
      open( SUMM, ">$summfile" )
         or die( "Failed to open $summfile for writing.\n" );
      print( SUMM join( "\n",
         "<!DOCTYPE documentsummary PUBLIC '$dtdname'>",
         "<documentsummary urlpath='$doc.htx/$doc.html' "
             . "urllinkpolicy='explicit'>",
         "<docinfo>",
         "<title>$title</title>",
         "<authorlist>",
         "<author id=$authkey affiliation='$authsite'>$authname</author>"
             . ( $etal ? "\n<otherauthors>\n<author>others</author>\n</otherauthors>" : "" ),
         "</authorlist>",
         "<docnumber documenttype=$doctype>$docnumber</docnumber>",
         "</docinfo>\n",
         "<docbody>",
         "</docbody>",
         "</documentsummary>",
         "",
      ) );
      close( SUMM );
   }


# $Id$
