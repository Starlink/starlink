#!/stardev/Perl/bin/perl -w
# $Id$

#+
#  Name:
#     htx2summ.pl
#
#  Purpose:
#     Write summary file from information in .htx directory.
#
#  Usage:
#     htx2summ.pl htxdir [ latexfile ]
#
#  Description:
#     This script gathers information from various places, but
#     principally the .htx file, in order to write a summary file
#     containing the structure of a starlink document, conforming to
#     the Starlink Summary DTD.
#
#     It makes use of the htx.index file and the html files themselves.
#     The htx.index file contains almost all the information which
#     is required, but it lacks the sectioning structure (i.e. which
#     of the html files represent sections, which subsections, etc).
#     It therefore makes a cursory examination of each html file
#     and looks to see what is the first H[1-6] element it can find
#     For performance reasons, it doesn't do a proper parse of the
#     files, so errors are possible.
#
#     It also, optionally, examines the corresponding latex file.
#     The only purpose of this is to try to work out where normal
#     sectioning finishes and appendix sectioning begins (and hence
#     where to place the APPENDICES start tag in the summary).
#     If no latex file is specified it assumes all sections are
#     normal ones.  It can only guarantee to get the appendix
#     placement right if there is an explicit \xlabel command in the
#     section preceeding and in the one succeeding the \appendix command
#     in the latex.  If this is not the case, it will warn on standard
#     error that placement may be incorrect.
#
#     Finally, it reads the $STARLINK/docs/docs_lis file to find the
#     principal authors of the document.  Absence of this file, or of
#     an appropriate entry in this file, is logged to standard error,
#     but is not fatal.  In this case, an empty AUTHOR element is
#     written to the summary file.
#
#     A summary file is printed on standard output.
#
#  Command line arguments:
#     htxdir
#        This is the name of a directory containing the HTX document.
#        It must contain an htx.index file, and the html files
#        referred to therein.
#     latexfile (optional)
#        This is the name of the latex file corresponding to the
#        HTX directory.  It is only used for one thing, to try to
#        determine where normal sections finish and appendices start.
#        If this file is not specified, the summary file is written
#        as though all sections are normal sections.
#
#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils
#
#  Authors:
#     MBT: Mark Taylor (IoA, Starlink)
#     {enter_new_authors_here}
#
#  History:
#     25-AUG-1999 (MBT):
#        Initial revision.
#-

#  Safe programming.
      use strict;

#  External dependencies.
      require( "read_docslis.pl" );

#  Set usage message.
      my( $self ) = $0;
      $self =~ s%.*/%%;
      my( $usage ) = "Usage:  $self htxdir [ latexfile ]\n";

#  Set default file locations and configuration options.
      my( $dtdname ) = "-//Starlink//DTD Document Summary 0.7//EN";
      my( $starlink ) = $ENV{ 'STARLINK' } || "/star";
      my( $docslisfile ) = "$starlink/docs/docs_lis";

#  Get arguments and check usage.
      my( $htxdir ) = shift( @ARGV ) or die( $usage );
      my( $latexfile ) = shift( @ARGV ) || '';
      if ( @ARGV > 0 ) {
         die( $usage );
      }

#  Try to find out where the appendices start in the latex document.
#  The method of doing this is quite crude (to do better would require
#  rather clever parsing of the LaTeX which we wish to avoid).
#  We record the id of the last \xlabel command which occurs in the
#  latex before the (first) \appendix command and the first one which
#  occurs after it.  This could of course be confused by \newcommands
#  or putting these inside verbatims etc, and it also relies on having
#  enough explicit \xlabels.  Too bad.
      my( $appendices ) = 0;
      my( $lastid_preapp ) = '';
      my( $firstid_postapp ) = '';
      if ( $latexfile ) {
         open( LATEX, $latexfile ) or die( "Failed to open $latexfile\n" );
         my( $in_appendix ) = 0;
         while( <LATEX> ) {
            if ( /\\appendix\b/ ) {
               $in_appendix = 1;
            }
            if ( /\\xlabel{([^}]*)}/ ) {
               my( $xlabel ) = $1;
               if ( ! $in_appendix ) {
                  $lastid_preapp = namify( $xlabel );
               }
               else {
                  $firstid_postapp = namify( $xlabel );
                  last;
               }
            }
         }
         close( LATEX );
         $appendices = $in_appendix;
      }

#  Enter working directory and get its name.
      chdir( $htxdir ) or die( "Failed to enter directory $htxdir\n" );
      my( $urlprefix ) = `pwd`;
      chomp( $urlprefix );
      $urlprefix =~ s%.*/%%;
      $urlprefix .= "/";

#  Provisionally get $docid, $doctype and $docnum from the htx directory
#  name.  This should get overwritten with a value from the htx.index file,
#  but occasionally there are problems in which case we can fall back on
#  this as a guess.
      my( $docid ) = $urlprefix;
      $docid =~ s%\.htx.*%%;
      my( $doctype, $docnum ) = ( $docid =~ /^([a-z]+)([0-9]+)/ );
      if ( $docid eq 'sug' ) {
         ( $doctype, $docnum ) = ( 'sug', '' );
      }

#  Read and parse htx.index file.  This relies of course on the htx.index
#  file having the right format.
      my( $indexfile ) = "htx.index";
      open( IDX, $indexfile )
         or die( "Failed to open $indexfile in $htxdir\n" );
      my( $doctitle );
      my( $topfile, %xlabels, %sectitles, %ids );
      while ( <IDX> ) {
         my( $ctrl, $file, $arg ) = ( /^(.) (\S+) ?(.*)/ );

#  Top level HTML file and title of document.
         if ( $ctrl eq 'T' ) {
            $doctitle = $arg;
            $topfile = $file;
            if ( $topfile =~ /^([a-z]+)([0-9]+)/ ) {
               ( $doctype, $docnum ) = ( $1, $2 );
               $docid = lc( $doctype ) . $docnum;
            }
         }

#  Exported label.  In treating xlabels, we take care not
#  to repeat them, since IDs have to be unique in SGML; we keep track
#  of which have been used so far using the %ids hash.
#  A multiply-defined \xlabel in the latex source is in any case a
#  bug, so that disregarding subsequent occurrences is certainly the
#  right thing to do.  However, some manipulation of the \xlabel
#  argument (using the namify() function) is done to yield an ID value,
#  since there are restrictions on what SGML likes for IDs.  This may
#  map two different \xlabel arguments to the same ID value (in
#  particular case is folded) which would have the unfortunate effect
#  of ignoring legitimately different \xlabels.
         elsif ( $ctrl eq '<' && $arg && $file =~ /^node\d+.html/ ) {
            my( $idkey ) = namify( $arg );
            if ( $ids{ $idkey } ) {
               starwarn( "Ignoring duplicated xlabel `$arg'." );
            }
            else {
               $ids{ $idkey } = 1;
               push( @{ $xlabels{ $file } }, $idkey );
               if ( uc( $arg ) ne uc( $idkey ) ) {
                  starwarn( "Changed xlabel `$arg' to `$idkey'." );
               }
            }
         }

#  (Sub)*section title.  We can only make sense of this if the filename
#  is of the expected format ('node123.html'), since otherwise it is
#  impossible to place the sections in order.  In this case (which will
#  not result from normal use of latex2html) we just have to ignore
#  the file, and warn the user accordingly.
         elsif ( $ctrl eq 't' && $arg ) {

#  Work round rare bug in latex2html output.
            $arg =~ s/<[a-z]+>//ig;
            if ( $file =~ /^node\d+.html/ ) {
               $sectitles{ $file } = $arg;
            }
            elsif ( $file =~ /footnode/ || $file eq "$docid.html" ) {
            }
            else {
               starwarn( "HTML file `$file' in $indexfile ",
                         " not of form `node123.html' - ignored." );
            }
         }
      }
      close( IDX );

#  Read docs_lis file to get author information (if the file cannot
#  be read don't cause a fatal error).
      my( $authors );
      my( $authinit );
      my( $r_docs, $r_authors ) = eval { read_docslis( $docslisfile ) };
      if ( $@ ) {
         starwarn( "Failed to open `$docslisfile' - authors unknown." );
      }
      else {
         $authinit = $r_docs->{ $docid }->{ 'authinit' };
         if ( $authinit ) {
            my( $authkey ) = uc( $authinit );
            my( $etal ) = $authkey ne $authinit;
            my( %authatts ) = %{ $r_authors->{ $authkey } };
            my( $authname ) = $authatts{ 'name' };
            my( $authsite ) = $authatts{ 'site' };
            $authors = "<author id=$authkey affiliation='$authsite'>"
                     . $authname . "</author>\n";
            if ( $etal ) {
               $authors .= "<otherauthors><author>others</author>\n";
            }
         }
         else {
            starwarn( "Failed to find authors from $docslisfile." );
         }
      }

#  If we have failed to get proper author information insert a dummy
#  element to keep the DTD happy.
      $authors ||= "<author></author>\n";

#  Output initial information.
      print(
         "<!DOCTYPE documentsummary PUBLIC '$dtdname'>\n",
         "<documentsummary urlpath='$urlprefix$topfile' ",
                          "urllinkpolicy='explicit'>\n",
         "<docinfo>\n",
         "<title>$doctitle</title>\n",
         "<authorlist>$authors</authorlist>\n",
         "<docnumber documenttype='$doctype'>$docnum</docnumber>\n",
         "</docinfo>\n\n",
         "<docbody>\n"
      );

#  Go through each html file.  Make sure we do them in the right order
#  (node1.html, ..., node9.html, node10.html, ...).
      my( $app_now ) = 0;
      my( $app_next ) = 0;
      my( $app_done ) = 0;
      my( $file );
      my( $lastel ) = '';
      my( @files ) =
         sort( { ( $a =~ /node(\d+)/ )[ 0 ] <=> ( $b =~ /node(\d+)/ )[ 0 ] }
               keys( %sectitles ) );
      foreach $file ( @files ) {

#  Get title and xlabels.
         my( $title ) = $sectitles{ $file };
         my( @xlabels ) = $xlabels{ $file } ? @{ $xlabels{ $file } } : ( );
         my( $element );

#  Abstract is treated specially, since it's not numbered.
         if ( $title =~ /^\s*abstract\s*$/i ) {
            $element = 'abstract';
         }

#  Certain sections probably shouldn't be there and are ignored.
         elsif ( $title =~ /^\s*(contents|revision history)\s*/i
              || $title =~ /^\s*(list of tables|list of figures)\s*$/i ) {
            $element = '';
         }
         else {

#  Open file and read it for long enough to find out the level of the
#  first heading (H[1-6]) element.
#  The pattern used here is not guaranteed to match all or only heading
#  start tags but it matches actual star2html behaviour and is probably
#  adequate unless star2html output changes.
            open( HTML, $file ) or die( "Failed to open file $htxdir/$file\n" );
            my( $level );
            while( <HTML> ) {
               if ( /<H([1-6])>/ ) {
                  $level = $1;
                  last;
               }
            }
            close( HTML );

#  Map html heading level to summary DTD element gi.  Levels greater
#  than 3 are ignored; an H4 could be translated into a subsubsubsect
#  but since they correspond to \paragraph commands in the latex, which
#  aren't really numbered anyway, this is probably going to cause
#  more trouble than it's worth.
            $element = ( $level == 1 ) && 'sect'
                    || ( $level == 2 ) && 'subsect'
                    || ( $level == 3 ) && 'subsubsect'
                    || '';
         }

#  Appendix processing, only required if we know appendices are present.
         if ( $appendices ) {

#  Find out if we should output an APPENDICES element before the next
#  SECT element (including this one).
            $app_now ||=
               grep( ( $_ eq $firstid_postapp ), @xlabels );

#  If it looks like it's time to output an APPENDICES element, then do so.
#  Warn if indicators are not in agreement (which will happen if there
#  are not enough explicit \xlabel commands in the latex to tie down
#  which sections it should happen between).
            if ( $element eq 'sect' ) {
               if ( ( $app_now || $app_next ) && ! $app_done ) {
                  print( "<appendices>\n" );
                  $app_done = 1;
                  if ( ! $app_now || ! $app_next ) {
                     starwarn( "APPENDICES element ",
                               "may be inserted at the wrong place." );
                  }
               }
            }

#  Find out if we should output an APPENDICES element before the next
#  SECT element (excluding this one).
            $app_next ||= grep( ( $_ eq $lastid_preapp ), @xlabels );
         }

         if ( $element =~ /sect/ ) {

#  If we are about to output a SUBSUBSECT and the last element
#  was a SECT, then we need to interpolate a dummy SUBSECT element.
            if ( $lastel eq 'sect' && $element eq 'subsubsect' ) {
               print( "<subsect><title>\n" );
            }

#  Output tag for (sub)*section.
            print( "<$element" );
            if ( @xlabels ) {
               my( $xlabel ) = shift( @xlabels );
               print( " id='$xlabel'",
                      " export",
                      " urlpath='$urlprefix$file'" );
            }
            print( ">" );
            print( "<title>$title</title>" );
            print( "\n" );

#  If there are additional xlabels in this file, output these as LABEL
#  elements.
            my( $xlabel );
            foreach $xlabel ( @xlabels ) {
               print( "<label id='$xlabel' export ",
                      "urlpath='$urlprefix$file'>\n" );
            }
         }
         elsif ( $element eq 'abstract' && $lastel !~ /sect/i ) {
            print( "<abstract></abstract>\n" );
         }
         $lastel = $element;
      }

#  Warn if we have not inserted an APPENDICES element but should have done.
      if ( $appendices && ! $app_done ) {
         starwarn( "APPENDICES element not inserted." );
      }

#  Finish off document.
      print( "</docbody>\n",
             "</documentsummary>\n" );



########################################################################
   sub starwarn {
      print( STDERR $docid, ":\t", @_, "\n" );
   }

   sub namify {
#+
#  Turns a string into a legal SGML name (which can be used as an ID value).
#-
      my( $id ) = @_;
      $id =~ s/^([^a-zA-Z])/X-$1/;         #  Cannot start with a number
      $id =~ s/[^a-zA-Z0-9\.\-\_]/./g;     #  Cannot contain certain characters
      $id = uc( $id );                     #  Fold to upper case
      return( $id );
   }


