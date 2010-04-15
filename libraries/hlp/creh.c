#include "hlpsys.h"
#include <string.h>
#include <ctype.h>
int hlpCreh ( int ( * nametr ) ( int, char*, int, char* ),
                                                 char *source, char *lib )
/*
**  - - - - - - - -
**   h l p C r e h
**  - - - - - - - -
**
**  Translate file of HELP source into direct-access HELP library file.
**
**  Given:
**     nametr    func    user-supplied name translation routine (see note)
**     *source   char    filename for HELP source
**     *lib      char    filename for HELP library
**
**  Returned (function value):
**               int     status: 0 = OK
**            hlp_CREATION_FAILURE = fail
**
**  The error codes are defined in the hlpsys.h #include file.
**
**  Note:
**      The user-supplied name translation function nametr has
**      arguments command, string1, lstring2, string2 and returns
**      a status:
**
**      * command (given) is an integer which the HELP system always
**        sets to zero;  the application may use other values of
**        command, for example to perform initializations or enquiries;
**
**      * string1 (given) is, for command=0, the HELP library name to
**        be translated;  this name can be is the value of the file
**        argument to the present routine (identifying the root library)
**        or the name following the '@' symbol in a HELP source file
**        (identifying another library grafted onto the current
**        library);
**
**      * lstring2 (given) is, for command=0, the length of string2
**        (defined next);
**
**      * string2 (returned) is, for command=0, the translation of
**        string2 into a filename for use in fopen calls;
**
**      * the nametr function return is a status, zero indicating
**        success.
**
**  FORMAT OF INPUT FILE:
**
**     Formatted alphanumeric records;  the maximum length is defined in
**     the present module.  Non-printing characters are converted to
**     spaces.
**
**     There are four sorts of record;  COMMENT, KEYWORD, TEXT and END
**     records.
**
**     COMMENT records have '!' as their first character, and are
**     ignored.
**
**     KEYWORD records are of two types: with and without a file pointer.
**     They consist of several fields separated by spaces, but without
**     leading spaces.
**
**     Those without a file pointer begin with a single numeric digit in
**     the range 0-9, indicating the hierarchical level, followed by the
**     keyword.  The keyword is ended by any embedded or trailing spaces.
**     The system is case-insensitive.  A new level must be no more than
**     1 greater than the current level.  The first keyword in the file
**     is the only one allowed to have that level number, which by
**     convention will be zero but need not be.  This top-level keyword
**     will never be matched against an input topic name and is thus
**     arbitrary;  it should be something that describes the whole HELP
**     library.  A keyword cannot be all spaces.
**
**     The file pointer variant of a keyword record is just the same as
**     the variant without a pointer, except that it begins with the name
**     of a HELP library to be attached at that point, prefixed by an '@'
**     character.  This file pointer, complete with the '@' prefix is
**     embedded in the direct-access HELP library produced by the present
**     routine, and is accessed at run-time.  Thus, a level 2 topic for
**     the keyword EXAMPLE contained in the HELP library LIB_EXAMPLE
**     would require a HELP source record like '@LIB_EXAMPLE 2 EXAMPLE'.
**     Note that LIB_EXAMPLE is a direct-access HELP library, not a file
**     of HELP source.  In summary, the '@' pointer allows branches of
**     HELP tree to be independently maintained and copied, and for
**     different versions to be substituted without rebuilding the whole
**     HELP tree.
**
**     TEXT records are just plain text, and apply to the keyword
**     immediately preceding.  Any that precede the first keyword are
**     ignored.  Blank lines before and after each group of TEXT records
**     are ignored.  Note that the rules for recognizing other sorts of
**     record mean that text records cannot begin with '0'-'9', '@' or
**     'END' etc.
**
**     The optional END record consists of the three characters 'END', in
**     any mixture of upper/lowercase.
**
**  FORMAT OF OUTPUT FILE:
**
**     Direct access, formatted.
**
**     Consists of logical records terminated with the null character.
**     There are no trailing spaces.
**
**     The first record is the HEADER record.  This gives the size of the
**     file, in characters, as a decimal number in printf %12.12ld format.
**
**     The next set of records is an INDEX;  each record corresponds to
**     one keyword.
**
**     The index is followed by an empty record.
**
**     The next set of records are the TEXT lines interspersed with
**     versions of the KEYWORD records consisting of the effective
**     level number and keyword proper.  Trailing spaces are eliminated.
**
**     The file is terminated by an empty record.
**
**     Each INDEX record is formatted as follows:
**
**       'ttttttttt ddddddddd aaaaaaaaa [@pppp....] n kkkk....'
**
**     The fields are separated by spaces.  There are no trailing blanks.
**
**     The fields ttttttttt, ddddddddd and aaaaaaaaa are all character
**     addresses in %9.9ld format.  The first character in the file has
**     address zero.  The address ttttttttt is the address of the
**     corresponding keyword record in the TEXT section of the file.
**
**     The addresses ddddddddd and aaaaaaaaa point to further index
**     entries.  The address ddddddddd ("down") is the entry which is
**     next in sequence when conducting a recursive search down each
**     branch of the tree.  The address aaaaaaaaa ("along") is the entry
**     which is next in sequence when searching for all subtopics at a
**     given level.
**
**     The optional field @pppp.... is present when a file pointer was
**     supplied at this point in the HELP source.  The string pppp.... is
**     the name of the direct-access HELP library which is logically
**     attached to the HELP tree at this point.
**
**     The field n is the level number, a single numeric character in the
**     range 0-9.  It is automatically adjusted so that all HELP
**     libraries start at level 0.
**
**     The field kkkk.... is the keyword, in uppercase, and in length up
**     to 130 characters.
**
**     Each TEXT record is the HELP text, exactly as supplied except
**     that keywords are placed following the level number with a single
**     intervening space and text following a keyword is ignored.
**
**  Called:  hlpFopr, hlpHinit, hlpHopenw, hlpHdwrit, hlpLength,
**           hlpHclose, hlpErrmes, hlpTrim
**
**  Last revision:   16 June 2000 (PTW)
**                   7 January 2006 (TIMJ)
**
**  Copyright 2000 P.T.Wallace.  All rights reserved.
*/

#define TRUE 1
#define FALSE 0

/* Maximum keyword depth. */
#define LEVMAX 9

/* Length of header record (not including '\0'). */
#define NHEAD 12

/* Number of digits in formatted addresses: NDIGS = 9   */

/* Overheads in index:                                  */
/* room for 3 addresses, 2 spaces and 1 EOS = 3*NDIGS+3 */
#define NOVERH 30

/* Input buffer length. */
#define LIN 132

/* Output buffer length: LIN+NDIGS */
#define LOUT 141
{
/* File pointer for reading HELP source. */
   FILE *fps;

/* Certain larger arrays are declared static to avoid stack      */
/* overflow on some small systems.  The static specifier can     */
/* be removed for systems where stack overflow is not a problem. */

/* Header record. */
   char header [ NHEAD + 1 ];

/* Buffers. */
   char iobuf [ LIN ], outbuf [ LOUT ];

/* Character counts for each chapter (=level) of the index. */
   int ncxlev [ LEVMAX + 1 ];

/* Start address for each chapter. */
   long icxlev [ LEVMAX + 1 ];

/* Most recent index entries at the given level and their addresses. */
   static char jumpob [ LEVMAX + 1 ] [ LOUT ] ;
   long jumpad [ LEVMAX + 1 ];

/* Last aindex entry, its level and its address (0=none). */
   char lastob [ LOUT ];
   int lastlv;
   long lastad;

/* State:  0 = expecting first keyword          */
/*         1 = current record is keyword        */
/*         2 = expecting new batch of HELP text */
/*         3 = processing HELP text             */
   int kstate;

   int jstat, more, need, ipass, levold, level, i, c1, nfp,
       levtop, npbl, ifrom, ito, l, nd, nx, k;

   long nhind, nindex, nhelp, iadr, il;

/* Initialize the HELP system. */
   hlpHinit ( lib );

/* Initialise some variables */
   nhind = 0;
   lastad = 0;
   levtop = 0;
   npbl = 0;
   lastlv = 0;

/* Open the input file for the first pass. */
   if ( hlpFopr ( nametr, source, &fps ) ) goto soperr;

/* Two passes of source file:  (1) find index size, (2) write the file. */
   for ( ipass = 1; ipass <= 2; ipass++ ) {

   /* Which pass? */
      if ( ipass == 1 ) {

      /* Pass 1: initialize the current level number. */
         levold = -1;
      } else {

      /* Pass 2: save size of index plus header plus terminators. */
         nhind = nindex + (long) ( NHEAD + 2 );

      /* Reopen input. */
         rewind ( fps );

      /* Open the output file (length allows for '\0' characters). */
         if ( ( jstat = hlpHopenw ( nametr, nhind + nhelp + 1l ) ) )
            goto loperr;

      /* Write the end-of-index record. */
         iadr = nhind - 1l;
         if ( ( jstat = hlpHdwrit( "\0", &iadr ) ) ) goto loperr;

      /* Flag no buffer waiting to be output. */
         lastad = 0l;

      /* Reset the deferred index entry addresses. */
         for ( level = 0; level <= LEVMAX; jumpad [ level++ ] = 0l );
      }

   /* Initialize the counts of index and HELP characters. */
      nindex = 0l;
      nhelp = 0l;
      for ( level = 0; level <= LEVMAX; ncxlev [ level++ ] = 0l );

   /* State = expecting first keyword. */
      kstate = 0;

   /* Read the input records. */
      more = TRUE;
      while ( more ) {

      /* Read a source record, ignoring comments. */
         need = TRUE;
         while ( need ) {
            if ( fgets ( iobuf, LIN, fps ) == NULL ) goto iend;
            if (iobuf [ 0 ] != (char) '!') need = FALSE;
         }

      /* Replace anything funny with spaces. */
         l = (int) strlen ( iobuf );
         for ( i = 0; i < l; i++ ) {
            if ( isspace ( (int) iobuf [ i ] ) )
               iobuf [ i ] = (char) ' ';
         }

      /* Look for explicit END record. */
         if ( hlpLength ( iobuf ) == 3 &&
              toupper ( (int) iobuf [ 0 ] ) == 'E' &&
              toupper ( (int) iobuf [ 1 ] ) == 'N' &&
              toupper ( (int) iobuf [ 2 ] ) == 'D' ) goto iend;

      /* Keyword or file pointer? */
         c1 = (int) iobuf [ 0 ];
         if ( ( isdigit ( c1 ) && (int) iobuf [ 1 ] == (char) ' ' ) ||
              c1 == '@' ) {

         /* Yes: count the number of characters in any file pointer      */
         /* plus the subsequent space (nfp) and then point to and pick   */
         /* up the level number, leaving i pointing to the level number. */
            nfp = 0;
            if ( c1 == '@' ) {
               while ( iobuf [nfp] != (char) ' ' ) {
                  nfp++;
                  if ( nfp > LIN - 4 ) goto badlev;
               }
               i = nfp + 1;
               c1 = (int) iobuf [ i ];
               if ( ! isdigit ( c1 ) ||
                    (int) iobuf [ i + 1 ] != ' ' ) goto badlev;
            } else {
               i = 0;
            }

         /* Get the level number. */
            level = c1 - '0';

         /* If the first keyword, note its level. */
            if ( kstate == 0 ) levtop = level;

         /* State = current record is keyword. */
            kstate = 1;

         /* Reset pending blank lines count. */
            npbl = 0;

         /* Correct the level number so that the top level is zero. */
            level -= levtop;

         /* If pass 1, validate level. */
            if ( ipass == 1 ) {
               if ( level - levold > 1 ) goto nokeyw;
               levold = level;
            }

         /* Find where the keyword starts (ifrom) and ends (ito). */
            ifrom = i + 2;
            while ( iobuf [ ifrom ] == (char) ' ' ) {
               ifrom++;
               if ( ifrom >= LIN ) goto badlev;
            }
            ito = ifrom;
            while ( iobuf [ ito ] != (char) ' ' && ito < LIN ) ito++;
            ito--;

         /* Length of data record including overheads. */
            nd = ito - ifrom + 4;

         /* Length of index record including overheads. */
            nx = NOVERH + nd;
            if ( nfp > 0 ) nx += nfp + 1;

         /* Pass 2? */
            if ( ipass == 2 ) {

            /* Yes: spacefill the output buffer. */
               for ( i = 0; i < LOUT - 1; outbuf [ i++ ] = (char) ' ' );
               outbuf [ i ] = (char) '\0';

            /* Format the effective level number. */
               c1 = '0' + level;

            /* Copy the file pointer, level and keyword into the */
            /* index record.                                     */
               i = NOVERH;
               if ( nfp > 0 ) {
                  strncpy ( outbuf + i, iobuf, nfp );
                  i += nfp + 1;
               }
               outbuf [ i ] = (char) c1;
               outbuf [ ++i ] = (char) ' ';
               strncpy ( outbuf + ++i, iobuf + ifrom, ito - ifrom + 1 );

            /* Build the data record. */
               iobuf [ 0 ] = (char) c1;
               iobuf [ 1 ] = (char) ' ';
               l = ito - ifrom + 1;
               strncpy ( iobuf + 2, iobuf + ifrom, l );
               for ( i = 2 + l; i < LIN - 1; iobuf [ i++ ] = (char) ' ' );
               iobuf [ i ] = (char) '\0';

            /* Format the "data" address for the current record. */
               sprintf ( outbuf, "%9.9ld", nhind + nhelp );
               outbuf [ 9 ] = (char) ' ';

            /* Compute the file address for the current index record. */
               iadr = icxlev [ level ] + ncxlev [ level ];

            /*
            ** The previous and current index records are as follows:
            **
            **                   previous          current
            **
            **    text            lastob           outbuf
            **    level           lastlv           level
            **    address         lastad           iadr
            **
            ** In both cases, the text contains the "data" address but
            ** neither the "down" nor the "along" addresses.  We now
            ** leave the current record for next time and proceed to
            ** process the previous index record, if any.
            */

            /* Is there a previous record? */
               if ( lastad > 0l ) {

               /* Yes: the address of the current record is the "down"   */
               /* address for the previous record: complete that record. */
                  sprintf ( lastob + 10, "%9.9ld", iadr );
                  lastob [ 19 ] = (char) ' ';

               /* Look at the list of deferred records for the current */
               /* level downwards.                                     */
                  for ( l = lastlv; l <= LEVMAX; l++ ) {

                  /* Is there a record waiting to be completed and */
                  /* output?                                       */
                     if ( jumpad [ l ] > 0 ) {

                     /* Yes: insert the "along" pointer, write the */
                     /* record and reset the list entry.           */
                        sprintf ( jumpob [ l ] + 20, "%9.9ld", lastad );
                        jumpob [ l ] [ 29 ] = (char) ' ';
                        if ( ( jstat = hlpHdwrit ( hlpTrim ( jumpob [ l ] ),
                                                 & jumpad [ l ] ) ) )
                           goto syserr;
                        jumpad [ l ] = 0l;
                     }
                  }

               /* Store the current partially-completed record and its   */
               /* future disc address, to be completed and output when   */
               /* the next entry at this or a higher level is available. */
                  strncpy ( jumpob [ lastlv ], lastob, LOUT );
                  jumpad [ lastlv ] = lastad;
               }

            /* Store the current record, level and address for next */
            /* time.                                                */
               strncpy ( lastob, outbuf, LOUT );
               lastlv = level;
               lastad = iadr;
            }

         /* Advance the characters-in-index counters. */
            nindex += nx;
            ncxlev [ level ] += nx;
         }

      /* All input records from the first keyword on. */
         if ( kstate > 0 ) {

         /* Blank record? */
            if ( ! hlpLength ( iobuf ) ) {
            /* Yes: if we are processing text, increment the pending */
            /* blank lines counter.                                  */
               if ( kstate == 3 ) npbl += 1;
            } else {

            /* Not a blank record: keyword? */
               if ( kstate != 1 ) {

              /* Not a keyword: output any pending blank lines. */
                  for ( i = 1; i <= npbl; i++ ) {
                     if ( ipass == 2 ) {
                        iadr = nhind + nhelp;
                        if ( ( jstat = hlpHdwrit ( " ", &iadr ) ) )
                           goto syserr;
                     }
                     nhelp += 2l;
                  }

               /* Reset the pending blank lines count. */
                  npbl = 0;

               /* Update the status. */
                  kstate = 3;
               }

            /* Strip trailing blanks from current record and, if */
            /* pass 2, output it.                                */
               k = hlpLength ( iobuf );
               iobuf [ k ] = (char) '\0';
               if ( ipass == 2 ) {
                  iadr = nhind + nhelp;
                  if ( ( jstat = hlpHdwrit ( iobuf, &iadr ) ) )
                     goto syserr;
               }
               nhelp += (long) ( k + 1 );
            }

         /* If we have just processed a keyword, update the status. */
            if ( kstate == 1 ) kstate = 2;
         }
         goto nextip;

      /* End of input file: stop the loop. */
iend:
         more = FALSE;

      /* Which pass? */
         if ( ipass == 1 ) {

         /* Pass 1: work out the index chapter start addresses. */
            il = (long) NHEAD + 1;
            for ( level = 0; level <= LEVMAX; level++ ) {
               icxlev [ level ] = il;
               il += ncxlev [ level ];
            }
         } else {

         /* Pass 2: output deferred index entries. */

         /* Is there a previous record? */
            if ( lastad > 0l ) {

            /* Yes: the address of the end of index is the "down"     */
            /* address for the previous record: complete that record. */
               iadr = nhind - 1;
               sprintf ( lastob + 10, "%9.9ld", iadr );
               lastob [ 19 ] = (char) ' ';

            /* Look at the list of deferred records for the current */
            /* level downwards.                                     */
               for ( l = lastlv; l <= LEVMAX; l++ ) {

               /* Is there a record waiting to be completed and output? */
                  if ( jumpad [ l ] > 0l ) {

                  /* Yes: insert the "along" pointer, write the record */
                  /* and reset the list entry.                         */
                     sprintf ( jumpob [ l ] + 20, "%9.9ld", lastad );
                     jumpob [ l ] [ 29 ] = (char) ' ';
                     if ( ( jstat = hlpHdwrit ( hlpTrim ( jumpob [ l ] ),
                                              jumpad + l ) ) ) goto syserr;
                     jumpad [ l ] = 0l;
                  }
               }

            /* Store the current partially-completed record and its     */
            /* future disc address, to be completed and output when the */
            /* next entry at this or a higher level is available.       */
               strncpy ( jumpob [ lastlv ], lastob, LOUT );
               jumpad [ lastlv ] = lastad;

            /* Look again at the list of deferred records. */
               for ( l = 0; l <= lastlv; l++ ) {

               /* Is there a record waiting to be completed and output? */
                  if ( jumpad [ l ] > 0l ) {

                  /* Yes: insert the "along" pointer and write the */
                  /* record.                                       */
                     sprintf ( jumpob [ l ] + 20, "%9.9ld", iadr );
                     jumpob [ l ] [ 29 ] = (char) ' ';
                     if ( ( jstat = hlpHdwrit ( hlpTrim ( jumpob [ l ] ),
                                              jumpad + l ) ) ) goto syserr;
                  }
               }
            }
         }

      /* Next input record if any. */
nextip:
         ;
      }

   /* Next pass. */
   }

/* Endmark, write header, close the output file, and exit. */
   iadr = nhind + nhelp;
   if ( ( jstat = hlpHdwrit ( "\0", &iadr ) ) ) goto syserr;
   sprintf ( header, "%12.12ld", nhind + nhelp + 1l );
   iadr = 0;
   if ( ( jstat = hlpHdwrit ( header, &iadr ) ) )
   if ( ( jstat = hlpHclose ( ) ) ) goto syserr;
   goto wrapup;

/*
** EXITS
*/

/* Error opening source file. */
soperr:
   puts ( "Unable to open help source file!" );
   goto abort;

/* Error opening library file. */
loperr:
   puts ( "Unable to open help library file!" );
   goto abort;

/* Invalid level. */
badlev:
   if ( hlpLength ( iobuf ) >= 40 )
      strcpy ( iobuf+40, "....." );
   printf ( "Invalid level: %s\n", iobuf );
   goto abort;

/* No keyword. */
nokeyw:
   printf ( "Keyword absent at level %d\n", level );
   goto abort;

/* HELP system error status. */
syserr:
   puts ( hlpErrmes ( jstat ) );
   goto abort;

/* Run aborted. */
abort:
   puts ( "Abort!" );
   jstat = hlp_CREATION_FAILURE;
   goto wrapup;

/* Wrap up. */
wrapup:
   fclose ( fps );
   return jstat;
}
