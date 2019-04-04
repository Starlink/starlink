#include <limits.h>
#include "dat_par.h"
#include "mers.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"
#include "sae_par.h"
#include "star/util.h"
#include <stdlib.h>
#include <string.h>

#define MXOUT 8*NDF__SZHMX /* Output buffer size in characters */

void ndf1Hfwrt( NdfDCB *dcb, const char *appn, int nlines,
                char *const text[], int trans, int wrap, int rjust,
                int *status ){
/*
*+
*  Name:
*     ndf1Hfwrt

*  Purpose:
*     Format text and write it to a history record.

*  Synopsis:
*     void ndf1Hfwrt( NdfDCB *dcb, const char *appn, int nlines,
*                     const char *text[], int trans, int wrap, int rjust,
*                     int *status )

*  Description:
*     This function writes a series of text lines to the current history
*     record, after first applying optional message token translation and
*     formatting operations.  If the history has not yet been modified by
*     the current application (and "appn" is not "<"append">"), it creates
*     a new history record, initialises it, and inserts the text suppled.
*     If the history has already been modified (or if "appn" is
*     "<"append">"), then the new text is simply appended to any already
*     present. The function returns without action if the NDF does not have
*     a history component.

*  Parameters:
*     dcb
*        Pointer to the NDF whose history is to be modified.
*     appn
*        Pointer to a null terminated string holding the name of the
*        application. This is only used (to initialise the new history
*        record) if the history has not yet been modified by the current
*        application, otherwise it is ignored. If a blank value is given,
*        then a suitable default will be used instead. The special value
*        "<"append">" may be supplied in order to append the text lines to
*        the current history text even if the  history has not yet been
*        modified by the current application.
*     nlines
*        Number of new lines of text to be added to the history record.
*     text
*        Pointer to an array with at least "nlines" elements, each being
*        a pointer to a null terminated string holding a line text.
*     trans
*        Whether message tokens embedded in the input text lines are to be
*        translated. If not, then all input text will be interpreted
*        literally.
*     wrap
*        Whether "wrapping" of paragraphs in the input text is to be
*        performed to adjust the text width to that of the history record
*        being written. If specified, wrapping takes place after message
*        token expansion.
*     rjust
*        Whether to pad lines of text so as to right justify them within
*        the width of the history record being written. If not, then the
*        right margin is left ragged.
*     *status
*        The global status.

*  Notes:
*     If a new history record is being initialised, then the text length
*     (characters per line) of the new record is determined by the length
*     of the longest element of the "text" array. If the history record has
*     already been written to, then the existing line text length is not
*     altered and formatting of new text takes place so as to fit the
*     existing text length.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char buf[ 2 * NDF__SZHMX + 2 ]; /* Rolling text buffer */
   char line[ NDF__SZHMX + 1 ];    /* Text for current input line */
   char linenx[ NDF__SZHMX + 1 ];  /* Text for next input line */
   char out[ MXOUT + 1 ];/* Formatted output buffer */
   int append;           /* Append text to current history record? */
   int empty;            /* BUF empty? */
   int fplast;           /* Last formatting position */
   int iline;            /* Loop counter for input text lines */
   int indent;           /* Indent for next output line */
   int lout;             /* No. output buffer characters used */
   int ncnx;             /* Length of next input line */
   int ok;               /* Output line formatted OK? */
   size_t fp;            /* Formatting position in BUF */
   size_t i1;            /* Source character position */
   size_t i2;            /* Destination character position */
   size_t l;             /* Length of output history text lines */
   size_t lbuf;          /* No. characters used in BUF */
   size_t nc;            /* Length of current input line */
   size_t textlen;       /* Length of longest supplied line of text */

/* Check inherited global status. */
   if( *status == SAI__OK ) {

/* Find the length of the longest line of text, excluding trailing spaces. */
      textlen = 0;
      for( iline = 0; iline < nlines; iline++ ) {
         if( text[ iline ] ) {
            nc = astChrLen( text[ iline ] );
            if( nc > textlen ) textlen = nc;
         }
      }

/* Check that the length of the input text lines is not too large for
   the internal buffers used for formatting. */
      if( textlen > NDF__SZHMX ) {
         *status = NDF__HISTL;
         msgSeti( "LEN", (int) textlen );
         msgSeti( "MAX", NDF__SZHMX );
         errRep( " ", "The length of the supplied history text lines "
                 "(^LEN) exceeds the allowed maximum NDF__SZHMX (^MAX) "
                 "(possible programming error).", status );
      }

/* Ensure that history information is available in the DCB. */
      ndf1Dh( dcb, status );
      if( *status == SAI__OK ) {

/* Check that a history component is present, otherwise there is nothing
   to do. */
         if( dcb->hloc ) {

/* Set a flag if we are appending the new text to the end of the current
   history record. */
            append = ( !strcmp( appn, "<APPEND>" ) );

/* Obtain the text length for the current history record. If this is
   zero (because there is not yet a current record), then use the
   length of the input text lines instead, or if "appn" is "<APPEND>" use
   the length of the current history record. */
            l = dcb->htlen;
            if( l == 0 ) {
               if( append ) {
                  ndf1Htlen( dcb, &l, status );
                  if( l == 0 ) l = textlen;
               } else {
                  l = textlen;
               }
            }

/* If we are appending text to the current history record, indicate that
   the first line should not be indented. Otherwise, indicate that spaces
   should be left unchanged (see ndf1Twrap). */
            if( append ) {
               indent = 0;
            } else {
               indent = -1;
            }

/* Initialise. */
            lbuf = 0;
            lout = 0;

/* The formatted output buffer receives a list of fixed-length,
   space-padded strings. So pre-fill it with spaces so we do not
   need to pad each individual string. */
            memset( out, ' ', sizeof( out ) );

/* Determine the length of the first input line. */
            ncnx = astChrLen( text[ 0 ] );

/* If the line is not blank and message token expansion is required,
   then expand the text. Renew the message tokens for later use. */
            if( ncnx > 0 ) {
               if( trans ) {
                  msgLoad( " ", text[ 0 ], linenx, sizeof(linenx), &ncnx,
                           status );
                  msgRenew();

/* If no expansion is required, then simply copy the input text. */
               } else {
                  strncpy( linenx, text[ 0 ], ncnx );
                  linenx[ ncnx ] = 0;
               }
            }

/* Convert any non-printing characters to blanks. */
            if( *status == SAI__OK ) {
               if( ncnx > 0 ) {
                  astChrClean( linenx );

/* If performing paragraph-wrapping, also compress multiple embedded
   blanks into single blanks. Leave leading blanks untouched, since the
   indentation of the first line should be preserved. */
                  if( wrap ) ndf1Cmpbl( 0, linenx );
               }

/* Re-evaluate the text length. */
               if( ncnx > 0 ) ncnx = astChrLen( linenx );
            }

/* Loop to process each input line. */
            if( *status == SAI__OK ) {
               for( iline = 0; iline < nlines; iline++ ){

/* Obtain the text and length of the current line (previously found). */
                  if( ncnx > 0 ) star_strlcpy( line, linenx,
                                               sizeof( line ) );
                  nc = ncnx;

/* Obtain the text of the next line, if it exists, by expanding or
   copying it, as for the first line. */
                  if( iline + 1 < nlines ) {
                     ncnx = astChrLen( text[ iline + 1 ] );
                     if( ncnx > 0 ) {
                        if( trans ) {
                           msgLoad( " ", text[ iline + 1 ], linenx,
                                    sizeof( linenx ), &ncnx, status );
                           msgRenew();
                        } else {
                           star_strlcpy( linenx, text[ iline + 1 ],
                                         sizeof( linenx ) );
                        }
                     }

/* Convert non-printing characters, compress multiple blanks and
   re-evaluate the text length. Do not compress leading blanks if
   the line before (i.e. the "current" line) was blank, since the
   indentation of the first line of each paragraph should be preserved. */
                     if( *status == SAI__OK ) {
                        if( ncnx > 0 ) {
                           astChrClean( linenx );
                           if( wrap ) ndf1Cmpbl( ( nc != 0 ), linenx );
                        }
                        if( ncnx > 0 ) ncnx = astChrLen( linenx );
                     }
                  }

/* If the current line is blank, then it does not need formatting (it
   acts as a paragraph separator, if relevant).  Append a blank line
   directly to the output buffer (the output buffer was previously
   filled with spaces, so just skip over them to add the blank line). */
                  if( nc == 0 ) {
                     lout += l;

/* If the output buffer is now full (it no longer contains space for
   further lines), then write its contents to the current history record.
   Then re-initialise its length to zero, and fill it with spaces again.  */
                     if( ( MXOUT - lout ) < l ) {
                        ndf1Hwrt( dcb, appn, lout/l, MXOUT, out, status );
                        lout = 0;
                        memset( out, ' ', sizeof( out ) );
                     }

/* If the current input line is not blank, then it must be appended to
   the text in "buf" prior to output. Append a separating blank to "buf"
   before adding it if necessary. */
                  } else {
                     if( ( lbuf != 0 ) && ( line[ 0 ] != ' ' ) ) {
                        buf[ ++lbuf ] = ' ';
                     }
                     star_strlcpy( buf + lbuf, line, sizeof( buf ) - lbuf );
                     lbuf += nc;

/* Loop to format the current contents of "buf" into output lines. */
                     fp = 0;
                     while( *status == SAI__OK ){
                        fplast = fp;

/* Format a line from "buf", appending the result to the output buffer,
   and determine whether the contents of "buf" have been exhausted. */
                        ndf1Twrap( buf, indent, &fp, l, out + lout,
                                   sizeof( out ) - lout );
                        empty = ( fp == UINT_MAX );

/* If we are appending text to the current history record, indicate that
   the second and all subsequent lines should be indentned by 3 spaces. */
                        if( append ) indent = 3;

/* Determine if the resulting output line will be correctly formatted
   (it may not be if "buf" has been prematurely exhausted so that the
   output line still requires text from the next input line). It will
   be "ok" if no paragraph wrapping is required or if further text still
   remains to be processed in "buf". */
                        ok = ( ( !wrap ) || ( !empty ) );

/* If necessary, check if the next input line is blank or if we are on
   the last line. Either of these indicates the end of a paragraph, so
   the text in "buf" is complete. */
                        if( !ok ) {
                           if( iline + 1 < nlines ) {
                              ok = ( ncnx == 0 );
                           } else {
                              ok = 1;
                           }
                        }

/* If the line is correctly formatted, then right-justify it if
   required, so long as we know there is more text to follow in the
   same input line or paragraph. */
                        if( ok ) {
                           if( rjust && ( !empty ) ) {
                              ndf1Rjust( out + lout, sizeof( out ) - lout );
                           }

/* Accept the line as part of the output. */
                           lout += l;

/* If the output buffer is now full (there is no longer room for
   further lines), then flush it by appending its contents to the
   current history record. Then re-initialise its length to zero,
   and fill it with spaces again.  */
                           if( ( MXOUT - lout ) < l ) {
                              ndf1Hwrt( dcb, appn, lout/l, MXOUT, out,
                                        status );
                              lout = 0;
                              memset( out, ' ', sizeof( out ) );
                           }
                        }

/* If the end of the text in "buf" has been reached and it has all been
   correctly formatted, then empty the buffer ready for more input. */
                        if( empty ) {
                           if( ok ) {
                              lbuf = 0;

/* If the last output line was not correctly formatted, then "buf"
   requires more input, so move its contents to the left, if necessary,
   to eliminate leading characters which have so far been correctly
   formatted. Then exit the formatting loop so as to process the next
   input line. */
                           } else {
                              if( fplast != 1 ) {
                                 for( i1 = fplast - 1; i1 < lbuf; i1++ ){
                                    i2 = i1 - fplast + 2;
                                    star_strlcpy( buf + i2 - 1, buf + i1,
                                                  sizeof( buf ) - i2 + 1 );
                                 }
                                 lbuf += -fplast + 1;
                              }
                           }
                           break;
                        }
                     }
                  }

/* Quit processing input lines if an error occurs. */
                  if( *status != SAI__OK ) break;
               }
            }

/* If any lines remain in the output buffer, then write them to the
   current history record. */
            if( lout > 0 ) ndf1Hwrt( dcb, appn, lout/l, MXOUT, out, status );
         }
      }

/* Call error tracing function. */
      if( *status != SAI__OK ) ndf1Trace( "ndf1Hfwrt", status );
   }

/* Before exiting, make a dummy call to "msgLoad" to ensure that all
   message tokens become undefined. */
   msgLoad( " ", " ", buf, sizeof( buf ), &ncnx, status );

}

