#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"
#include <string.h>

void ndfHput_( const char *hmode, const char *appn, int repl, int nlines,
              char *const text[], int trans, int wrap, int rjust, int indf,
              int *status ){
/*
*+
*  Name:
*     ndfHput

*  Purpose:
*     Write history information to an NDF.

*  Synopsis:
*     void ndfHput( const char *hmode, const char *appn, int repl,
*                   int nlines, char *const text[], int trans, int wrap,
*                   int rjust, int indf, int *status )

*  Description:
*     This function writes textual information to the history component of
*     an NDF, creating a new history record if necessary. A variety of
*     formatting options are available and values may be substituted for
*     message tokens embedded within the text. The text supplied may either
*     augment or replace the history information normally written by
*     default.

*  Parameters:
*     hmode
*        Pointer to a null terminated string holding the priority for the
*        text being written: "VERBOSE", "NORMAL" or "QUIET", where
*        "VERBOSE" signifies the lowest priority and "QUIET" signifies the
*        highest.  The value given may be abbreviated, to no less then
*        three characters. A blank value is accepted as a synonym for
*        "NORMAL".
*     appn
*        Pointer to a null terminated string holding the name of the
*        current application. This will only be used if a new history
*        record is created by this function, otherwise it is ignored. If a
*        blank value is given, then a system-supplied default will be used
*        instead. If the special value "<APPEND>" is supplied, then the
*        text is always appended to the current history record, even if it
*        was created by a previous application.
*     repl
*        Whether the text supplied is intended to replace the history
*        information which is supplied by default. If a non-zero value is
*        given and no default history information has yet been written to
*        the NDF, then subsequent writing of this information will be
*        suppressed. If a zero value is given, then default history
*        information is not suppressed and will later be appended to the
*        text supplied (unless suppressed by another call to ndfHput). The
*        supplied value is ignored, and a value of non-zero assumed, if
*        "appn" is supplied equal to "<APPEND>".
*     nlines
*        Number of lines of history text supplied in array "text".
*     text
*        Pointer to an array in which each element is a pointer to a null
*        terminated string holding a line of history text to be written.
*        The length of this array is given by "nlines". The length of each
*        string should not exceed the value NDF__SZHMX. The recommended
*        length of each string is given by the constant NDF__SZHIS. (Both
*        of these constants are defined in the include file "ndf.h".)
*     trans
*        If a non-zero value is supplied, then any message tokens embedded
*        in the supplied text will be expanded before it is written to the
*        history record (see SUN/104 for a description of how to use
*        message tokens). If a zero value is given, then the supplied text
*        is taken literally; no special characters will be recognised and
*        no message token expansion will occur.
*     wrap
*        If a non-zero value is given, then paragraph wrapping will be
*        performed on the supplied text (after message token expansion if
*        appropriate) so as to make as much text fit on to each line of a
*        history record as possible. Blank input lines may be used to
*        delimit paragraphs. If a zero value is given, then input lines
*        which exceed the history record's text width will simply be broken
*        (at a space if possible) and continued on a new line.
*     rjust
*        If a non-zero value is given, then lines of history text will be
*        padded out with blanks (after message token expansion and
*        paragraph wrapping if appropriate) so as to give a justified right
*        margin. If a zero value is given, then the right margin will
*        remain ragged.
*     indf
*        NDF identifier.
*     *status
*        The global status.

*  Notes:
*     -  This function will return without action if (a) there is no
*     history component present in the NDF, (b) the priority specified via
*     "hmode" is lower than the NDF's current history update mode setting,
*     or (c) the NDF's history update mode is currently set to "DISABLED".
*     -  It is expected that the "appn" parameter will usually be left
*     blank. A non-blank value should normally only be given for this
*     parameter if a more complete identification of the current
*     application can be given than is supplied by default.
*     -  If no previous history information has been written to the NDF by
*     the current application, then this function will create a new history
*     record whose text width will be determined by the maximum length of
*     the lines of the "text" array. If history information has already been
*     written, then this function will append to the existing history record.
*     In this case, the text width will already have been defined, so the
*     supplied text will be re-formatted if necessary (by line breaking or
*     paragraph wrapping) to fit into the available width.
*     -  Paragraph wrapping is recommended, when appropriate, as a means of
*     saving space while retaining a neat appearance in the resulting
*     history record. It is particularly useful when message tokens are
*     used within normal text, since these make it hard to predict the
*     precise length of history lines in advance.  The right justification
*     flag may be used to improve the cosmetic appearance of history text,
*     but it has no effect on the amount of space used.
*     -  On exit from this function, all message tokens in the current
*     message context are left in an undefined state.

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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char cdummy[2];       /* Dummy character string */
   int hum;              /* History update mode code */
   int idummy;           /* Dummy integer variable */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* If "status" is set on entry, then make a dummy call to "msgLoad" to
   ensure that message tokens are left in an undefined state. */
   if( *status != SAI__OK ) {
      msgLoad( " ", " ", cdummy, sizeof( cdummy ), &idummy, status );
   } else {

/* Otherwise, import the NDF identifier. */
      ndf1Impid( indf, &acb, status );

/* If OK, check that WRITE access is available to the NDF. */
      if( *status == SAI__OK ) {
         ndf1Chacc( acb, "WRITE", status );

/* If a blank "hmode" value was given, then supply a default. Otherwise
   validate this string. */
         if( astChrLen( hmode ) == 0 ) {
            hum = NDF__HNORM;
         } else {
            ndf1Chhum( hmode, &hum, status );
            if( *status == SAI__OK ) {

/* If "DISABLED" was specified, then report an error. */
               if( hum == NDF__HDISA ) {
                  *status = NDF__HUMIN;
                  errRep( " ", "The 'DISABLED' history update mode may not "
                          "be specified as the priority parameter when "
                          "writing history text to an NDF (possible "
                          "programming error).", status );
               }
            }
         }

/* IF OK, obtain an index to the data object entry in the DCB and ensure
   that DCB history component information is available. */
         if( *status == SAI__OK ) {
            dcb = acb->dcb;
            ndf1Dh( dcb, status );
            if( *status == SAI__OK ) {

/* Check whether a history component is present. Otherwise there is
   nothing to do. */
               if( dcb->hloc ) {

/* Check that its update mode permits history text of the priority
   supplied.  If so, then write the information into the history
   record. */
                  if( hum <= dcb->humod ) {
                     ndf1Hfwrt( dcb, appn, nlines, text, trans, wrap,
                                rjust, status );

/* If history text has been written successfully, then cancel the
   default history writing flag if required. */
                     if( *status == SAI__OK ) {
                        if( repl || !strcmp( appn, "<APPEND>" ) )
                        dcb->hdef = 0;
                     }
                  }
               }
            }
         }
      }

/* Make a dummy call to "msgLoad" to ensure that message tokens are left
   in an undefined state. */
      msgLoad( " ", " ", cdummy, sizeof(cdummy), &idummy, status );

/* If an error occurred, then report context information and call the
   error tracing function. */
      if( *status != SAI__OK ) {
         errRep( " ", "ndfHput: Error writing history information to an "
                 "NDF.", status );
         ndf1Trace( "ndfHput", status );
      }
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

