#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "mers.h"
#include <string.h>
#include "star/util.h"

void ndf1Hwdef( NdfDCB *dcb, const char *appn, int *status ){
/*
*+
*  Name:
*     ndf1Hwdef

*  Purpose:
*     Write default history information to an NDF.

*  Synopsis:
*     void ndf1Hwdef( NdfDCB *dcb, const char *appn, int *status )

*  Description:
*     This function writes default history information to the current
*     history record of an NDF, creating a new record if necessary. It
*     returns without action if (a) the NDF does not have a history
*     component, (b) it has been accessed read-only, (c) default history
*     information has already been written to the current history record
*     or, (d) the history update mode is "DISABLED".

*  Parameters:
*     dcb
*        Pointer to the data object whose history is to be updated.
*     appn
*        Pointer to a null terminated string holding the name of the
*        currently-executing application. This is used if a new history
*        record has to be created by this function, otherwise it is
*        ignored. If a blank value is given, then a suitable default will
*        be used instead.
*     *status
*        The global status.

*  Notes:
*     If this function has to create a new history record, then the text
*     width used for this record will be equal to NDF__SZHIS. If it is
*     appending information to a pre-existing current history record, then
*     the new text will be formatted to the pre-existing text width.

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
   char file[ NDF__SZFIL + 1 ];    /* Executing file name */
   char *text[ 2 ];      /* Pointers to history text buffer */
   int nlines;           /* Number of lines of text to write */
   NdfUNAME info;        /* System information */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Mark the error stack to prevent use of message tokens from affecting
   any which may already be defined. */
   errMark();

/* Ensure that history information is available in the DCB. */
   ndf1Dh( dcb, status );
   if( *status == SAI__OK ) {

/* Check that a history component exists. Otherwise there is nothing to
   do. */
      if( dcb->hloc ) {

/* Check that the NDF has been accessed for modification, that the
   default history writing flag is still set, and that the update mode
   is not "DISABLED". Otherwise there is nothing to do. */
         if( ( strcmp( dcb->mod, "READ" ) ) && dcb->hdef && ( dcb->humod != NDF__HDISA ) ) {

/* Write information specific to the current software environment into
   the history record. */
            ndf1Hwenv( dcb, appn, status );

/* Now write additional environment independent information. Obtain
   general system details (if required) and the name of the currently
   executing file, indicating if it cannot be determined. */
            ndf1Gtfil( file, sizeof( file ), status );
            if( file[ 0 ] == 0 ) {
               star_strlcpy( file, "<unknown>", sizeof( file ) );
            }
            if( dcb->humod == NDF__HVERB ) ndf1Uname( &info, status );

/* Define message tokens for these values. */
            if( dcb->humod == NDF__HVERB ) {
               msgSetc( "M", info.machine );
               msgSetc( "R", info.release );
               msgSetc( "S", info.sysname );
               msgSetc( "V", info.version );
            }
            msgSetc( "F", file );

/* Set up history text containing these values, adapting to the amount
   of verbosity required. */
            if( dcb->humod == NDF__HVERB ) {
               nlines = 2;
               text[ 0 ] = "Software: ^F";
               text[ 1 ] = "Machine: ^M, System: ^S ^V (release ^R)";
            } else {
               nlines = 1;
               text[ 0 ] = "Software: ^F";
            }

/* Append the text to the history record. */
            ndf1Hfwrt( dcb, appn, nlines, text, 1, 0, 0, status );

/* If OK (and history information was written), then clear the default
   history writing flag. */
            if( *status == SAI__OK ) {
               dcb->hdef = 0;

/* Use ndf1Event to flag a "default history written" event. If the caller
   has registered a handler for this type of event (using ndfHndlr), it
   will be called. First, assign the name of the NDF to the MSG token
   "NDF_EVENT", then raise the event. */
               ndf1Evmsg( "NDF_EVENT", dcb );
               ndf1Event( "DEF_HISTORY", status );

            }
         }
      }
   }

/* Release the error stack. */
   errRlse();

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hwdef", status );

}

