#include "sae_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfHecho_( int nlines, char *const text[], int *status ){
/*
*+
*  Name:
*     ndfHecho

*  Purpose:
*     Write out lines of history text.

*  Synopsis:
*     void ndfHecho( int nlines, char *const text[], int *status )

*  Description:
*     This function writes a series of lines of text to the standard output
*     channel, indented by three spaces. It is provided as a default
*     service function which may be passed as an parameter to ndfHout in
*     order to display NDF history information.
*
*     The specification of this function may be used as a template when
*     writing alternative service functions for use by ndfHout.

*  Parameters:
*     nlines
*        Number of lines of text to be written.
*     text
*        An array containing "nlines" elements, each of which is a
*        pointer to a null-terminated character string holding a line
*        of text.
*     *status
*        The global status.

*  Notes:
*     Before passing this (or a similar) function as an parameter to
*     ndfHout the calling function should declare it in a Fortran EXTERNAL
*     statement.

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
   int i;                /* Loop counter for text lines */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Mark the message/error stack. This prevents use of MSG_ functions
   here from interfering with previously-defined message tokens, etc. */
   errMark();

/* Loop to write out each line of text. */
   for( i = 0; i < nlines; i++ ){

/* Assign each line to a message token and write out a message
   containing the translation of the token.  This prevents any
   "special" characters in the line from being interpreted. */
      msgSetc( "LINE", text[ i ] );
      msgOut( " ", "   ^LINE", status );

/* Quit if an error occurs. */
      if( *status != SAI__OK ) break;
   }

/* Release the message/error stack. */
   errRlse();

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHecho: Error writing out lines of history text.",
              status );
      ndf1Trace( "ndfHecho", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

