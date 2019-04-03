#include "star/util.h"
#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "star/util.h"
#include "mers.h"

#define MXOUT 2*NDF__SZHMX /* Output buffer size in characters */

void ndf1Hwenv( NdfDCB *dcb, const char *appn, int *status ){
/*
*+
*  Name:
*     ndf1Hwenv

*  Purpose:
*     Write default history text derived from the standalone environment.

*  Synopsis:
*     void ndf1Hwenv( NdfDCB *dcb, const char *appn, int *status )

*  Description:
*     This function writes default history text to the current history
*     record of an NDF, creating a new history record if necessary. This
*     version is specific to the standalone environment and writes a list
*     of command line arguments.

*  Parameters:
*     dcb
*        Pointer to the NDF whose history record is to be updated.
*     appn
*        Pointer to a null terminated string holding the name of the
*        current application. This will be used to initialise the new
*        history record if a current record does not initially exist,
*        otherwise it is ignored. If a blank value is given, then s
*        suitable default will be used instead.
*     *status
*        The global status.

*  Notes:
*     An NDF history structure must exist and DCB information must be
*     available for it. This function does not check for this itself.

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
   char arg[ NDF__SZHMX + 1 ];     /* Argument value */
   char buf[ NDF__SZHMX + 1 ];     /* Intermediate text buffer */
   char out[ MXOUT + 1 ];/* Formatted output buffer */
   int br;               /* Character position for breaking text */
   int iarg;             /* Index into argument list */
   int lout;             /* No. characters used in OUT */
   int there0;           /* Is zeroth argument available? */
   int there;            /* Is argument available? */
   size_t indent;        /* No. spaces of indentation to use */
   size_t l;             /* Text width for formatting */
   size_t i;             /* Loop count */
   size_t larg;          /* Length of the current argument */
   size_t lbuf;          /* No. characters used in BUF */
   size_t newlen;        /* New buffer length */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Only write history information if the history update mode is at
   least "NORMAL". */
   if( dcb->humod >= NDF__HNORM ) {

/* Mark the error stack to prevent use of message tokens from affecting
   any which are already defined. */
      errMark();

/* Initialise. */
      lout = 0;

/* Determine the text length to be used for the current history record.
   If this is zero (because the current record does not yet exist),
   then use the default preferred text length NDF__SZHIS. */
      l = dcb->htlen;
      if( l == 0 ) l = NDF__SZHIS;

/* Determine how much indentation to use. Normally use 3 spaces, but
   reduce this if the line length is short. */
      indent = NDF_MIN( l/6, 3 );

/* Initialise "buf" with a heading for the list of command line arguments
   which will follow. */
      lbuf = 10;
      star_strlcpy( buf, "Arguments:", sizeof( buf ) );

/* Obtain the length of the zeroth parameter. (If this is zero, and
   subsequent arguments also have zero length, then we will assume that
   no parameter information is available.) */
      ndf1Gtarg( 0, arg, sizeof(arg), &there0, status );

/* Loop to process each parameter. */
      iarg = 0;
      while( *status == SAI__OK ){

/* Obtain the next parameter. */
         iarg++;
         ndf1Gtarg( iarg, arg,  sizeof(arg), &there, status );
         if( *status == SAI__OK ) {
            larg = astChrLen( arg );

/* If there are no arguments (the first one is blank), then manufacture
   one to document their absence. Use an appropriate string, according
   to whether there are simply no arguments, or whether parameter
   information is not available. */
            if( iarg == 1 && !there ) {
               if( there0 ) {
                  star_strlcpy( arg, "<none>", sizeof( arg ) );
               } else {
                  star_strlcpy( arg, "<unknown>", sizeof( arg ) );
               }
            }

/* Quit looping if there are no more arguments to process. */
            if( !there ) break;

/* Convert any non-printing characters to blanks, evaluate the
   parameter length and truncate the string to remove trailing spaces. */
            astChrClean( arg );
            larg = astChrLen( arg );
            if( larg > 0 ) {
               arg[ larg ] = 0;

/* Find how much text there will be in "buf" if this parameter value is
   appended to the existing contents. Allow for indentation if "buf" is
   currently empty, otherwise allow a separating blank. */
               newlen = lbuf + larg;
               if( lbuf == 0 ) {
                  newlen += indent;
               } else {
                  newlen++;
               }
            }

/* If the output line length will not be exceeded, then append the new
   text to "buf", with indentation or a separating space as appropriate. */
            if( newlen <= l ) {
               if( lbuf == 0 ) {
                  lbuf = indent;
                  for( i = 0; i < lbuf; i++ ) buf[ i ] = ' ';
               } else {
                  lbuf++;
                  buf[ lbuf - 1 ] = ' ';
               }
               star_strlcpy( buf + lbuf, arg, sizeof( buf ) - lbuf );
               lbuf = newlen;

/* Otherwise, transfer the existing contents of "buf" (if any) to the
   next output line and loop to refill "buf" repeatedly from the parameter
   value "arg" so as to break it into further output lines. */
            } else {
               br = 0;

               while( *status == SAI__OK ) {
                  if( lbuf > 0 ) {
                     star_strlcpy( out + lout, buf, sizeof( out ) - lout );
                     lout += l;
                  }

/* If the output buffer is now full (there is no longer room for
   further lines), then flush it by appending its contents to the
   current history record. */
                  if( ( MXOUT - lout ) < l ) {
                     ndf1Hwrt( dcb, appn, lout/l, l, out, status );
                     lout = 0;
                  }

/* Determine how many of the characters remaining in "arg" will fit into
   a single output line, starting at the character following the last
   line break position "br". Allow for indentation. */
                  lbuf = NDF_MIN( l - indent, larg - br );

/* Transfer this number of characters into "buf", adding indentation, and
   update "br". */
                  if( lbuf > 0 ) {
                     for( i = 0; i < indent; i++ ) buf[ i ] = ' ';
                     star_strlcpy( buf + indent, arg + br,
                                   sizeof( buf ) - indent );
                     lbuf += indent;
                  }
                  br += lbuf - indent;

/* If "buf" now contains a complete output line, then return to transfer
   it to the output buffer. Otherwise, continue to process the next
   parameter. */
                  if( lbuf != l ) break;
               }
            }
         }
      }

/* Transfer any remaining text from "buf" to the output buffer and
   perform a final flush. */
      if( *status == SAI__OK ) {
         if( lbuf > 0 ) {
            star_strlcpy( out + lout, buf, sizeof( out ) - lout );
            lout += l;
         }
         if( lout > 0 ) ndf1Hwrt( dcb, appn, lout/l, l, out, status );
      }

/* Release the error stack. */
      errRlse();
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Hwenv", status );

}

