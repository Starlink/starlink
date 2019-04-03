#include <stdlib.h>
#include "sae_par.h"
#include "star/util.h"
#include "star/subpar.h"
#include "ndf1.h"
#include "msg_par.h"
#include "par_par.h"
#include "par.h"
#include "ndf_ast.h"
#include "star/util.h"
#include "mers.h"
#include <string.h>

#define MXOUT 2*NDF__SZHMX /* Output buffer size in characters */

void ndf1Hwenv( NdfDCB *dcb, const char *appn, int *status ){
/*
*+
*  Name:
*     ndf1Hwenv

*  Purpose:
*     Write default history text derived from the ADAM environment.

*  Synopsis:
*     void ndf1Hwenv( NdfDCB *dcb, const char *appn, int *status )

*  Description:
*     This function writes default history text to the current history
*     record of an NDF, creating a new history record if necessary. This
*     version is specific to the ADAM software environment and writes a
*     list of current parameter keywords and values.

*  Parameters:
*     dcb
*        Pointer to the DCB for the NDF whose history record is to be
*        updated.
*     appn
*        Pointer to a null terminated string holding the name of the
*        current application. This will be used to initialise the new
*        history record if a current record does not initially exist,
*        otherwise it is ignored. If a blank value is given, then a
*        suitable default will be used instead.
*     *status
*        The global status.

*  Notes:
*     An NDF history structure must exist and DCB information must be
*     available for it. This function does not check for this itself.

*  Implementation Deficiencies:
*     -  This function makes calls to SUBPAR functions in order to obtain
*     the current values of parameters. This deficiency is a work-around
*     for a problem with message token expansion in "msgLoad", which
*     prevents the string "$^P" from expanding to the current value of the
*     parameter whose name is in message token P. Calls to SUBPAR should be
*     eliminated when this problem is fixed.

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
   char buf[ NDF__SZHMX + 1 ];     /* Intermediate text buffer */
   char msg[ MSG__SZMSG + 1 ];     /* Expanded message text */
   char out[ MXOUT + 1 ];/* Formatted output buffer */
   char param[ PAR__SZNAM + 1 ];   /* Parameter name */
   char value[ MSG__SZMSG + 1 ];   /* Parameter value */
   const char *fmt;      /* Parameter message format string */
   int br;               /* Character position for breaking text */
   int first;            /* First parameter? */
   int i;                /* Loop count */
   int lmsg;             /* Length of expanded message text */
   int lout;             /* No. characters used in OUT */
   int state;            /* Parameter state */
   size_t indent;        /* No. spaces of indentation to use */
   size_t ipar;          /* Index into parameter names list */
   size_t l;             /* Text width for formatting */
   size_t lbuf;          /* No. characters used in BUF */
   size_t lpar;          /* Length of parameter name */
   size_t ncode;         /* Parameter namecode */
   size_t newlen;        /* New buffer length */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Only write history information if the history update mode is at
   least "NORMAL". */
   if( dcb->humod >= NDF__HNORM ) {

/* Mark the error stack to prevent use of message tokens from affecting
   any which are already defined. */
      errMark();

/* Initialise the used length of the formatted output buffer to zero and
   fill it with spaces (ndf1Hwrt requires it contain a list of space-padded
   lines of text). */
      lout = 0;
      memset( out, ' ', sizeof( out ) );

/* Determine the text length to be used for the current history record.
   If this is zero (because the current record does not yet exist),
   then use the default preferred text length NDF__SZHIS. */
      l = dcb->htlen;
      if( l == 0 ) l = NDF__SZHIS;

/* Determine how much indentation to use. Normally use 3 spaces, but
   reduce this if the line length is short. */
      indent = NDF_MIN( l/6, 3 );

/* Initialise "buf" with a heading for the list of parameters which will
   follow. */
      lbuf = 11;
      star_strlcpy( buf, "Parameters:", sizeof( buf ) );

/* Loop to process each parameter. */
      ipar = 0;
      while( *status == SAI__OK ){

/* Initialise the length of text to be output. */
         lmsg = 0;

/* Obtain the name of the next parameter. */
         first = ( ipar == 0 );
         subParIndex( &ipar, status );
         if( *status == SAI__OK ) {

/* If there are no parameters (none is found on the first attempt),
   then provide some text to document their absence. */
            if( ( ipar == 0 ) && first ) {
               star_strlcpy( msg, "<none>", sizeof( msg ) );
               lmsg = 6;

/* Quit looping when there are no more parameters to process. */
            } else if( ipar == 0 ) {
               break;

/* Otherwise, obtain the parameter's name and state. */
            } else {
               subParParname( ipar, param, sizeof(param), &lpar, status );
               parState( param, &state, status );
               if( *status == SAI__OK ) {

/* Classify the parameter state and assign an appropriate message
   format for representing its value. States which are not recognised
   here have their format left blank and are subsequently ignored. */
                  fmt = NULL;
                  if( state == PAR__ACTIVE ) {
                     fmt = "%^P=^V";
                  } else if( state == PAR__NULLST ) {
                     fmt = "%^P=!";
                  } else if( state == PAR__CANCEL ) {
                     fmt = "%^P=";
                  }

/* If the state was recognised, then define a message token for the
   parameter name. If the parameter is in the active state, then also
   obtain its current value as a character string and assign this to a
   message token. */
                  if( fmt ) {
                     msgSetc( "P", param );
                     if( state == PAR__ACTIVE ) {
                        subParFindpar( param, &ncode, status );

/* A parameter will be in the active state if a value is supplied for it
   on the command line, but that value is only saved as the current value
   when the value of the parameter is accessed by the application using
   "parGet"x etc. So if this function is called before the parameter is
   accessed, it will be in the active state but may not have a current
   value. In which case the following call to subParCurval will report
   an error. Annull this error and use a "value" that indicates that the
   real value is unknown. This is not fool-proof since even if a
   parameter has a current value, it may have been set on a previous
   invocation of the application, and a different value may be used on
   the current invocation if the parameter has not yet been accessed. For
   this reason this function should be called once all parameters have been
   accessed. Maybe this function should report an error if the parameter
   value has not yet been accessed by the application. But how do you
   tell if the current value of a parameter has been set during the
   current invocation of the application or a previous one? */
                        if( *status == SAI__OK ) {
                           subParCurval( ncode, value, sizeof(value), status );
                           if( *status != SAI__OK ) {
                              errAnnul( status );
                              star_strlcpy( value, "<not yet accessed>",
                                            sizeof( value ) );
                           }
                           msgSetc( "V", value );
                        }
                     }

/* Expand the message text. */
                     msgLoad( " ", fmt, msg, sizeof(msg), &lmsg, status );
                     if( *status == SAI__OK ) {

/* Convert any non-printing characters to blanks and re-evaluate the
   expanded text length. */
                        astChrClean( msg );
                        lmsg = astChrLen( msg );
                     }
                  }
               }
            }
         }

/* If OK, and there is some text to output, then find how much text
   there will be in "buf" if it is appended to the existing contents.
   Allow for indentation if "buf" is currently empty, otherwise allow a
   separating blank. */
         if( ( *status == SAI__OK ) && ( lmsg > 0 ) ) {
            newlen = lbuf + lmsg;
            if( lbuf == 0 ) {
               newlen += indent;
            } else {
               newlen++;
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
               star_strlcpy( buf + lbuf, msg, sizeof( buf ) - lbuf );
               lbuf = newlen;

/* Otherwise, transfer the existing contents of "buf" (if any) to the
   next output line and loop to refill "buf" repeatedly from the expanded
   text buffer "msg" so as to break it into further output lines. */
            } else {
               br = 0;

               while( *status == SAI__OK ) {
                  if( lbuf > 0 ) {
                     star_strlcpy( out + lout, buf, sizeof( out ) - lout );

/* The formatted output buffer ("out") should be a set of space-padded lines
   of text without any terminating nulls. So replace the terminating null
   added above with a space. */
                     out[ lout + lbuf ] = ' ';

                     lout += l;
                  }

/* If the output buffer is now full (there is no longer room for
   further lines), then flush it by appending its contents to the
   current history record. Re-initialise its length to zero, and 
   fill it with spaces. */
                  if( ( MXOUT - lout ) < l ) {
                     ndf1Hwrt( dcb, appn, lout/l, l, out, status );
                     lout = 0;
                     memset( out, ' ', sizeof( out ) );
                  }

/* Determine how many of the characters remaining in "msg" will fit into
   a single output line, starting at the character following the last
   line break position "br". Allow for indentation. */
                  lbuf = NDF_MIN( l - indent, lmsg - br );

/* Transfer this number of characters into "buf", adding indentation, and
   update "br". */
                  if( lbuf > 0 ) {
                     for( i = 0; i < indent; i++ ) buf[ i ] = ' ';
                     star_strlcpy( buf + indent, msg + br, sizeof( buf ) - indent );
                     lbuf += indent;
                  }
                  br += lbuf - indent;

/* If "buf" now contains a complete output line, then loop to transfer
   it to the output buffer. Otherwise, break out of the loop and continue
   to process the next parameter. */
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
            out[ lout + lbuf ] = ' ';
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

