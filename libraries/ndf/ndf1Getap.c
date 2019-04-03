#include <string.h>
#include "mers.h"
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "ndf_err.h"

void ndf1Getap( char *appn, size_t appn_length, int *status ){
/*
*+
*  Name:
*     ndf1Getap

*  Purpose:
*     Get the name of the currently-executing application.

*  Synopsis:
*     void ndf1Getap( char *appn, size_t appn_length, int *status )

*  Description:
*     This function returns the name of the currently-running application,
*     left justified. The returned value will be truncated without error if
*     the variable supplied is too short.

*  Parameters:
*     appn
*        Pointer to an array in which to return a null terminated string
*        holding application name.
*     appn_length
*        The length of the supplied 'appn' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  This is the "standalone" version of this routine. It returns
*     the name of the currently executing file or command.

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

/* Local variables */
   char *parg;
   char arg[ NDF__SZHMX + 1 ];
   int there;
   size_t nc;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get the value of the zero'th argument (the name of the command being
   executed). */
   ndf1Gtarg( 0, arg, sizeof( arg ), &there, status );

/* If an error was reported due to the string being truncated, annul it. */
   if( *status == NDF__ARGIN ) errAnnul( status );

/* If the argument value is not known, use "<unknown>" as the
   application name. */
   if ( !there ) {
      parg = "<unknown>";

/* Otherwise, find the last '/' which marks the end of the directory
   path, so as to select just the name field. */
   } else {
      parg = strrchr( arg, '/' );

/* If a '/' was found, increment the pointer so that it points to the
   start of the name field. */
      if( parg ) {
         parg++;

/* If no '/' was found, use the whole argument value. */
      } else {
         parg = arg;
      }
   }

/* Get the number of characters to return, excluding trailing spaces.
   Limit it to the length opf the supplied buffer. */
   nc = astChrLen( parg );
   if( nc > appn_length - 1 ) nc = appn_length - 1;

/* If OK, copy the application name back to the caller, truncating if
   necessary. */
   if( *status == SAI__OK ) {
      memcpy( appn, parg, nc );
      appn[ nc ] = 0;
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Getap", status );

}

