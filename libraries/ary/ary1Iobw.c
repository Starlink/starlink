#include <string.h>
#include "ary_ast.h"
#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"

void ary1Iobw( const char *type, const char *inopt, size_t el, void *pntr,
               int *status ) {
/*
*+
*  Name:
*     ary1Iobw

*  Purpose:
*     Initialise an object for writing.

*  Synopsis:
*     void ary1Iobw( const char *type, const char *inopt, size_t el,
*                    void *pntr, int *status )

*  Description:
*     The routine performs initialisation of a vectorised array, of any
*     numeric data type, prior to its use for 'WRITE' access.
*     Initialisation may be to zero, or to "bad" values, according to
*     the value supplied for the 'inopt' argument. If this argument is
*     a blank string, then no action is taken.

*  Parameters:
*     type
*        An HDS primitive numeric type string specifying the data type
*        of the object to be initialised (case insensitive).
*     inopt
*        The initialisation option required; either ' ', 'ZERO' or
*        'BAD' (case insensitive).
*     el
*        Number of elements in the array.
*     pntr
*        Pointer to the array to be initialised.
*     status
*        The global status.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Do nothing if 'inopt' is a blank string. */
   if( astChrLen( inopt ) == 0 ){

/* If requested, set all the array elements to zero. */
   } else if( !strcasecmp( inopt, "ZERO" ) ){
      ary1Vzero( type, el, pntr, status );

/* Otherwise, if requested, set all the array elements to a "bad" value. */
   } else if( !strcasecmp( inopt, "BAD" ) ){
      ary1Vbad( type, el, pntr, status );

/* If the initialisation option specified was not valid, then report an
   error. */
   } else {
      *status = ARY__FATIN;
      msgSetc( "ROUTINE", "ARY1_IOBW" );
      msgSetc( "BADINOPT", inopt );
      errRep( " ", "Routine ary1Iobw called with an invalid 'inopt' argument "
              "of '^BADINOPT' (internal programming error).", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Iobw", status );

}
