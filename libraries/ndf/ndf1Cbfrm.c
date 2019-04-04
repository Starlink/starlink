#include "ndf1.h"
#include "ndf_ast.h"
#include "sae_par.h"
#include "star/util.h"
#include <stdlib.h>

void ndf1Cbfrm( int ndim, const hdsdim lbnd[], const hdsdim ubnd[], char *form,
                size_t form_length, int *status ){
/*
*+
*  Name:
*     ndf1Cbfrm

*  Purpose:
*     Convert a storage form string if NDF bounds require it.

*  Synopsis:
*     void ndf1Cbfrm( int ndim, const hdsdim lbnd[], const hdsdim ubnd[],
*                     char *form, size_t form_length, int *status )

*  Description:
*     This function checks an NDF's bounds for compatibility with an array
*     storage form string and converts the string to describe an
*     alternative storage form if necessary.

*  Parameters:
*     ndim
*        Number of NDF dimensions.
*     lbnd
*        NDF lower bounds. The supplied "lbnd" array should have at least
*        "ndim" elements.
*     ubnd
*        NDF upper bounds. The supplied "ubnd" array should have at least
*        "ndim" elements.
*     form
*        Pointer to a null terminated string holding array storage form
*        (case insensitive).
*     form_length
*        The length of the supplied 'form' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  This function converts a storage form of "PRIMITIVE" to "SIMPLE"
*     if any of the NDF's lower bounds are not equal to 1.

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
   int i;                /* Loop counter for dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* See if the storage form is primitive. */
   if( astChrMatch( form, "PRIMITIVE" ) ) {

/* If so, then loop to check the lower bound of each dimension. */
      for( i = 0; i < ndim; i++ ){

/* If any lower bound is not 1, then convert the storage form to simple. */
         if( lbnd[ i ] != 1 ) {
            star_strlcpy( form, "SIMPLE", form_length );
            break;
         }
      }

/* See if the storage form is scaled or delta. If so, convert to simple. */
   } else if( astChrMatch( form, "SCALED" ) || astChrMatch( form, "DELTA" ) ) {
      star_strlcpy( form, "SIMPLE", form_length );

   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Cbfrm", status );

}

