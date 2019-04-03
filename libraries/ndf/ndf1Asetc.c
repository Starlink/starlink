#include "sae_par.h"
#include "ndf_ast.h"
#include "ndf1.h"

void ndf1Asetc( AstFrameSet *iast, const char *value, const char *attrib,
                int *status ){
/*
*+
*  Name:
*     ndf1Asetc

*  Purpose:
*     Set an AST_ character attribute value.

*  Synopsis:
*     void ndf1Asetc( AstFrameSet *iast, const char *value,
*                     const char *attrib, int *status )

*  Description:
*     This function calls "astSetC" to assign a character attribute value
*     to an AST_ Object. It is a simple wrap-up of this AST_ function with
*     the character parameter order swapped. This is done so that mapped
*     character data may be used for the attribute value.

*  Parameters:
*     iast
*        Pointer to the AST_ Object.
*     value
*        Pointer to a null terminated string holding the attribute vakue to
*        be assigned.
*     attrib
*        Pointer to a null terminated string holding the attribute name.
*     *status
*        The global status.

*  Notes:
*     - Trailing blanks are removed from the attribute value before use.

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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Assign the value. */
   astSetC( iast, attrib, value );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Asetc", status );

}

