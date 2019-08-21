#include <stdlib.h>
#include "sae_par.h"
#include "ndf_ast.h"
#include "star/util.h"

void ndf1Gtenv( const char *name, int *def, char *val, size_t val_length,
                size_t *lval, int *status ){
/*
*+
*  Name:
*     ndf1Gtenv

*  Purpose:
*     Translate an environment variable.

*  Synopsis:
*     void ndf1Gtenv( const char *name, int *def, char *val,
*                     size_t val_length, size_t *lval, int *status )

*  Description:
*     This function translates an environment variable, returning a logical
*     value to indicate if a translation exists. If it does, the translated
*     value and its length are also returned.

*  Parameters:
*     name
*        Pointer to a null terminated string holding the name of the
*        environment variable to be translated.
*     *def
*        Returned holding the returns non-zero if a translation exists,
*        otherwise zero.
*     val
*        Pointer to an array in which to return a null terminated string
*        holding the translation value, if defined.
*     val_length
*        The length of the supplied 'val' array. This should include
*        room for the terminating null.
*     *lval
*        The number of significant characters in the translation value
*        (i.e. ignoring trailing blanks). A value of zero is returned if no
*        translation is defined.
*     *status
*        The global status.

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

/* Local variables; */
   const char *c;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   *def = 0;
   *lval = 0;

/* Attempt to translate the environment variable. */
   c = getenv( name );

/* If translation succeeded, copy it into the supplied buffer, note the
   environment variable is defined and find the length of its translation. */
   if( c ) {
      star_strlcpy( val, c, val_length );
      *def = 1;
      *lval = astChrLen( c );
   }

}

