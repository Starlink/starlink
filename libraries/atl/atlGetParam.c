
#include "atl.h"

#include "ast.h"
#include "par.h"
#include "sae_par.h"

#define MAXVAL 100   /* Maximum length of a vector-valued parameter */
#define MAXLEN 255   /* Maximum length of an individual formatted value */

void atlGetParam( const char *param, AstKeyMap *keymap, int *status ){
/*
*+
*  Name:
*     atlGetParam

*  Purpose:
*     Create a vector character string entry in an AST KeyMap from a list
*     of fixed length strings.

*  Language:
*     C.

*  Invocation:
*     void atlGetParam( const char *param, AstKeyMap *keymap, int *status )

*  Description:
*     This function obtains a value for a named environment parameter and
*     stores it in the supplied KeyMap, using the parameter name as the
*     key.

*  Arguments:
*     param
*        The parameter name.
*     keymap
*        A pointer to an existing KeyMap.
*     status
*        Pointer to the global status variable.

*  Notes:
*     - An error will be reported if the parameter value obtained from
*     the environment is a vector with more than 100 elements.
*     - An error will be reported if any individual element in the parameter
*     value obtained from the environment requires more than 255 when
*     represented as a string.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry  (JAC, HAwaii)
*     {enter_new_authors_here}

*  History:
*     16-APR-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   char *ptrs[ MAXVAL ];          /* Pointers to formatted parameter values */
   char buff[ MAXVAL ][ MAXLEN ]; /* Array of formatted parameter values */
   int *old_status;               /* Original AST status pointer */
   int actval;                    /* Number of parameter values supplied */
   int i;                         /* Element index */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status pointer. */
   old_status = astWatch( status );

/* Store a set of pointers to the buffers used to store the formatted
   parameter values. */
   for( i = 0; i < MAXVAL; i++ ) ptrs[ i ] = buff[ i ];

/* Get the parameter values as an array of strings. */
   parGet1c( param, MAXVAL, ptrs, MAXLEN, &actval, status );

/* If only one value was supplied, store it as a scalar entry in the
   KeyMap, using the parameter name as the key. */
   if( actval == 1 ) {
      astMapPut0C( keymap, param, ptrs[ 0 ], NULL );

/* If more than one value was supplied, store them as a vector entry in the
   KeyMap, using the parameter name as the key. */
   } else {
      astMapPut1C( keymap, param, actval, (const char **) ptrs, NULL );
   }

/* Revert to using the old AST status pointer. */
   astWatch( old_status );
}

