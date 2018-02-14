#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>

#include "dat1.h"


/*
*+
* Name:
*   dat1Getenv

* Purpose:
*    Obtain an integer value from an environment variable.

* Invocation:
*    dat1Getenv( varname, def, val );

* Description:
*    This function obtains the value of a specified environment variable
*    as an integer. If the variable is not defined, or its value does not
*    make sense as an integer, or any other error occurs, then a default
*    value is returned instead.

* Parameters:
*    const char *varname
*       Pointer to a null-terminated character string giving the name of
*       the environment variable. The use of upper case is recommended.
*    int def
*       The default value to be used if an integer value cannot be
*       obtained from the environment variable.
*    int *val
*       Pointer to an integer in which the result will be returned.

* Notes:
*    This routine does not perform error checking.

* Authors:
*    RFWS: R.F. Warren-Smith (STARLINK)
*    {@enter_new_authors_here@}

* History:
*    25-FEB-1992 (RFWS):
*       Original version.
*    {@enter_changes_here@}

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program.  If not, see <http://www.gnu.org/licenses/>.

*-
*/
void dat1Getenv( const char *varname, int def, int *val ) {

  /* Local Variables: */
  char *txt;                 /* Pointer to translation value */

  /* Obtain a pointer to the environment variable's value. */
  txt = getenv( varname );

  /* If no value was obtained, then use the default. */
  if ( txt == NULL ) {

    *val = def;

    /* Otherwise, try to read an integer value from it. If unsuccessful, then   */
    /* use the default. */
  } else if ( sscanf( txt, "%d", val ) != 1 ) {
    *val = def;
  }

  return;
}
