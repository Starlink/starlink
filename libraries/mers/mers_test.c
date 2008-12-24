
/*
*+
*  Name:
*     ERR_TEST

*  Purpose:
*     Test the installation of the ERR/MSG libraries.

*  Description:
*     Test calls to the ERR/MSG libraries.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.

*  Language:
*     Starlink ANSI C*  Licence:

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-SEP-2008 (TIMJ):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/

#include "merswrap.h"
#include "sae_par.h"

#include <stdlib.h>


int main ( void ) {
  int exstat = EXIT_SUCCESS;
  int status = SAI__OK;
  
  /* msgOut */
  msgBell( &status );
  msgBlank( &status );
  msgSetc( "TOK", "(SETC)" );
  msgOut( " ", "MSG is installed and %%working - ^TOK - ^PF", &status );
  
  msgOut(" ", "STATUS token check: ^STATUS", &status);

  msgFmt( "D", "%g", 536.7 );
  msgFmt( "C", "%s", "formatted" );
  msgOut(" ", "msgFmt: '^C' and '^D'", &status );

  msgOutif( MSG__VERB, " ", "Should not see this message", &status );
  msgIfset( MSG__VERB, &status );
  msgOutif( MSG__VERB, " ", "Should see this verbose message", &status );

  return exstat;
}

