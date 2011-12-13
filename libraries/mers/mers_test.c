
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
*     Starlink ANSI C

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
#include <stdio.h>

int main ( void ) {
  int exstat = EXIT_SUCCESS;
  int status = SAI__OK;
  int perc = 0;
  char curlev[MSG__SZLEV];

  /* msgOut */
  msgBell( &status );
  msgBlank( &status );
  msgSetc( "TOK", "(SETC)" );
  msgOut( " ", "MSG is installed and %%working - ^TOK - ^PF "
          "< missing token", &status );

  msgOut(" ", "STATUS token check: ^STATUS", &status);

  msgFmt( "D", "%g", 536.7 );
  msgFmt( "C", "%s", "formatted" );
  msgOut(" ", "msgFmt: '^C' and '^D'", &status );

  msgSetc( "PC", "(token with % per cent)");
  msgOutf(" ", "Formatted int with leading zeroes: %05d ^PC", &status, 52);

  msgOutif( MSG__VERB, " ", "Should not see this message", &status );
  msgIfset( MSG__VERB, &status );
  msgOutif( MSG__VERB, " ", "Should be blank line after this", &status );
  msgBlankif( MSG__VERB, &status );
  msgOutif( MSG__VERB, " ", "Should see this verbose message", &status );
  msgOutiff( MSG__VERB, " ", "Formatted %05d conditional message and embedded per cent: %% ", &status,
             42);

  msgTune( "STREAM", 1, &status );

  for (perc = 0; perc < 100; perc += 10 ) {
    msgOutiff( MSG__VERB, "", "%3d %% done[9D[A",
               &status, perc );
  }
  msgTune( "STREAM", 0, &status );
  msgOutiff( MSG__VERB, "", "%3d %% done", &status, perc);

  msgIfset( MSG__NONE, &status );
  (void)msgIflev( curlev, &status );
  printf( "Current level= %s\n", curlev );
  msgOutif( MSG__QUIET, " ", "Should not see this message", &status );
  msgIfset( MSG__ALL, &status );
  (void)msgIflev( curlev, &status );
  printf( "Current level= %s\n", curlev );
  msgOutif( MSG__DEBUG20," ", "Should see this message", &status );


  /* ERR */
  errMark();
  msgSetc("PC", "Single % token");
  status = SAI__ERROR;
  errRepf( " ", "Formatted error message %s - ^PC", &status,
           "text via sprintf");
  errRlse();

  status = SAI__OK;
  errMark();
  msgBlank( &status );

  status = SAI__ERROR;
  errRep(" ", "Test flush", &status );
  errRep(" ", "Line 2 of flush but this one has to be a bit longer so that we can force a word wrap", &status );
  errFlush( &status );

  msgBlank( &status );
  status = SAI__ERROR;
  errRep(" ", "Test flush via MSG", &status );
  errRep(" ", "Line 2 of flush but this one has to be a bit longer so that we can force a word wrap and it should be output by MSG", &status );
  msgFlusherr( &status );


  errRlse();

  msgTune( "ENVIRONMENT", 0, &status );

  return exstat;
}

