/*
 *+
 *  Name:
 *     slaTest

 *  Purpose:
 *     Test C interface to SLA

 *  Language:
 *     Starlink ANSI C

 *  Description:
 *     Provides a simple test of the C interface. Test coverage is not
 *     complete because not all Fortran routines have wrappers.

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Engineering Research Council

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
 *     07-AUG-2006 (TIMJ):
 *       Original version.

 *-
 */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "slalib.h"

#if HAVE_FC_MAIN
void FC_MAIN ( void );
void FC_MAIN ( ) {}
#endif


int main ( void ) {
  double w, p, h;
  char telname[41];
  char telshort[11];
  int  exstatus = EXIT_SUCCESS;


  /* call slaObs - initialise to recognisable state */
  w = 0.0; p = 0.0; h = -1.0;

  /* first call by short name */
  slaObs( 0, "JCMT", telname, &w, &p, &h );
  if ( h == -1.0 ) {
    printf( "Error obtaining information on JCMT\n");
    exstatus = EXIT_FAILURE;
  } else {
    printf( "Telescope JCMT is '%s' w = %f, p = %f, h = %f\n",
	    telname, w, p, h);
  }

  /* call by index */
  h = -1.0; w = 0.0; p = 0.0;
  slaObs( 1, telshort, telname, &w, &p, &h );
  if (h == -1.0 ) {
    printf( "Error obtaining information on telescope 1\n");
    exstatus = EXIT_FAILURE;
  } else {
    printf( "Telescope 1 is '%s' aka '%s' w = %f, p = %f, h = %f\n",
	    telshort, telname, w, p, h);
  }

  /* deliberately fail - with bad index */
  h = -1.0; w = 0.0; p = 0.0; strcpy( telshort, "unknown" );
  slaObs( 100000, telshort, telname, &w, &p, &h );
  if (h != -1.0 || telname[0] != '?') {
    printf("Attempt to decode unfeasibly large telescope index should have failed\n");
    printf("Got this result:  Tel: '%s' aka '%s', w=%f p=%f h=%f\n", telshort,
	   telname, w, p, h);
    exstatus = EXIT_FAILURE;
  }

  /* deliberately fail - with bad name */
  h = -1.0; w = 0.0; p = 0.0;
  slaObs( 0, "AFakeTel", telname, &w, &p, &h );
  if (h != -1.0 || telname[0] != '?') {
    printf("Attempt to decode unknown telescope should have failed\n");
    printf("Got this result:  Tel: '%s', w=%f p=%f h=%f\n", telname, w, p, h);
    exstatus = EXIT_FAILURE;
  }

  return exstatus;
}
