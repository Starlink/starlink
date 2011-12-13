/*
*+
*  Name:
*     gsdac_velEncode.c

*  Purpose:
*     Routine to encode contents of LSRFLG.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_velEncode ( const char *vframe, const char *vdef,
                        int *LSRFlg, int *status )

*  Arguments:
*     vframe = const char* (Given)
*        Velocity reference frame
*     vdef = const char* (Given)
*        Velocity definition code
*     LSRFlg = int* (Given and Returned)
*        LSR flag
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*    Encodes contents of LSRFLG given velocity frame and velocity law
*    used to observe data: Must be consistent with longstanding usage.
*
*    The assumption is that the LO frequency offsets were made to
*    bring a souce *at velocity VRAD* in the nominated frame to the
*    centre of the spectrum.
*
*    C version of specx routine velencode.f

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-02-26 (JB):
*        Original
*     2008-03-19 (JB):
*        Removed unused variables.
*     2010-06-25 (TIMJ):
*        Use strncasecmp

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     All Rights Reserved.

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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*-
*/

/* Standard includes */
#include <strings.h>
#include <ctype.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "gsdac.h"
#include "smurf_par.h"

#define FUNC_NAME "gsdac_velEncode.c"

void gsdac_velEncode ( const char *vframe, const char *vdef,
                       int *LSRFlg, int *status )

{

  /* Local variables */
  int i;                      /* index for vdef */
  int j;                      /* index for vframe */
  int k;                      /* loop counter */
  const char *vdefs[] = { "", "RAD", "OPT", "REL" };
  const char *vframes[] = { "", "TOPO", "LSR", "HELI", "GEO", "BARY", "TELL" };

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* compare input with values in vdefs */
  i = 0;
  for (k=1; k<=3; k++) {
    if (strncasecmp( vdef, vdefs[k], 3 ) == 0 ) {
      i = k;
      break;
    }
  }

  if ( i == 0 ) {
    msgOut ( FUNC_NAME,
             "Did not understand velocity definition string, assuming RADIO.",
             status );
    i = 1;
  }


  /* compare input with values in vframes */
  j = 0;
  for (k=1; k<=6; k++) {
    if (strncasecmp( vframe, vframes[k], 3 ) == 0 ) {
      j = k;
      break;
    }
  }

  /* Equate BARYCENTRIC with GEOCENTRIC. */
  if ( j == 5 ) j = 4;

  /* Equsate TELLURIC with TOPOCENTRIC. */
  if ( j == 6 ) j = 1;

  if ( j == 0 ) {
    msgOut ( FUNC_NAME,
             "Did not understand velocity frame string, assuming LSR.",
             status );
    j = 2;
  }

  /* Calculate the LSRFlg. */
  *LSRFlg = 16 * ( i - 1 ) + ( j - 1 );

}
