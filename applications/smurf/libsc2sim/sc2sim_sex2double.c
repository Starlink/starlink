/*
 *+
 *  Name:
 *     sc2sim_sex2double.c

 *  Purpose:
 *     Convert a sexagesimal string into a double

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_sex2double ( char *string, double *value, int *status )

 *  Arguments:
 *     string = char* (Given)
 *        Sexagesimal string to be converted
 *     value = double* (Returned)
 *        Double conversion of sexagesimal string
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Parse a sexagesimal string a convert it to a double.

 *  Authors:
 *     B.D.Kelly (bdk@roe.ac.uk)
 *     J. Balfour (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2005-05-12 (BDK) : Original
 *     2006-09-15 (JB): Converted from dxml_ to sc2sim_

 *  Copyright:
 *     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
 *     Council. University of British Columbia. All Rights Reserved.

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
 *     {note_any_bugs_here}
 *-
 */

/* Standard includes */
#include <string.h>
#include <math.h>
#include <stdlib.h>

#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"

#include "sc2sim.h"

void sc2sim_sex2double ( char *string, double *value, int *status )

{
  /* Local variables */
  double a;             /* temporary value */
  double b;             /* temporary value */
  double c;             /* temporary value */
  int j;                /* loop counter */
  char *p;              /* pointer into work string */
  int sign;             /* sign of value */
  char *t;              /* pointer to string after translation */
  static char w[64];    /* work string */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Copy into the work string, replacing ':' by space */
  *value = 0.0;
  a = 0.0;
  b = 0.0;
  c = 0.0;
  sign = 1;

  for ( j=0; j<64; j++ ) {
    w[j] = string[j];
    if ( w[j] == ':' ) w[j] = ' ';
    if ( w[j] == 0 ) break;
  }

  w[63] = 0;

  /* Set the pointer to the beginning of the string and
     retrieve the first value */
  p = w;
  a = strtod ( p, &t );

  /* Check to make sure the pointer was incremented */
  if ( t == p ) {
    *status = DITS__APP_ERROR;
  } else {

    if ( a < 0 ) {
      a = fabs(a);
      sign = -1;
    }

    /* Set the pointer to the next part of the sexagesimal
       string and get the value */
    p = t;
    b = strtod ( p, &t );

    /* If this is the last value, set the rest to zero. */
    if ( t == p )      {
      b = 0.0;
    } else {

      /* Set the pointer to the next part of the sexagesimal
         string and get the value */
      p = t;
      c = strtod ( p, &t );

      /* If this is the last value, set the rest to zero. */
      if ( t == p ) {
        c = 0.0;
      }
    }

    /* Calculate the signed version of the sexagesimal number */
    *value = sign * ( a + b/60.0 + c/3600.0 );
  }

}
