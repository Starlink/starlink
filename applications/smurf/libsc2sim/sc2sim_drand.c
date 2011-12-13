/*
 *+
 *  Name:
 *     sc2sim_drand

 *  Purpose:
 *     Return a random number with zero mean

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_drand ( double sigma )

 *  Arguments:
 *     sigma = double (Given)
 *        Sigma of distribution

 *  Description:
 *     Generate a double random number from a population with zero mean
 *     and given sigma and a bell-shaped distribution.

 *  Authors:
 *     B.D.Kelly (bdk@roe.ac.uk)
 *     {enter_new_authors_here}

 *  History :
 *     2001-07-19 (BDK):
 *        Original
 *     2002-08-10 (BDK):
 *        C version
 *     2006-07-20 (JB):
 *        Split from dsim.c

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
#include <stdlib.h>

/* SC2SIM includes */
#include "sc2sim.h"

double sc2sim_drand
(
 double sigma          /* sigma of distribution (given) */
 )

{
  /* Local variables */
  double value;        /* random number generated */
  double tvalue;       /* intermediate result */

  /* Obtain random number from integer function, adding three results
     together to shape the distribution. */

  tvalue = (double)rand() + (double)rand() + (double)rand();

  /* tvalue is now in the range 0 to 3*RAND_MAX with sigma 0.5*RAND_MAX */

  tvalue = 2.0 * ( ( tvalue / (double)RAND_MAX ) - 1.5 );

  value = sigma * tvalue;

  return value;

}



