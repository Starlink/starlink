#include "sae_par.h"
#include "cupid.h"
#include "star/pda.h"

float cupidRanVal( int dist, float p[2], int *status ){
/*
*+
*  Name:
*     cupidRanVal

*  Purpose:
*     Return a random sample from a uniform, normal or poisson distribution.

*  Language:
*     Starlink C

*  Synopsis:
*     float cupidRanVal( int dist, float p[2], int *status )

*  Description:
*     This function returns a random sample from a uniform or normal
*     distribution.

*  Parameters:
*     normal
*        0 - Use a uniform distribution
*        1 - Use a normal distribution
*        2 - Use a Poisson distribution
*     p
*        Element zero contains the mean of the distribution, element one
*        contains the width (i.e. the standard deviation for a normal
*        distribution, or the half-width for a uniform distribution). The
*        second element is ignored when using a Poisson distribution.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     The model random value.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     31-OCT-2005 (DSB):
*        Original version.
*     22-OCT-2009 (DSB):
*        Add support for Poisson distribution.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Constants: */
#define PMEAN 1.0E8

/* Local Variables: */
   float ret;             /* Returned value */

/* Initialise */
   ret = p[ 0 ];

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Get the required sample. */
   if( dist == 1 ) {
      ret = pdaRnnor( p[ 0 ], p[ 1 ] );
   } else if( dist == 2 ) {
      ret = p[ 0 ]*( pdaRnpoi( PMEAN )/PMEAN );
   } else {
      ret = p[ 0 ] + 2*( pdaRand() - 0.5 )*p[ 1 ];
   }

/* Return the required value */
   return ret;

}
