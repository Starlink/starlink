/*
*+
*  Name:
*     gsdac_getWCS.c

*  Purpose:
*     Determines the time and pointing values for each 
*     time step in the observation.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_getWCS ( const gsdVars *gsdVars, const int nSteps,
*                    gsdWCS *wcs, int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     nSteps = const int (Given)
*        Number of steps in the observation
*     wcs = gsdWCS* (Given and Returned)
*        Time and Pointing values
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*    This routine calculates the pointing, time, and airmass 
*    values to fill the JCMTState. NOTE: adequate memory for the
*    arrays must be allocated prior to calling this function.

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-02-14 (JB):
*        Original

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     Many of the values are currently kludged with defaults.
*     These are indicated by //k.
*-
*/

/* Standard includes */
#include <string.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"

/* SMURF includes */
#include "gsdac.h"

#define FUNC_NAME "gsdac_getWCS.c"

void gsdac_getWCS ( const gsdVars *gsdVars, const int nSteps,
                    gsdWCS *wcs, int *status )

{

  /* Local variables */
  int i;                      /* loop counter */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  for ( i = 0; i < nSteps; i++ ) {

    /* Kludges for now... */
    wcs[i].airmass = 0.0;
    wcs[i].acAz = 0.0;
    wcs[i].acEl = 0.0;
    wcs[i].acTr1 = 0.0;
    wcs[i].acTr2 = 0.0;    
    wcs[i].azAng = 0.0;  
    wcs[i].baseAz = 0.0;
    wcs[i].baseEl = 0.0;  
    wcs[i].baseTr1 = 0.0;
    wcs[i].baseTr2 = 0.0;
    wcs[i].index = 0.0;
    wcs[i].tai = 0.0;
    wcs[i].trAng = 0.0;

  }  

}
