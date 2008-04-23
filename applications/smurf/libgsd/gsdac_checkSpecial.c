/*
*+
*  Name:
*     gsdac_checkSpecial.c

*  Purpose:
*     Check to see if this is a special configuration.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_checkSpecial ( const gsdVars *gsdVars, int *special, 
*                          int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD file access parameters.
*     special = int* (Given and Returned)
*        Flag for special configurations.
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     This routine checks the spacing of the frequencies in the 
*     subbands to see if this is a special configuration.  

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-04-18 (JB):
*        Original.
*     2008-04-23 (JB):
*        Fix broken sorting algorithm, set boundary value to 150 MHz.

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
*-
*/

/* Standard includes */
#include <string.h>
#include <ctype.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "gsdac.h"

#define FUNC_NAME "gsdac_checkSpecial"

void gsdac_checkSpecial ( const gsdVars *gsdVars, int *special, 
                          int *status )

{

  /* Local variables */
  double centreFreqs[16];     /* Array of frequencies to sort */
  double freq;                /* Current frequency being sorted */
  int i;                      /* Loop counter */
  int j;                      /* Loop counter */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  *special = 0;

  /* Copy the frequencies into the sorting array. */
  for ( i = 0; i < gsdVars->nBESections; i++ ) {
    centreFreqs[i] = gsdVars->centreFreqs[i];
  }

  /* Sort the frequencies from lowest to highest. */
  for ( i = 1; i < gsdVars->nBESections; i++ ) {
 
    freq = centreFreqs[i];

    for ( j = i - 1; j >= 0 && centreFreqs[j] > freq; j-- ) {
      centreFreqs[j + 1] = centreFreqs[j];
    }

    centreFreqs[j + 1] = freq;

  }

  /* Check for any spacing greater than 125 MHz. */
  for ( i = 1; i < gsdVars->nBESections; i++ ) {

    if ( 1000.0 * fabs ( centreFreqs[i] - centreFreqs[i - 1] ) > 150.0 ) {

      i = gsdVars->nBESections;
      *special = 1;
      msgOutif(MSG__VERB," ", 
      	       "This appears to be a special configuration", status); 

    }

  }  

}
