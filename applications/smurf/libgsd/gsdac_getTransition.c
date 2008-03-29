/*
*+
*  Name:
*     gsdac_getTransition

*  Purpose:
*     Determine the name of the molecular species and the transition.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_getTransition ( const gsdVars *gsdVars, char *molecule,
*                           char *transition, int *status );

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     molecule = char* (Given and Returned)
*        Name of the molecular species
*     transition = char* (Given and Returned)
*        Name of the transition line
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Checks the molecular transition line lookup table for the 
*     nearest frequency and retrieves the name of the molecular
*     species and transition.

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-03-28 (JB):
*        Original.

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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "gsdac.h"

#define FUNC_NAME "gsdac_getTransition"

void gsdac_getTransition ( const gsdVars *gsdVars, char *molecule,
                           char *transiti, int *status )
{

  /* Local variables */
  int binHigh;                /* high index for binary search */
  int binIndex;               /* index of closest frequency */
  int binLow;                 /* low index for binary search */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Do a binary search on the transition lookup table to find the closest
     transition. */
  binHigh = TRANSITIONS_SIZE - 1;
  binLow = 0;

  /* First, check to make sure that our frequency lies within the 
     boundaries of our lookup table. */
  if ( gsdVars->restFreqs[0] * 1000.0 < transitions[0].freq ) {
 
    strcpy ( molecule, transitions[0].molecule );
    strcpy ( transiti, transitions[0].transiti );

  } else if ( gsdVars->restFreqs[0] * 1000.0 > 
              transitions[TRANSITIONS_SIZE - 1].freq ) {
 
    strcpy ( molecule, transitions[TRANSITIONS_SIZE - 1].molecule );
    strcpy ( transiti, transitions[TRANSITIONS_SIZE - 1].transiti );

  } else {

    while ( binLow <= binHigh ) {

      binIndex = ( binLow + binHigh ) / 2;

      if ( transitions[binIndex].freq > gsdVars->restFreqs[0] * 1000.0 )
        binHigh = binIndex - 1;

      else if ( transitions[binIndex].freq < gsdVars->restFreqs[0] * 1000.0 )
        binLow = binIndex + 1;

    }

    /* Check to see which value is closest (this or the neighbouring
       values. */
    if ( binIndex > 0 && 
      fabs ( gsdVars->restFreqs[0] * 1000.0 - transitions[binIndex - 1].freq ) 
      < fabs ( gsdVars->restFreqs[0] * 1000.0 - transitions[binIndex].freq ) ) {

      binIndex = binIndex - 1;

    } else if ( binIndex < TRANSITIONS_SIZE - 1 &&
      fabs ( gsdVars->restFreqs[0] * 1000.0 - transitions[binIndex + 1].freq ) 
      < fabs ( gsdVars->restFreqs[0] * 1000.0 - transitions[binIndex].freq ) ) { 

      binIndex = binIndex + 1;

    } 

    strcpy ( molecule, transitions[binIndex].molecule );
    strcpy ( transiti, transitions[binIndex].transiti );  

  }

}

