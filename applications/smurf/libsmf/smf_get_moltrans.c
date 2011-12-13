/*
*+
*  Name:
*     smf_get_moltrans

*  Purpose:
*     Get the molecule name and transition for a rest frequency

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_get_moltrans( double restFreq, char **molecule,
*                       char **transition, int * status);

*  Arguments:
*     restFreq = double (Given)
*        Rest frequency in MHz.
*     molecule = char** (Given and Returned)
*        Pointer to the name of the molecular species.
*     transition = char** (Given and Returned)
*        Pointer to the name of the transition.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function uses the look up table of frequencies in
*     smf_moltrans.h to determine the most likely target transition
*     for a given rest frequency.  The name of the molecule and
*     transition of the closest frequency is returned.

*  Authors:
*     Jen Balfour (JAC)
*     Tim Jenness (JAC)
*     {enter_new_authors_here}

*  History:
*     2008-03-31 (JB):
*        Original.
*     2008-06-24 (TIMJ):
*        const goodness
*     {enter_further_changes_here}

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
*     {note_any_bugs_here}
*-
*/

/* System includes */
#include <stdio.h>
#include <string.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_moltrans.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_get_moltrans"

void smf_get_moltrans( double restFreq, const char **molecule,
                       const char **transition, int *status ) {

  /* Local variables. */
  size_t i;                      /* Loop counter */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* First check to make sure that the frequency is positive. */
  if ( restFreq < 0 ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Rest frequency must be positive", status);
  }

  /* First, check to see if the given rest frequency is less than
     the lowest one in the table. */
  if ( restFreq < transitions[0].freq ) {
    *molecule = &(transitions[0].molecule[0]);
    *transition = &(transitions[0].transiti[0]);
    return;
  }

  /* Go through the list of frequencies and find the closest one. */
  i = 1;

  while ( i < ( sizeof(transitions) / sizeof(molTrans) ) ) {

    if ( transitions[i].freq > restFreq ) {

      /* Check to see which was closer, this frequency or
         the previous one. */
      if ( fabs ( transitions[i].freq - restFreq ) <
           fabs ( transitions[i-1].freq - restFreq ) ) {

        *molecule = &(transitions[i].molecule[0]);
        *transition = &(transitions[i].transiti[0]);
        return;

      } else {

        *molecule = &(transitions[i-1].molecule[0]);
        *transition = &(transitions[i-1].transiti[0]);
        return;

      }

    }

    i++;

  }

  /* We got to the end of the list, so select the last
     transition. */
  *molecule = &(transitions[i-1].molecule[0]);
  *transition = &(transitions[i-1].transiti[0]);

}

