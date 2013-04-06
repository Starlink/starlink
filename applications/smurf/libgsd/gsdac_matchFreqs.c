/*
*+
*  Name:
*     gsdac_matchFreqs.c

*  Purpose:
*     Checks for special configurations in order to determine
*     which subbands should share a rest frequency and IF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_matchFreqs ( const gsdVars *gsdVars, double *lineFreqs,
*                        double *IFFreqs, int *status )

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD file access parameters.
*     lineFreqs = double* (Given and Returned)
*        Line frequency of molecular transition.
*     IFFreqs = double* (Given and Returned)
*        Array of IFFreqs to be associated with each subband.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine checks the spacing of the frequencies in the
*     subbands to see if this is a special configuration.  It then
*     determines which subbands should share a common IFFreq and
*     what it should be for those subbands.  Each subband's IFFreq
*     is then stored in the IFFreqs array.  In order to determine
*     the correct rest frequency for the molecular transition,
*     the same matching is performed.
*
*     NOTE: currently this routine only determines whether all
*     subbands belong together or not.  It does not deal with
*     observation in which some BUT NOT ALL subbands belong
*     together.  In its current form, matchFreqs is an
*     all-or-nothing method.

*  Authors:
*     J.Balfour (UBC)
*     V. Tilanus (JAC)
*     MJC: Malcolm J. Currie (JAC)
*     {enter_new_authors_here}

*  History :
*     2008-04-18 (JB):
*        Original.
*     2008-04-23 (JB):
*        Fix broken sorting algorithm, set boundary value to 150 MHz.
*     2008-04-23 (JB):
*        Transform gsdac_checkSpecial into generic subband-matching
*        routine.
*     2008-04-30 (JB):
*        Change IF for MPI frontend.
*     2010-07-01 (VT):
*        Simplify IF freq logic
*     2010-07-22 (VT):
*        Added a check the the freq logic to deal with special cases.
*     2013 April 4 (MJC):
*        The line frequencies become the subband central frequencies
*        for special data.

*  Copyright:
*     Copyright (C) 2008, 2010, 2013  Science and Technology Facilities
*     Council.
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
*     NOTE: currently this routine only determines whether all
*     subbands belong together or not.  It does not deal with
*     observation in which some BUT NOT ALL subbands belong
*     together.  In its current form, matchFreqs is an
*     all-or-nothing method.
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

#define FUNC_NAME "gsdac_matchFreqs"

void gsdac_matchFreqs ( const gsdVars *gsdVars, double *lineFreqs,
                        double *IFFreqs, int *status )

{

  /* Local variables */
  double centreFreqs[16];     /* Array of frequencies to sort */
  double sortFreq;            /* Current frequency being sorted */
  int i;                      /* Loop counter */
  int j;                      /* Loop counter */
  int special;                /* Flag for special configurations */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  special = 0;

  /* Copy the frequencies into the sorting array. */
  for ( i = 0; i < gsdVars->nBESections; i++ ) {
    centreFreqs[i] = gsdVars->centreFreqs[i];
  }

  /* Sort the frequencies from lowest to highest. */
  for ( i = 1; i < gsdVars->nBESections; i++ ) {

    sortFreq = centreFreqs[i];

    for ( j = i - 1; j >= 0 && centreFreqs[j] > sortFreq; j-- ) {
      centreFreqs[j + 1] = centreFreqs[j];
    }

    centreFreqs[j + 1] = sortFreq;

  }

  /* Check the index of each spectrum to see if their values are the same
   * or different. If they are the same then we have a special case, If they
   * they are different we don't have a special case unless the spacing is
   * geater then 150 MHz. */

  i = 1;
  while ( special == 0 && i < gsdVars->nBESections ) {
    if ( gsdVars->BESubsys[i] != gsdVars->BESubsys[i-1] ) {
      special = 1;
    }
    i++;
  }

  /* Check for any spacing greater than 150 MHz. */
  if (special == 1) {
    special = 0; /* Assume it is wideband and set it back to special if there
                  * is no overlap */
    for ( i = 1; i < gsdVars->nBESections; i++ ) {
      if ( 1000.0 * fabs ( centreFreqs[i] - centreFreqs[i - 1] ) > 150.0 ) {
        i = gsdVars->nBESections;
        special = 1;
        msgOutif(MSG__VERB," ",
      	         "This appears to be a special configuration", status);
      }
    }
  }


  /* Set IF frequency for each subsystem */

  for ( i = 0; i < gsdVars->nBESections; i++ ) {

     IFFreqs[i] = gsdVars->totIFs[i];
     if ( special == 0 ) {
       gsdVars->BESubsys[i] = 0;
       lineFreqs[i] = gsdVars->restFreqs[i];
     } else {
       lineFreqs[i] = gsdVars->centreFreqs[i];
     }
  }

}
