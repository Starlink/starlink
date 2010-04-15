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

  /* Check for any spacing greater than 150 MHz. */
  for ( i = 1; i < gsdVars->nBESections; i++ ) {

    if ( 1000.0 * fabs ( centreFreqs[i] - centreFreqs[i - 1] ) > 150.0 ) {

      i = gsdVars->nBESections;
      special = 1;
      msgOutif(MSG__VERB," ",
      	       "This appears to be a special configuration", status);

    }

  }

  /* If this is flagged as a special configuration, set each of the
     IFFreqs to the corresponding value found in the GSD BES_TOT_IF
     array.  Otherwise, set all IFFreqs to 4.0 GHz. */
  if ( special ) {

    for ( i = 0; i < gsdVars->nBESections; i++ ) {

      IFFreqs[i] = gsdVars->totIFs[i];
      lineFreqs[i] = gsdVars->centreFreqs[i];

    }

  } else {

    for ( i = 0; i < gsdVars->nBESections; i++ ) {

      /* MPI frontends used an IF of 3.5 GHz, other frontends
         used 4.0GHz. */
      if ( strncmp ( gsdVars->frontend, "MPI", 3 ) == 0 )
        IFFreqs[i] = 3.5 * gsdVars->sbSigns[i];
      else
        IFFreqs[i] = 4.0 * gsdVars->sbSigns[i];

      lineFreqs[i] = gsdVars->restFreqs[i];

    }

  }

}
