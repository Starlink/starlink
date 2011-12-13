/*
*+
*  Name:
*     gsdac_putSpecHdr

*  Purpose:
*     Fill the ACSISSpecHdr.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_putSpecHdr ( const gsdVars *gsdVars, const int nSteps,
*                        const unsigned int stepNum, const int subBandNum,
*                        const int recepFlags[],
*                        const dasFlag dasFlag, const JCMTState *record,
*                        struct ACSISSpecHdr *specHdr, int *status );

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     nSteps = const int (Given)
*        Number of steps in the observation
*     stepNum = const unsigned int (Given)
*        Time step of this spectrum
*     subBandNum = const int (Given)
*        Subband number
*     recepFlags = const int[] (Given)
*        Flags for which receptors were used
*     dasFlag = const dasFlag (Given)
*        DAS file structure type
*     record = const JCMTState* (Given)
*        JCMTState headers
*     specHdr = struct ACSISSpecHdr* (Given and Returned)
*        ACSIS meta data structure
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Fills the ACSISSpecHdr with values retrieved from the
*     GSD file.

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-01-28 (JB):
*        Original
*     2008-02-14 (JB):
*        Use gsdVars struct to store headers/arrays
*     2008-02-22 (JB):
*        Fill feed and trx, correct tsys for DAS_CONT_CAL
*     2008-02-28 (JB):
*        Replace subsysNum with subBandNum
*     2008-03-19 (JB):
*        Removed unused variables.
*     2008-03-28 (JB):
*        Check which receptors were used.
*     2008-04-03 (JB):
*        Convert acs_feedx and acs_feedy to degrees.


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
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     Currently kludged with default values.
*-
*/

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "ast.h"
#include "sae_par.h"

/* SMURF includes */
#include "gsdac.h"
#include "jcmt/state.h"

#define FUNC_NAME "gsdac_putSpecHdr"

#define SZ_RECNAME 80
#define MAXRECEP 8

void gsdac_putSpecHdr ( const gsdVars *gsdVars, const unsigned int nSteps,
                        const unsigned int stepNum, const int subBandNum,
                        const int recepFlags[],
                        const dasFlag dasFlag, const JCMTState *record,
                        struct ACSISSpecHdr *specHdr, int *status )
{

  /* Local variables. */
  int index;                  /* index into array */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Check for continuous calibration to get the correct index
     into the array of source system temperatures. */
  if ( dasFlag == DAS_CONT_CAL ) {
    index = ( stepNum * gsdVars->nBESections ) + subBandNum  ;
  } else {
    index = subBandNum  ;
  }

  /* Fill the specHdr. */
  specHdr->rts_endnum = nSteps + 1;
  specHdr->acs_feedx = record->tcs_tr_ac1 / AST__DD2R;
  specHdr->acs_feedy = record->tcs_tr_ac2 / AST__DD2R;

  /* Check to see if only the "second" receptor was used. */
  if ( recepFlags[0] == 0 && recepFlags[1] == 1 )
    specHdr->acs_feed = gsdVars->mixNums[subBandNum] - 2;
  else
    specHdr->acs_feed = gsdVars->mixNums[subBandNum] - 1;

  specHdr->acs_tsys = gsdVars->sourceSysTemps[index];
  specHdr->acs_trx = gsdVars->recTemps[subBandNum];

}
