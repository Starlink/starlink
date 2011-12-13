/*
*+
*  Name:
*     gsdac_freeArrays

*  Purpose:
*     Free data allocated for GSD data arrays.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_freeArrays ( const dasFlag dasFlag, gsdVars *gsdVars,
*                        int *status );

*  Arguments:
*     dasFlag = const dasFlag (Given)
*        DAS file structure type
*     gsdVars = gsdVars* (Given and returned)
*        GSD headers and array data
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Frees memory allocated for GSD data arrays.

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-02-13 (JB):
*        Original.
*     2008-02-19 (JB):
*        Check dasFlag
*     2008-03-19 (JB):
*        Include mers.h

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
*     {note_any_bugs_here}
*-
*/

/* STARLINK includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "gsdac.h"
#include "libsmf/smf.h"

#define FUNC_NAME "gsdac_freeArrays.c"

void gsdac_freeArrays ( const dasFlag dasFlag, gsdVars *gsdVars,
                        int *status )
{

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  if ( dasFlag == DAS_CROSS_CORR ) {
    astFree( gsdVars->FEFreqs );
    astFree( gsdVars->FESBSigns );
    astFree( gsdVars->FELOFreqs );
  }

  if ( dasFlag == DAS_NONE || dasFlag == DAS_CONT_CAL ) {
    astFree( gsdVars->vRadial );
  }

  astFree( gsdVars->scanVars1 );
  astFree( gsdVars->scanVars2 );
  astFree( gsdVars->scanTable1 );
  astFree( gsdVars->scanTable2 );
  astFree( gsdVars->mapTable );
  astFree( gsdVars->phaseVars );
  astFree( gsdVars->phaseTable );
  astFree( gsdVars->corrModes );
  astFree( gsdVars->bitModes );
  astFree( gsdVars->sbOverlaps );
  astFree( gsdVars->mixNums );
  astFree( gsdVars->BEInputChans );
  astFree( gsdVars->BEConnChans );
  astFree( gsdVars->BEChans );
  astFree( gsdVars->BESubsys );
  astFree( gsdVars->centreFreqs );
  astFree( gsdVars->restFreqs );
  astFree( gsdVars->LOFreqs );
  astFree( gsdVars->totIFs );
  astFree( gsdVars->sbSigns );
  astFree( gsdVars->BEInputFreqs );
  astFree( gsdVars->freqRes );
  astFree( gsdVars->bandwidths );
  astFree( gsdVars->recTemps );
  astFree( gsdVars->sourceSysTemps );
  astFree( gsdVars->skyTemps );
  astFree( gsdVars->telTemps );
  astFree( gsdVars->gains );
  astFree( gsdVars->calTemps );
  astFree( gsdVars->opacities );
  astFree( gsdVars->skyTrans );
  astFree( gsdVars->alphas );
  astFree( gsdVars->sbGainNorms );
  astFree( gsdVars->telTrans );
  astFree( gsdVars->FETSkyIm );
  astFree( gsdVars->FESkyTrans );
  astFree( gsdVars->FETSysIm );
  astFree( gsdVars->sbRatios );
  astFree( gsdVars->intTimes );
  astFree( gsdVars->data );

  if ( dasFlag == DAS_CROSS_CORR ) {
    astFree( gsdVars->hotPower );
    astFree( gsdVars->skyPower );
    astFree( gsdVars->samples );
  }

  if ( dasFlag == DAS_CROSS_CORR || dasFlag == DAS_TP ) {
    astFree( gsdVars->totPower );
  }

  if ( *status != SAI__OK ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Error freeing GSD data arrays", status );
    return;
  }


}
