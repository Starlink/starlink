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
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

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
    smf_free ( gsdVars->FEFreqs, status );
    smf_free ( gsdVars->FESBSigns, status );
    smf_free ( gsdVars->FELOFreqs, status );
  }

  if ( dasFlag == DAS_NONE || dasFlag == DAS_CONT_CAL ) {
    smf_free ( gsdVars->vRadial, status );
  }

  smf_free ( gsdVars->scanVars1, status );
  smf_free ( gsdVars->scanVars2, status );
  smf_free ( gsdVars->scanTable1, status );
  smf_free ( gsdVars->scanTable2, status );
  smf_free ( gsdVars->mapTable, status );
  smf_free ( gsdVars->phaseVars, status );
  smf_free ( gsdVars->phaseTable, status );
  smf_free ( gsdVars->corrModes, status );
  smf_free ( gsdVars->bitModes, status );
  smf_free ( gsdVars->sbOverlaps, status );
  smf_free ( gsdVars->mixNums, status );
  smf_free ( gsdVars->BEInputChans, status );
  smf_free ( gsdVars->BEConnChans, status );
  smf_free ( gsdVars->BEChans, status );
  smf_free ( gsdVars->BESubsys, status );
  smf_free ( gsdVars->centreFreqs, status );
  smf_free ( gsdVars->restFreqs, status );
  smf_free ( gsdVars->LOFreqs, status );
  smf_free ( gsdVars->totIFs, status );
  smf_free ( gsdVars->sbSigns, status );
  smf_free ( gsdVars->BEInputFreqs, status );
  smf_free ( gsdVars->freqRes, status );
  smf_free ( gsdVars->bandwidths, status );
  smf_free ( gsdVars->recTemps, status );
  smf_free ( gsdVars->sourceSysTemps, status );
  smf_free ( gsdVars->skyTemps, status );
  smf_free ( gsdVars->telTemps, status );
  smf_free ( gsdVars->gains, status );
  smf_free ( gsdVars->calTemps, status );
  smf_free ( gsdVars->opacities, status );
  smf_free ( gsdVars->skyTrans, status );
  smf_free ( gsdVars->alphas, status );
  smf_free ( gsdVars->sbGainNorms, status );
  smf_free ( gsdVars->telTrans, status );
  smf_free ( gsdVars->FETSkyIm, status );
  smf_free ( gsdVars->FESkyTrans, status );
  smf_free ( gsdVars->FETSysIm, status );
  smf_free ( gsdVars->sbRatios, status );
  smf_free ( gsdVars->intTimes, status );
  smf_free ( gsdVars->data, status );

  if ( dasFlag == DAS_CROSS_CORR ) {
    smf_free ( gsdVars->hotPower, status );
    smf_free ( gsdVars->skyPower, status );
    smf_free ( gsdVars->samples, status );
  }

  if ( dasFlag == DAS_CROSS_CORR || dasFlag == DAS_TP ) {
    smf_free ( gsdVars->totPower, status );
  }

  if ( *status != SAI__OK ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Error freeing GSD data arrays", status );
    return;
  }


}
