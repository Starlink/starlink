/*
*+
*  Name:
*     gsdac_flagBad

*  Purpose:
*     Flag GSD bad values with STARLINK VAL__BAD values.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_flagBad ( const dasFlag dasFlag, gsdVars *gsdVars,
*                     int *status );

*  Arguments:
*     dasFlag = const dasFlag (Given)
*        DAS file type
*     gsdVars = gsdac_gsdVars_struct* (Given and returned)
*        GSD headers and array data
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Checks GSD headers and arrays for the GSD bad value, then
*     replaces bad values with the STARLINK bad value.

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-04-21 (JB):
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
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <string.h>
#include <stdio.h>
#include <math.h>

/* STARLINK includes */
#include "sae_par.h"
#include "prm_par.h"
#include "mers.h"

/* SMURF includes */
#include "gsdac.h"

#define FUNC_NAME "gsdac_flagBad.c"

void gsdac_flagBad ( const dasFlag dasFlag, gsdVars *gsdVars,
                     int *status )
{

  /* Local variables.*/
  long i;                     /* loop counter */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Replace GSD bad values with Starlink bad values. */
  if ( dasFlag == DAS_CROSS_CORR ) {

    for ( i = 0; i < gsdVars->nFEChans; i++ ) {
      if ( gsdVars->FEFreqs[i] == gsdVars->badVal )
        gsdVars->FEFreqs[i] = VAL__BADD;
      if ( gsdVars->FESBSigns[i] == gsdVars->badVal )
        gsdVars->FESBSigns[i] = VAL__BADI;
      if ( gsdVars->FELOFreqs[i] == gsdVars->badVal )
        gsdVars->FELOFreqs[i] = VAL__BADD;
    }

  }

  if ( dasFlag == DAS_NONE || dasFlag == DAS_CONT_CAL ) {

    for ( i = 0; i < gsdVars->nVRad; i++ ) {
      if ( gsdVars->vRadial[i] == gsdVars->badVal )
        gsdVars->vRadial[i] = VAL__BADD;
    }

  }

  for ( i = 0; i < gsdVars->nScanVars1 * gsdVars->noScans; i++ ) {
    if ( gsdVars->scanTable1[i] == gsdVars->badVal )
      gsdVars->scanTable1[i] = VAL__BADR;
    if ( gsdVars->scanTable2[i] == gsdVars->badVal )
      gsdVars->scanTable2[i] = VAL__BADR;
    }

  for ( i = 0; i < gsdVars->nMapDims * gsdVars->nMapPts; i++ ) {
    if ( gsdVars->mapTable[i] == gsdVars->badVal )
      gsdVars->mapTable[i] = VAL__BADR;
  }

  for ( i = 0; i < gsdVars->nPhaseVars * gsdVars->nPhases; i++ ) {
    if ( gsdVars->phaseTable[i] == gsdVars->badVal )
      gsdVars->phaseTable[i] = VAL__BADR;
  }

  for ( i = 0; i < gsdVars->nBESections; i++ ) {

    if ( gsdVars->corrModes[i] == gsdVars->badVal )
      gsdVars->corrModes[i] = VAL__BADI;
    if ( gsdVars->bitModes[i] == gsdVars->badVal )
      gsdVars->corrModes[i] = VAL__BADI;
    if ( gsdVars->sbOverlaps[i] == gsdVars->badVal )
      gsdVars->sbOverlaps[i] = VAL__BADR;
    if ( gsdVars->mixNums[i] == gsdVars->badVal )
      gsdVars->mixNums[i] = VAL__BADI;
    if ( gsdVars->BEInputChans[i] == gsdVars->badVal )
      gsdVars->BEInputChans[i] = VAL__BADI;
    if ( gsdVars->BEChans[i] == gsdVars->badVal )
      gsdVars->BEChans[i] = VAL__BADI;
    if ( gsdVars->BESubsys[i] == gsdVars->badVal )
      gsdVars->BESubsys[i] = VAL__BADI;
    if ( gsdVars->centreFreqs[i] == gsdVars->badVal )
      gsdVars->centreFreqs[i] = VAL__BADD;
    if ( gsdVars->restFreqs[i] == gsdVars->badVal )
      gsdVars->restFreqs[i] = VAL__BADD;
    if ( gsdVars->LOFreqs[i] == gsdVars->badVal )
      gsdVars->LOFreqs[i] = VAL__BADD;
    if ( gsdVars->totIFs[i] == gsdVars->badVal )
      gsdVars->totIFs[i] = VAL__BADD;
    if ( gsdVars->sbSigns[i] == gsdVars->badVal )
      gsdVars->sbSigns[i] = VAL__BADI;
    if ( gsdVars->freqRes[i] == gsdVars->badVal )
      gsdVars->freqRes[i] = VAL__BADR;
    if ( gsdVars->bandwidths[i] == gsdVars->badVal )
      gsdVars->bandwidths[i] = VAL__BADR;
    if ( gsdVars->recTemps[i] == gsdVars->badVal )
      gsdVars->recTemps[i] = VAL__BADR;
    if ( gsdVars->telTemps[i] == gsdVars->badVal )
      gsdVars->telTemps[i] = VAL__BADR;
    if ( gsdVars->gains[i] == gsdVars->badVal )
      gsdVars->gains[i] = VAL__BADR;
    if ( gsdVars->calTemps[i] == gsdVars->badVal )
      gsdVars->calTemps[i] = VAL__BADR;
    if ( gsdVars->opacities[i] == gsdVars->badVal )
      gsdVars->opacities[i] = VAL__BADR;
    if ( gsdVars->alphas[i] == gsdVars->badVal )
      gsdVars->alphas[i] = VAL__BADR;
    if ( gsdVars->sbGainNorms[i] == gsdVars->badVal )
      gsdVars->sbGainNorms[i] = VAL__BADR;
    if ( gsdVars->telTrans[i] == gsdVars->badVal )
      gsdVars->telTrans[i] = VAL__BADR;

    if ( dasFlag != DAS_CONT_CAL ) {

      if ( gsdVars->sourceSysTemps[i] == gsdVars->badVal )
        gsdVars->sourceSysTemps[i] = VAL__BADR;
      if ( gsdVars->skyTemps[i] == gsdVars->badVal )
        gsdVars->skyTemps[i] = VAL__BADR;
      if ( gsdVars->skyTrans[i] == gsdVars->badVal )
        gsdVars->skyTrans[i] = VAL__BADR;
      if ( gsdVars->FETSkyIm[i] == gsdVars->badVal )
        gsdVars->FETSkyIm[i] = VAL__BADR;
      if ( gsdVars->FESkyTrans[i] == gsdVars->badVal )
        gsdVars->FESkyTrans[i] = VAL__BADR;
      if ( gsdVars->FETSysIm[i] == gsdVars->badVal )
        gsdVars->FETSysIm[i] = VAL__BADR;
      if ( gsdVars->sbRatios[i] == gsdVars->badVal )
        gsdVars->sbRatios[i] = VAL__BADR;

    }

  }

  if ( dasFlag == DAS_CONT_CAL ) {

    for ( i = 0; i < gsdVars->nBESections * gsdVars->noScans; i++ ) {
      if ( gsdVars->sourceSysTemps[i] == gsdVars->badVal )
        gsdVars->sourceSysTemps[i] = VAL__BADR;
      if ( gsdVars->skyTemps[i] == gsdVars->badVal )
        gsdVars->skyTemps[i] = VAL__BADR;
      if ( gsdVars->skyTrans[i] == gsdVars->badVal )
        gsdVars->skyTrans[i] = VAL__BADR;
      if ( gsdVars->FETSkyIm[i] == gsdVars->badVal )
        gsdVars->FETSkyIm[i] = VAL__BADR;
      if ( gsdVars->FESkyTrans[i] == gsdVars->badVal )
        gsdVars->FESkyTrans[i] = VAL__BADR;
      if ( gsdVars->FETSysIm[i] == gsdVars->badVal )
        gsdVars->FETSysIm[i] = VAL__BADR;
      if ( gsdVars->sbRatios[i] == gsdVars->badVal )
        gsdVars->sbRatios[i] = VAL__BADR;
    }

  }

  for ( i = 0; i < gsdVars->nBEChansIn; i++ ) {
    if ( gsdVars->BEConnChans[i] == gsdVars->badVal )
      gsdVars->BEConnChans[i] = VAL__BADI;
    if ( gsdVars->BEInputFreqs[i] == gsdVars->badVal )
      gsdVars->BEInputFreqs[i] = VAL__BADD;
  }

  for ( i = 0; i < gsdVars->noScans; i++ ) {
    if ( gsdVars->intTimes[i] == gsdVars->badVal )
      gsdVars->intTimes[i] = VAL__BADI;
  }

  for ( i = 0; i < gsdVars->nBEChansOut  * gsdVars->nScanPts  *
	gsdVars->noScans; i++ ) {
    if ( gsdVars->data[i] == gsdVars->badVal )
      gsdVars->data[i] = VAL__BADR;
  }

  if ( dasFlag == DAS_CROSS_CORR ) {

    for ( i = 0; i < gsdVars->nBESections * gsdVars->IFPerSection; i++ ) {
      if ( gsdVars->hotPower[i] == gsdVars->badVal )
        gsdVars->hotPower[i] = VAL__BADR;
      if ( gsdVars->skyPower[i] == gsdVars->badVal )
        gsdVars->skyPower[i] = VAL__BADR;
    }

    for ( i = 0; i < gsdVars->nBEChansOut * gsdVars->IFONPhase *
	  gsdVars->noCycles; i++ ) {
      if ( gsdVars->samples[i] == gsdVars->badVal )
        gsdVars->samples[i] = VAL__BADR;
    }

    for ( i = 0; i < gsdVars->nBESections * gsdVars->IFPerSection *
	  gsdVars->IFONPhase * gsdVars->noCycles; i++ ) {
      if ( gsdVars->totPower[i] == gsdVars->badVal )
        gsdVars->totPower[i] = VAL__BADR;
    }

  }

  if ( dasFlag == DAS_TP ) {

    for ( i = 0; i < gsdVars->nBESections * gsdVars->IFPerSection *
	  gsdVars->IFONPhase *
          gsdVars->noCycles; i++ ) {
      if ( gsdVars->totPower[i] == gsdVars->badVal )
        gsdVars->totPower[i] = VAL__BADR;
    }

  }

}
