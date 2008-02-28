/*
*+
*  Name:
*     gsdac_getGSDVars
*  Purpose:
*     Get all the GSD header and array data.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_getGSDVars ( const gsdac_gsd_struct *gsd,
*                        const dasFlag dasFlag,
*                        struct gsdac_gsdVars_struct *gsdVars,
*                        int *status );

*  Arguments:
*     gsd = const gsdac_gsd_struct* (Given)
*        GSD file access parameters
*     dasFlag = const dasFlag (Given)
*        DAS file type
*     gsdVars = gsdac_gsdVars_struct* (Given and returned)
*        GSD headers and array data 
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Retrieves all GSD headers and data arrays.

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-02-12 (JB):
*        Original.
*     2008-02-19 (JB):
*        Check dasFlag, get CROSS_CORR and TP data
*     2008-02-28 (JB):
*        Replace GSD bad values with Starlink bad values

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
#include "prm_par.h"

/* SMURF includes */
#include "gsdac.h"
#include "libsmf/smf.h"

#define FUNC_NAME "gsdac_getGSDVars.c"

void gsdac_getGSDVars ( const struct gsdac_gsd_struct *gsd,
                        const dasFlag dasFlag,
                        struct gsdac_gsdVars_struct *gsdVars,
                        int *status )
{

  /* Local variables.*/
  long i;                     /* loop counter */
  long j;                     /* loop counter */  

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the telescope name. */
  gsdac_get0c ( gsd, "C1TEL", gsdVars->telName, status );

  /* Get the Project name. */
  gsdac_get0c ( gsd, "C1PID", gsdVars->project, status );

  /* Get the Project details. */
  gsdac_get0c ( gsd, "C1OBS", gsdVars->projectObs1, status );
  gsdac_get0c ( gsd, "C1ONA1", gsdVars->projectObs2, status );
  gsdac_get0c ( gsd, "C1ONA2", gsdVars->projectObs3, status );

  /* Get the Object name. */
  gsdac_get0c ( gsd, "C1SNA1", gsdVars->object1, status );
  gsdac_get0c ( gsd, "C1SNA2", gsdVars->object2, status );

  /* Get the centre coordinate frame. */
  gsdac_get0c ( gsd, "C4CSC", gsdVars->centreCoords, status );

  /* Get the centre code. */
  gsdac_get0i ( gsd, "C4CECO", &(gsdVars->centreCode), status );

  /* Get the epoch type. */
  gsdac_get0c ( gsd, "C4EPT", gsdVars->epochType, status );
  
  /* Get the centre moving flag. */
  gsdac_get0l ( gsd, "C4MCF", &(gsdVars->centreMoving), status );

  /* Get the centre epoch. */
  gsdac_get0d ( gsd, "C4EPH", &(gsdVars->centreEpoch), status );
 
  /* Get the RA/Dec 1950 of the centre. */
  gsdac_get0d ( gsd, "C4ERA", &(gsdVars->centreRA1950), status );
  gsdac_get0d ( gsd, "C4EDEC", &(gsdVars->centreDec1950), status );

  /* Get the RA/Dec of the centre. */
  gsdac_get0d ( gsd, "C4RADATE", &(gsdVars->centreRA), status );
  gsdac_get0d ( gsd, "C4DECDATE", &(gsdVars->centreDec), status );

  /* Get the RA/Dec 2000 of the centre. */
  gsdac_get0d ( gsd, "C4RA2000", &(gsdVars->centreRA2000), status );
  gsdac_get0d ( gsd, "C4EDEC2000", &(gsdVars->centreDec2000), status );

  /* Get the Galactic longitude & latitude. */
  gsdac_get0d ( gsd, "C4GL", &(gsdVars->centreGL), status );
  gsdac_get0d ( gsd, "C4GB", &(gsdVars->centreGB), status );

  /* Get the Az/El at observation date. */
  gsdac_get0d ( gsd, "C4AZ", &(gsdVars->centreAz), status );
  gsdac_get0d ( gsd, "C4EL", &(gsdVars->centreEl), status );

  /* Get the cell coordinate frame. */
  gsdac_get0c ( gsd, "C4LSC", gsdVars->cellCoords, status );

  /* Get the cell code. */
  gsdac_get0i ( gsd, "C6FC", &(gsdVars->cellCode), status );

  /* Get the units of cell and mapping. */
  gsdac_get0c ( gsd, "C4ODCO", gsdVars->cellUnit, status );

  /* Get the size of the cells. */
  gsdac_get0d ( gsd, "C6DX", &(gsdVars->cellX), status );
  gsdac_get0d ( gsd, "C6DY", &(gsdVars->cellY), status );  

  /* Get the angles of the cells. */
  gsdac_get0d ( gsd, "C6MSA", &(gsdVars->cellV2X), status );
  gsdac_get0d ( gsd, "CELL_V2Y", &(gsdVars->cellV2Y), status );
  gsdac_get0d ( gsd, "C4AXY", &(gsdVars->cellX2Y), status );

  /* Cet the centre offsets. */
  gsdac_get0d ( gsd, "C4SX", &(gsdVars->centreOffsetX), status );
  gsdac_get0d ( gsd, "C4SY", &(gsdVars->centreOffsetY), status );

  /* Get the reference position offsets. */ 
  gsdac_get0d ( gsd, "C4RX", &(gsdVars->referenceX), status );
  gsdac_get0d ( gsd, "C4RY", &(gsdVars->referenceY), status );

  /* Get the location of the telescope. */
  gsdac_get0d ( gsd, "C1HGT", &(gsdVars->telHeight), status );
  gsdac_get0d ( gsd, "C1LONG", &(gsdVars->telLongitude), status );
  gsdac_get0d ( gsd, "C1LAT", &(gsdVars->telLatitude), status );

  /* Get the observation number. */
  gsdac_get0d ( gsd, "C1SNO", &(gsdVars->nObs), status );

  /* Get the observation type. */
  gsdac_get0c ( gsd, "C6ST", gsdVars->obsType, status );

  /* Get the frontend and backend. */
  gsdac_get0c ( gsd, "C1RCV", gsdVars->frontend, status );
  gsdac_get0c ( gsd, "C1FTYP", gsdVars->FEType, status );   
  gsdac_get0c ( gsd, "C1BKE", gsdVars->backend, status );
  gsdac_get0c ( gsd, "C1BTYP", gsdVars->BEType, status ); 

  /* Get the UT1 date and hour and UT1-UTC correction. */
  gsdac_get0d ( gsd, "C3DAT", &(gsdVars->obsUT1d), status );
  gsdac_get0d ( gsd, "C3UT", &(gsdVars->obsUT1h), status );
  gsdac_get0d ( gsd, "C3UT1C", &(gsdVars->obsUT1C), status );

  /* Get the LST at start of observation. */
  gsdac_get0d ( gsd, "C3LST", &(gsdVars->obsLST), status );

  /* Get the observation type flags. */
  gsdac_get0l ( gsd, "C3CAL", &(gsdVars->obsCalibration), status );
  gsdac_get0l ( gsd, "C3CEN", &(gsdVars->obsCentre), status );
  gsdac_get0l ( gsd, "C3FLY", &(gsdVars->obsContinuous), status );
  gsdac_get0l ( gsd, "C3FOCUS", &(gsdVars->obsFocus), status );
  gsdac_get0l ( gsd, "C3MAP", &(gsdVars->obsMap), status );

  /* Get the map parameters. */
  gsdac_get0i ( gsd, "C3NPP", &(gsdVars->nMapDims), status );
  gsdac_get0i ( gsd, "C3NMAP", &(gsdVars->nMapPts), status );
  gsdac_get0i ( gsd, "C6XNP", &(gsdVars->nMapPtsX), status );
  gsdac_get0i ( gsd, "C6YNP", &(gsdVars->nMapPtsY), status );
  gsdac_get0r ( gsd, "C6XGC", &(gsdVars->mapStartX), status );
  gsdac_get0r ( gsd, "C6YGC", &(gsdVars->mapStartY), status );  

  /* Get the scan reversal flag and scan direction. */
  gsdac_get0l ( gsd, "C6REV", &(gsdVars->scanRev), status );
  gsdac_get0c ( gsd, "C6SD", gsdVars->obsDirection, status );   

  /* Get the flags for direction of row/column increases. */
  gsdac_get0l ( gsd, "C6XPOS", &(gsdVars->mapPosX), status );
  gsdac_get0l ( gsd, "C6YPOS", &(gsdVars->mapPosY), status );

  /* Get the scan parameters. */
  gsdac_get0i ( gsd, "C3NIS", &(gsdVars->noScans), status );
  gsdac_get0i ( gsd, "C3NSAMPLE", &(gsdVars->nScan), status );
  gsdac_get0i ( gsd, "C3NO_SCAN_VARS1", &(gsdVars->nScanVars1), status );
  gsdac_get0i ( gsd, "C3NO_SCAN_VARS2", &(gsdVars->nScanVars2), status );
  gsdac_get0i ( gsd, "C3SRT", &(gsdVars->scanTime), status );
  gsdac_get0i ( gsd, "C3MXP", &(gsdVars->nScanPts), status );

  /* Get the cycle & phase parameters. */
  gsdac_get0i ( gsd, "C3NCI", &(gsdVars->noCycles), status );
  gsdac_get0i ( gsd, "C3NCYCLE", &(gsdVars->nCycle), status );
  gsdac_get0i ( gsd, "C3CL", &(gsdVars->cycleTime), status );
  gsdac_get0i ( gsd, "C3NCP", &(gsdVars->noCyclePts), status );
  gsdac_get0i ( gsd, "C6NP", &(gsdVars->nCyclePts), status );
  gsdac_get0i ( gsd, "C3NSV", &(gsdVars->nPhaseVars), status );
  gsdac_get0i ( gsd, "C3PPC", &(gsdVars->nPhases), status );

  /* Get environment variables. */
  gsdac_get0d ( gsd, "C5AT", &(gsdVars->tamb), status );
  gsdac_get0d ( gsd, "C5PRS", &(gsdVars->pamb), status );  
  gsdac_get0d ( gsd, "C5RH", &(gsdVars->hamb), status );

  /* Get the Az/El offsets. */
  gsdac_get0d ( gsd, "C4AZERR", &(gsdVars->errAz), status );
  gsdac_get0d ( gsd, "C4ELERR", &(gsdVars->errEl), status );
  gsdac_get0d ( gsd, "UAZ", &(gsdVars->userAz), status );
  gsdac_get0d ( gsd, "UEL", &(gsdVars->userEl), status );

  /* Get the number of elements in the vradial array. */
  gsdac_get0i ( gsd, "C7SZVRAD", &(gsdVars->nVRad), status );

  /* Get the efficiencies. */
  gsdac_get0d ( gsd, "C8AAE", &(gsdVars->apertureEff), status );
  gsdac_get0d ( gsd, "C8ABE", &(gsdVars->beamEff), status );
  gsdac_get0d ( gsd, "C8GN", &(gsdVars->antennaGain), status );
  gsdac_get0d ( gsd, "C8EL", &(gsdVars->etal), status );
  gsdac_get0d ( gsd, "C8EF", &(gsdVars->etafss), status );

  /* Get the chopping parameters. */
  gsdac_get0l ( gsd, "C4SM", &(gsdVars->chopping), status );
  gsdac_get0c ( gsd, "C4FUN", gsdVars->chopWaveform, status );
  gsdac_get0r ( gsd, "C4FRQ", &(gsdVars->chopFrequency), status );
  gsdac_get0c ( gsd, "C4SMCO", gsdVars->chopCoords, status );
  gsdac_get0r ( gsd, "C4THROW", &(gsdVars->chopThrow), status );
  gsdac_get0r ( gsd, "C4POSANG", &(gsdVars->chopPA), status );

  /* Get the SMU parameters. */
  gsdac_get0r ( gsd, "C4OFFS_EW", &(gsdVars->smuOffsEW), status );
  gsdac_get0r ( gsd, "C4OFFS_NS", &(gsdVars->smuOffsNS), status );
  gsdac_get0r ( gsd, "C4X", &(gsdVars->smuX), status ); 
  gsdac_get0r ( gsd, "C4Y", &(gsdVars->smuY), status ); 
  gsdac_get0r ( gsd, "C4Z", &(gsdVars->smuZ), status ); 
  gsdac_get0r ( gsd, "C4EW_SCALE", &(gsdVars->smuEWScale), status ); 
  gsdac_get0r ( gsd, "C4NS_SCALE", &(gsdVars->smuNSScale), status );
  gsdac_get0i ( gsd, "C4EW_ENCODER", &(gsdVars->smuEWEnc), status );
  gsdac_get0i ( gsd, "C4NS_ENCODER", &(gsdVars->smuNSEnc), status );
  gsdac_get0r ( gsd, "C2FV", &(gsdVars->smuDX), status );
  gsdac_get0r ( gsd, "C2FL", &(gsdVars->smuDY), status );
  gsdac_get0r ( gsd, "C2FR", &(gsdVars->smuDZ), status );

  /* Get the telescope mount coordinates. */
  gsdac_get0c ( gsd, "C4MOCO", gsdVars->telCoords, status );

  /* Get the number of frontend output channels. */
  gsdac_get0i ( gsd, "C3NFOC", &(gsdVars->nFEChans), status );

  /* Get the radial velocity of the source. */
  gsdac_get0d ( gsd, "C7VR", &(gsdVars->velocity), status );

  /* Get the load temperatures. */
  gsdac_get0r ( gsd, "C12TCOLD", &(gsdVars->tCold), status );
  gsdac_get0r ( gsd, "C12TAMB", &(gsdVars->tHot), status );

  /* Get the velocity definitions. */
  gsdac_get0c ( gsd, "C12VDEF", gsdVars->velDefn, status );
  gsdac_get0c ( gsd, "C12VREF", gsdVars->velRef, status );

  /* Get the number of backend channels and sections. */
  gsdac_get0i ( gsd, "C3NRC", &(gsdVars->nBEChansIn), status );
  gsdac_get0i ( gsd, "C3NCH", &(gsdVars->nBEChansOut), status );
  gsdac_get0i ( gsd, "C3NRS", &(gsdVars->nBESections), status );

  /* Get the bad channel value. */
  gsdac_get0r ( gsd, "C7BCV", &(gsdVars->badVal), status );

  /* Get the units of the data. */
  gsdac_get0c ( gsd, "C12CAL", gsdVars->dataUnits, status );

  /* Get the switch mode. */
  gsdac_get0c ( gsd, "C6MODE", gsdVars->swMode, status );

  /* Get the calibration parameters. */
  gsdac_get0c ( gsd, "C12CALTASK", gsdVars->calInstrument, status );
  gsdac_get0c ( gsd, "C12CALTYPE", gsdVars->calType, status );
  gsdac_get0c ( gsd, "C12REDMODE", gsdVars->calMode, status );

  /* Get the number of IF inputs per to each section. */
  gsdac_get0i ( gsd, "C3NOIFPBES", &(gsdVars->IFPerSection), status );

  /* Get the backend configuration. */
  gsdac_get0i ( gsd, "C3CONFIGNR", &(gsdVars->BEConfig), status );

  /* Get the description of the data. */
  gsdac_get0c ( gsd, "C3DASOUTPUT", gsdVars->dataOutput, status );

  /* Get the calibration source. */
  gsdac_get0c ( gsd, "C3DASCALSRC", gsdVars->calSource, status );

  /* ?? */
  gsdac_get0r ( gsd, "C3DASSHFTFRAC", &(gsdVars->shiftFrac), status );

  /* Get the CSO Tau parameters. */
  gsdac_get0r ( gsd, "C7TAU225", &(gsdVars->tau225), status );
  gsdac_get0r ( gsd, "C7TAURMS", &(gsdVars->tauRMS), status );
  gsdac_get0c ( gsd, "C7TAUTIME", gsdVars->tauTime, status );

  /* Get the seeing parameters. */
  gsdac_get0r ( gsd, "C7SEEING", &(gsdVars->seeing), status );
  gsdac_get0c ( gsd, "C7SEETIME", gsdVars->seeTime, status );

  /* Get the polarity? */
  gsdac_get0c ( gsd, "C3POLARITY", gsdVars->polarity, status );

  /* Get the sideband mode. */
  gsdac_get0c ( gsd, "C3SBMODE", gsdVars->sbMode, status );

  if ( dasFlag == DAS_TP || dasFlag == DAS_CROSS_CORR ) {

    /* Get the interferometry observation values. */
    gsdac_get0i ( gsd, "C55NPH", &(gsdVars->IFONPhase), status );

    /* Get the data processing values. */
    gsdac_get0i ( gsd, "C55DASPRBIT", &(gsdVars->procBits), status );
    gsdac_get0c ( gsd, "C55DASPRLOC", gsdVars->procLoc, status );

  }

  if ( dasFlag == DAS_CROSS_CORR ) {

    /* Get the cross correlation values. */
    gsdac_get0i ( gsd, "C55NCYC", &(gsdVars->IFONCycle), status );
    gsdac_get0i ( gsd, "C55NINT", &(gsdVars->IFONIntCycle), status );
    gsdac_get0i ( gsd, "C55NCORR", &(gsdVars->nCorrCycle), status );
    gsdac_get0d ( gsd, "C55LX", &(gsdVars->RXJLengthX), status );
    gsdac_get0d ( gsd, "C55LY", &(gsdVars->RXJLengthY), status );
    gsdac_get0d ( gsd, "C55LZ", &(gsdVars->RXJLengthZ), status );
    gsdac_get0d ( gsd, "C55A", &(gsdVars->RXJSin), status );
    gsdac_get0d ( gsd, "C55B", &(gsdVars->RXJCos), status );
    gsdac_get0d ( gsd, "C55C", &(gsdVars->RXJConstant), status );
    gsdac_get0i ( gsd, "C55CSOSW", &(gsdVars->RXJCSOSwitch), status );
    gsdac_get0i ( gsd, "C55JCMTSW", &(gsdVars->RXJJCMTSwitch), status );
    gsdac_get0i ( gsd, "C55SECOND", &(gsdVars->RXJNSecs), status );
    gsdac_get0c ( gsd, "C55ABSORB", gsdVars->CSOAbsorb, status );
    gsdac_get0r ( gsd, "C55TAU", &(gsdVars->CSOTau), status );
    gsdac_get0r ( gsd, "C55DAZ", &(gsdVars->CSODAz), status );
    gsdac_get0r ( gsd, "C55DEL", &(gsdVars->CSODEl), status );
    gsdac_get0d ( gsd, "C55RA", &(gsdVars->CSORA), status );
    gsdac_get0d ( gsd, "C55DEC", &(gsdVars->CSODec), status );
    gsdac_get0d ( gsd, "C55EPOCH", &(gsdVars->CSOEpoch), status );
    gsdac_get0r ( gsd, "C55PAZ", &(gsdVars->CSOPAz), status );
    gsdac_get0r ( gsd, "C55PEL", &(gsdVars->CSOPEl), status );
    gsdac_get0c ( gsd, "C55TRACK", gsdVars->CSOTrack, status );
    gsdac_get0c ( gsd, "C55FMODE", gsdVars->CSOFocus, status );
    gsdac_get0r ( gsd, "C55FX", &(gsdVars->CSOFocusX), status );
    gsdac_get0r ( gsd, "C55FY", &(gsdVars->CSOFocusY), status );
    gsdac_get0r ( gsd, "C55FZ", &(gsdVars->CSOFocusZ), status );
    gsdac_get0r ( gsd, "C55VLSR", &(gsdVars->CSOVelocity), status );
    gsdac_get0r ( gsd, "C55VOFF", &(gsdVars->CSOVelOffset), status );
    gsdac_get0r ( gsd, "C55VRAD", &(gsdVars->CSORadVel), status );
    gsdac_get0c ( gsd, "C55PLOCK", gsdVars->CSOPhaseLock, status );
    gsdac_get0d ( gsd, "C55RFREQ", &(gsdVars->CSORestFreq), status );
    gsdac_get0d ( gsd, "C55IFFREQ", &(gsdVars->CSOIFFreq), status );
    gsdac_get0d ( gsd, "C55FREQOFF", &(gsdVars->CSOFreqOffset), status );
    gsdac_get0c ( gsd, "C55SIDEBAND", gsdVars->CSOSideband, status );
    gsdac_get0i ( gsd, "C55MHN", &(gsdVars->CSOMultHarm), status );
    gsdac_get0i ( gsd, "C55CSOSTATUS", &(gsdVars->CSOStatus), status );
    gsdac_get0d ( gsd, "C55TELAZ", &(gsdVars->telAz), status );
    gsdac_get0d ( gsd, "C55TELEL", &(gsdVars->telEl), status );
  }

  if ( *status != SAI__OK ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Error getting scalar GSD headers", status );
    return;
  }

  /* Allocate memory for the GSD arrays. */

  if ( dasFlag == DAS_CROSS_CORR ) {
    gsdVars->FEFreqs = smf_malloc ( gsdVars->nFEChans, 
                                    sizeof(double), 0, status );
    gsdVars->FESBSigns = smf_malloc ( gsdVars->nFEChans, 
                                      sizeof(int), 0, status );   
    gsdVars->FELOFreqs = smf_malloc ( gsdVars->nFEChans, 
                                      sizeof(double), 0, status );   
  }

  if ( dasFlag == DAS_NONE || dasFlag == DAS_CONT_CAL ) {
    gsdVars->vRadial = smf_malloc ( gsdVars->nVRad, 
                                    sizeof(double), 0, status );
  }

  gsdVars->scanVars1 = smf_malloc ( gsdVars->nScanVars1, 
                                    MAXSTRING-1, 0, status );
  gsdVars->scanVars2 = smf_malloc ( gsdVars->nScanVars2, 
                                    MAXSTRING-1, 0, status );  
  gsdVars->scanTable1 = smf_malloc ( ( gsdVars->nScanVars1 * gsdVars->noScans ), 
                                     sizeof(float), 0, status );
  gsdVars->scanTable2 = smf_malloc ( ( gsdVars->nScanVars2 * gsdVars->noScans ), 
                                     sizeof(float), 0, status );
  gsdVars->mapTable = smf_malloc ( ( gsdVars->nMapDims * gsdVars->nMapPts ), 
                                   sizeof(float), 0, status );
  gsdVars->phaseVars = smf_malloc ( gsdVars->nPhaseVars, 
                                    MAXSTRING-1, 0, status );
  gsdVars->phaseTable = smf_malloc ( ( gsdVars->nPhaseVars * gsdVars->nPhases ),
                                     sizeof(float), 0, status );
  gsdVars->corrModes = smf_malloc ( gsdVars->nBESections, 
                                    sizeof(int), 0, status );
  gsdVars->bitModes = smf_malloc ( gsdVars->nBESections, 
                                   sizeof(int), 0, status );
  gsdVars->sbOverlaps = smf_malloc ( gsdVars->nBESections, 
                                     sizeof(float), 0, status );
  gsdVars->mixNums = smf_malloc ( gsdVars->nBESections, 
                                  sizeof(float), 0, status );
  gsdVars->BEInputChans = smf_malloc ( gsdVars->nBESections, 
                                       sizeof(int), 0, status );
  gsdVars->BEConnChans = smf_malloc ( gsdVars->nBEChansIn, 
                                      sizeof(int), 0, status );
  gsdVars->BEChans = smf_malloc ( gsdVars->nBESections, 
                                  sizeof(int), 0, status );
  gsdVars->BESubsys = smf_malloc ( gsdVars->nBESections, 
                                   sizeof(int), 0, status );
  gsdVars->centreFreqs = smf_malloc ( gsdVars->nBESections, 
                                      sizeof(double), 0, status );
  gsdVars->restFreqs = smf_malloc ( gsdVars->nBESections, 
                                    sizeof(double), 0, status );
  gsdVars->LOFreqs = smf_malloc ( gsdVars->nBESections, 
                                  sizeof(double), 0, status );
  gsdVars->totIFs = smf_malloc ( gsdVars->nBESections, 
                                 sizeof(double), 0, status );
  gsdVars->sbSigns = smf_malloc ( gsdVars->nBESections, 
                                  sizeof(int), 0, status );
  gsdVars->BEInputFreqs = smf_malloc ( gsdVars->nBEChansIn, 
                                       sizeof(double), 0, status );
  gsdVars->freqRes = smf_malloc ( gsdVars->nBESections, 
                                  sizeof(float), 0, status );
  gsdVars->bandwidths = smf_malloc ( gsdVars->nBESections,
                                     sizeof(float), 0, status );
  gsdVars->recTemps = smf_malloc ( gsdVars->nBESections, 
                                   sizeof(float), 0, status );

  if ( dasFlag == DAS_CONT_CAL ) {
    gsdVars->sourceSysTemps = smf_malloc ( gsdVars->nBESections * gsdVars->noScans, 
                                           sizeof(float), 0, status );
    gsdVars->skyTemps = smf_malloc ( gsdVars->nBESections * gsdVars->noScans, 
                                     sizeof(float), 0, status );
  } else {
    gsdVars->sourceSysTemps = smf_malloc ( gsdVars->nBESections, 
                                           sizeof(float), 0, status );
    gsdVars->skyTemps = smf_malloc ( gsdVars->nBESections, 
                                     sizeof(float), 0, status );
  }

  gsdVars->telTemps = smf_malloc ( gsdVars->nBESections, 
                                   sizeof(float), 0, status );
  gsdVars->gains = smf_malloc ( gsdVars->nBESections, 
                                sizeof(float), 0, status );
  gsdVars->calTemps = smf_malloc ( gsdVars->nBESections,
                                   sizeof(float), 0, status );
  gsdVars->opacities = smf_malloc ( gsdVars->nBESections, 
                                    sizeof(float), 0, status );

  if ( dasFlag == DAS_CONT_CAL ) {
    gsdVars->skyTrans = smf_malloc ( gsdVars->nBESections * gsdVars->noScans, 
                                     sizeof(float), 0, status );
  } else {
    gsdVars->skyTrans = smf_malloc ( gsdVars->nBESections, 
                                     sizeof(float), 0, status );
  }

  gsdVars->alphas = smf_malloc ( gsdVars->nBESections, 
                                 sizeof(float), 0, status );
  gsdVars->sbGainNorms = smf_malloc ( gsdVars->nBESections, 
                                      sizeof(float), 0, status );
  gsdVars->telTrans = smf_malloc ( gsdVars->nBESections, 
                                   sizeof(float), 0, status );

  if ( dasFlag == DAS_CONT_CAL ) {
    gsdVars->FETSkyIm = smf_malloc ( gsdVars->nBESections * gsdVars->noScans, 
                                     sizeof(float), 0, status );
    gsdVars->FESkyTrans = smf_malloc ( gsdVars->nBESections * gsdVars->noScans, 
                                       sizeof(float), 0, status );
    gsdVars->FETSysIm = smf_malloc ( gsdVars->nBESections * gsdVars->noScans, 
                                     sizeof(float), 0, status );
    gsdVars->sbRatios = smf_malloc ( gsdVars->nBESections * gsdVars->noScans,
                                     sizeof(float), 0, status );
  } else {
    gsdVars->FETSkyIm = smf_malloc ( gsdVars->nBESections, 
                                     sizeof(float), 0, status );
    gsdVars->FESkyTrans = smf_malloc ( gsdVars->nBESections, 
                                       sizeof(float), 0, status );
    gsdVars->FETSysIm = smf_malloc ( gsdVars->nBESections, 
                                     sizeof(float), 0, status );
    gsdVars->sbRatios = smf_malloc ( gsdVars->nBESections, 
                                     sizeof(float), 0, status );
  }

  gsdVars->intTimes = smf_malloc ( gsdVars->noScans, 
                                   sizeof(int), 0, status );
  gsdVars->data = smf_malloc ( ( gsdVars->nBEChansOut * gsdVars->nScanPts
                                 * gsdVars->noScans ), sizeof(float), 0, status );

  if ( dasFlag == DAS_CROSS_CORR ) {
    gsdVars->hotPower = smf_malloc ( gsdVars->nBESections * gsdVars->IFPerSection, 
                                     sizeof(float), 0, status );
    gsdVars->skyPower = smf_malloc ( gsdVars->nBESections * gsdVars->IFPerSection, 
                                     sizeof(float), 0, status );
    gsdVars->samples = smf_malloc ( gsdVars->nBEChansOut * gsdVars->IFONPhase *
                                    gsdVars->noCycles, sizeof(float), 0, status );
    gsdVars->totPower = smf_malloc ( gsdVars->nBESections * gsdVars->IFPerSection *
                                     gsdVars->IFONPhase * gsdVars->noCycles,
                                     sizeof(float), 0, status );
  } else if ( dasFlag == DAS_TP ) {
    gsdVars->totPower = smf_malloc ( gsdVars->nBESections * gsdVars->IFPerSection *
                                     gsdVars->IFONPhase * gsdVars->nPhases *
                                     gsdVars->noCycles, sizeof(float), 0, status );
  }

  if ( dasFlag == DAS_CROSS_CORR ) {
    gsdac_get1d ( gsd, "C55FENUOBS", gsdVars->FEFreqs, status );
    gsdac_get1i ( gsd, "C55FESBSIGN", gsdVars->FESBSigns, status );
    gsdac_get1d ( gsd, "C55FENULO", gsdVars->FELOFreqs, status );
  }

  if ( dasFlag == DAS_NONE || dasFlag == DAS_CONT_CAL ) {
    /* Get the radial velocities. */
    gsdac_get1d ( gsd, "C7VRADIAL", gsdVars->vRadial, status );
  }

  /* Get the scan table column names and data. */
  gsdac_get1c ( gsd, "C12SCAN_VARS1", gsdVars->scanVars1, status );
  gsdac_get1c ( gsd, "C12SCAN_VARS2", gsdVars->scanVars2, status );
  gsdac_get1r ( gsd, "C12SCAN_TABLE_1", gsdVars->scanTable1, status );
  gsdac_get1r ( gsd, "C12SCAN_TABLE_2", gsdVars->scanTable2, status );

  /* Get the map xy offsets. */
  gsdac_get1r ( gsd, "C14PHIST", gsdVars->mapTable, status );

  /* Get the phase table column names and data. */
  gsdac_get1c ( gsd, "C11VD", gsdVars->phaseVars, status );
  gsdac_get1r ( gsd, "C11PHA", gsdVars->phaseTable, status );

  /* Get the correlation arrays. */
  gsdac_get1i ( gsd, "C12CM", gsdVars->corrModes, status );
  gsdac_get1i ( gsd, "C12BM", gsdVars->bitModes, status );

  /* Get the subband overlaps. */
  gsdac_get1r ( gsd, "C3OVERLAP", gsdVars->sbOverlaps, status );

  /* ?? */
  gsdac_get1i ( gsd, "C3MIXNUM", gsdVars->mixNums, status );

  /* Get the number of BE input channels for each section. */
  gsdac_get1i ( gsd, "C3BESCONN", gsdVars->BEInputChans, status );
  
  /* Get the number of IF output channels connected to 
     BE input channels. */
  gsdac_get1i ( gsd, "C3BEINCON", gsdVars->BEConnChans, status );

  /* Get the number of channels per backend section. */
  gsdac_get1i ( gsd, "C3LSPC", gsdVars->BEChans, status );

  /* Get the subsystem number for each backend section. */
  gsdac_get1i ( gsd, "C3BESSPEC", gsdVars->BESubsys, status );

  /* Get the centre frequencies. */
  gsdac_get1d ( gsd, "C12CF", gsdVars->centreFreqs, status );

  /* Get the rest frequencies. */
  gsdac_get1d ( gsd, "C12RF", gsdVars->restFreqs, status );

  /* Get the frontend LO frequencies. */
  gsdac_get1d ( gsd, "C3BEFENULO", gsdVars->LOFreqs, status );

  /* Get the total IF for each backend section. */
  gsdac_get1d ( gsd, "C3BETOTIF", gsdVars->totIFs, status );

  /* Get the FE sideband signs for each backend section. */
  gsdac_get1i ( gsd, "C3BEFESB", gsdVars->sbSigns, status );

  /* Get the BE input frequencies. */
  gsdac_get1d ( gsd, "C12INFREQ", gsdVars->BEInputFreqs, status );

  /* Get the frequency resolutions. */
  gsdac_get1r ( gsd, "C12FR", gsdVars->freqRes, status );

  /* Get the bandwidths. */
  gsdac_get1r ( gsd, "C12BW", gsdVars->bandwidths, status );
 
  /* Get the temperatures and gains. */
  gsdac_get1r ( gsd, "C12RT", gsdVars->recTemps, status );
  gsdac_get1r ( gsd, "C12SST", gsdVars->sourceSysTemps, status );
  gsdac_get1r ( gsd, "C12TSKY", gsdVars->skyTemps, status );
  gsdac_get1r ( gsd, "C12TTEL", gsdVars->telTemps, status );
  gsdac_get1r ( gsd, "C12GAINS", gsdVars->gains, status );
  gsdac_get1r ( gsd, "C12CT", gsdVars->calTemps, status );

  /* Get the water opacities. */
  gsdac_get1r ( gsd, "C12WO", gsdVars->opacities, status );

  /* Get the sky transmissions. */
  gsdac_get1r ( gsd, "C12ETASKY", gsdVars->skyTrans, status );

  /* ??. */
  gsdac_get1r ( gsd, "C12ALPHA", gsdVars->alphas, status );

  /* Get the sideband gain normalizations. */
  gsdac_get1r ( gsd, "C12GS", gsdVars->sbGainNorms, status );

  /* Get the telescope transmissions. */
  gsdac_get1r ( gsd, "C12ETATEL", gsdVars->telTrans, status );
  
  /* Get the FE-derived tsky image sideband for each section. */
  gsdac_get1r ( gsd, "C12TSKYIM", gsdVars->FETSkyIm, status );

  /* Get the FE-derived sky transmission. */
  gsdac_get1r ( gsd, "C12ETASKYIM", gsdVars->FESkyTrans, status );

  /* Get the FE-derived tsys image sideband for each section. */
  gsdac_get1r ( gsd, "C12TSYSIM", gsdVars->FETSysIm, status );

  /* Get the ratio of signal sideband to image sideband
     sky transmission for each section. */
  gsdac_get1r ( gsd, "C12TASKY", gsdVars->sbRatios, status );

  /* Get the scan integration times. */
  gsdac_get1i ( gsd, "C3INTT", gsdVars->intTimes, status );

  /* Get the data. */
  gsdac_get1r ( gsd, "C13DAT", gsdVars->data, status );

  if ( dasFlag == DAS_CROSS_CORR ) {
    gsdac_get0r ( gsd, "C55HOTPOWER", gsdVars->hotPower, status );
    gsdac_get0r ( gsd, "C55SKYPOWER", gsdVars->skyPower, status );
    gsdac_get0r ( gsd, "C55SAM", gsdVars->samples, status );    
 } 

  if ( dasFlag == DAS_TP || dasFlag == DAS_CROSS_CORR ) {
    gsdac_get0r ( gsd, "C55POWER", gsdVars->totPower, status );
  }

  if ( *status != SAI__OK ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Error getting GSD arrays", status );
    return;
  }

  /* Replace GSD bad values with Starlink bad values. */

  if ( dasFlag == DAS_CROSS_CORR ) {

    for ( i = 0; i < gsdVars->nFEChans; i++ ) {
      if ( (gsdVars->FEFreqs)[i] == gsdVars->badVal )
        (gsdVars->FEFreqs)[i] = VAL__BADD;
      if ( (gsdVars->FESBSigns)[i] == gsdVars->badVal ) 
        (gsdVars->FESBSigns)[i] = VAL__BADI;
      if ( (gsdVars->FELOFreqs)[i] == gsdVars->badVal ) 
        (gsdVars->FELOFreqs)[i] = VAL__BADD;
    } 

  }

  if ( dasFlag == DAS_NONE || dasFlag == DAS_CONT_CAL ) {

    for ( i = 0; i < gsdVars->nVRad; i++ ) {
      if ( (gsdVars->vRadial)[i] == gsdVars->badVal )
        (gsdVars->vRadial)[i] = VAL__BADD;
    }

  }

  for ( i = 0; i < gsdVars->nScanVars1 * gsdVars->noScans; i++ ) {
    if ( (gsdVars->scanTable1)[i] == gsdVars->badVal )
      (gsdVars->scanTable1)[i] = VAL__BADR;
    if ( (gsdVars->scanTable2)[i] == gsdVars->badVal )
      (gsdVars->scanTable2)[i] = VAL__BADR; 
  }  

  for ( i = 0; i < gsdVars->nMapDims * gsdVars->nMapPts; i++ ) {
    if ( (gsdVars->mapTable)[i] == gsdVars->badVal )
      (gsdVars->mapTable)[i] = VAL__BADR;
  }

  for ( i = 0; i < gsdVars->nPhaseVars * gsdVars->nPhases; i++ ) {
    if ( (gsdVars->phaseTable)[i] == gsdVars->badVal )
      (gsdVars->phaseTable)[i] = VAL__BADR;
  }
  
  for ( i = 0; i < gsdVars->nBESections; i++ ) {

    if ( (gsdVars->corrModes)[i] == gsdVars->badVal )
      (gsdVars->corrModes)[i] = VAL__BADI;
    if ( (gsdVars->bitModes)[i] == gsdVars->badVal )
      (gsdVars->corrModes)[i] = VAL__BADI;    
    if ( (gsdVars->sbOverlaps)[i] == gsdVars->badVal )
      (gsdVars->sbOverlaps)[i] = VAL__BADR;  
    if ( (gsdVars->mixNums)[i] == gsdVars->badVal )
      (gsdVars->mixNums)[i] = VAL__BADI;  
    if ( (gsdVars->BEInputChans)[i] == gsdVars->badVal )
      (gsdVars->BEInputChans)[i] = VAL__BADI; 
    if ( (gsdVars->BEChans)[i] == gsdVars->badVal )
      (gsdVars->BEChans)[i] = VAL__BADI; 
    if ( (gsdVars->BESubsys)[i] == gsdVars->badVal )
      (gsdVars->BESubsys)[i] = VAL__BADI; 
    if ( (gsdVars->centreFreqs)[i] == gsdVars->badVal )
      (gsdVars->centreFreqs)[i] = VAL__BADD; 
    if ( (gsdVars->restFreqs)[i] == gsdVars->badVal )
      (gsdVars->restFreqs)[i] = VAL__BADD; 
    if ( (gsdVars->LOFreqs)[i] == gsdVars->badVal )
      (gsdVars->LOFreqs)[i] = VAL__BADD; 
    if ( (gsdVars->totIFs)[i] == gsdVars->badVal )
      (gsdVars->totIFs)[i] = VAL__BADD; 
    if ( (gsdVars->sbSigns)[i] == gsdVars->badVal )
      (gsdVars->sbSigns)[i] = VAL__BADI; 
    if ( (gsdVars->freqRes)[i] == gsdVars->badVal )
      (gsdVars->freqRes)[i] = VAL__BADR; 
    if ( (gsdVars->bandwidths)[i] == gsdVars->badVal )
      (gsdVars->bandwidths)[i] = VAL__BADR; 
    if ( (gsdVars->recTemps)[i] == gsdVars->badVal )
      (gsdVars->recTemps)[i] = VAL__BADR; 
    if ( (gsdVars->telTemps)[i] == gsdVars->badVal )
      (gsdVars->telTemps)[i] = VAL__BADR;
    if ( (gsdVars->gains)[i] == gsdVars->badVal )
      (gsdVars->gains)[i] = VAL__BADR;
    if ( (gsdVars->calTemps)[i] == gsdVars->badVal )
      (gsdVars->calTemps)[i] = VAL__BADR;
    if ( (gsdVars->opacities)[i] == gsdVars->badVal )
      (gsdVars->opacities)[i] = VAL__BADR;
    if ( (gsdVars->alphas)[i] == gsdVars->badVal )
      (gsdVars->alphas)[i] = VAL__BADR;
    if ( (gsdVars->sbGainNorms)[i] == gsdVars->badVal )
      (gsdVars->sbGainNorms)[i] = VAL__BADR;
    if ( (gsdVars->telTrans)[i] == gsdVars->badVal )
      (gsdVars->telTrans)[i] = VAL__BADR;

    if ( dasFlag != DAS_CONT_CAL ) {

      if ( (gsdVars->sourceSysTemps)[i] == gsdVars->badVal )
        (gsdVars->sourceSysTemps)[i] = VAL__BADR;
      if ( (gsdVars->skyTemps)[i] == gsdVars->badVal )
        (gsdVars->skyTemps)[i] = VAL__BADR;
      if ( (gsdVars->skyTrans)[i] == gsdVars->badVal )
        (gsdVars->skyTrans)[i] = VAL__BADR;  
      if ( (gsdVars->FETSkyIm)[i] == gsdVars->badVal )
        (gsdVars->FETSkyIm)[i] = VAL__BADR;    
      if ( (gsdVars->FESkyTrans)[i] == gsdVars->badVal )
        (gsdVars->FESkyTrans)[i] = VAL__BADR; 
      if ( (gsdVars->FETSysIm)[i] == gsdVars->badVal )
        (gsdVars->FETSysIm)[i] = VAL__BADR;
      if ( (gsdVars->sbRatios)[i] == gsdVars->badVal )
        (gsdVars->sbRatios)[i] = VAL__BADR; 
 
    }

  }

  if ( dasFlag == DAS_CONT_CAL ) {

    for ( i = 0; i < gsdVars->nBESections * gsdVars->noScans; i++ ) {
      if ( (gsdVars->sourceSysTemps)[i] == gsdVars->badVal )
        (gsdVars->sourceSysTemps)[i] = VAL__BADR;
      if ( (gsdVars->skyTemps)[i] == gsdVars->badVal )
        (gsdVars->skyTemps)[i] = VAL__BADR;
      if ( (gsdVars->skyTrans)[i] == gsdVars->badVal )
        (gsdVars->skyTrans)[i] = VAL__BADR;  
      if ( (gsdVars->FETSkyIm)[i] == gsdVars->badVal )
        (gsdVars->FETSkyIm)[i] = VAL__BADR;    
      if ( (gsdVars->FESkyTrans)[i] == gsdVars->badVal )
        (gsdVars->FESkyTrans)[i] = VAL__BADR; 
      if ( (gsdVars->FETSysIm)[i] == gsdVars->badVal )
        (gsdVars->FETSysIm)[i] = VAL__BADR;
      if ( (gsdVars->sbRatios)[i] == gsdVars->badVal )
        (gsdVars->sbRatios)[i] = VAL__BADR;  
    }

  }

  for ( i = 0; i < gsdVars->nBEChansIn; i++ ) {
    if ( (gsdVars->BEConnChans)[i] == gsdVars->badVal )
      (gsdVars->BEConnChans)[i] = VAL__BADI; 
    if ( (gsdVars->BEInputFreqs)[i] == gsdVars->badVal )
      (gsdVars->BEInputFreqs)[i] = VAL__BADD; 
  } 

  for ( i = 0; i < gsdVars->noScans; i++ ) {
    if ( (gsdVars->intTimes)[i] == gsdVars->badVal )
      (gsdVars->intTimes)[i] = VAL__BADI;    
  }

  for ( i = 0; i < gsdVars->nBEChansOut  * gsdVars->nScanPts  * 
	gsdVars->noScans; i++ ) {
    if ( (gsdVars->data)[i] == gsdVars->badVal )
      (gsdVars->data)[i] = VAL__BADR;
  }

  if ( dasFlag == DAS_CROSS_CORR ) {

    for ( i = 0; i < gsdVars->nBESections * gsdVars->IFPerSection; i++ ) {
      if ( (gsdVars->hotPower)[i] == gsdVars->badVal )
        (gsdVars->hotPower)[i] = VAL__BADR;
      if ( (gsdVars->skyPower)[i] == gsdVars->badVal )
        (gsdVars->skyPower)[i] = VAL__BADR;
    }

    for ( i = 0; i < gsdVars->nBEChansOut * gsdVars->IFONPhase *
	  gsdVars->noCycles; i++ ) {
      if ( (gsdVars->samples)[i] == gsdVars->badVal )
        (gsdVars->samples)[i] = VAL__BADR;
    }

    for ( i = 0; i < gsdVars->nBESections * gsdVars->IFPerSection *
	  gsdVars->IFONPhase * gsdVars->noCycles; i++ ) {
      if ( (gsdVars->totPower)[i] == gsdVars->badVal )
        (gsdVars->totPower)[i] = VAL__BADR;
    }

  }

  if ( dasFlag == DAS_TP ) {

    for ( i = 0; i < gsdVars->nBESections * gsdVars->IFPerSection *
	  gsdVars->IFONPhase * gsdVars->nPhases *
          gsdVars->noCycles; i++ ) {
      if ( (gsdVars->totPower)[i] == gsdVars->badVal )
        (gsdVars->totPower)[i] = VAL__BADR;
    }    

  }    

}
