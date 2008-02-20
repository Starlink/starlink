/*
*+
*  Name:
*     gsdac_putJCMTStateC

*  Purpose:
*     Fill the subsystem-independent JCMTState headers. 

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_putJCMTStateC ( const gsdVars *gsdVars, const dasFlag dasFlag,
*                           const gsdWCS *wcs,
*                           const unsigned int stepNum,
*                           const char *backend,
*                           struct JCMTState *record, int *status );

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     dasFlag = const dasFlag (Given)
*        DAS file structure type
*     wcs = const gsdWCS* (Given)
*        Pointing and time values
*     stepNum = const unsigned int (Given)
*        Time step of this spectrum
*     backend = const char* (Given)
*        Name of the backend
*     record = JCMTState* (Given and returned)
*        JCMTState headers
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Determines the headers required for a JCMTState header
*     in an ACSIS format file from a GSD file.  This routine determines
*     the values of the JCMTState elements which are common to all
*     subsystems.   

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-01-29 (JB):
*        Original
*     2008-02-14 (JB):
*        Use gsdVars struct to store headers/arrays
*     2008-02-18 (JB):
*        Get values from gsdWCS

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
*     Currently kludged with default values.
*-
*/

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "gsdac.h"
#include "jcmt/state.h"

#define FUNC_NAME "gsdac_putJCMTStateC"

void gsdac_putJCMTStateC ( const gsdVars *gsdVars, const dasFlag dasFlag,
                           const gsdWCS *wcs,
                           const unsigned int stepNum, 
                           const char *backend,
                           struct JCMTState *record, int *status )
{

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Fill the JCMTState (KLUDGED with default vals). */
  /* KLUDGES indicated by "//k". */
    
  record->rts_num = stepNum;

  record->rts_end = 0.0;//k

  /* Check the frequency band to determine tasklist. */
  if ( (gsdVars->centreFreqs)[0] < 290.0 ) {
    strncpy ( record->rts_tasks, "PTCS FE_A ", 11 );
  } else if ( (gsdVars->centreFreqs)[0] < 395.0 ) {  
    strncpy ( record->rts_tasks, "PTCS FE_B ", 11 );
  } else if ( (gsdVars->centreFreqs)[0] < 600.0 ) {
    strncpy ( record->rts_tasks, "PTCS FE_C ", 11 );
  } else {
    strncpy ( record->rts_tasks, "PTCS FE_D ", 11 );
  }
  strcat ( record->rts_tasks, backend );

  record->smu_x = 0.0;

  record->smu_y = 0.0;

  record->smu_z = 0.0; 

  strncpy( record->smu_chop_phase, "M", 1 );

  record->smu_jig_index = VAL__BADI;

  record->smu_az_jig_x = 0.0;

  record->smu_az_jig_y = 0.0;

  record->smu_az_chop_x = 0.0;

  record->smu_az_chop_y = 0.0;

  record->smu_tr_jig_x = 0.0;

  record->smu_tr_jig_y = 0.0;

  record->smu_tr_chop_x = 0.0;

  record->smu_tr_chop_y = 0.0;

  record->tcs_tai = wcs[stepNum].tai;

  record->tcs_airmass = wcs[stepNum].airmass;

  record->tcs_az_ang = wcs[stepNum].azAng;

  record->tcs_az_ac1 = wcs[stepNum].acAz;

  record->tcs_az_ac2 = wcs[stepNum].acEl;

  record->tcs_az_ac1 = record->tcs_az_ac1;

  record->tcs_az_ac2 = record->tcs_az_ac2;

  record->tcs_az_bc1 = wcs[stepNum].baseAz;

  record->tcs_az_bc2 = wcs[stepNum].baseEl;

  strncpy( record->tcs_beam, "M", 1 );    

  record->tcs_index = wcs[stepNum].index;

  strncpy( record->tcs_source, "SCIENCE", 8 ); 

  strncpy( record->tcs_tr_sys, "COORDS", 7 );

  record->tcs_tr_ang = wcs[stepNum].trAng;

  record->tcs_tr_ac1 = wcs[stepNum].acTr1;

  record->tcs_tr_ac2 = wcs[stepNum].acTr2;

  record->tcs_tr_dc1 = record->tcs_tr_ac1;

  record->tcs_tr_dc2 = record->tcs_tr_ac2;

  record->tcs_tr_bc1 = wcs[stepNum].baseTr1;

  record->tcs_tr_bc2 = wcs[stepNum].baseTr2;

  record->jos_drcontrol = 0;

  record->enviro_rel_hum = gsdVars->hamb;

  record->enviro_pressure = (gsdVars->pamb) * 1.33322;

  record->acs_exposure = 0.0;//k

  record->acs_offexposure = 0.0;//k  

  record->acs_no_prev_ref = VAL__BADI;

  record->acs_no_next_ref = VAL__BADI;

  record->acs_no_ons = VAL__BADI;

  strncpy( record->acs_source_ro, "SPECTRUM_RESULT", 16 );

  record->pol_ang = VAL__BADD;

  record->fe_doppler = 0.0;//k

}
