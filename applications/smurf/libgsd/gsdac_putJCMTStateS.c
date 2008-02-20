/*
*+
*  Name:
*     gsdac_putJCMTStateS

*  Purpose:
*     Fill the subsystem-dependent JCMTState headers. 

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_putJCMTStateS ( const gsdVars *gsdVars, const dasFlag dasFlag,  
*                           const unsigned int stepNum, 
*                           const unsigned int subsysNum, 
*                           struct JCMTState *record, 
*                           int *status );

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     dasFlag = const dasFlag (Given)
*        DAS file structure type
*     stepNum = const unsigned int (Given)
*        time step of this spectrum
*     subsysNum = const unsigned int (Given)
*        Subsystem number
*     record = JCMTState** (Given and returned)
*        JCMTState headers
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Determines the headers required for a JCMTState header
*     in an ACSIS format file from a GSD file.  This routine determines
*     the values of the JCMTState elements which are dependent upon
*     this particular subsystem.  

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-01-29 (JB):
*        Original
*     2008-02-14 (JB):
*        Use gsdVars struct to store headers/arrays
*     2008-02-18 (JB):
*        Check dasFlag

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
#include "sae_par.h"

/* SMURF includes */
#include "gsdac.h"
#include "jcmt/state.h"

void gsdac_putJCMTStateS ( const gsdVars *gsdVars, const dasFlag dasFlag,
                           const unsigned int stepNum, 
                           const unsigned int subsysNum,
                           struct JCMTState *record, int *status )
{

  /* Local variables */
  int arrayIndex;             /* index into array for retrieving values */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

 /* Get the frontend LO frequency. */
  record->fe_lofreq = (gsdVars->LOFreqs)[subsysNum-1]; 

  /* Use the dasFlag to determine the dimensionality/size of
     the TSKY array. */
  if ( dasFlag == DAS_CONT_CAL ) {
    arrayIndex =  ( stepNum * gsdVars->nBESections ) +
                  subsysNum - 1;
  } else {
    arrayIndex = subsysNum - 1;
  }

  /* Set the enviro_air_temp to the correct value from the
     TSKY array. */
  record->enviro_air_temp = (gsdVars->skyTemps)[arrayIndex];

  record->enviro_air_temp = 0.0;

}
