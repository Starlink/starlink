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
*     gsdac_putSpecHdr ( const gsdVars *gsdVars,
*                        const unsigned int nSteps,
*                        const unsigned int stepNum,
*                        const unsigned int subsysNum,
*                        const JCMTState *record,
*                        struct ACSISSpecHdr *specHdr, int *status );

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     nSteps = const unsigned int (Given)
*        Number of time steps
*     stepNum = const unsigned int (Given)
*        Time step of this spectrum
*     subsysNum = const unsigned int (Given)
*        Subsystem number
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

/* STARLINK includes */
#include "sae_par.h"

/* SMURF includes */
#include "gsdac.h"
#include "jcmt/state.h"

#define FUNC_NAME "gsdac_putSpecHdr"

#define SZ_RECNAME 80
#define MAXRECEP 8  

void gsdac_putSpecHdr ( const gsdVars *gsdVars, 
                        const unsigned int nSteps,
                        const unsigned int stepNum,
                        const unsigned int subsysNum,
                        const JCMTState *record,
                        struct ACSISSpecHdr *specHdr, int *status )
{

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  specHdr->acs_tsys = (gsdVars->sourceSysTemps)[subsysNum-1];

  /* Fill the specHdr. */
  specHdr->rts_endnum = nSteps;

  specHdr->acs_feedx = record->tcs_tr_ac1;
  specHdr->acs_feedy = record->tcs_tr_ac2;

  /* KLUDGE with defaults. */
  specHdr->acs_feed = 0;//k
  specHdr->acs_trx = 0.0;//k

}
