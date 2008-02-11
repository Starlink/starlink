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
*     gsdac_putSpecHdr ( const gsdac_gsd_struct *gsd,
*                        const unsigned int nSteps,
*                        const unsigned int stepNum,
*                        const unsigned int subsysNum,
*                        const JCMTState *record,
*                        struct ACSISSpecHdr *specHdr, int *status );

*  Arguments:
*     gsd = const gsdac_gsd_struct* (Given)
*        GSD file access parameters
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
*     2008-01-18 (JB):
*        Original

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
#include "mers.h"
#include "ndf.h"
#include "gsd.h"
#include "sae_par.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libacsis/specwrite.h"
#include "libgsd/gsdac_struct.h"
#include "libgsd/gsdac.h"

#define FUNC_NAME "gsdac_putSpecHdr"

#define SZ_RECNAME 80
#define MAXRECEP 8  

void gsdac_putSpecHdr ( const struct gsdac_gsd_struct *gsd, 
                        const unsigned int nSteps,
                        const unsigned int stepNum,
                        const unsigned int subsysNum,
                        const JCMTState *record,
                        struct ACSISSpecHdr *specHdr, int *status )
{

  /* Local variables */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Get the tsys for this subsystem. */
  gsdac_getElemr ( gsd, "C12SST", subsysNum-1, &(specHdr->acs_tsys), status );

  if ( *status != SAI__OK ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Error retrieving SpecHdr values", status );
    return;
  }    

  /* Fill the specHdr. */
  specHdr->rts_endnum = nSteps;

  specHdr->acs_feedx = record->tcs_tr_ac1;
  specHdr->acs_feedy = record->tcs_tr_ac2;

  /* KLUDGE with defaults. */
  specHdr->acs_feed = 0;//k
  specHdr->acs_trx = 0.0;//k

}
