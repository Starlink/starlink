/*
*+
*  Name:
*     smf_close_mapcoord

*  Purpose:
*     Close MAPCOORD in smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_close_mapcoord( smfData *data, int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData struct 
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Search for MAPCOORD and free resources.
*
*     
*  Authors:
*     Edward Chapin (UBC)

*  History:
*     2006-06-25 (EC):
*        Initial version

*  Notes:

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"
#include "sae_par.h"
#include "star/hds.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_close_mapcoord"

void smf_close_mapcoord( smfData *data, int *status ) {

  smfFile *file = NULL;        /* Pointer to smfFile */
  
  /* Main routine */
  if (*status != SAI__OK) return;
  
  file = data->file;

  if( file != NULL ) {
    /* annul mapcoord NDF if it exists (frees memory used by LUT) */
    if( file->mapcoordid != NDF__NOID ) {
      ndfAnnul( &(file->mapcoordid), status );

      if( *status == SAI__OK ) {
        data->lut = NULL;
      } else {
        errRep( FUNC_NAME, "Error annulling MAPCOORD", status );
      }
    }
  }

}
