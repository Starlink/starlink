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
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     2006-06-25 (EC):
*        Initial version
*     2006-08-08 (TIMJ):
*        Do not return immediately if status is bad.
*     2007-11-15 (EC):
*        Added check for existence of data->file
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour

*  Notes:
*     - This function attempts to free resources even if status is bad
*       on entry.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
  int dofree = 0;              /* LUT is not memory mapped so free it */

  /* We are freeing resources so we should not return if status
     is bad. Instead we should be defensive and check pointers
     before dereferencing them. */

  if( data ) {
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
      } else {
	dofree = 1;
      }
    } else {
      dofree = 1;
    }

    /* If the LUT pointer is non-null and dofree=1 we should free it */
    if( dofree && data->lut ) {
      data->lut = astFree( data->lut );
    }
  }
}
