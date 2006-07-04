/*
*+
*  Name:
*     smf_open_mapcoord

*  Purpose:
*     Load .SCU2RED.MAPCOORD extension into smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_open_mapcoord( smfData *data, int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData struct 
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Search for .SCU2RED.MAPCOORD extension and map pointer to lookup table.
*     For use with files that are left open! smf_close_file frees resources.
*
*     
*  Authors:
*     Edward Chapin (UBC)

*  History:
*     2006-06-08 (EC):
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
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "prm_par.h"
#include "sae_par.h"

#include "star/hds.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_open_mapcoord"

void smf_open_mapcoord( smfData *data, int *status ) {
  int lbnd[1];                  /* Pixel bounds for 1d pointing array */
  void *mapptr[3];              /* Pointer to array of mapped components */
  int nbolo;                    /* Number of bolometers */
  int nmap;                     /* Number of elements mapped */
  HDSLoc *scu2redloc=NULL;        /* HDS locator to the SCU2RED extension */
  int ubnd[1];                  /* Pixel bounds for 1d pointing array */
  
  /* Main routine */
  if (*status != SAI__OK) return;
  
  /* Get HDS locator for the SCU2RED extension  */
  scu2redloc = smf_get_xloc( data, "SCU2RED", "SCU2RED_Calculations",
                           "READ", 0, 0, status );

  /* Since other things may eventually get put into the SCU2RED
     extension, and to prevent it from having problems if called multiple
     times, check for NULL states first for the HDS locator / NDF id / LUT
     before trying to get them */
    
  if( *status == SAI__OK ) {
    nbolo = (data->dims)[0] * (data->dims)[1];

    lbnd[0] = 0;
    ubnd[0] = nbolo*(data->dims)[2]-1;

    if( (data->file)->mapcoordid == NDF__NOID ) {
      (data->file)->mapcoordid = smf_get_ndfid( scu2redloc, 
						"MAPCOORD", "READ", "UNKNOWN",
						"_INTEGER", 1, lbnd, ubnd, 
						status );
    }
     
    if( data->lut == NULL ) {
      ndfMap( (data->file)->mapcoordid, "DATA", "_INTEGER", "READ", mapptr, 
	      &nmap, status );    
      
      if( *status == SAI__OK ) {
	data->lut = mapptr[0];
      } else {
	errRep( FUNC_NAME, "Unable to map LUT in SCU2RED extension",
		status);
      }
    }
  
    /* Annul the HDS locator to the extension */
    datAnnul( &scu2redloc , status );
  } else {
    errRep( FUNC_NAME, 
            "Couldn't get locator for SCU2RED extension",
            status);
  }
}
