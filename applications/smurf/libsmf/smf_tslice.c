/*
*+
*  Name:
*     smf_tslice

*  Purpose:
*     Retrieve data for a specific time slice

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_tslice( smfData *idata, smfData **tdata, int index, int * status);

*  Arguments:
*     idata = smfData* (Given)
*        Data structure containing time series data.
*        The smfHead item in the structure will be updated to receive
*        the updated FrameSet. In addition, if sc2head is non-null, the
*        contents of hdr->sc2head will be updated for this time slice.
*     tdata = smfData** (Returned)
*        Data structure containing the data for a given time slice 
*     index = int (Given)
*        Index of time slice
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:

*     This function is used to copy the data for specified time slice
*     from the supplied data structure into a pointer to a 2-D array
*     containing the bolometer data. The header is set from the input
*     smfData struct, which is assumed to be correct for that time
*     slice (i.e. no cross-checking is carried out). Time slices
*     created this way have the `virtual' flag set.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2005-12-22 (AGG):
*        Initial version.
*     {enter_further_changes_here}

*  Notes:
*     The header is assumed to be correct for the given
*     timeslice. Thus it is up to the user to make sure this is so by
*     calling smf_tslice_ast before calling this routine.

*  Copyright:
*     Copyright (C) 2005 University of British Columbia.
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

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "star/hds_types.h"

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_tslice"

void smf_tslice (const smfData *idata, smfData **tdata, int index, int *status ) {

  double *tslicedata;
  double *indata;
  int npts;
  int offset;
  int i;

  smfHead *hdr;
  void *ipntr[3];             /* Input D, Q and V arrays */
  void *opntr[3];             /* Output D, Q and V arrays */

  /* Allocate space for the tdata struct */
  *tdata = malloc( sizeof( smfData ) );

  /* Copy the current header */
  hdr = malloc( sizeof( smfHead ) );
  memcpy( hdr, idata->hdr, sizeof( smfHead ) );
  (*tdata)->hdr = hdr;

  /* Set the virtual flag */
  (*tdata)->virtual = 1;

  /* Store number of dimensions */
  (*tdata)->ndims = idata->ndims - 1; 
  ((*tdata)->dims)[0] = (idata->dims)[0];
  ((*tdata)->dims)[1] = (idata->dims)[1];

  npts = ((*tdata)->dims)[0] * ((*tdata)->dims)[1];
  offset = npts * index;

  ipntr[0] = (idata->pntr)[0];

  indata = ipntr[0];

  tslicedata = malloc( npts * sizeof( double ) );
  /*  memcpy( tslicedata, ipntr[0], npts*sizeof( double ) );*/
  
  for (i=0; i<npts; i++) {
    tslicedata[i] = indata[offset + i];
    /*    printf("index = %d, bol = %d, data = %g\n",index,i,tslicedata[i]);*/
  }

  ((*tdata)->pntr)[0] = tslicedata;

}
