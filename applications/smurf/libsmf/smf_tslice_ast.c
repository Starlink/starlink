/*
*+
*  Name:
*     smf_tslice_ast

*  Purpose:
*     Configure an AST FrameSet for a specified time slice

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_tslice_ast( smfData * data, int index, int * status);

*  Arguments:
*     data = smfData* (Given & Returned)
*        Data structure containing time series data.
*        The smfHead item in the structure will be updated to receive
*        the updated FrameSet. In addition, if sc2head is non-null, the
*        contents of hdr->sc2head will be updated for this time slice.
*     index = int (Given)
*        Index into the time series data (the 3rd dimension).
*        If the data structure does not contain the specified index
*        a bad error is reported. Ignored for 2D data.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function is used to create an AST FrameSet for the specified
*     time slice from the supplied data structure. It only creates a new
*     frameset if required, else the supplied frameset is modified for
*     efficiency. The FrameSet is stored in the "hdr" component of the
*     supplied data structure.
*
*     For 2D data files the routine returns without modification of
*     the "data" structure if the header struct already contains a framset.
*     Bad status is set if a 2D data struct does not contain a framset already.
*
*     The frameset will be freed automatically when the data struct is
*     annulled.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2005-11-07 (TIMJ):
*        Initial version.
*     {enter_further_changes_here}

*  Notes:
*     The API is currently uncertain since it may make more sense to pass in a smfHead
*     rather than a smfData (assuming the xloc field is moved to smfHead).

*  Copyright:
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
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
#include "dat_par.h"

/* Data Acquisition Includes */
#include "sc2da/sc2store_struct.h"
#include "sc2da/sc2ast.h"
#include "sc2da/sc2store.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_tslice_ast"

void smf_tslice_ast (smfData * data, int index, int * status ) {

  smfHead *       hdr;       /* Local copy of the header structure */
  smfFile *       file;      /* Local copy of File structure */
  double          pa;        /* Parallactic angle for time slice */
  struct sc2head  sc2hdr;    /* Local structure for sc2head data if required */
  struct sc2head *sc2tmp;    /* Pointer to either sc2hdr or hdr->sc2head */
  int             subsysnum; /* Subsystem numeric id. 0 - 8 */


  if (*status != SAI__OK) return;

  /* First need to determine what type of data we have */
  /* Note that in this routine we return immediately if status is set to bad.
     This means that we only need to check for goodness again once a routine
     is called that may set status. */
  if ( data == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Supplied Data is a NULL pointer. Possible programming error.", status);
    return;
  }

  /* Get the header struct since we need that early */
  hdr = data->hdr;
  if ( hdr == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Supplied smfData->hdr is a NULL pointer. Possible programming error.", status);
    return;
  }

  /* 2-D case. Just check to see if we have a frameset */
  if (data->ndims == 2) {
    if (hdr->wcs == NULL) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "FrameSet is not attached to 2D data file. Possible programming error.", status);
    }
    /* Return immediately as nothing to do */
    return;
  }


  /* Work out the largest index we are allowed to have given that we have 2d data. */
  if (data->ndims != 3) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      msgSeti( "D", data->ndims );
      errRep( FUNC_NAME, "Dimensionality of ^D is not the expected 3", status);
      return;
    }
  }

  /* Check index bounds */
  if ( index < 0 || index >= (data->dims)[2] ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSeti( "I", index );
      msgSeti( "TMX", (data->dims)[2] );
      msgSeti( "TMN", 0 );
      errRep( FUNC_NAME, "Index out of bounds ( ^TMN <= ^I < ^TMX )", status );
      return;
    }
  }

  /* Now get the file information */
  file = data->file;
  if ( file == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Supplied smfFile is a NULL pointer. Possible programming error.", status);
    return;
  }

  /* Decide whether we are populating sc2head from the header or a local copy */
  if (hdr->sc2head != NULL ) {
    sc2tmp = hdr->sc2head;
  } else {
    sc2tmp = &sc2hdr;
  }

  /* Need to get the sub system ID */
  smf_fits_getI( hdr, "SUBSYSNR", &subsysnum, status );

  /* Get the sc2head structure for this time slice */
  sc2store_headget( index, sc2tmp, status );

  /* Need the parallactic angle: FPANG = EL + PA */
  pa = sc2tmp->tcs_tr_ang - sc2tmp->tcs_az_ang;

  /* See if we have a WCS or not */
  if (hdr->wcs == NULL ) {
    /* Must create one */
    sc2ast_createwcs( subsysnum, sc2tmp->tcs_az_ac1, sc2tmp->tcs_az_ac2, sc2tmp->rts_end, pa, &(hdr->wcs), status );
  } else {
    /* Modify in place */
    
  }

  astShow( hdr->wcs );

  return;
}
