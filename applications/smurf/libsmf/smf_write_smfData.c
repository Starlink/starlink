/*
*+
*  Name:
*     smf_write_smfData

*  Purpose:
*     Writes a smfData to disk for later inspection.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_write_smfData( const smfData * data, const char * filename,
*                        int provid, int *status );

*  Arguments:
*     data = const smfData* (Given)
*        Pointer to smfData to dump to disk file. Returns without action
*        if NULL pointer.
*     filename = const char * (Given)
*        Name of output NDF.
*     provid = int (Given)
*        NDF id to propagate provenance from. Can be NDF__NOID.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine provides a quick way of writing a smfData to disk file.
*     Any smfFile contents will be ignored. smfHead will be stored if
*     appropriate. smfDA will be ignored.

*  Notes:
*     Will not write Data Acquisition information.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-08-25 (TIMJ):
*        Initial version.

*     {enter_further_changes_here}

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


#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par.h"

#include "smf.h"
#include "smurf_par.h"
#include "libsmurf/smurflib.h"
#include "smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smf_write_smfData"

void smf_write_smfData ( const smfData * data, const char * filename,
                              int provid, int * status ) {

  size_t i;                     /* Loop counter */
  int flags = 0;                /* Flags for open file */
  int lbnd[NDF__MXDIM];         /* Lower pixel bounds */
  Grp * ogrp = NULL;            /* Small group for output filename */
  smfData * outdata = NULL;     /* Mapped output file */
  char prvname[2*PAR__SZNAM+1]; /* provenance ID string */
  int ubnd[NDF__MXDIM];         /* Upper pixel bounds */

  if (*status != SAI__OK) return;
  if (data == NULL) return;

  /* see if we need to write variance and quality */
  if ( (data->pntr)[1] ) flags |= SMF__MAP_VAR;
  if ( (data->pntr)[2] ) flags |= SMF__MAP_QUAL;
 
  /* create a group so that we can reuse smf_open_newfile interface */
  ogrp = grpNew( "", status );
  grpPut1( ogrp, filename, 0, status );

  /* Calculate bounds */
  for (i = 0; i < data->ndims; i++) {
    lbnd[i] = 1;
    ubnd[i] = lbnd[i] + (data->dims)[i] - 1;
  }

  /* Open the file */
  smf_open_newfile( ogrp, 1, data->dtype, data->ndims, lbnd, ubnd,
                    flags, &outdata, status );

  if (*status == SAI__OK) {
    smfFile * outfile = outdata->file;
    smfHead * inhdr = data->hdr;
    size_t nbperel = 0;
    size_t nelem = 1;

    /* provenance propagation - but only if we have provenance to propagate */
    if ( (data->file && data->file->ndfid && data->file->ndfid != NDF__NOID) ||
         (provid != NDF__NOID) ) {
      smf_get_taskname( NULL, prvname, status );
      smf_updateprov( outfile->ndfid, data, provid, prvname, status );
    }

    /* number of bytes per element */
    nbperel = smf_dtype_size( data, status );

    /* Calculate how many elements to copy */
    for (i = 0; i< data->ndims; i++) {
      nelem *= (data->dims)[i];
    }

    /* Copy the data and variance and quality */
    if (*status == SAI__OK) {
      for (i = 0; i <= 2; i++ ) {
        if (i == 2) nbperel = 1; /* quality = 1 byte */
        if ((data->pntr)[i]) memcpy( (outdata->pntr)[i], (data->pntr)[i],
                                     nelem * nbperel );
      }
    }

    /* header information */
    if (inhdr && *status == SAI__OK) {

      /* FITS header */
      if (inhdr->fitshdr) kpgPtfts( outfile->ndfid, inhdr->fitshdr, status );

      /* Labels */
      if ( strlen(inhdr->units) ) ndfCput( inhdr->units, outfile->ndfid,
                                           "Unit", status );
      if ( strlen(inhdr->title) ) ndfCput( inhdr->title, outfile->ndfid,
                                           "Title", status );
      if ( strlen(inhdr->dlabel) ) ndfCput( inhdr->dlabel, outfile->ndfid,
                                           "Label", status );



    }
  }
  
  /* Close the output file */
  smf_close_file( &outdata, status );
  grpDelet( &ogrp, status );

}
