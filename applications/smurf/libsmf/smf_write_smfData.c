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
*     smf_write_smfData ( const smfData * data, void *variance,
*                        unsigned char *quality, const char * filename,
*                        int provid, int * status );

*  Arguments:
*     data = const smfData* (Given)
*        Pointer to smfData to dump to disk file. Returns without action
*        if NULL pointer.
*     variance = void * (Given)
*        If set, use this buffer instead of VARIANCE associated with data.
*     quality = unsigned char * (Given)
*        If set, use this buffer instead of QUALITY associated with data.
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
*     Will not write Data Acquisition information. If time-series WCS
*     information is in the header, it is assumed to be consistent with
*     the data array. If a JCMTState array is present, it can only be written
*     if the number of time slices can be determined. ntslices is assumed
*     to be the length of the only axis if data is 1-d, and the length of the
*     3rd axis for 3d data (ICD format).

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-08-25 (TIMJ):
*        Initial version.
*     2008-09-30 (EC):
*        Added functionality from NDFexport: 
*        - write WCS information
*        - write JCMT State array
*        - add variance and quality overrides
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     University of British Columbia.
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

void smf_write_smfData ( const smfData * data, void *variance,
                         unsigned char *quality, const char * filename,
                         int provid, int * status ) {

  size_t i;                     /* Loop counter */
  int flags = 0;                /* Flags for open file */
  HDSLoc *jcmtstateloc=NULL;    /* HDS Locator for JCMT headers */
  int lbnd[NDF__MXDIM];         /* Lower pixel bounds */
  dim_t ntslice=0;              /* Number of time slices */
  int haventslice=0;            /* Flag indicating whether ntslice is known */
  Grp * ogrp = NULL;            /* Small group for output filename */
  smfData * outdata = NULL;     /* Mapped output file */
  char prvname[2*PAR__SZNAM+1]; /* provenance ID string */
  unsigned char *qual=NULL;     /* Pointer to QUALITY buffer */
  int ubnd[NDF__MXDIM];         /* Upper pixel bounds */
  void *var=NULL;               /* Pointer to VARIANCE buffer */

  if (*status != SAI__OK) return;
  if (data == NULL) return;

  /* Check for VARIANCE and QUALITY components, and header */
  if( variance ) var = variance;
  else var = (data->pntr)[1];    

  if( quality ) qual = quality;
  else qual = (data->pntr)[2];
  
  /* see if we need to write variance and quality */
  if ( var ) flags |= SMF__MAP_VAR;
  if ( qual ) flags |= SMF__MAP_QUAL;
 
  /* create a group so that we can reuse smf_open_newfile interface */
  ogrp = grpNew( "", status );
  grpPut1( ogrp, filename, 0, status );

  /* Calculate bounds */
  for (i = 0; i < data->ndims; i++) {
    lbnd[i] = 1;
    ubnd[i] = lbnd[i] + (data->dims)[i] - 1;
  }

  /* Get ntslice -- assume ICD ordered data, and only handle 1 & 3-d data */
  if( data->ndims == 1 ) {
    ntslice = data->dims[0];
    haventslice = 1;
  }

  if( data->ndims == 3 ) {
    ntslice = data->dims[2];
    haventslice = 1;
  }

  msgSetc( "NAME", filename );
  msgOutif( MSG__VERB, "", FUNC_NAME ": writing ^NAME", status );

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

      if( (data->pntr)[0] ) memcpy( (outdata->pntr)[0], (data->pntr)[0],
                                    nelem * nbperel );

      if( var ) memcpy( (outdata->pntr)[1], var, nelem * nbperel );
      if( qual ) memcpy( (outdata->pntr)[2], qual, nelem * 1 );
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

      /* WCS */
      if( inhdr->tswcs ) ndfPtwcs( inhdr->tswcs, outfile->ndfid, status );

      /* JCMT State -- if ntslice is known */
      if( inhdr->allState && haventslice ) {
        /* Get an HDS locator */
        ndfXnew( outfile->ndfid, JCMT__EXTNAME, JCMT__EXTTYPE, 0, 0,
                 &jcmtstateloc, status );
        
        /* Map the header */
        sc2store_headcremap( jcmtstateloc, ntslice, INST__SCUBA2, status  );
        
        /* Write out the per-frame headers */
        for( i=0; (*status==SAI__OK)&&(i<ntslice); i++ ) {
          sc2store_headput( i, inhdr->allState[i], status );
        }
      }
      
    }
  }
  
  /* Close the output file */
  smf_close_file( &outdata, status );
  grpDelet( &ogrp, status );

}
