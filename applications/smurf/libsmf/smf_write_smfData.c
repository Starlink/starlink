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
*     smf_write_smfData ( const smfData *data, const smfData *variance,
*                        unsigned char *quality, const char * filename,
*                        const Grp * igrp, size_t grpindex,
*                        int provid, int * status );

*  Arguments:
*     data = const smfData* (Given)
*        Pointer to smfData to dump to disk file. Returns without action
*        if NULL pointer.
*     variance = void * (Given)
*        Override VARIANCE component of data with variance stored as the
*        main data array of a second smfData called variance. variance
*        can have the length of the time dimension be 0 in which case it
*        is replicated at each time slice in the output file.
*     quality = unsigned char * (Given)
*        If set, use this buffer instead of QUALITY associated with data.
*     filename = const char * (Given)
*        Name of output NDF if non-NULL. If NULL the filename is obtained
*        from the group.
*     igrp = const Grp * (Given)
*        Group containing the required filename. Can be NULL, in which
*        case the explicitly supplied filename is used.
*     grpindex = size_t (Given)
*        Index into group.
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
*
*     It is an error for both "grp" and "filename" to be NULL.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-08-25 (TIMJ):
*        Initial version.
*     2008-09-30 (EC):
*        Added functionality from smf_NDFexport: 
*        - write WCS information
*        - write JCMT State array
*        - add variance and quality overrides
*     2009-07-31 (EC):
*        - enable 2d variance arrays, supply external variance with smfData
*     2009-09-09 (EC):
*        - fix so that files that are neither 1- nor 3-dimensions can be written
*     2009-09-29 (TIMJ):
*        Use pixel origin from smfData
*     2009-10-13 (TIMJ):
*        Add Grp argument
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     Copyright (C) 2008,2009 University of British Columbia.
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
#include "smf_err.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smf_write_smfData"

void smf_write_smfData( const smfData *data, const smfData *variance,
                        unsigned char *quality, const char * filename,
                        const Grp * igrp, size_t grpindex,
                        int provid, int * status ) {

  size_t dbstride;              /* bolo stride of data */
  size_t dtstride;              /* tstride of data */
  size_t i;                     /* Loop counter */
  int flags = 0;                /* Flags for open file */
  size_t j;                     /* Loop counter */
  HDSLoc *jcmtstateloc=NULL;    /* HDS Locator for JCMT headers */
  int lbnd[NDF__MXDIM];         /* Lower pixel bounds */
  dim_t nbolo;                  /* number of bolos */
  dim_t nelem;                  /* total number of elements in data array */
  dim_t ntslice=0;              /* Number of time slices */
  Grp * ogrp = NULL;            /* Small group for output filename */
  smfData * outdata = NULL;     /* Mapped output file */
  double *outvar = NULL;        /* pointer to output variance component */
  char prvname[2*PAR__SZNAM+1]; /* provenance ID string */
  unsigned char *qual=NULL;     /* Pointer to QUALITY buffer */
  int ubnd[NDF__MXDIM];         /* Upper pixel bounds */
  double *var=NULL;             /* Pointer to VARIANCE buffer */
  size_t vbstride;              /* bolo stride of variance */
  dim_t vnbolo;                 /* number of bolos in variance */
  dim_t vntslice;               /* number of bolos in variance */
  size_t vtstride;              /* tstride of variance */

  if (*status != SAI__OK) return;
  if (!data) return;

  /* if we have a filename we need to make a group */
  if (filename) {
    ogrp = grpNew( "", status );
    grpPut1( ogrp, filename, 0, status );
  } else if (igrp) {
    /* Copy the required entry to a new group */
    ogrp = ndgCopy( igrp, grpindex, grpindex, 0, status );
  } else {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Must supply a filename or a group."
            " Possible programming error.", status );
    return;
  }

  /* Check for QUALITY components, and header */
  if( quality ) qual = quality;
  else qual = (data->pntr)[2];

  /* see if we need to write quality */
  if ( qual ) flags |= SMF__MAP_QUAL;
   
  /* Calculate bounds */
  for (i = 0; i < data->ndims; i++) {
    lbnd[i] = (data->lbnd)[i];
    ubnd[i] = lbnd[i] + (data->dims)[i] - 1;
  }
  
  if( data->ndims == 1 ) {
    /* Dimensions for 1-d data */
    ntslice = data->dims[0];
    nelem = ntslice;
    nbolo = 1;
    dbstride = 0;
    dtstride = 1;
  } else if( data->ndims == 3 ) {
    /* Dimensions for 3-d data */
    smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, &nelem, &dbstride, 
                  &dtstride, status );

    /* Only handle variance for 3d data */
    if( variance ) {
      var = variance->pntr[0];

      smf_get_dims( variance, NULL, NULL, &vnbolo, &vntslice, NULL, &vbstride, 
                    &vtstride, status );
      
      /* Check that the variance dimensions are compatible with data */
      if( (vnbolo != nbolo) || ( (vntslice>1) && (vntslice!=ntslice) ) ) {
        *status = SAI__ERROR;
        errRep(" ", FUNC_NAME ": variance dimensions incompatible with data", 
               status ); 
        return;
      }
      
      /* We've assumed that var is double precision... need to check */
      if( variance->dtype != SMF__DOUBLE ) {
        *status = SAI__ERROR;
        errRep(" ", FUNC_NAME ": variance array is not double precision", 
               status ); 
        return;        
      }

    } else {
      var = (data->pntr)[1];    
    } 

    if( var ) flags |= SMF__MAP_VAR;
  } else {
    /* For strange dimensions (especially 2- or 4-dimensions for FFTs) don't
       try to write variance or quality */
    var = NULL;
    qual = NULL;

    /* Word out size of data */
    nelem = 1;
    for( i=0; i<data->ndims; i++ ) {
      nelem = nelem*data->dims[i];
    }
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

    /* provenance propagation - but only if we have provenance to propagate */
    if ( (data->file && data->file->ndfid && data->file->ndfid != NDF__NOID) ||
         (provid != NDF__NOID) ) {
      smf_get_taskname( NULL, prvname, status );
      smf_updateprov( outfile->ndfid, data, provid, prvname, status );
    }

    /* number of bytes per element */
    nbperel = smf_dtype_size( data, status );

    /* Copy the data and variance and quality */
    if (*status == SAI__OK) {

      if( (data->pntr)[0] ) memcpy( (outdata->pntr)[0], (data->pntr)[0],
                                    nelem * nbperel );

      /* Do variance on a timeslice basis in case we are repeating a 2-d
         variance array over time slice */
      if( var ) {
        outvar = (outdata->pntr)[1];
        for( i=0; i<nbolo; i++ ) {
          for( j=0; j<ntslice; j++ ) {
            outvar[i*dbstride+j*dtstride] = var[i*vbstride+
                                                (j%vntslice)*vtstride];
          }
        }
      }

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
      if( inhdr->tswcs ) {
        ndfPtwcs( inhdr->tswcs, outfile->ndfid, status );
        /* annul bad status caused by dimension mismatch */
        if( *status == NDF__NAXIN ) {
          errAnnul( status );
        }
      }

      /* JCMT State -- if ntslice is known */
      if( inhdr->allState ) {

        /* Get an HDS locator */
        ndfXnew( outfile->ndfid, JCMT__EXTNAME, JCMT__EXTTYPE, 0, 0,
                 &jcmtstateloc, status );
        
        /* Map the header */
        sc2store_headcremap( jcmtstateloc, inhdr->nframes, INST__SCUBA2, 
                             status  );
        
        /* Write out the per-frame headers */
        for( i=0; (*status==SAI__OK)&&(i<inhdr->nframes); i++ ) {
          sc2store_headput( i, inhdr->allState[i], status );
        }
      }
      
    }
  }
  
  /* Close the output file */
  smf_close_file( &outdata, status );
  grpDelet( &ogrp, status );

}
