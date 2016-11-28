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
*     smf_write_smfData ( ThrWorkForce *wf, const smfData *data, const smfData *variance,
*                         const char * filename, const Grp * igrp, size_t grpindex,
*                         int provid, msglev_t msglev, int single,
*                         void (*func)(ThrWorkForce *wf,int indf,void *info,int *status),
*                         void *info, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     data = const smfData* (Given)
*        Pointer to smfData to dump to disk file. Returns without action
*        if NULL pointer.
*     variance = void * (Given)
*        Override VARIANCE component of data with variance stored as the
*        main data array of a second smfData called variance. variance
*        can have the length of the time dimension be 0 in which case it
*        is replicated at each time slice in the output file. Only works
*        for 3d time-series data.
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
*     msglev = msglev_t (Given)
*        What message level to report the filename?
*     single = int (Given)
*        If non-zero, then only a single bolometer is written out, and the
*        output NDF is 1-dimensional. The bolometer written out is the
*        first one found to contain any good, unflagged, values. If zero,
*        then the full 3D data array is written out. Only used if the
*        smfData is 3-dimensional.
*     func = void (*func)( ThrWorkForce *wf, int indf, void *info, int *status )
*        Pointer to a function, or NULL. If not NULL, the function is
*        invoked just before the output NDF is closed. It is supplied
*        with the workforce, the identifier for the output NDF, and an arbitrary
*        pointer supplied by the caller ("info"). This facility may be used to
*        store extra information in the NDF.
*     info = void *
*        A pointer to arbitrary information to pass to the "func"
*        function. May be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine provides a quick way of writing a smfData to disk file.
*     Any smfFile contents will be ignored. smfHead will be stored if
*     appropriate. smfDA will be ignored.

*  Notes:
*     Will write out dark squids if present. If time-series WCS
*     information is in the header, it is assumed to be consistent
*     with the data array. If a JCMTState array is present, it can
*     only be written if the number of time slices can be
*     determined. ntslices is assumed to be the length of the only
*     axis if data is 1-d, and the length of the 3rd axis for 3d data
*     (ICD format).
*
*     It is an error for both "grp" and "filename" to be NULL.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     COBA: Coskun Oba (UoL)
*     Matt Sherwood (MS, UofL)
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
*     2010-01-25 (TIMJ):
*        Write variance for 2d images. For 3d images with 3d variance do a quick
*        memcpy.
*     2010-01-26 (EC):
*        Write out dark squids
*     2010-01-29 (TIMJ):
*        Fix writing of 3d variance for 3d data
*     2010-06-10 (EC):
*        Write dark squid quality
*     2010-09-17 (COBA):
*        Write FTS2 data
*     2010-09-22 (COBA):
*        Validate FTS2 data before writing
*     2011-01-11 (TIMJ):
*        Use sc2store_writejcmtstate
*     2011-02-17 (TIMJ):
*        Get the file name from the group for display since the filename
*        can be a null pointer. Use grpMsg.
*     2011-08-08 (EC):
*        Added msglev
*     2012-01-04 (EC):
*        Try to writing wcs if tswcs doesn't exist to handle images
*     2013-01-22 (DSB):
*        Added argument single.
*     2014-01-23 (DSB):
*        Add items to the SMURF extension in the output NDF to hold the
*        STEPTIME and SCANVEL values that were actually used. These may be
*        different to the values in the FITS header. This helps when importing
*        the data into a subsequent run of makemap (i.e. SKYLOOP).
*     2015-02-20 (MS):
*        Added new smfFts fields for quality statistics
*     2015-02-20 (DSB):
*        Added arguments "func" and "info".
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2011 Science and Technology Facilities Council.
*     Copyright (C) 2008-2012 University of British Columbia.
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

void smf_write_smfData ( ThrWorkForce *wf, const smfData *data, const smfData *variance,
                         const char * filename, const Grp * igrp, size_t grpindex,
                         int provid, msglev_t msglev, int single,
                         void (*func)( ThrWorkForce *wf, int indf, void *info, int *status ),
                         void *info, int * status ){

  double *pd=NULL;              /* Pointer to DATA buffer */
  size_t dbstride;              /* bolo stride of data */
  size_t dtstride;              /* tstride of data */
  size_t i;                     /* Loop counter */
  int flags = 0;                /* Flags for open file */
  size_t j;                     /* Loop counter */
  int lbnd[NDF__MXDIM];         /* Lower pixel bounds */
  dim_t ibolo;                  /* bolo index */
  dim_t itime;                  /* time slice index */
  int singlebolo = -1;          /* The index of the bolometer to use */
  dim_t nbolo;                  /* number of bolos */
  dim_t ncols;                  /* number of columns */
  dim_t nrows;                  /* number of rows */
  dim_t nelem;                  /* total number of elements in data array */
  dim_t ntslice=0;              /* Number of time slices */
  Grp * ogrp = NULL;            /* Small group for output filename */
  smfData * outdata = NULL;     /* Mapped output file */
  double *outvar = NULL;        /* pointer to output variance component */
  char prvname[2*PAR__SZNAM+1]; /* provenance ID string */
  smf_qfam_t qfamily = SMF__QFAM_NULL; /* Quality family */
  const smf_qual_t *qual=NULL;  /* Pointer to QUALITY buffer */
  const smf_qual_t *pq=NULL;    /* Pointer to next QUALITY value */
  int ubnd[NDF__MXDIM];         /* Upper pixel bounds */
  double *var=NULL;             /* Pointer to VARIANCE buffer */
  double *pv=NULL;              /* Pointer to next VARIANCE value */
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
    ogrp = smf_ndg_copy( igrp, grpindex, grpindex, 0, status );
  } else {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Must supply a filename or a group."
            " Possible programming error.", status );
    return;
  }

  /* Check for QUALITY components, and header */
  qual = smf_select_cqualpntr( data, &qfamily, status );

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
  } else if ( data->ndims == 2 ) {
    ntslice = 1;
    nelem = data->dims[0] * data->dims[1];
    nbolo = nelem;
    var = (data->pntr)[1];
    dbstride = 0;
    dtstride = 1;
    if( var ) flags |= SMF__MAP_VAR;
  } else if( data->ndims == 3 ) {
    /* Dimensions for 3-d data */
    smf_get_dims( data, &nrows, &ncols, &nbolo, &ntslice, &nelem, &dbstride,
                  &dtstride, status );

    /* Only handle variance override for 3d data */
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
      vntslice = ntslice;
      vnbolo = nbolo;
      vbstride = dbstride;
      vtstride = dtstride;
    }

    if( var ) flags |= SMF__MAP_VAR;

    if( single ) {

       /* Find a bolometer with a good value */
       for( ibolo = 0; ibolo < nbolo && singlebolo == -1; ibolo++ ) {
          pd = ((double *) (data->pntr)[0] ) + ibolo*dbstride;
          pq = qual ? qual + ibolo*dbstride : NULL;
          for( itime = 0; itime < ntslice; itime++ ) {
             if( *pd != VAL__BADD && ( !pq || *pq == 0 ) ) {
                singlebolo = ibolo;
                break;
             }
             pd += dtstride;
             if( pq ) pq += dtstride;
          }
       }

       /* If no good values were found, just use the first bolometer. */
       if( singlebolo == -1 ) singlebolo = 0;

       /* Modify the bounds for the output NDF to make it effectively 1D. */
       for (i = 0; i < data->ndims; i++) {
         if( (data->dims)[i] == nrows || (data->dims)[i] == ncols ) {
           ubnd[i] = lbnd[i] = 1;
         }
       }

    }

  } else {
    /* For strange dimensions don't
       try to write variance or quality */
    var = NULL;
    qual = NULL;

    /* Word out size of data */
    nelem = 1;
    for( i=0; i<data->ndims; i++ ) {
      nelem = nelem*data->dims[i];
    }
  }

  /* Say that we are going to write a file */
  grpMsg( "NAME", ogrp, 1 );
  msgOutif( msglev, "", FUNC_NAME ": writing ^NAME", status );

  /* Open the file */
  smf_open_newfile( wf, ogrp, 1, data->dtype, data->ndims, lbnd, ubnd,
                    flags, &outdata, status );

  if (*status == SAI__OK) {
    smfFile * outfile = outdata->file;
    smfHead * inhdr = data->hdr;
    size_t nbperel = 0;

    /* provenance propagation - but only if we have provenance to propagate */
    if ( (data->file && data->file->ndfid && data->file->ndfid != NDF__NOID) ||
         (provid != NDF__NOID) ) {
      smf_get_taskname( NULL, prvname, status );
      smf_updateprov( outfile->ndfid, data, provid, prvname, NULL, status );
    }

    /* number of bytes per element */
    nbperel = smf_dtype_size( data, status );

    /* Copy the data and variance and quality */
    if (*status == SAI__OK && ( !single || data->ndims != 3 ) ) {

      if( (data->pntr)[0] ) memcpy( (outdata->pntr)[0], (data->pntr)[0],
                                    nelem * nbperel );

      /* Do variance on a timeslice basis in case we are repeating a 2-d
         variance array over time slice. */
      if( var ) {
        outvar = (outdata->pntr)[1];
        /* special cae 2d */
        if (data->ndims == 2) {
          if (outvar) memcpy( outvar, var, nelem * nbperel );
        } else if (data->ndims == 3 && nelem == (vnbolo*vntslice) ) {
          if (outvar) memcpy( outvar, var, nelem * nbperel );
        } else {
          for( i=0; i<nbolo; i++ ) {
            for( j=0; j<ntslice; j++ ) {
              outvar[i*dbstride+j*dtstride] = var[i*vbstride+
                                                  (j%vntslice)*vtstride];
            }
          }
        }
      }

      /* Quality. Just copy from input to output */
      outdata->qbits = data->qbits;
      outdata->qfamily = qfamily;
      if (qual) memcpy( outdata->qual, qual, nelem * sizeof(*qual) );

    /* Now deal with cases were we are storing only a single bolometer
    from a 3D smfData. */
    } else {
       pd = ((double *) (data->pntr)[0]) + singlebolo*dbstride;
       pq = qual ? qual + singlebolo*dbstride : NULL;
       pv = var ? var + singlebolo*vbstride : NULL;
       for( itime = 0; itime < ntslice; itime++ ) {
          ((double *)(outdata->pntr)[0])[itime] = *pd;
          if( pq ) ((smf_qual_t *)(outdata->qual))[itime] = *pq;
          if( var ) {
             ((double *)(outdata->pntr)[1])[itime] = pv[(itime%vntslice)*vtstride];
          }
          pd += dtstride;
          if( pq ) pq += dtstride;
       }
       outdata->qfamily = qfamily;
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

      /* WCS: if tswcs exists use it, otherwise wcs to handle 2d images */
      if( inhdr->tswcs ) {
        ndfPtwcs( inhdr->tswcs, outfile->ndfid, status );
        /* annul bad status caused by dimension mismatch */
        if( *status == NDF__NAXIN ) {
          errAnnul( status );
          msgOutif( MSG__VERB, "", FUNC_NAME
                    ": warning, tswcs not written due to dimension mismatch",
                    status );
        }
      } else if( inhdr->wcs ) {
        ndfPtwcs( inhdr->wcs, outfile->ndfid, status );
        /* annul bad status caused by dimension mismatch */
        if( *status == NDF__NAXIN ) {
          errAnnul( status );
          msgOutif( MSG__VERB, "", FUNC_NAME
                    ": warning, wcs not written due to dimension mismatch",
                    status );
        }
      }

      /* JCMT State -- if ntslice is known */
      if( inhdr->allState ) {

        sc2store_writejcmtstate( outfile->ndfid, inhdr->nframes,
                                 inhdr->allState, status );

      }

      /* Other stuff - more accurate than using a FITS header. */
      HDSLoc *xloc = NULL;
      int there = 0;
      ndfXstat( outfile->ndfid, SMURF__EXTNAME, &there, status );
      if( !there ) ndfXnew( outfile->ndfid, SMURF__EXTNAME, SMURF__EXTTYPE,
                            0, NULL, &xloc, status );
      ndfXpt0d( inhdr->steptime, outfile->ndfid, SMURF__EXTNAME,
                "STEPTIME", status );
      ndfXpt0d( inhdr->scanvel, outfile->ndfid, SMURF__EXTNAME,
                "SCAN_VEL", status );
      datAnnul( &xloc, status );
    }

    /* Dark squids */
    if( *status == SAI__OK ) {
      smfDA *da = data->da;
      HDSLoc *loc=NULL;
      int id;
      int nmap;
      void *pntr[]={NULL,NULL};
      double *outdksquid=NULL;

      if( da && da->dksquid && da->dksquid->pntr[0] && outdata &&
          outdata->file && outdata->file->ndfid &&
          (outdata->file->ndfid != NDF__NOID) ) {

        loc = smf_get_xloc( outdata, "SCUBA2", "SCUBA2", "UPDATE", 0, 0,
                            status );

        lbnd[0]=0;
        lbnd[1]=1;
        ubnd[0]=lbnd[0]+ncols-1;
        ubnd[1]=lbnd[1]+ntslice-1;

        id = smf_get_ndfid( loc, "DKSQUID", "WRITE", "UNKNOWN", "_DOUBLE",
                            2, lbnd, ubnd, status );

        ndfMap( id, "DATA", "_DOUBLE", "WRITE", &pntr[0], &nmap, status );
        outdksquid = pntr[0];

        if( (*status == SAI__OK) && outdksquid ) {
          memcpy( outdksquid, da->dksquid->pntr[0], nmap*sizeof(*outdksquid) );
        }

        /* Also do the quality if it exists. Map it as smf_qual_t, copy the
           quality, unmap it to force write to disk. */
        if( da->dksquid->qual ) {
          size_t nqmap;
          smf_qual_t * outdkqual = smf_qual_map( wf, id, "WRITE", NULL, &nqmap, status );
          da->dksquid->qfamily = SMF__QFAM_TSERIES; /* always */
          if( (*status==SAI__OK) && outdkqual ) {
            memcpy( outdkqual, da->dksquid->qual, nmap*sizeof(*outdkqual) );
          }
          outdkqual = smf_qual_unmap( wf, id, da->dksquid->qfamily,
                                      outdkqual, SMF__Q_GOOD, status );
        }

        ndfAnnul( &id, status );
        datAnnul( &loc, status );
      }
    }

    /* FTS2 */
    smfFts* fts = data->fts;
    if( *status == SAI__OK &&
        fts &&
        outdata &&
        outdata->file &&
        outdata->file->ndfid &&
        (outdata->file->ndfid != NDF__NOID) ) {
      int id            = 0;
      int nmap          = 0;
      void* pntr        = NULL;
      int* outzpd       = NULL;
      double* outfpm    = NULL;
      double* outsigma  = NULL;
      int* outdead      = NULL;
      double* outa      = NULL;
      double* outb      = NULL;
      double* outc      = NULL;
      double* outd      = NULL;
      double* outphaseFit= NULL;
      double* outcosmicRays = NULL;
      double* outfluxJumps = NULL;
      HDSLoc* loc       = NULL;

      if( (fts->zpd && fts->zpd->pntr[0]) ||
          (fts->fpm && fts->fpm->pntr[0]) ||
          (fts->sigma && fts->sigma->pntr[0]) ||
          (fts->dead && fts->dead->pntr[0]) ||
          (fts->a && fts->a->pntr[0]) ||
          (fts->b && fts->b->pntr[0]) ||
          (fts->c && fts->c->pntr[0]) ||
          (fts->d && fts->d->pntr[0]) ||
          (fts->phaseFit && fts->phaseFit->pntr[0]) ||
          (fts->cosmicRays && fts->cosmicRays->pntr[0]) ||
          (fts->fluxJumps && fts->fluxJumps->pntr[0])) {
        loc = smf_get_xloc(outdata, "FTS2", "FTS2", "UPDATE", 0, 0, status);
      }

      if(*status == SAI__OK && loc != NULL) {
        /* WRITE ZPD */
        if(fts->zpd && fts->zpd->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          ubnd[0] = lbnd[0] + fts->zpd->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->zpd->dims[1] - 1;
          id = smf_get_ndfid( loc, "ZPD", "WRITE", "UNKNOWN", "_INTEGER",
                              fts->zpd->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_INTEGER", "WRITE", &pntr, &nmap, status);
          outzpd = pntr;
          if((*status == SAI__OK) && outzpd) {
            memcpy(outzpd, fts->zpd->pntr[0], nmap * sizeof(*outzpd));
          }
          ndfAnnul(&id, status);
        }

        /* WRITE FPM */
        if(fts->fpm && fts->fpm->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          lbnd[2] = 1;
          ubnd[0] = lbnd[0] + fts->fpm->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->fpm->dims[1] - 1;
          ubnd[2] = lbnd[2] + fts->fpm->dims[2] - 1;
          id = smf_get_ndfid( loc, "FPM", "WRITE", "UNKNOWN", "_DOUBLE",
                              fts->fpm->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap, status);
          outfpm = pntr;
          if((*status == SAI__OK) && outfpm) {
            memcpy(outfpm, fts->fpm->pntr[0], nmap * sizeof(*outfpm));
          }
          ndfAnnul(&id, status);
        }

        /* WRITE STANDARD DEVIATION, SIGMA */
        if(fts->sigma && fts->sigma->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          ubnd[0] = lbnd[0] + fts->sigma->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->sigma->dims[1] - 1;
          id = smf_get_ndfid( loc, "SIGMA", "WRITE", "UNKNOWN", "_DOUBLE",
                              fts->sigma->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap, status);
          outsigma = pntr;
          if((*status == SAI__OK) && outsigma) {
            memcpy(outsigma, fts->sigma->pntr[0], nmap * sizeof(*outsigma));
          }
          ndfAnnul(&id, status);
        }

        /* WRITE DEAD */
        if(fts->dead && fts->dead->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          ubnd[0] = lbnd[0] + fts->dead->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->dead->dims[1] - 1;
          id = smf_get_ndfid( loc, "DEAD", "WRITE", "UNKNOWN", "_INTEGER",
                              fts->dead->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_INTEGER", "WRITE", &pntr, &nmap, status);
          outdead = pntr;
          if((*status == SAI__OK) && outdead) {
              memcpy(outdead, fts->dead->pntr[0], nmap * sizeof(*outdead));
          }
          ndfAnnul(&id, status);
        }

        /* WRITE A */
        if(fts->a && fts->a->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          ubnd[0] = lbnd[0] + fts->a->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->a->dims[1] - 1;
          id = smf_get_ndfid( loc, "A", "WRITE", "UNKNOWN", "_DOUBLE",
                              fts->a->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap, status);
          outa = pntr;
          if((*status == SAI__OK) && outa) {
            memcpy(outa, fts->a->pntr[0], nmap * sizeof(*outa));
          }
          ndfAnnul(&id, status);
        }

        /* WRITE B */
        if(fts->b && fts->b->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          ubnd[0] = lbnd[0] + fts->b->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->b->dims[1] - 1;
          id = smf_get_ndfid( loc, "B", "WRITE", "UNKNOWN", "_DOUBLE",
                              fts->b->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap, status);
          outb = pntr;
          if((*status == SAI__OK) && outb) {
            memcpy(outb, fts->b->pntr[0], nmap * sizeof(*outb));
          }
          ndfAnnul(&id, status);
        }

        /* WRITE C */
        if(fts->c && fts->c->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          ubnd[0] = lbnd[0] + fts->c->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->c->dims[1] - 1;
          id = smf_get_ndfid( loc, "C", "WRITE", "UNKNOWN", "_DOUBLE",
                              fts->c->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap, status);
          outc = pntr;
          if((*status == SAI__OK) && outc) {
            memcpy(outc, fts->c->pntr[0], nmap * sizeof(*outc));
          }
          ndfAnnul(&id, status);
        }

        /* WRITE D */
        if(fts->d && fts->d->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          ubnd[0] = lbnd[0] + fts->d->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->d->dims[1] - 1;
          id = smf_get_ndfid( loc, "D", "WRITE", "UNKNOWN", "_DOUBLE",
                              fts->d->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap, status);
          outd = pntr;
          if((*status == SAI__OK) && outd) {
            memcpy(outd, fts->d->pntr[0], nmap * sizeof(*outd));
          }
          ndfAnnul(&id, status);
        }

        /* WRITE PHASEFIT */
        if(fts->phaseFit && fts->phaseFit->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          ubnd[0] = lbnd[0] + fts->phaseFit->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->phaseFit->dims[1] - 1;
          id = smf_get_ndfid( loc, "PHASEFIT", "WRITE", "UNKNOWN", "_DOUBLE",
                              fts->phaseFit->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap, status);
          outphaseFit = pntr;
          if((*status == SAI__OK) && outphaseFit) {
            memcpy(outphaseFit, fts->phaseFit->pntr[0], nmap * sizeof(*outphaseFit));
          }
          ndfAnnul(&id, status);
        }

        /* WRITE COSMICRAYS */
        if(fts->cosmicRays && fts->cosmicRays->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          ubnd[0] = lbnd[0] + fts->cosmicRays->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->cosmicRays->dims[1] - 1;
          id = smf_get_ndfid( loc, "COSMICRAYS", "WRITE", "UNKNOWN", "_DOUBLE",
                              fts->cosmicRays->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap, status);
          outcosmicRays = pntr;
          if((*status == SAI__OK) && outcosmicRays) {
            memcpy(outcosmicRays, fts->cosmicRays->pntr[0], nmap * sizeof(*outcosmicRays));
          }
          ndfAnnul(&id, status);
        }

        /* WRITE FLUXJUMPS */
        if(fts->fluxJumps && fts->fluxJumps->pntr[0]) {
          lbnd[0] = 0;
          lbnd[1] = 0;
          ubnd[0] = lbnd[0] + fts->fluxJumps->dims[0] - 1;
          ubnd[1] = lbnd[1] + fts->fluxJumps->dims[1] - 1;
          id = smf_get_ndfid( loc, "FLUXJUMPS", "WRITE", "UNKNOWN", "_DOUBLE",
                              fts->fluxJumps->ndims, lbnd, ubnd, status);
          ndfMap(id, "DATA", "_DOUBLE", "WRITE", &pntr, &nmap, status);
          outfluxJumps = pntr;
          if((*status == SAI__OK) && outfluxJumps) {
            memcpy(outfluxJumps, fts->fluxJumps->pntr[0], nmap * sizeof(*outfluxJumps));
          }
          ndfAnnul(&id, status);
        }

        datAnnul(&loc, status);
      }
    }

  /* If required, call the user supplied function. */
    if( func ) (*func)( wf, outfile->ndfid, info, status );
  }

  /* Close the output file */
  smf_close_file( wf, &outdata, status );
  grpDelet( &ogrp, status );

}
