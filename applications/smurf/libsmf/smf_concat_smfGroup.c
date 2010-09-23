/*
 *+
 *  Name:
 *     smf_concat_smfGroup

 *  Purpose:
 *     Concatenate many small chunks of data into single large chunks.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     C function

 *  Invocation:
 *     smf_concat_smfGroup( smfWorkForce *wf, const smfGroup *igrp,
 *                          const smfArray *darks, const smfArray *bbms,
 *                          const smfArray *flatramps,
 *                          size_t whichchunk, int ensureflat, int isTordered,
 *                          AstFrameSet *outfset, int moving,
 *                          int *lbnd_out, int *ubnd_out, dim_t req_padStart,
 *                          dim_t req_padEnd, int flags, int tstep,
 *                          smfArray **concat, int *status )

 *  Arguments:
 *     wf = smfWorkForce * (Given)
 *        Pointer to a pool of worker threads (can be NULL)
 *     igrp = const smfGroup* (Given)
 *        Group of input data files
 *     darks = const smfArray * (Given)
 *        Collection of darks that can be applied to non-flatfielded data.
 *        Can be NULL.
 *     bbms = const smfArray * (Given)
 *        Masks for each subarray (e.g. returned by smf_reqest_mask call)
 *     flatramps = const smfArray * (Given)
 *        Collection of flatfield ramps. Will be passed to smf_open_and_flatfield.
 *     whichchunk = size_t (Given)
 *        Which continuous subset of igrp will get concatenated?
 *     ensureflat = int (Given)
 *        If true, ensure that the flatfield is applied when opening the data
 *        files, else, if they are already flatfielded read them as is, or if
 *        they are raw files read them as raw files converted to double.
 *     isTordered = int (Given)
 *        If 0, ensure concatenated data is ordered by bolometer. If 1 ensure
 *        concatenated data is ordered by time slice (default ICD ordering)
 *     outfset = AstFrameSet* (Given)
 *        Frameset containing the sky->output map mapping if calculating
 *        pointing LUT on-the-fly
 *     moving = int (Given)
 *        Is coordinate system tracking moving object? (if outfset specified)
 *     lbnd_out = double* (Given)
 *        2-element array pixel coord. for the lower bounds of the output map
 *        (if outfset specified)
 *     ubnd_out = double* (Given)
 *        2-element array pixel coord. for the upper bounds of the output map
 *        (if outfset specified)
 *     req_padStart = dim_t (Given)
 *        Pad start of concatenated array with this many samples. Will have no
 *        effect if the data have been padded previously.
 *     req_padEnd = dim_t (Given)
 *        Pad end of concatenated array with this many samples. Will have no
 *        effect if the data have been padded previously.
 *     flags = int (Given)
 *        Additional flags to control processing of individual data files
 *        as they are being concatenated.
 *     tstep = int (Given)
 *        The increment in time slices between full Mapping calculations.
 *        The Mapping for intermediate time slices will be approximated.
 *     concat = smfArray ** (Returned)
 *        smfArray containing concatenated data for each subarray
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This function takes an input group containing data taken continuously,
 *     but chopped up into smaller files (possibly from multiple subarrays).
 *     This routine attempts to load all of the data into memory at once,
 *     concatenates it into a single contiguous piece of memory for each
 *     subarray, and optionally re-orders the data to bolo-ordered rather
 *     than time-ordered if desired. If a pointing LUT is to be calculated
 *     as data is being loaded, specify outfset, moving, lbnd_out and
 *     ubnd_out. Otherwise set outfset to NULL.
 *
 *  Authors:
 *     EC: Edward Chapin (UBC)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     COBA: Coskun Oba (UoL)
 *     {enter_new_authors_here}

 *  History:
 *     2007-10-19 (EC):
 *        Initial version.
 *     2007-10-29 (EC):
 *        -Fixed loop bounds for addressing DATA/VARIANCE/QUALITY memory
 *        -Inserted Ast status check after copying FITS headers
 *        -Fixed bug in reference file dimensions
 *        -Modified interface to smf_open_file.
 *     2007-11-15 (EC):
 *        -Added projection information, flags and isTordered to interface.
 *        -With projection info pointing LUT can now be calculated on-the-fly
 *     2007-11-28 (EC):
 *        -Use smf_open_file with SMF__NOCREATE_DATA for first pass
 *        -Set bad status if input is not ICD-compliant time-ordered data
 *        -Fixed bug in time-axis length which depends on output data order
 *     2007-12-14 (EC):
 *        -close reference data
 *        -modified smf_calc_mapcoord interface
 *        -properly set isTordered flag
 *     2008-01-25 (EC):
 *        -use smf_open_and_flatfield in case input data is raw
 *        -store name of first file of subarray with "_con" suffix
 *     2008-02-08 (EC):
 *        -Fixed data type for QUALITY
 *        -generate QUALITY array if not present
 *        -Use SMF__NOCREATE* flags
 *     2008-04-16 (EC):
 *        -added chunking based on time stamps
 *     2008-04-17 (EC):
 *        -fixed calculation of number of subarrays
 *     2008-04-23 (EC):
 *        -propagate time series WCS
 *     2008-06-24 (EC):
 *        Added ability to pad start and end of the data (padStart/padEnd)
 *     2008-07-03 (EC):
 *        Correct time origin in tswcs if padStart set.
 *     2008-07-11 (TIMJ):
 *        Use strlcat/strlcpy
 *     2008-07-22 (TIMJ):
 *        Apply darks.
 *     2008-07-29 (TIMJ):
 *        Steptime is now in smfHead.
 *     2008-09-09 (EC):
 *        Concat dark squid signals.
 *     2008-12-04 (EC):
 *        Padded JCMTState filled with first/last real values.
 *     2009-01-07 (EC):
 *        Stride-ify
 *     2009-01-12 (EC):
 *        Add bad pixel masks (bpms) to interface
 *     2009-09-29 (TIMJ):
 *        Handle pixel origin in concatenated smfData
 *     2009-10-02 (TIMJ):
 *        Copy more information from reference smfData
 *        Allow flatfielding to be disabled.
 *     2010-01-08 (AGG):
 *        Change BPM to BBM.
 *     2010-03-11 (TIMJ):
 *        Add flatramps argument.
 *     2010-05-27 (EC):
 *        Calculate tswcs properly when padding using smf_create_tswcs
 *     2010-06-10 (EC):
 *        Handle dark squid QUALITY
 *     2010-06-14 (TIMJ):
 *        Refactor loops for separate quality component in smfData
 *     2010-07-01 (TIMJ):
 *        Fix smfDA logic so that we again copy more than just
 *        the dksquid item.
 *     2010-07-14 (TIMJ):
 *        Simplify multi-pass logic
 *        Propagate quality that was in flatfielded data even if not in raw
 *     2010-07-16 (TIMJ):
 *        Only pad if the data have not already been padded.
 *     2010-09-21 (COBA):
 *        Add SMF__NOCREATE_FTS

 *  Notes:
 *     If projection information supplied, pointing LUT will not be
 *     concatenated if SMF__NOCREATE_LUT is specified. By default, a
 *     QUALITY array is created even if one is not present in the
 *     template file. This behaviour can be avoided by setting flag
 *     bit SMF__NOCREATE_QUALITY. Additionally, if VARIANCE and/or
 *     QUALITY is present in the template, prevent propagation to the
 *     concatenated file by setting SMF__NOCREATE_VARIANCE /
 *     SMF__NOCREATE_QUALITY. Specifying padStart and/or padEnd will
 *     pad the data with the specified number of samples unless the data
 *     have already been padded. Also note that the new padded region will have:
 *       - DATA and VARIANCE values set to 0
 *       - QUALITY set to SMF__Q_PAD
 *       - QUALITY also set to SMF__Q_BADB if the BADB flag was set at the
 *         start of the real data
 *       - LUT is set to VAL__BADI
 *       - the JCMTState values all set to 0

 *  Copyright:
 *     Copyright (C) 2007-2010 University of British Columbia.
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *
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
 *     MA 02111-1307, USA.

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"
#include "kpg_err.h"
#include "star/one.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libaztec/aztec.h"

#define FUNC_NAME "smf_concat_smfGroup"

void smf_concat_smfGroup( smfWorkForce *wf, const smfGroup *igrp,
                          const smfArray *darks, const smfArray *bbms,
                          const smfArray *flatramps, size_t whichchunk,
                          int ensureflat, int isTordered,
                          AstFrameSet *outfset, int moving,
                          int *lbnd_out, int *ubnd_out, dim_t req_padStart,
                          dim_t req_padEnd, int flags, int tstep,
                          smfArray **concat, int *status ) {

  /* Local Variables */
  size_t bstr;                  /* Concatenated bolo stride */
  smfDA *da=NULL;               /* Pointer to smfDA struct */
  smfData *data=NULL;           /* Concatenated smfData */
  int flag;                     /* Flag */
  char filename[GRP__SZNAM+1];  /* Input filename, derived from GRP */
  dim_t firstpiece = 0;         /* index to start of whichchunk */
  int foundfirst=0;             /* Flag indicates if first index found */
  int foundlast=0;              /* Flag indicates if last index found */
  int havearray[2];             /* flags for DATA/VARIANCE present */
  int havequal=0;               /* flag for QUALITY present */
  int havelut=0;                /* flag for pointing LUT present */
  smfHead *hdr;                 /* pointer to smfHead in concat data */
  dim_t i;                      /* Loop counter */
  Grp *ingrp=NULL;              /* Pointer to 1-element input group */
  dim_t j;                      /* Loop counter */
  dim_t k;                      /* Loop counter */
  dim_t l;                      /* Loop counter */
  dim_t m;                      /* Loop counter */
  dim_t lastpiece = 0;          /* index to end of whichchunk */
  dim_t nbolo=0;                /* Number of detectors */
  dim_t ncol=0;                 /* Number of columns */
  dim_t ndata;                  /* Total data points: nbolo*tlen */
  dim_t nrelated;               /* Number of subarrays */
  dim_t nrow;                   /* Number of rows */
  Grp *outgrp=NULL;             /* Pointer to 1-element output group */
  size_t outgrpsize;            /* Size of outgrp */
  dim_t padEnd = 0;             /* Padding to use for end of this chunk */
  dim_t padStart = 0;           /* Padding to use for start of this chunk */
  char *pname;                  /* Pointer to input filename */
  smfData *refdata=NULL;        /* Reference smfData */
  smf_dtype refdtype;           /* reference DATA/VARIANCE type */
  const char *refdtypestr;      /* const string for reference data type */
  smfHead *refhdr=NULL;         /* pointer to smfHead in ref data */
  dim_t refncol=0;              /* reference number of rows */
  dim_t refndata;               /* Number data points in reference file */
  dim_t refnrow=0;              /* reference number of rows */
  dim_t reftlen;                /* Number of time slices in reference file */
  size_t rbstr;                 /* Reference bolo stride */
  size_t rtstr;                 /* Reference time slice stride */
  JCMTState *sourceState=NULL;  /* temporary JCMTState pointer */
  dim_t tchunk = 0;             /* Time offset in concat. array this chunk */
  dim_t tend;                   /* Time at start of padded region */
  dim_t tlen;                   /* Time length entire concatenated array */
  dim_t tstart;                 /* Time at end of padded region */
  size_t tstr;                  /* Concatenated time slice stride */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Verify that we have a valid whichchunk, and determine the range of
     indices into igrp->chunk */
  if( whichchunk > igrp->chunk[igrp->ngroups-1] ) {
    msgSeti( "WHICHCHUNK", whichchunk );
    msgSeti( "MAXCHUNK", igrp->chunk[igrp->ngroups-1] );
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME
            ": Invalid whichchunk: ^WHICHCHUNK. Must be 0 - ^MAXCHUNK",
            status );
  } else {
    /* Find the range of indices */
    foundfirst = 0;
    foundlast = 0;

    for( i=0; i<igrp->ngroups; i++ ) {
      if( (igrp->chunk[i] == whichchunk) && (!foundfirst) ) {
        firstpiece = i;
        foundfirst = 1;
      }

      if( igrp->chunk[i] == whichchunk ) {
        lastpiece = i;
        foundlast = 1;
      }
    }
  }

  /* Check that a valid range was actually found */
  if( (!foundfirst) || (!foundlast) ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME
            ": Possible programming error, couldn't find valid chunk range",
            status );
    return;
  }

  /* Allocate space for the smfArray */
  *concat = smf_create_smfArray( status );

  /* Determine how many subarrays there actually are in this chunk*/
  nrelated = 0;
  for( i=0; (*status == SAI__OK) && i<igrp->nrelated; i++ ) {
    for( j=firstpiece; j<=lastpiece; j++ ) {
      if( (igrp->subgroups[j][i] > 0) && ((i+1) > nrelated) ) {
        nrelated = i+1;
      }
    }
  }

  /* Loop over related elements (number of subarrays) */
  for( i=0; (*status == SAI__OK) && i<nrelated; i++ ) {
    /* Initialize time length of concatenated array. We will add on
       padding if the first and last files have not already recevied
       padding */
    tlen = 0;
    padEnd = 0;
    padStart = 0;

    /* Two passes over data for the subarray: first time to identify
       dimensions of each file, second time to actually open each file
       and copy into single array. */

    /* Loop over subgroups (number of time chunks), continuing only
       if the chunk is equal to whichchunk */
    for( j=firstpiece; j<=lastpiece; j++ ) {
      /* First pass through the data - get dimensions */

      smf_open_file( igrp->grp, igrp->subgroups[j][i], "READ",
                     flags, &refdata, status );

      /* Verify that the array is 3-dimensional and compatible with the
         reference array dimensions. */
      if( *status == SAI__OK ) {
        smf_smfFile_msg( refdata->file, "FILE", 1, "<unknown file", status );

        if( refdata->ndims != 3 ) {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME
                  ": ^FILE does not contain 3-dimensional data!",
                  status );
        }
      }

      /* If data order is 0 (bolo-ordered) then fail since that case
         is not currently handled. */
      if( (*status == SAI__OK) && (refdata->isTordered == 0) ) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME
                ": ^FILE contains bolo-ordered data (unsupported)",
                status);
      }

      /* Get data dimensions */
      smf_get_dims( refdata, &nrow, &ncol,
                    NULL, &reftlen, &refndata, NULL, NULL, status );

      if( *status == SAI__OK ) {
        if( j == firstpiece ) {
          /* If this is the first chunk we will use it for refdims
             - check the number of bolometers! */
          refnrow = nrow;
          refncol = ncol;

          /* Check for DATA/VARIANCE/QUALITY and data type */
          for( k=0; k<2; k++ ) {
            havearray[k] = (refdata->pntr[k] != NULL);
          }
          havequal = ( refdata->qual != NULL );

          /* Look for padding at start. Just need to look at the quality
             of the very first element. */
          if (refdata && refdata->qual && (refdata->qual)[0] & SMF__Q_PAD ) {
            padStart = 0;
          } else {
            /* Use the requested value */
            padStart = req_padStart;
          }

          /* Concatenated data is always double precision */
          refdtype = SMF__DOUBLE;
          refdtypestr = smf_dtype_string(refdata, status);

        } else {
          /* Check these dims against refdims */
          if( (nrow != refnrow) || (ncol != refncol) ) {
            *status = SAI__ERROR;
            msgSeti( "XREF", refnrow );
            msgSeti( "YREF", refncol );
            msgSeti( "X", nrow );
            msgSeti( "Y", ncol );
            errRep( "", FUNC_NAME ": Detector dimensions (^X,^Y) in "
                    "^FILE do not match reference (^XREF,^YREF)", status );
          }

          /* Check existence of DATA/QUALITY/VARIANCE */
          if( (refdata->pntr[0] != NULL) != havearray[0] ) {
            *status = SAI__ERROR;
            if( havearray[0] ) msgSetc( "FLAG", "is missing" );
            else msgSetc( "FLAG", "has extra" );
            errRep( "", FUNC_NAME ": ^FILE ^FLAG component DATA", status );
          }

          if( (refdata->pntr[1] != NULL) != havearray[1] ) {
            *status = SAI__ERROR;
            if( havearray[1] ) msgSetc( "FLAG", "is missing" );
            else msgSetc( "FLAG", "has extra" );
            errRep( "", FUNC_NAME
                    ": ^FILE ^FLAG component VARIANCE", status );
          }

          if( (refdata->qual != NULL) != havequal ) {
            *status = SAI__ERROR;
            if( havequal ) msgSetc( "FLAG", "is missing" );
            else msgSetc( "FLAG", "has extra" );
            errRep( "", FUNC_NAME
                    ": ^FILE ^FLAG component QUALITY", status );
          }
        }

        /* Check padding at end */
        if (j == lastpiece) {
          if (refdata && refdata->qual && (refdata->qual)[refndata-1] & SMF__Q_PAD ) {
            padEnd = 0;
          } else {
            padEnd = req_padEnd;
          }
        }

      }

      if( *status == SAI__OK ) {
        /* At this stage increment tlen for this chunk */
        tlen += reftlen;
      }

      /* Close the reference file */
      smf_close_file( &refdata, status );
    }

    /* Add any padding to the length */
    tlen += padStart + padEnd;

    /* Loop over subgroups (number of time chunks), continuing only
       if the chunk is equal to whichchunk */
    for( j=firstpiece; j<=lastpiece; j++ ) {

      /* Second pass copy data over to new array */
      if( *status == SAI__OK ) {
        /* Open the file corresponding to this chunk. Data may
           require flat-fielding. */
        if (ensureflat) {
          smf_open_and_flatfield( igrp->grp, NULL, igrp->subgroups[j][i],
                                  darks, flatramps, &refdata, status );
        } else {
          /* open as raw if raw else just open as whatever we have */
          smfData * tmpdata = NULL;
          smf_open_file( igrp->grp, igrp->subgroups[j][i], "READ",
                         SMF__NOCREATE_DATA, &tmpdata, status );
          if (tmpdata && tmpdata->file && tmpdata->file->isSc2store) {
            smf_open_raw_asdouble( igrp->grp, igrp->subgroups[j][i],
                                   darks, &refdata, status );
          } else {
            smf_open_and_flatfield( igrp->grp, NULL, igrp->subgroups[j][i],
                                    darks, flatramps, &refdata, status );
          }
          smf_close_file( &tmpdata, status );
        }

        /* Set havequal flag based on first file. This is required
           because the initial pass through for dimensions only looked
           at the file itself and not the result of flatfielding.
           Flatfielding creates a quality array. Without this raw data
           will not have quality values propagated. */
        if ( j == firstpiece ) {
          havequal = ( refdata->qual != NULL );
        }

        /* Apply bad bolometer mask */
        smf_apply_mask( refdata, bbms, SMF__BBM_DATA, 0, status );

        /* Calculate the pointing LUT if requested */
        if( !(flags & SMF__NOCREATE_LUT) && outfset ) {

          /* Set havelut flag */
          havelut = 1;

          /* Calculate the LUT for this chunk */
          smf_calc_mapcoord( wf, refdata, outfset, moving, lbnd_out, ubnd_out,
                             SMF__NOCREATE_FILE, tstep, status );
        } else {
          havelut = 0;
        }

        /* Get reference dimensions/strides */
        smf_get_dims( refdata, NULL, NULL, &nbolo, &reftlen, &refndata,
                      &rbstr, &rtstr, status );

        if( *status == SAI__OK ) {
          /* If first chunk initialize the concatenated array */
          if( j == firstpiece ) {

            /* Copy first data right after the initial padding */
            tchunk = padStart;

            /* Allocate memory for empty smfData with a smfHead. Create
               a DA struct only if the input file has one. Create it as
               a clone rather than creating an empty smfDa. */
            data = smf_create_smfData( SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status );
            if (refdata->da && data) {
              /* do not copy dark squids. We do that below */
              data->da = smf_deepcopy_smfDA( refdata, 0, status );
              da = data->da;
            }

            if (refdata->history) data->history = astCopy( refdata->history );

            if( *status == SAI__OK ) {
              data->qfamily = refdata->qfamily;
              /* Copy over basic header information from the reference */
              hdr = data->hdr;
              refhdr = refdata->hdr;
              hdr->instrument = refhdr->instrument;
              hdr->scanvel = refhdr->scanvel;
              hdr->steptime = refhdr->steptime;
              hdr->obsmode = refhdr->obsmode;
              hdr->obstype = refhdr->obstype;
              hdr->swmode = refhdr->swmode;
              smf_set_clabels( refhdr->title, refhdr->dlabel,
                               refhdr->units, hdr, status );

              switch ( hdr->instrument ) {
              case INST__AZTEC:
                aztec_fill_smfHead( hdr, NDF__NOID, status );
                break;
              default:
                break;
                /* SCUBA-2 has nothing special here because the focal plane
                   coordinates are derived using an AST polyMap */
              }

              /* Copy over the name of the first file in
                 subarray. Use a grpex to strip off the path, and
                 then add the suffix "_con.dimm" to denote concatenated
                 iterative map-maker data */

              ingrp = grpNew( "GRP", status );
              outgrp = grpNew( "GRP", status );

              ndgCpsup( igrp->grp, igrp->subgroups[j][i], ingrp, status );
              ndgCrexp( "./*_con" SMF__DIMM_SUFFIX "|.sdf||", ingrp, &outgrp,
                        &outgrpsize, &flag, status );

              pname = filename;
              grpGet( outgrp, 1, 1, &pname, SMF_PATH_MAX, status);

              grpDelet( &ingrp, status );
              grpDelet( &outgrp, status );

              one_strlcpy( data->file->name, filename,
                           sizeof(data->file->name), status );

              /* Allocate space for the concatenated allState */
              hdr->nframes = tlen;
              hdr->allState = astCalloc( tlen, sizeof(*(hdr->allState)), 1 );

              /* Allocate space in the smfData for DATA/VARAIANCE/QUALITY */
              if( isTordered ) {
                data->dims[SC2STORE__COL_INDEX] = refncol;
                data->dims[SC2STORE__ROW_INDEX] = refnrow;
                data->dims[2] = tlen;
                ncol = data->dims[SC2STORE__COL_INDEX];

                /* lbounds in the concatenated data match the reference */
                for (k = 0; k < 3; k++) {
                  (data->lbnd)[k] = (refdata->lbnd)[k];
                }
              } else {
                data->dims[0] = tlen;
                data->dims[SC2STORE__ROW_INDEX+1] = refnrow;
                data->dims[SC2STORE__COL_INDEX+1] = refncol;
                ncol = data->dims[1+SC2STORE__COL_INDEX];

                /* lbounds must be altered if bolo-ordered concatenated data*/
                data->lbnd[0] = refdata->lbnd[2];
                data->lbnd[1] = refdata->lbnd[0];
                data->lbnd[2] = refdata->lbnd[1];
              }
              data->ndims = 3;

              /* Set the data type and order */
              data->dtype = refdtype;
              data->isTordered = isTordered;
              ndata = nbolo*tlen;

              /* get the strides */
              smf_get_dims( data, NULL, NULL, NULL, NULL, NULL,
                            &bstr, &tstr, status );

              /* Allocate space for enlarged dksquid array. */
              if( da && refdata->da && refdata->da->dksquid ) {
                da->dksquid = smf_create_smfData(SMF__NOCREATE_FILE |
                                                 SMF__NOCREATE_HEAD |
                                                 SMF__NOCREATE_DA |
                                                 SMF__NOCREATE_FTS, status );

                /* Dimensions */
                da->dksquid->dtype = SMF__DOUBLE;
                da->dksquid->isTordered = 1;
                da->dksquid->ndims = 3;

                da->dksquid->dims[0] = ncol;
                da->dksquid->dims[1] = 1;
                da->dksquid->dims[2] = tlen;
                da->dksquid->lbnd[0] = 0;
                da->dksquid->lbnd[1] = 0;
                da->dksquid->lbnd[2] = 1;

                da->dksquid->pntr[0] = astCalloc(ncol*tlen,
                                                 smf_dtype_size(da->dksquid,
                                                                status), 1 );

                /* QUALITY too if requested */
                if( !(flags&SMF__NOCREATE_QUALITY) ) {
                  da->dksquid->qfamily = refdata->da->dksquid->qfamily;
                  da->dksquid->qual = astCalloc(ncol*tlen, sizeof(*(da->dksquid->qual)), 1);
                  if( *status == SAI__OK ) {
                    memset( da->dksquid->qual, 0,
                            ncol*tlen*sizeof(*(da->dksquid->qual)) );
                  }
                }

              }

              /* Un-set havearray values corresponding to flags */
              havearray[0] = havearray[0] && !(flags&SMF__NOCREATE_DATA);
              havearray[1] = havearray[1] && !(flags&SMF__NOCREATE_VARIANCE);
              havequal = havequal && !(flags&SMF__NOCREATE_QUALITY);

              /* Allocate space for arrays being propagated from template */
              for( k=0; k<2; k++ ) if( havearray[k] ) {
                  size_t sz = smf_dtype_sz(data->dtype, status );
                  data->pntr[k] = astCalloc( ndata, sz, 1 );
                }
              if (havequal) {
                data->qual = astCalloc( ndata, sizeof(*(data->qual)), 1 );
              }

              /* Check to see if havearray for QUALITY is not set,
                 but SMF__NOCREATE_QUALITY is also not set. In this
                 case, allocate a fresh QUALITY component that will
                 not require propagation from the template */
              if( !havequal && !(flags & SMF__NOCREATE_QUALITY) ) {
                data->qual = astCalloc(ndata, sizeof(*(data->qual)), 1);
              }

              /* Allocate space for the pointing LUT if needed */
              if( havelut ) {
                data->lut = astCalloc(ndata, sizeof(*(data->lut)), 1 );
              }

              /* Copy over the FITS header */
              if( (*status == SAI__OK) && (refhdr->fitshdr) ) {
                hdr->fitshdr = astCopy( refhdr->fitshdr );
              }

              /* Copy over the TSWCS */
              if( (*status == SAI__OK) && (refhdr->tswcs) ) {
                hdr->tswcs = astCopy( refhdr->tswcs );
              }
            }
          }

          /* Copy DATA/QUALITY/VARIANCE and JCMTstate information into
             concatenated smfData */
          if( *status == SAI__OK ) {
            /* Copy over JCMTstate */
            hdr = data->hdr;
            refhdr = refdata->hdr;

            memcpy( (void *) &(hdr->allState[tchunk]), refhdr->allState,
                    reftlen*sizeof(*hdr->allState) );

            /* Copy LUT */
            if( havelut ) {
              for( l=0; l<nbolo; l++ ) {
                for( m=0; m<reftlen; m++ ) {
                  data->lut[(tchunk+m)*tstr + l*bstr] =
                    refdata->lut[m*rtstr + l*rbstr];
                }
              }
            }

            /* dark squids */
            if( da && da->dksquid && refdata->da && refdata->da->dksquid) {
              double *ptr = da->dksquid->pntr[0];
              smf_qual_t *qptr = da->dksquid->qual;

              ptr += tchunk*ncol;
              memcpy( ptr, refdata->da->dksquid->pntr[0],
                      reftlen*ncol*smf_dtype_size(da->dksquid,status));

              if( qptr && refdata->da->dksquid->qual ) {
                qptr += tchunk*ncol;
                memcpy( qptr, refdata->da->dksquid->qual,
                        reftlen*ncol*sizeof(*qptr) );
              }
            }

            /* Now do DATA/QUALITY/VARIANCE */
            for( k=0; k<2; k++ ) if( havearray[k] ) {
                switch( data->dtype ) {
                case SMF__DOUBLE:
                  for( l=0; l<nbolo; l++ ) {
                    for( m=0; m<reftlen; m++ ) {
                      ((double *)data->pntr[k])[(tchunk+m)*tstr + l*bstr] =
                        ((double *)refdata->pntr[k])[m*rtstr + l*rbstr];
                    }
                  }
                  break;
                default:
                  msgSetc("DTYPE",smf_dtype_string(data, status));
                  *status = SAI__ERROR;
                  errRep( "", FUNC_NAME
                          ": Don't know how to handle ^DTYPE type.", status);
                }
              }
            /* Quality */
            if ( havequal ) {
              for( l=0; l<nbolo; l++ ) {
                for( m=0; m<reftlen; m++ ) {
                  (data->qual)[(tchunk+m)*tstr + l*bstr] =
                    (refdata->qual)[m*rtstr + l*rbstr];
                }
              }
            }

            /* increment tchunk */
            tchunk += reftlen;
          }
        }
        /* Close the file we had open */
        smf_close_file( &refdata, status );
      }

    }

    /* Full subarray is now concatenated. Finish up by filling the padded
       regions of the data with something intelligent.  */

    for( j=0; (*status==SAI__OK)&&(j<2); j++ ) { /* Loop padded region */
      tstart = 0;
      tend = 0;

      if( (j==0) && padStart ) {
        tstart = 0;
        tend = padStart-1;
      }

      if( (j==1) && padEnd ) {
        tstart = tlen-padEnd;
        tend = tlen-1;
      }

      /* Clean up padded region if nonzero length */
      if( tend != tstart ) {

        /* If QUALITY present, set SMF__Q_BADB as needed and SMF__Q_PAD */
        if( data->qual ) {
          /* Loop over bolometer */
          for( k=0; k<nbolo; k++ ) {
            /* SMF__Q_PAD always set */
            smf_qual_t qual = SMF__Q_PAD;

            /* Check for SMF__Q_BADB in first sample of this bolo */
            qual |= (data->qual)[padStart*tstr+k*bstr]&SMF__Q_BADB;

            /* Loop over relevant time slices at set quality */
            for( l=tstart; l<=tend; l++ ) {
              (data->qual)[l*tstr+k*bstr] |=  qual;
            }
          }
        }

        /* Similarly handle QUALITY for dark squids */
        if( da && da->dksquid && da->dksquid->qual ) {
          dim_t dnbolo;
          size_t dbstr, dtstr;

          smf_get_dims( da->dksquid, NULL, NULL, &dnbolo, NULL, NULL, &dbstr,
                        &dtstr, status );

          /* Loop over bolometer (column for dark squids) */
          for( k=0; k<dnbolo; k++ ) {
            /* SMF__Q_PAD always set */
            smf_qual_t qual = SMF__Q_PAD;

            /* Check for SMF__Q_BADB in first sample of this column */
            qual |= (da->dksquid->qual)[padStart*dtstr+k*dbstr] & SMF__Q_BADB;

            /* Loop over relevant time slices at set quality */
            for( l=tstart; l<=tend; l++ ) {
              (da->dksquid->qual)[l*dtstr+k*dbstr] |=  qual;
            }
          }
        }

        /* If LUT present, set data to VAL__BADI */
        if( data->lut ) {
          for( l=tstart; l<=tend; l++ ) {
            for( k=0; k<nbolo; k++ ) {
              data->lut[l*tstr + k*bstr] = VAL__BADI;
            }
          }
        }

        /* If smfHead->allState present, pad with start/finish values, except
           for the RTS_END which we will linearly extrapolate in order to
           making plotting easier on us (since the time axis is implemented
           with a LutMap in the tswcs frameset) */
        if( (data->hdr) && (data->hdr->allState) ) {
          JCMTState *allState = data->hdr->allState;
          double step_rts;
          double ref_rts;

          /* average step size in RTS_END */
          step_rts = (allState[tlen-padEnd-1].rts_end -
                      allState[padStart].rts_end) / (tlen-padStart-padEnd);

          if( step_rts <= 0 ) {
            *status = SAI__ERROR;
            errRep( "", FUNC_NAME ": Error calculating average RTS_END step!",
                    status );
          } else {
            /* Pointer to first/last real JCMTState, and reference RTS_END
               for linear extrapolation. */
            if( j==0 ) {
              sourceState = allState + padStart;
              ref_rts = sourceState->rts_end - padStart*step_rts;
            } else {
              sourceState = allState + tlen - padEnd - 1;
              ref_rts = sourceState->rts_end + step_rts;
            }

            /* Loop Over Time slice */
            for( l=tstart; l<=tend; l++ ) {
              memcpy( &(allState[l]), sourceState, sizeof(*sourceState) );

              /* Extrapolated rts_end into padded region */
              allState[l].rts_end = ref_rts + (l-tstart)*step_rts;
            }
          }
        }
      }
    }

    /* Calculate a new tswcs using the concatenated JCMTState */
    if( (*status==SAI__OK) && data->hdr && data->hdr->allState ) {

      if( data->hdr->tswcs ) data->hdr->tswcs = astAnnul( data->hdr->tswcs );

      smf_create_tswcs( data->hdr, &data->hdr->tswcs, status );

      /* If we couldn't make the tswcs just annul error and continue... we
         don't really need it for anything */
      if( *status != SAI__OK ) {
        errAnnul(status);
        msgOut( "", FUNC_NAME ": Warning, Couldn't create TSWCS for data cube",
                status );

        /* Annul just in case it got made */
        if( data->hdr->tswcs ) data->hdr->tswcs = astAnnul( data->hdr->tswcs );
      }

    }

    /* Put this concatenated subarray into the smfArray */
    smf_addto_smfArray( *concat, data, status );
  }
}
