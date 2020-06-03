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
 *     smf_concat_smfGroup( ThrWorkForce *wf, AstKeyMap *config,
 *                          const smfGroup *igrp, const smfArray *darks,
 *                          const smfArray *bbms, const smfArray *flatramps,
 *                          AstKeyMap * heateffmap, size_t whichchunk,
 *                          int ensureflat, int isTordered,
 *                          AstFrameSet *outfset, int moving, int *lbnd_out,
 *                          int *ubnd_out, fts2Port fts_port,
 *                          dim_t req_padStart,
 *                          dim_t req_padEnd, int flags, smfArray **concat,
 *                          smfData **first, int *status )

 *  Arguments:
 *     wf = ThrWorkForce * (Given)
 *        Pointer to a pool of worker threads (can be NULL)
 *     config = AstKeyMap * (Given)
 *        Pointer to a KeyMap holding configuration parameters. May
 *        be NULL, in which case hard-wired defaults are used for any
 *        configuration parameters that are needed.
 *     igrp = const smfGroup* (Given)
 *        Group of input data files
 *     darks = const smfArray * (Given)
 *        Collection of darks that can be applied to non-flatfielded data.
 *        Can be NULL.
 *     bbms = const smfArray * (Given)
 *        Masks for each subarray (e.g. returned by smf_reqest_mask call)
 *     flatramps = const smfArray * (Given)
 *        Collection of flatfield ramps to apply (optionally) when flatfielding.
 *     heateffmap = AstKeyMap * (Given)
 *        Details of heater efficiency data to be applied during flatfielding.
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
 *     fts_port = fts2Port (Given)
 *        FTS-2 port.
 *     req_padStart = dim_t (Given)
 *        Pad start of concatenated array with this many samples. Will have no
 *        effect if the data have been padded previously.
 *     req_padEnd = dim_t (Given)
 *        Pad end of concatenated array with this many samples. Will have no
 *        effect if the data have been padded previously.
 *     flags = int (Given)
 *        Additional flags to control processing of individual data files
 *        as they are being concatenated.
 *     concat = smfArray ** (Returned)
 *        smfArray containing concatenated data for each subarray. The
 *        supplied pointer may be NULL, in which case "first" argument
 *        is assigned a value and this functon then returns without doing
 *        anything else.
 *     first = smfData ** (Returned)
 *        Address of a smfData pointer in which to return a pointer to a
 *        deep copy of the first smfData in the requested chunk. May be NULL.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This function takes an input group containing data taken
 *     continuously, but chopped up into smaller files (possibly from
 *     multiple subarrays). It returns a smfData for the first input
 *     data file that contributes to the requested chunk (returned via
 *     argument "first"). If "concat" is not NULL, it then attempts to load
 *     all of the data into memory at once, concatenates it into a single
 *     contiguous piece of memory for each subarray, and optionally
 *     re-orders the data to bolo-ordered rather than time-ordered if
 *     desired. If a pointing LUT is to be calculated as data is being
 *     loaded, specify outfset, moving, lbnd_out, ubnd_out and fts_port.
 *     Otherwise set outfset to NULL.
 *
 *     In the case of 4D FFT data, no concatenation is performed. Each input
 *     file (subarray) at the given "whichchunk" is propagated as-is to concat.

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

 *  Authors:
 *     EC: Edward Chapin (UBC)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     COBA: Coskun Oba (UoL)
 *     DSB: David Berry (JAC, Hawaii)
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
 *     2010-10-22 (EC):
 *        Add downsampscale
 *     2010-10-25 (EC):
 *        Move down-sampling length calc from here to smf_grp_related
 *     2010-11-01 (EC):
 *        Handle 4D FFT data
 *     2010-11-04 (COBA):
 *        Propagate FTS2 info
 *     2010-11-15 (EC):
 *        Concatenate theta
 *     2010-12-06 (TIMJ):
 *        Use smf_flat_override
 *        Copy obsidss from refhdr when concatenating.
 *     2011-02-07 (DSB):
 *        Copy instap and telpos from reference header to returned header.
 *     2011-04-07 (DSB):
 *        Open files in a separate thread.
 *     2011-04-20(DSB):
 *        - Added argument first.
 *        - Allow "concat" to be null.
 *     2011-04-21 (TIMJ):
 *        Move some NDG/GRP code earlier in the loop to avoid
 *        a thread problem.
 *     2011-04-28 (EC):
 *        Set SMF__Q_BADDA if ensureflat=0 and SMF__NOCREATE_QUALITY not set
 *     2011-05-04 (DSB):
 *        Set data values to VAL__BADD if they are assigned a qualitu of BADDA.
 *     2011-12-13 (DSB):
 *        Added exportlonlat to API.
 *     2012-02-20 (DSB):
 *        Added "config" to API, and removed "tstep" and "exportlonlat".
 *     2012-04-03 (TIMJ):
 *        Copy smfFile in deepcopy so that flatfielding can tell the file name
 *        when reporting an error.
 *     2014-01-08 (DSB):
 *        Add option to import LUT model from an external NDF (for SKYLOOP).
 *     2014-01-13 (DSB):
 *        Multi-thread copying of arrays from reference to output data.
 *     2014-01-28 (DSB):
 *        Correct ordering of grid axes in time series WCS.
 *     2014-01-31 (DSB):
 *        Ensure it is safe to supply a NULL value for config.
 *     2018-03-15 (DSB):
 *        Read existing model data from NDFs stored in the directory specified
 *        by the config parameter "dumpdir", rather than from the current
 *        directory.
 *     2018-03-29 (DSB):
 *        Use dumpdir value if "config" is NULL.
 *     2018-09-21 (DSB):
 *        Modified so that the FITS header in the output smfData contains
 *        the result of merging the FITS headers from all the concatenated
 *        input smfDatas. Previously, the output FITS header was just a
 *        copy of the FITS header from the first input smfData.
 *     2019-04-02 (GSB):
 *        Accumulate onmap flag value in subgroup loop rather than retaining
 *        only the last value.
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2007-2011 University of British Columbia.
 *     Copyright (C) 2008-2014 Science and Technology Facilities Council.
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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA.

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

#include <stdio.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "star/one.h"
#include "prm_par.h"
#include "par_par.h"
#include "kpg_err.h"
#include "star/one.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "libaztec/aztec.h"

/* Prototypes for local static functions. */
static void smf1_concat_smfGroup( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfConcatSmfGroupData {
   dim_t b1;
   dim_t b2;
   dim_t ntslice;
   size_t bstride;
   size_t tstride;
   size_t rbstride;
   size_t rtstride;
   dim_t tchunk;
   int oper;
   const double *in;
   double *out;
   const int *ini;
   int *outi;
   const smf_qual_t *inq;
   smf_qual_t *outq;
} SmfConcatSmfGroupData;

#define FUNC_NAME "smf_concat_smfGroup"

void smf_concat_smfGroup( ThrWorkForce *wf, AstKeyMap *config, const smfGroup *igrp,
                          const smfArray *darks, const smfArray *bbms,
                          const smfArray *flatramps, AstKeyMap *heateffmap,
                          size_t whichchunk, int ensureflat, int isTordered,
                          AstFrameSet *outfset, int moving,
                          int *lbnd_out, int *ubnd_out, fts2Port fts_port,
                          dim_t req_padStart,
                          dim_t req_padEnd, int flags, smfArray **concat,
                          smfData **first, int *status ) {

  /* Local Variables */
  size_t bstr;                  /* Concatenated bolo stride */
  smfDA *da=NULL;               /* Pointer to smfDA struct */
  smfData *data=NULL;           /* Concatenated smfData */
  dim_t *dslen=NULL;            /* Down-sampled lengths */
  char *ename = NULL;           /* Name of file to import */
  char *dumpdir=NULL;           /* Directory for exported models etc */
  const char *tempstr = NULL;   /* Temporary string pointer */
  int flag;                     /* Flag */
  char filename[GRP__SZNAM+1];  /* Input filename, derived from GRP */
  dim_t firstpiece = 0;         /* index to start of whichchunk */
  int foundfirst=0;             /* Flag indicates if first index found */
  int foundlast=0;              /* Flag indicates if last index found */
  int havearray[2];             /* flags for DATA/VARIANCE present */
  int havequal=0;               /* flag for QUALITY present */
  int havelut=0;                /* flag for pointing LUT present */
  smfHead *hdr=NULL;            /* pointer to smfHead in concat data */
  dim_t i;                      /* Loop counter */
  int importlut;                /* Import LUT array from an NDF? */
  Grp *ingrp=NULL;              /* Pointer to 1-element input group */
  int isFFT=-1;                 /* Data are 4d FFTs */
  int iw;
  dim_t j;                      /* Loop counter */
  SmfConcatSmfGroupData *job_data = NULL;
  dim_t k;                      /* Loop counter */
  dim_t l;                      /* Loop counter */
  dim_t lastpiece = 0;          /* index to end of whichchunk */
  dim_t nbolo=0;                /* Number of detectors */
  int nc;                       /* Character count */
  dim_t ncol=0;                 /* Number of columns */
  dim_t ndata;                  /* Total data points: nbolo*tlen */
  dim_t nrelated;               /* Number of subarrays */
  dim_t nrow;                   /* Number of rows */
  int nw;
  Grp *outgrp=NULL;             /* Pointer to 1-element output group */
  size_t outgrpsize;            /* Size of outgrp */
  dim_t padEnd = 0;             /* Padding to use for end of this chunk */
  dim_t padStart = 0;           /* Padding to use for start of this chunk */
  SmfConcatSmfGroupData *pdata;
  char *pname;                  /* Pointer to input filename */
  smfData *refdata=NULL;        /* Reference smfData */
  smf_dtype refdtype;           /* reference DATA/VARIANCE type */
  smfHead *refhdr=NULL;         /* pointer to smfHead in ref data */
  dim_t refncol=0;              /* reference number of rows */
  dim_t refndata;               /* Number data points in reference file */
  dim_t refnrow=0;              /* reference number of rows */
  dim_t reftlen;                /* Effective time slices in reference file */
  dim_t reftlenr;               /* real time slices in ref (before downsamp) */
  size_t rbstr;                 /* Reference bolo stride */
  size_t rtstr;                 /* Reference time slice stride */
  JCMTState *sourceState=NULL;  /* temporary JCMTState pointer */
  dim_t tchunk = 0;             /* Time offset in concat. array this chunk */
  dim_t tend;                   /* Time at start of padded region */
  dim_t tlen;                   /* Time length entire concatenated array */
  dim_t tstart;                 /* Time at end of padded region */
  size_t tstr;                  /* Concatenated time slice stride */
  dim_t bolostep;

  /* Initialise returned values. */
  if( first ) *first = NULL;
  if( concat ) *concat = NULL;

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get the path to the directory in which to place exported models
     etc. Ensure its end with "/"  */
  if( config ) {
     tempstr = NULL;
     astMapGet0C( config, "DUMPDIR", &tempstr );
     if( tempstr ) {
        size_t clen = strlen( tempstr );
        dumpdir = astStore( NULL, tempstr, clen + 2 );
        if( dumpdir[clen-1] != '/' ) strcpy( dumpdir + clen, "/" );
     }
  }

  /* How many threads do we get to play with */
  nw = wf ? wf->nworker : 1;

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
      if( igrp->chunk[i] == whichchunk ) {
        if (!foundfirst) {
          firstpiece = i;
          foundfirst = 1;
        }
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

  /* See if we will be importing a LUT model from an NDF. */
  importlut = 0;
  if( config ) astMapGet0I( config, "IMPORTLUT", &importlut );

  /* Allocate space for the smfArray if required. */
  if( concat ) *concat = smf_create_smfArray( status );

  /* Determine how many subarrays there actually are in this chunk*/
  nrelated = 0;
  for( i=0; (*status == SAI__OK) && i<igrp->nrelated; i++ ) {
    for( j=firstpiece; j<=lastpiece; j++ ) {
      if( (igrp->subgroups[j][i] > 0) && ((i+1) > nrelated) ) {
        nrelated = i+1;
      }
    }
  }

  /* Allocate space for array of downsampled lengths for each file */
  dslen = astCalloc( lastpiece-firstpiece+1, sizeof(*dslen) );

  /* Loop over related elements (number of subarrays). */
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

      smf_open_file( wf, igrp->grp, igrp->subgroups[j][i], "READ",
                     flags, &refdata, status );

      if( isFFT == -1 ) {
        isFFT = smf_isfft(refdata, NULL, NULL, NULL, NULL, NULL, status);
      } else if( smf_isfft(refdata, NULL, NULL, NULL, NULL, NULL, status) !=
                 isFFT ) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME
                ": mixture of time-series and FFT data encountered!",
                status );
      }

      /* Verify that the array is 3-dimensional and compatible with the
         reference array dimensions if we aren't working with 4d FFT
          data */
      if( (*status == SAI__OK) && (!isFFT) ) {
        smf_smfFile_msg( refdata->file, "FILE", 1, "<unknown file" );

        if( refdata->ndims != 3 ) {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME
                  ": ^FILE does not contain 3-dimensional data!",
                  status );
        }


        /* If data order is 0 (bolo-ordered) then fail since that case
           is not currently handled. */
        if( (*status == SAI__OK) && (refdata->isTordered == 0) ) {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME
                  ": ^FILE contains bolo-ordered data (unsupported)",
                  status);
        }
      }

      /* Get data dimensions */
      smf_get_dims( refdata, &nrow, &ncol,
                    NULL, &reftlenr, &refndata, NULL, NULL, status );

      /* Data may be down-sampled so use the value calculated in
         smf_grp_related for the length in time slices */
      reftlen = igrp->tlen[j];

      if( reftlenr != reftlen ) {
        /* dslen is 0 if we're not down-sampling, and igrp->tlen[j] if
           we are */
        dslen[j-firstpiece] = reftlen;
      }

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
          if (refdata && refdata->qual && (refdata->qual)[refndata-1] &
              SMF__Q_PAD ) {
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

      /* If required return a deep copy of the first data being
         concatenated. */
      if( first && *first == NULL ) {
        *first = smf_deepcopy_smfData( wf, refdata, 0, 0, 0, 0, status );
      }

      /* Close the reference file */
      smf_close_file( wf, &refdata, status );

      /* If we are not concatenating any data, we can leave the "j"
         loop now that we have the returned "first" value. */
      if( !concat ) break;
    }

    /* If we are not concatenating any data, we can leave the "i" loop now
       that we have determined the returned "first" value. */
    if( !concat ) break;

    if( isFFT ) {
      /* **********************************************************************
         Are we working with FFTs? If so, no concatenation or downsampling
         will happen. Each file is placed as-is into the smfArray
         **********************************************************************/

      if( *status == SAI__OK ) {
        smfData *tmpdata = NULL; /* for the original data before resamp */

        /* Loop over subgroups (number of time chunks). j is really only
           going to take on a single value in this loop (whichchunk) */
        for( j=firstpiece; j<=lastpiece; j++ ) {
          smf_open_file( wf, igrp->grp, igrp->subgroups[j][i], "READ", 0,
                         &tmpdata, status );

          /* Make a deepcopy since we are going to close the memory-mapped
             tmpdata. */
          data = smf_deepcopy_smfData( wf, tmpdata, 0, 0, 0, 0, status );
        }
      }

    } else {
      /* **********************************************************************
         Otherwise proceed with concatenation
         **********************************************************************/

      smfData *tmpdata = NULL;      /* for the original data before resamp */
      smfData *tmpdata_next = NULL; /* for the next data before resamp */
      int doflat;
      int israw;

      /* Add any padding to the length */
      tlen += padStart + padEnd;

      /* Loop over subgroups (number of time chunks), continuing only
         if the chunk is equal to whichchunk */
      for( j=firstpiece; (*status==SAI__OK) && (j<=lastpiece); j++ ) {

        /* If this is the first piece, open the file directly in the
           current thread. */
        if( j == firstpiece ) {

          /* Copy over the name of the first file in
             subarray. Use a grpex to strip off the path, and
             then add the suffix "_con.dimm" to denote concatenated
             iterative map-maker data */

          ingrp = smf_grp_new( igrp->grp, "GRP", status );
          outgrp = smf_grp_new( igrp->grp, "GRP", status );

          ndgCpsup( igrp->grp, igrp->subgroups[j][i], ingrp, status );
          ndgCrexp( "./*_" SMF__CON_SUFFIX SMF__DIMM_SUFFIX "|.sdf||", ingrp,
                    &outgrp, &outgrpsize, &flag, status );

          pname = filename;
          grpGet( outgrp, 1, 1, &pname, SMF_PATH_MAX, status);

          grpDelet( &ingrp, status );
          grpDelet( &outgrp, status );

          smf_open_file( wf, igrp->grp, igrp->subgroups[j][i], "READ", 0,
                         &tmpdata, status );
        }

        /* If any pieces remain to be opened, start a job to open the next
           piece, running the job in a separate thread. Return as soon
           as the job is submitted (i.e. do not wait for the job to
           complete). */
        if( j < lastpiece ) {
           smf_open_file_job( wf, 0, igrp->grp, igrp->subgroups[j+1][i],
                              "READ", 0, &tmpdata_next, status );
        }

        /* Meanwhile, whilst the next piece in being opened in a
           separate thread, we continue to process the already opened
           piece in the main thread... */

        /* See if the data needs to be flat-fielded. If the data has
           already been flat-fielded, then we clearly do not need to
           flat-field it again. If the data has not yet been flat-fielded,
           then we flat-field it below only if requested and otherwise
           just convert the raw integers to doubles. */
        smf_check_flat( tmpdata, status );
        if( *status == SMF__FLATN ) {
          israw = 0;
          doflat = 0;
          errAnnul( status );
        } else {
          israw = 1;
          doflat = ensureflat;
        }

        /* If required, downsample the data and if it is raw, convert it to
           double precision. Then release the original data. */
        if( dslen && dslen[j-firstpiece] ) {
          smf_downsamp_smfData( wf, tmpdata, &refdata, dslen[j-firstpiece],
                                1, 0, status );
          smf_close_file( wf, &tmpdata, status );

        /* Otherwise, if the data is raw, convert it to double precision
           then release the original. We copy the smfFile so that the
           flatfielding can report a file name associated with any failure. */
        } else if( israw ) {
          refdata = smf_deepcopy_smfData( wf, tmpdata, 1,  0, 0, 0,
                                          status );
          smf_close_file( wf, &tmpdata, status );

        /* Otherwise, just continue to use the original data. */
        } else {
           refdata = tmpdata;
        }

        /* Do dark subtraction */
        smf_apply_dark( refdata, darks, status );

        /* If required, apply the flat-field correction. We force the flatfield without
           checking because we just switched to _DOUBLE and smf_check_flat does not yet
           handle that properly. */
        if( doflat ) smf_flatfield_smfData( refdata, flatramps, heateffmap, 1, status );

        /* Set havequal flag based on first file. This is required
           because the initial pass through for dimensions only looked
           at the file itself and not the result of flatfielding.
           Flatfielding creates a quality array. Without this raw data
           will not have quality values propagated. */
        if ( j == firstpiece && refdata ) {
          havequal = ( refdata->qual != NULL );
        }

        /* Apply bad bolometer mask */
        smf_apply_mask( wf, refdata, bbms, SMF__BBM_DATA, 0, status );

        /* Get reference dimensions/strides */
        smf_get_dims( refdata, NULL, NULL, &nbolo, &reftlenr, &refndata,
                      &rbstr, &rtstr, status );

        /* Calculate the pointing LUT if requested, unless we will be
           importing it from an NDF later. */
        if( !(flags & SMF__NOCREATE_LUT) && outfset && !importlut ) {

          /* Set havelut flag */
          havelut = 1;

          /* Calculate a new LUT for this chunk. Note, this call divides up
             its work between several threads.  It takes care to use a
             separate job context so that any other jobs currently being
             performed by the workforce (e.g. opening the next file) do
             not cause the call to block.  */
          smf_calc_mapcoord( wf, config, refdata, outfset, moving, lbnd_out,
                             ubnd_out, fts_port, SMF__NOCREATE_FILE, status );

        } else {
          havelut = 0;
        }

        if( dslen && dslen[j-firstpiece] ) {
          reftlen = dslen[j-firstpiece];
        } else {
          reftlen = reftlenr;
        }

        if( *status == SAI__OK ) {
          /* If first chunk initialize the concatenated array */
          if( j == firstpiece ) {

            /* Copy first data right after the initial padding */
            tchunk = padStart;

            /* Allocate memory for empty smfData with a smfHead. Create
               a DA struct only if the input file has one. Create it as
               a clone rather than creating an empty smfDa. */
            data = smf_create_smfData(SMF__NOCREATE_DA, status );
            if (refdata->da && data) {
              /* do not copy dark squids. We do that below */
              data->da = smf_deepcopy_smfDA( wf, refdata, 0, status );
              da = data->da;
            }

            /* PROPAGATE FTS2 DATA */
            if ( data &&
                 refdata->fts &&
                 refdata->fts->fpm &&
                 refdata->fts->sigma) {
              data->fts = smf_deepcopy_smfFts(refdata, status );
            }

            if (refdata->history) data->history = astCopy( refdata->history );

            if( *status == SAI__OK ) {
              data->qfamily = refdata->qfamily;
              /* Copy over basic header information from the reference */
              hdr = data->hdr;
              refhdr = refdata->hdr;
              hdr->fitshdr = NULL;
              hdr->instrument = refhdr->instrument;
              hdr->scanvel = refhdr->scanvel;
              hdr->steptime = refhdr->steptime;
              hdr->obsmode = refhdr->obsmode;
              hdr->obstype = refhdr->obstype;
              hdr->seqtype = refhdr->seqtype;
              hdr->swmode = refhdr->swmode;
              hdr->instap[ 0 ] = refhdr->instap[ 0 ];
              hdr->instap[ 1 ] = refhdr->instap[ 1 ];
              hdr->telpos[ 0 ] = refhdr->telpos[ 0 ];
              hdr->telpos[ 1 ] = refhdr->telpos[ 1 ];
              hdr->telpos[ 2 ] = refhdr->telpos[ 2 ];
              one_strlcpy(hdr->obsidss, refhdr->obsidss, sizeof(hdr->obsidss), status);
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

              /* Override the reference name */
              one_strlcpy( data->file->name, filename,
                           sizeof(data->file->name), status );

              /* Allocate space for the concatenated allState */
              hdr->nframes = tlen;
              hdr->allState = astCalloc( tlen, sizeof(*(hdr->allState)) );

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
              data->isFFT = -1;
              ndata = nbolo*tlen;

              /* get the strides */
              smf_get_dims( data, NULL, NULL, NULL, NULL, NULL, &bstr, &tstr,
                            status );

              /* Allocate space for enlarged dksquid array. */
              if( da && refdata->da && refdata->da->dksquid ) {
                da->dksquid = smf_create_smfData(SMF__NOCREATE_FILE |
                                                 SMF__NOCREATE_HEAD |
                                                 SMF__NOCREATE_DA |
                                                 SMF__NOCREATE_FTS, status );

                /* Dimensions */
                da->dksquid->dtype = SMF__DOUBLE;
                da->dksquid->isTordered = 1;
                da->dksquid->isFFT = -1;
                da->dksquid->ndims = 3;

                da->dksquid->dims[0] = ncol;
                da->dksquid->dims[1] = 1;
                da->dksquid->dims[2] = tlen;
                da->dksquid->lbnd[0] = 0;
                da->dksquid->lbnd[1] = 0;
                da->dksquid->lbnd[2] = 1;

                da->dksquid->pntr[0] = astCalloc(ncol*tlen,
                                                 smf_dtype_size(da->dksquid,
                                                                status) );

                /* QUALITY too if requested */
                if( !(flags&SMF__NOCREATE_QUALITY) ) {
                  da->dksquid->qfamily = refdata->da->dksquid->qfamily;
                  da->dksquid->qual = astCalloc(ncol*tlen,
                                                sizeof(*(da->dksquid->qual)));
                }

              }

              /* Un-set havearray values corresponding to flags */
              havearray[0] = havearray[0] && !(flags&SMF__NOCREATE_DATA);
              havearray[1] = havearray[1] && !(flags&SMF__NOCREATE_VARIANCE);
              havequal = havequal && !(flags&SMF__NOCREATE_QUALITY);

              /* Allocate space for arrays being propagated from template */
              for( k=0; k<2; k++ ) if( havearray[k] ) {
                  size_t sz = smf_dtype_sz(data->dtype, status );
                  data->pntr[k] = astCalloc( ndata, sz );
                }
              if (havequal) {
                data->qual = astCalloc( ndata, sizeof(*(data->qual)) );
              }

              /* Check to see if havearray for QUALITY is not set,
                 but SMF__NOCREATE_QUALITY is also not set. In this
                 case, allocate a fresh QUALITY component that will
                 not require propagation from the template */
              if( !havequal && !(flags & SMF__NOCREATE_QUALITY) ) {
                data->qual = astCalloc(ndata, sizeof(*(data->qual)) );
              }

              /* Allocate space for the pointing LUT, and theta if needed */
              if( havelut || importlut ) {
                data->lut = astCalloc(ndata, sizeof(*(data->lut)) );
                data->theta = astCalloc(tlen, sizeof(*(data->theta)) );
              }

              /* Copy over the TSWCS, ensuring the axes are in the required
                 order. */
              if( (*status == SAI__OK) && (refhdr->tswcs) ) {
                hdr->tswcs = astCopy( refhdr->tswcs );
                smf_tswcsOrder( &(hdr->tswcs), isTordered, status );
              }
            }
          }

          /* Merge the FITS headers. This filters out headers that have
             different values, except for those that relate to the start and
             end values for a specific quantity - these are merged. */
          refhdr = refdata->hdr;
          if( hdr && refhdr && refhdr->fitshdr ) smf_fits_outhdr( refhdr->fitshdr, &hdr->fitshdr, status );

          /* If we have not yet done so, set up threading info. */
          if( ! job_data ) {

            /* Find how many bolometers to process in each worker thread. */
            bolostep = nbolo/nw;
            if( bolostep == 0 ) bolostep = 1;

            /* Allocate job data for threads, and store the range of
               bolos, to be processed by each one. Ensure that the last
               thread picks up any left-over bolos.  */
            job_data = astCalloc( nw, sizeof(*job_data) );
            if( *status == SAI__OK ) {
              for( iw = 0; iw < nw; iw++ ) {
                pdata = job_data + iw;
                pdata->b1 = iw*bolostep;
                if( iw < nw - 1 ) {
                   pdata->b2 = pdata->b1 + bolostep - 1;
                } else {
                   pdata->b2 = nbolo - 1 ;
                }
                if( pdata->b2 >= nbolo ) pdata->b2 = 0;
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

            /* Copy LUT and theta */
            if( havelut ) {

              thrBeginJobContext( wf, status );
              for( iw = 0; iw < nw; iw++ ) {
                pdata = job_data + iw;
                pdata->oper = 1;
                pdata->ini = refdata->lut;
                pdata->outi = data->lut;
                pdata->tchunk = tchunk;
                pdata->bstride = bstr;
                pdata->tstride = tstr;
                pdata->rbstride = rbstr;
                pdata->rtstride = rtstr;
                pdata->ntslice = reftlen;
                thrAddJob( wf, 0, pdata, smf1_concat_smfGroup, 0, NULL, status );
              }
              thrWait( wf, status );
              thrEndJobContext( wf, status );

              if( data->theta && refdata->theta ) {
                memcpy( data->theta + tchunk, refdata->theta,
                        reftlen*sizeof(*(data->theta)) );
              }

              data->onmap = (j != firstpiece && data->onmap) || refdata->onmap;
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

                  thrBeginJobContext( wf, status );
                  for( iw = 0; iw < nw; iw++ ) {
                    pdata = job_data + iw;
                    pdata->oper = 2;
                    pdata->in = refdata->pntr[k];
                    pdata->out = data->pntr[k];
                    pdata->tchunk = tchunk;
                    pdata->bstride = bstr;
                    pdata->tstride = tstr;
                    pdata->rbstride = rbstr;
                    pdata->rtstride = rtstr;
                    pdata->ntslice = reftlen;
                    thrAddJob( wf, 0, pdata, smf1_concat_smfGroup, 0, NULL, status );
                  }
                  thrWait( wf, status );
                  thrEndJobContext( wf, status );

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

              thrBeginJobContext( wf, status );
              for( iw = 0; iw < nw; iw++ ) {
                pdata = job_data + iw;
                pdata->oper = 3;
                pdata->inq = refdata->qual;
                pdata->outq = data->qual;
                pdata->tchunk = tchunk;
                pdata->bstride = bstr;
                pdata->tstride = tstr;
                pdata->rbstride = rbstr;
                pdata->rtstride = rtstr;
                pdata->ntslice = reftlen;
                thrAddJob( wf, 0, pdata, smf1_concat_smfGroup, 0, NULL, status );
              }
              thrWait( wf, status );
              thrEndJobContext( wf, status );
            }

            /* increment tchunk */
            tchunk += reftlen;
          }
        }

        /* Close the file we had open */
        smf_close_file( wf, &refdata, status );

        /* If any pieces remain to be processed, wait for the completion
           of the job that is opening the next piece. Then lock the
           smfData for use by this thread, and use it in place of the
           previous smfData pointer. */
        if( j < lastpiece ) {
           thrWait( wf, status );
           smf_lock_data( tmpdata_next, 1, status );
           tmpdata = tmpdata_next;
        }

      }

      /* If ensureflat=0 and SMF__NOCREATE_QUALITY is not set, we may
         have a bunch of bolometers that don't change with time that
         were turned off by the DA system. Explicitly do that check
         here in the non-padded region of the concatenated array and
         set SMF__Q_BADDA. Also set the data values to VAL__BADD to
         prevent quality/data inconsistencies being reported. */
      if( (*status == SAI__OK) && (!ensureflat) && (data->qual) ) {
        if( data->dtype == SMF__DOUBLE ) {
          int change;
          double *d = data->pntr[0];

          for( j=0; j<nbolo; j++ ) {
            change = 0;
            for( k=padStart; k<(tlen-padEnd); k++) {
              if( d[j*bstr + k*tstr] != d[j*bstr] ) {
                /* If even one sample is different than first, we've detected
                   a change so don't flag this bolo */
                change = 1;
                break;
              }
            }

            if( !change ) {
              size_t ii = j*bstr + padStart*tstr;
              for( k=padStart; k<(tlen-padEnd); k++) {
                data->qual[ii] |= SMF__Q_BADDA;
                d[ii] = VAL__BADD;
                ii += tstr;
              }
            }
          }
        } else {
          *status = SAI__ERROR;
          errRepf( "", FUNC_NAME
                   ": Don't know how to handle %s type when flagging BADDA.",
                   status, smf_dtype_string(data,status) );
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

          /* If theta present, set to VAL__BADD */
          if( data->theta ) {
            for( l=tstart; l<=tend; l++ ) {
              data->theta[j] = VAL__BADD;
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

        smf_create_tswcs( data->hdr, isTordered, &data->hdr->tswcs, status );

        /* If we couldn't make the tswcs just annul error and continue... we
           don't really need it for anything */
        if( *status != SAI__OK ) {
          errFlush(status);
          msgOut( "", FUNC_NAME
                  ": Warning, Couldn't create TSWCS for data cube", status );

          /* Annul just in case it got made */
          if(data->hdr->tswcs) data->hdr->tswcs = astAnnul( data->hdr->tswcs );
        }

      }

      /* If we are importing the LUT model from an NDF, do it now. */
      if( !(flags & SMF__NOCREATE_LUT) && outfset && importlut ) {
        nc = strstr( pname, "_con" ) - pname + 4;
        ename = astStore( NULL, pname, nc + 1 );
        ename[ nc ] = 0;
        ename = astAppendString( ename, &nc, "_lut" );
        msgOutiff( MSG__VERB, "", FUNC_NAME ": using external LUT "
                  "model imported from '%s'.", status, ename );
        smf_import_array( wf, data, dumpdir, ename, 0, 0, SMF__INTEGER,
                          data->lut, lbnd_out, ubnd_out, status );
        ename = astFree( ename );
        data->onmap = 1;
      }
    }

    /* Put this concatenated subarray (or directly copied subarray in the
       case of an FFT) into the smfArray */
    if( concat ) smf_addto_smfArray( *concat, data, status );
  }

  /* Clean up */
  dumpdir = astFree( dumpdir );
  dslen = astFree( dslen );
  job_data = astFree( job_data );
}




static void smf1_concat_smfGroup( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_concat_smfGroup

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_concat_smfGroup.

*  Invocation:
*     smf1_concat_smfGroup( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfConcatSmfGroupData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfConcatSmfGroupData *pdata;
   const double *dp2;
   const double *dp4;
   const int *ip2;
   const int *ip4;
   const smf_qual_t *qp4;
   const smf_qual_t *qp2;
   dim_t b1;
   dim_t b2;
   dim_t ibolo;
   dim_t itime;
   dim_t ntslice;
   double *dp1;
   double *dp3;
   int *ip1;
   int *ip3;
   size_t bstride;
   size_t rbstride;
   size_t rtstride;
   size_t tstride;
   smf_qual_t *qp3;
   smf_qual_t *qp1;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfConcatSmfGroupData *) job_data_ptr;

   b1 = pdata->b1;
   b2 = pdata->b2;
   ntslice = pdata->ntslice;
   bstride = pdata->bstride;
   rbstride = pdata->rbstride;
   tstride = pdata->tstride;
   rtstride = pdata->rtstride;

/* Integer copy.
   ============= */
   if( pdata->oper == 1 ) {
      ip3 = pdata->outi + pdata->tchunk*tstride + b1*bstride;
      ip4 = pdata->ini + b1*rbstride;
      for( ibolo = b1; ibolo <= b2; ibolo++ ) {
         ip1 = ip3;
         ip2 = ip4;

         for( itime = 0; itime < ntslice; itime++ ) {
            *ip1 = *ip2;
            ip1 += tstride;
            ip2 += rtstride;
         }

         ip3 += bstride;
         ip4 += rbstride;
      }

/* Double precision copy.
   ==================== */
   } else if( pdata->oper == 2 ) {
      dp3 = pdata->out + pdata->tchunk*tstride + b1*bstride;
      dp4 = pdata->in + b1*rbstride;
      for( ibolo = b1; ibolo <= b2; ibolo++ ) {
         dp1 = dp3;
         dp2 = dp4;

         for( itime = 0; itime < ntslice; itime++ ) {
            *dp1 = *dp2;
            dp1 += tstride;
            dp2 += rtstride;
         }

         dp3 += bstride;
         dp4 += rbstride;
      }

/* Quality copy.
   ==================== */
   } else if( pdata->oper == 3 ) {
      qp3 = pdata->outq + pdata->tchunk*tstride + b1*bstride;
      qp4 = pdata->inq + b1*rbstride;
      for( ibolo = b1; ibolo <= b2; ibolo++ ) {
         qp1 = qp3;
         qp2 = qp4;

         for( itime = 0; itime < ntslice; itime++ ) {
            *qp1 = *qp2;
            qp1 += tstride;
            qp2 += rtstride;
         }

         qp3 += bstride;
         qp4 += rbstride;
      }

/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_concat_smfGroup: Invalid operation (%d) supplied.",
               status, pdata->oper );
   }
}








