/*
*+
*  Name:
*     smf_checkmem_dimm

*  Purpose:
*     Verify that there is enough memory to run the dynamic iterative map-maker

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_checkmem_dimm( dim_t maxlen, inst_t instrument, int nrelated,
*                        smf_modeltype *modeltyps, dim_t nmodels, dim_t msize,
*                        AstKeyMap *keymap, size_t available,
*                        dim_t maxfilelen, size_t *necessary,
*                        int *status );

*  Arguments:
*     maxlen = dim_t (Given)
*        The longest chunk of data that will be handled by the DIMM (samples).
*     instrument = inst_t (Given)
*        For which instrument is the map-maker being run?
*     nrelated = int (Given)
*        How many subarrays are being processed simultaneously
*     modeltyps = smf_modeltype* (Given)
*        Array indicating which model components are being solved for
*     nmodels = dim_t (Given)
*        Number of elements in modeltyps
*     msize = dim_t (Given)
*        Number of elements in the map
*     keymap = AstKeyMap* (Given)
*        keymap containing parameters to control map-maker
*     available = size_t (Given)
*        Maximum memory in bytes that the mapped arrays may occupy
*     maxfilelen = dim_t (Given)
*        Max length in time samples of an individual data file.
*     necessary = size_t * (Returned)
*        If non-null return estimate of the actual amount of memory required
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function checks the amount of memory required to run the
*     dynamic iterative map-maker (DIMM). The calculation obtains the
*     number of detectors at each time slice from the instrument
*     type. The estimate is based on all of the requested model
*     components, and the number of subarrays being processed
*     simultaneously. It also uses the keymap containing map-making
*     parameters to decide if any extra variable amounts of memory are
*     needed (e.g. for data pre-processing). If the amount of memory
*     (necessary) exceeds available, SMF__NOMEM status is set.

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-04-28 (EC):
*        Initial version.
*     2008-04-30 (EC):
*        Added SMF__EXT
*     2009-03-12 (EC):
*        Added SMF__FLT
*     2009-03-20 (EC):
*        Don't fail on SMF__AST (but don't add more memory)
*     2010-05-27 (TIMJ):
*        Add SMF__SMO model.
*     2010-06-08 (EC):
*        Add SMF__TWO model.
*     2010-08-18 (EC):
*        -supply keymap to check for FFT temp space
*        -space for JCMTState
*     2010-08-19 (EC):
*        -space for concatenated dark squids
*        -space for initial data read (uses new maxfilelen parameter)
*        -added fudge factor for static model memory usage (empirical)
*        -added rough estimate for data cleaning (empirical)
*     2010-09-09 (EC):
*        Add msize to interface so we can add space for ast.zero_circle
*     2010-09-20 (TIMJ):
*        We are using MiB not Mb (or MB)
*     2010-10-26 (EC):
*        Add memory for fakemaps
*     2011-11-21 (EC):
*        No more need for AST contrib. to static memory usage since we
*        now just use the map everywhere.
*     2013-03-25 (TIMJ):
*        Silence compiler warning in default case.
*     2014-01-16 (DSB):
*        The NOI model has "maxlen" times slices, not 1, if NOI.BOXSIZE
*        is non-zero.
*     2014-12-18 (DSB):
*        Added SSN.
*     2015-06-15 (DSB):
*        Added PCA
*     {enter_further_changes_here}

*  Notes:
*     This should match memory allocated in smf_grp_related.

*  Copyright:
*     Copyright (C) 2010-2014 Science & Technology Facilities Council.
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2005-2011 University of British Columbia.
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
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "jcmt/state.h"


#define FUNC_NAME "smf_checkmem_dimm"

#define CHECKMEM_FUDGE 1.5 /* Fudge estimate by this empirical factor */


/* Macro to check if a mask is required, and update the total memory
   requirement accordingly. */
#define CHECK_MASK(model) { \
    int use_mask = 0; \
    double dval = 0.0; \
    astMapGet0A( keymap, model, &kmap ); \
    astMapGet0D( kmap, "ZERO_LOWHITS", &dval ); \
    if( dval > 0.0 ) use_mask = 1; \
    astMapGet0D( kmap, "ZERO_SNR", &dval ); \
    if( dval > 0.0 ) use_mask = 1; \
    if( astMapType( kmap, "ZERO_CIRCLE" ) != AST__BADTYPE || \
        astMapType( kmap, "ZERO_MASK" ) != AST__BADTYPE ) use_mask = 1; \
    if( use_mask ) total += msize*sizeof(unsigned char); \
    kmap = astAnnul( kmap ); \
}



void smf_checkmem_dimm( dim_t maxlen, inst_t instrument, int nrelated,
			smf_modeltype *modeltyps, dim_t nmodels, dim_t msize,
                        AstKeyMap *keymap, size_t available, dim_t maxfilelen,
                        size_t *necessary, int *status ) {


  /* Local Variables */
  int dofft=0;                 /* flag if we need temp space for FFTs */
  double dval;                 /* Parameter value */
  dim_t i;                     /* Loop counter */
  dim_t gain_box;              /* Length of blocks for GAI/COM model */
  AstKeyMap *kmap=NULL;        /* Local keymap */
  dim_t nblock;                /* Number of blocks for GAI/COM model */
  size_t ncol = 0;             /* Number of columns */
  size_t ndet = 0;             /* Number of detectors each time step */
  size_t ndks;                 /* dksquid samples in a subarray, ncol*maxlen */
  size_t nrow;                 /* Number of rows */
  size_t nsamp;                /* bolo samples in a subarray, ndet*maxlen */
  const char *tempstr=NULL;    /* Temporary pointer to static char buffer */
  size_t total = 0;            /* Total bytes required */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check inputs */

  if( maxlen < 1 ) {
    *status = SAI__ERROR;
    errRep("", FUNC_NAME ": maxlen cannot be < 1", status);
    return;
  }

  if( (nrelated < 1) || (nrelated > SMF__MXSMF) ) {
    msgSeti("NREL",nrelated);
    msgSeti("MAXREL",SMF__MXSMF);
    *status = SAI__ERROR;
    errRep("", FUNC_NAME ": nrelated, ^NREL, must be in the range [1,^MAXREL]",
	   status);
    return;
  }

  if( modeltyps ) {
    if( nmodels < 1 ) {
    *status = SAI__ERROR;
    errRep("", FUNC_NAME ": modeltyps specified, mmodels cannot be < 1",
           status);
    return;
    }
  }

  if( *status == SAI__OK ) {

    /* Work out the data dimensions */
    switch( instrument ) {
    case INST__SCUBA2:
      /* Kludgey, but at least we check SC2STORE__COL_INDEX so this will help
         us catch possible future problems if order is changed */
      if( SC2STORE__COL_INDEX ) {
        ncol = 32;
        nrow = 40;
      } else {
        ncol = 40;
        nrow = 32;
      }
      ndet = ncol*nrow;
      break;
    default:
      *status = SAI__ERROR;
      errRep("", FUNC_NAME ": Invalid instrument given.", status);
    }
  }

  /* Number of samples in a full data cube for one subarray */
  nsamp = ndet*maxlen;

  /* Check to see if we need to do filtering as part of
     pre-processing. If so, we need an extra nsamp-sized buffer to
     store the FFT. */

  smf_get_cleanpar( keymap, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    &dofft, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                    NULL, NULL, status );

  /* Calculate memory usage of static model components: -------------------- */

  if( *status == SAI__OK ) {
    total += nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;   /* RES */
    total += nsamp*smf_dtype_sz(SMF__INTEGER,status)*nrelated;  /* LUT */
    total += nsamp*smf_dtype_sz(SMF__QUALTYPE,status)*nrelated; /* QUA */
  }

  /* Add on memory usage for the JCMTState (one per time slice per array) */
  if( *status == SAI__OK ) {
    total += maxlen*sizeof(JCMTState)*nrelated;
  }

  /* Add on space for dark squids */
  ndks = ncol*maxlen;
  total += ndks*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;

  /* Add on space for fakemap */
  astMapGet0C( keymap, "FAKEMAP", &tempstr );
  if( tempstr ) {
    total += msize*sizeof(double);
  }

  /* Apply fudge factor */
  total *= CHECKMEM_FUDGE;

  /* Calculate memory usage of dynamic model components: ------------------- */

  if( *status == SAI__OK ) {

    /* Most of these will have data arrays associated with each
       subarray (hence the multiplication by nrelated). An exception
       is SMF__COM for which a single-common mode is used across all
       subarrays. */

    if( modeltyps ) {
      for( i=0; i<nmodels; i++ ) {
	switch( modeltyps[i] ) {
	case SMF__NOI:
          /* SMF__NOI also estimates the noise in each detector from the
             power spectra, requiring a temporary buffer to store an FFT.
             The number of variances per detector depends on NOI.BOX_SIZE
             - 1 if NOI.BOXSIZE is zero and "maxlen" otherwise. . */
          dofft = 1;

          if( astMapGet0A( keymap, "NOI", &kmap ) ) {
             astMapGet0D( kmap, "BOX_SIZE", &dval );
             if( dval == 0 ) {
                total += ndet*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
             } else {
                total += nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
             }
             kmap = astAnnul( kmap );
          }
	  break;
	case SMF__COM:
          CHECK_MASK("COM")
	  total += maxlen*smf_dtype_sz(SMF__DOUBLE,status);
	  break;
	case SMF__EXT:
	  total += nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
	  break;
        case SMF__DKS:
          total += (maxlen + nrow*3)*ncol*smf_dtype_sz(SMF__DOUBLE,status) *
            nrelated;
          break;
        case SMF__GAI:
          /* Every COM.GAIN_BOX samples there are 3 planes of data
             corresponding to each bolometer in the subarray. The
             conversion of COM.GAIN_BOX from seconds to samples within
             smf_get_nsamp assumes a sample rate of 200 Hz. Later
             downsampling may result in a lower sample rate, but at least
             we are erring on the conservative side by assuming 200 Hz. */
          if( astMapGet0A( keymap, "COM", &kmap ) ) {
             smf_get_nsamp( kmap, "GAIN_BOX", NULL, &gain_box, status );
             if( gain_box > 0 ) {
                nblock = maxlen/gain_box;
                if( nblock == 0 ) nblock = 1;
             } else {
                nblock = 1;
             }
             total += nblock*3*nrow*ncol*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
             kmap = astAnnul( kmap );
          }
          break;
        case SMF__FLT:
          /* Presently the filter temporarily transforms the entire
             data cube into a second array. We therefore need to
             ensure enough memory to temporarily store the data cube
             twice. */
          CHECK_MASK("FLT")
          dofft = 1;
          total += nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
          break;
        case SMF__PLN:
          total += nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
          break;
        case SMF__SMO:
          total += nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
          break;
        case SMF__PCA:
          total += nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
          break;
        case SMF__SSN:
          total += nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
          break;
        case SMF__TMP:
          /* An externally supplied template. The model just stores the gain,
             offset and correlation coefficient for each bolometer, similar
             to SMF__GAI except with only one chunk considered */
          total += 3*nrow*ncol*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
          break;
        case SMF__TWO:
          /* two common-mode time series and coefficients for all the
             detectors */
          total += 2*(ndet+maxlen)*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
          break;
        case SMF__AST:
          /* Mostly accounted for as static memory usage above, but add space
             for mask if required */
          CHECK_MASK("AST")
          break;
	default:
	  *status = SAI__ERROR;
	  errRep("", FUNC_NAME ": Invalid smf_modeltype given.", status);
	}

	/* Exit on bad status */
	if( *status != SAI__OK ) {
	  i = nmodels;
	}
      }
    }

    /* Calculate temporary space here -------------------------------------- */

    if( *status == SAI__OK ) {
      size_t temp=0;      /* current temp memory required */
      size_t maxtemp=0;   /* max temp memory required */

      /* Some temp space required when we initially read in the
         data. Normally smf_concat_smfGroup will
         smf_open_and_flatfield one file at a time before copying into
         the concatenated data array. If there are N time slices in a
         file, smf_open_and_flatfield will usually require space both
         for the raw, and double-precision flatfielded data
         simultaneously. This is an upper limit, since if previously
         flatfielded data are provided no extra space for the raw
         data will be required. We use the supplied maxfilelen to figure
         out this maximum temporary buffer size. Remember to account
         for bolo data, dark squids, and JCMTState. Also, the way
         smf_iteratemap is currently written, a concatenated memory-mapped
         pointing LUT is initially created, and then copied to a new
         malloc'd array, so we need extra temp space for the full LUT. */

      temp = (ndet + ncol)*( smf_dtype_sz(SMF__INTEGER,status)+
                             smf_dtype_sz(SMF__DOUBLE,status) )*maxfilelen +
        2*maxfilelen*sizeof(JCMTState);

      temp += nsamp*smf_dtype_sz(SMF__INTEGER,status)*nrelated;

      if( temp > maxtemp ) maxtemp = temp;

      /* If we are doing FFTs */
      if( dofft ) {
        temp = nsamp*smf_dtype_sz(SMF__DOUBLE,status)*nrelated;
        if( temp > maxtemp ) maxtemp = temp;
      }

      /* Data cleaning... seems to be a lot. Just putting in a rough
         estimate of 3 extra full sized arrays for the standard
         options. */
      temp = 3.*nsamp*smf_dtype_sz(SMF__DOUBLE,status);
      if( temp > maxtemp ) maxtemp = temp;

      /* Add on the maximum chunk of temporary memory that is required */
      total += maxtemp;
    }


    /* Set bad status if too big */
    if( (*status == SAI__OK) && (total > available) ) {
      *status = SMF__NOMEM;
      msgSeti("REQ",total/SMF__MIB);
      msgSeti("AVAIL",available/SMF__MIB);
      errRep("", FUNC_NAME
	     ": Requested memory ^REQ MiB for map exceeds available ^AVAIL MiB",
	     status);
    }

    /* Return the required space */
    if( necessary ) {
      *necessary = total;
    }
  }

}
