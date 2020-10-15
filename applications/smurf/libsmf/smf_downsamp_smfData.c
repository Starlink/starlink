/*
*+
*  Name:
*     smf_downsamp_smfData

*  Purpose:
*     Produce a down-sampled copy of a 3-dimensional smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_downsamp_smfData( ThrWorkForce *wf, smfData *idata, smfData **odata,
*                           dim_t ontslice, int todouble, int method,
*                           int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     idata = smfData* (Given)
*        Pointer to an input smfData struct
*     odata = smfData** (Given and Returned)
*        Pointer to a newly created smfData struct.
*     ontslice = dim_t (Given)
*        Length in time slices of odata (must be less than or equal to
*        length of idata)
*     todouble = int (Given)
*        If set, odata will be converted to SMF__DOUBLE even if the input
*        has a fixed-point data type.
*     method = int (Given)
*        If 0 use time-domain boxcar filter method
*        If 1 use FFT frequency truncation method
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine produces a copy of a smfData at a lower sample
*     rate.  All bolometer and dark squid data are downsampled using
*     either simple time-domain boxcar averages if method = 0, or by
*     taking the FFT, and simply removing all modes above the new
*     target Nyquist frequency before transforming back (i.e., a
*     boxcar filter in the frequency domain) if method = 1. Most
*     JCMTState information is propagated using a nearest-neighbour
*     resampling, although the pointing information is re-sampled
*     using the time-domain boxcar smooth. The time-domain method is a
*     SINC function in the frequency domain (so it will suffer some
*     aliasing artifacts). Similarly, the FFT method will result in a
*     SINC function time-domain response.

*  Notes:
*     This routine does not downsample the VARIANCE or QUALITY components.

*  Authors:
*     EC: Ed Chapin (UBC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-10-20 (EC):
*        Initial version
*     2010-10-21 (EC):
*        Nearest-neighbour for most of JCMTState, but do proper resampling
*        for important fast-changing fields.
*     2010-10-22 (EC)
*        Add todouble flag
*     2011-06-22 (EC)
*        Add FFT-based method
*     2012-05-24 (DSB)
*        Multi-thread.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2011 University of British Columbia
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

/* Standard includes */
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"
#include "libsmf/smf_err.h"

/* Macro to simplify resampling of individual JCMTState fields */

#define RESAMPSTATE(in,out,member,intslice,ontslice,isang) smf_downsamp1D( wf, &(in->member),sizeof(JCMTState),1,intslice,&(out->member), sizeof(JCMTState),1,ontslice,1,1,isang,status );

#define FUNC_NAME "smf_downsamp_smfData"

void smf_downsamp_smfData( ThrWorkForce *wf, smfData *idata,
                           smfData **odata, dim_t ontslice, int todouble,
                           int method, int *status ) {

  dim_t i;                 /* loop counter */
  dim_t ibstride;          /* bstride of idata */
  smfData *indksquid=NULL; /* Pointer to input dksquid data */
  JCMTState *instate=NULL; /* Pointer to input JCMTState */
  dim_t intslice;          /* ntslice of idata */
  dim_t itstride;          /* tstride of idata */
  dim_t j;                 /* loop counter */
  dim_t nbolo;             /* number of bolos */
  dim_t ncols;             /* number of columns */
  dim_t nrows;             /* number of rows */
  dim_t obstride;          /* bstride of odata */
  dim_t ondata;            /* ndata of odata */
  dim_t otstride;          /* tstride of odata */
  JCMTState *outstate=NULL;/* Pointer to output JCMTState */
  double scale;            /* how much longer new samples are */
  size_t size;             /* size of array element in bytes */



  if( *status != SAI__OK ) return;

  if( !idata || !odata ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL inputs supplied", status );
    return;
  }

  if( idata->ndims != 3 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": idata is not 3-dimensional", status );
    return;
  }

  if( (idata->dtype!=SMF__DOUBLE) && (!todouble) && method ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": downsampling method requires target data to be "
            "double precision", status );
    return;
  }

  /* Dimensions of input */
  smf_get_dims( idata, &nrows, &ncols, &nbolo, &intslice, NULL, &ibstride,
                &itstride, status );

  if( ontslice > intslice ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": in order to down-sample ontslice must be "
            "less than idata->dims[2]", status );
    return;
  }

  scale = (double) intslice / (double) ontslice;

  /* First create the output data structure, but not copying any of the data
     that needs to be downsampled */

  if( idata->hdr ) {
    /* We want to copy everything in the smfHead except for allState. So
       we make a copy of the allState pointer, and then set it to NULL in
       the header before the copy */
    instate = idata->hdr->allState;
    idata->hdr->allState = NULL;
  }

  if( idata->da ) {
    /* Similarly, we want everything in the smfDa except for the dksquid
       which we will downsample later */
    indksquid = idata->da->dksquid;
    idata->da->dksquid = NULL;
  }

  *odata = smf_deepcopy_smfData( wf, idata, 0, SMF__NOCREATE_DATA |
                                 SMF__NOCREATE_VARIANCE | SMF__NOCREATE_QUALITY,
                                 0, 0, status );

  if( instate ) {
    /* Replace the allState pointer now that we're done */
    idata->hdr->allState = instate;
  }

  if( indksquid ) {
    /* Replace the dksquid pointer now that we're done */
    idata->da->dksquid = indksquid;
  }

  /* Length of time-axis for down-sampled bolometer data */
  if( *status == SAI__OK ) {
    if( (*odata)->isTordered ) {
      (*odata)->dims[2] = ontslice;
    } else {
      (*odata)->dims[0] = ontslice;
    }
  }

  smf_get_dims( *odata, NULL, NULL, NULL, NULL, &ondata, &obstride,
                &otstride, status );

  /* Data type depends on input data and todouble flag */
  if( *status == SAI__OK ) {
    if( todouble ) {
      size = sizeof(double);
      (*odata)->dtype = SMF__DOUBLE;
    } else {
      (*odata)->dtype = idata->dtype;
      size = smf_dtype_size(*odata,status);
    }
  }

  if( (*status==SAI__OK) && (*odata) ) {

    if( method ) {
      /* Truncated FFT method ---------------------------------------------- */

      dim_t fdims[2];
      smfData *fft_idata=NULL;
      smfData *fft_odata=NULL;
      smfData *tempdata=NULL;
      dim_t nf_i;               /* Number of frequencies in input data */
      dim_t nf_o;               /* Number of frequencies in output data */

      /* Calculate the FFT of the data */
      fft_idata = smf_fft_data( NULL, idata, NULL, 0, 0, status );
      smf_isfft( fft_idata, NULL, NULL, fdims, NULL, NULL, status );
      nf_i = fdims[0];

      /* Output data have fewer frequencies */
      nf_o = ontslice/2 + 1;

      /* Create container for FFT of down-sampled data and copy over
         up to the Nyquist frequency. Note that we set the isFFT flag
         to the correct length in time-slices for the down-sampled data
         so that smf_isfft can get the unambiguous real-space axis length
         when we do the inverse transform. */

      fft_odata = smf_deepcopy_smfData( wf, fft_idata, 0, SMF__NOCREATE_DATA |
                                        SMF__NOCREATE_VARIANCE |
                                        SMF__NOCREATE_QUALITY |
                                        SMF__NOCREATE_FILE | SMF__NOCREATE_HEAD,
                                        0, 0, status );

      if( *status == SAI__OK ) {
        fft_odata->dims[0] = nf_o;
        fft_odata->isFFT = ontslice;
        fft_odata->pntr[0] = astCalloc( nf_o*nbolo*2,
                                        smf_dtype_size(fft_odata,status) );
      }

      if( *status == SAI__OK ) {
        dim_t co_i = nf_i*nbolo;           /* Component offset input */
        dim_t co_o = nf_o*nbolo;           /* Component offset output */
        double *ip = fft_idata->pntr[0];    /* Pointer to input data */
        double *op = fft_odata->pntr[0];    /* Pointer to output data */

        for( i=0; i<nbolo; i++ ) {
          for( j=0; j<nf_o; j++ ) {
            op[i*nf_o + j] = ip[i*nf_i + j];                 /* Real Values */
            op[i*nf_o + j + co_o] = ip[i*nf_i + j + co_i];   /* Imag Values */
          }

          /* If target time-series has even number of samples, Nyquist
             frequency in FFT'd data needs to be real valued */
          if( !(ontslice % 2) ) {
            j = nf_o - 1;
            op[i*nf_o + j + co_o] = 0;
          }
        }
      }

      /* Transform back to the time-domain and convert to the same order
         as the input */
      tempdata = smf_fft_data( NULL, fft_odata, NULL, 1, 0, status );
      smf_dataOrder( wf, tempdata, idata->isTordered, status );

      if( *status == SAI__OK ) {
        /* Copy over the data pointer and then null it in tempdata to
           avoid closing it twice. */
        (*odata)->pntr[0] = tempdata->pntr[0];
        tempdata->pntr[0] = NULL;

        if( (*odata)->ndims != tempdata->ndims ) {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME ": programming error, wrong number of dims!",
                  status );
        }

        for( i=0; (*status==SAI__OK)&&(i<(*odata)->ndims); i++ ) {
          if( (*odata)->dims[i] != tempdata->dims[i] ) {
            *status = SAI__ERROR;
            errRepf( "", FUNC_NAME ": programming error, dim(%zu) don't match! "
                     "got %" DIM_T_FMT ", should be %" DIM_T_FMT, status, i,
                     (*odata)->dims[i], tempdata->dims[i] );
          }
        }
      }

      /* Clean up */
      smf_close_file( wf, &fft_idata, status );
      smf_close_file( wf, &fft_odata, status );
      smf_close_file( wf, &tempdata, status );
    } else {
      /* Time-domain filter method ----------------------------------------- */

      (*odata)->pntr[0] = astCalloc( ondata, size );

      if( *status == SAI__OK ) {

        if( idata->dtype == SMF__DOUBLE ) {
          /* Input and output data are doubles */

          double *idat = idata->pntr[0];
          double *odat = (*odata)->pntr[0];

          for( i=0; (*status==SAI__OK) && i<nbolo; i++ ) {
            smf_downsamp1D( wf, idat+i*ibstride, itstride, 0, intslice,
                            odat+i*obstride, otstride, 0, ontslice, 1, 0,
                            0, status );
          }
        } else if( idata->dtype==SMF__INTEGER ) {
          /* Input data are integers */

          int *idat = idata->pntr[0];

          if( todouble ) {
            /* converting int to double */
            double *odat = (*odata)->pntr[0];

            (*odata)->dtype = SMF__DOUBLE;

            for( i=0; (*status==SAI__OK) && i<nbolo; i++ ) {
              smf_downsamp1I( wf, idat+i*ibstride, itstride, 0, intslice,
                              odat+i*obstride, otstride, 0, ontslice, 1, 0,
                              0, status);
            }
          } else {
            /* output will also be int */
            int *odat = (*odata)->pntr[0];

            for( i=0; (*status==SAI__OK) && i<nbolo; i++ ) {
              smf_downsamp1I( wf, idat+i*ibstride, itstride, 0, intslice,
                              odat+i*obstride, otstride, 0, ontslice, 0, 0,
                              0, status);
            }
          }
        } else {
          *status = SAI__ERROR;
          errRep( "", FUNC_NAME ": Don't know how to handle data type",
                  status );
        }

        /* Wait for all the above smf_downsamp1 jobs to finish. */
        thrWait( wf, status );

      }
    }
  }

  /* Down-sample the smfHead -------------------------------------------------*/
  if( (*status==SAI__OK) && (*odata) && (*odata)->hdr ) {
    smfHead *hdr = (*odata)->hdr;

    hdr->curframe = (dim_t) (((double) hdr->curframe + 0.5) / scale);
    hdr->nframes = ontslice;
    hdr->steptime *= scale;

    if( instate ) {
      /* Down-sample all the JCMTState values using nearest neighbours */

      hdr->allState = astCalloc( ontslice, sizeof(*instate) );
      outstate = hdr->allState;

      if( *status == SAI__OK ) {
        dim_t frame;  /* index of nearest neighbour JCMTState */

        for( i=0; i<ontslice; i++ ) {
          frame = (dim_t) round(((double) i + 0.5)*scale);
          memcpy( outstate + i, instate + frame, sizeof(*instate) );
        }

        /* Then go back and properly down-sample the more important
           fast-changing fields like pointing. Note that since there
           are approximate values there already we need to explicitly
           re-initialize to 0. */

        RESAMPSTATE(instate, outstate, rts_end, intslice, ontslice, 0);

        RESAMPSTATE(instate, outstate, smu_az_jig_x, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, smu_az_jig_y, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, smu_az_chop_x, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, smu_az_chop_y, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, smu_tr_jig_x, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, smu_tr_jig_y, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, smu_tr_chop_x, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, smu_tr_chop_y, intslice, ontslice, 0);

        RESAMPSTATE(instate, outstate, tcs_tai, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, tcs_airmass, intslice, ontslice, 0);

        /* Second coordinates (Dec, El etc) can not wrap 0 to 360 so we
           do not need to test for those cases */
        RESAMPSTATE(instate, outstate, tcs_az_ang, intslice, ontslice, 1);
        RESAMPSTATE(instate, outstate, tcs_az_ac1, intslice, ontslice, 1);
        RESAMPSTATE(instate, outstate, tcs_az_ac2, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, tcs_az_dc1, intslice, ontslice, 1);
        RESAMPSTATE(instate, outstate, tcs_az_dc2, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, tcs_az_bc1, intslice, ontslice, 1);
        RESAMPSTATE(instate, outstate, tcs_az_bc2, intslice, ontslice, 0);

        RESAMPSTATE(instate, outstate, tcs_tr_ang, intslice, ontslice, 1);
        RESAMPSTATE(instate, outstate, tcs_tr_ac1, intslice, ontslice, 1);
        RESAMPSTATE(instate, outstate, tcs_tr_ac2, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, tcs_tr_dc1, intslice, ontslice, 1);
        RESAMPSTATE(instate, outstate, tcs_tr_dc2, intslice, ontslice, 0);
        RESAMPSTATE(instate, outstate, tcs_tr_bc1, intslice, ontslice, 1);
        RESAMPSTATE(instate, outstate, tcs_tr_bc2, intslice, ontslice, 0);

        RESAMPSTATE(instate, outstate, tcs_en_dc1, intslice, ontslice, 1);
        RESAMPSTATE(instate, outstate, tcs_en_dc2, intslice, ontslice, 0);

        RESAMPSTATE(instate, outstate, tcs_dm_abs, intslice, ontslice, 1);
        RESAMPSTATE(instate, outstate, tcs_dm_rel, intslice, ontslice, 0);

        /* Wait for all the above smf_downsamp1 jobs to finish. */
        thrWait( wf, status );

      }

    }
  }

  /* Down-sample the smfDA */
  if( (*status==SAI__OK) && (*odata) && (*odata)->da ) {
    smfDA *da = (*odata)->da;

    /* Down-sample the dark squids */
    if( indksquid ) {
      smf_downsamp_smfData( wf, indksquid, &da->dksquid, ontslice, todouble,
                            method, status );
    }
  }

}

