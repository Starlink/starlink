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
*     smf_downsamp_smfData( smfData *idata, smfData **odata, dim_t ontslice,
*                           int todouble, int *status );

*  Arguments:
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
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine produces a copy of a smfData at a lower sample rate.
*     All bolometer and dark squid data are downsampled using smf_downsamp1
*     which uses simple averages. JCMTState information is propagated
*     using a nearest-neighbour resampling.

*  Notes:
*     This routine does not downsample the NOISE or QUALITY components.

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-10-20 (EC):
*        Initial version
*     2010-10-21 (EC):
*        Nearest-neighbour for most of JCMTState, but do proper resampling
*        for important fast-changing fields.
*     2010-10-22 (EC)
*        Add todouble flag
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 University of British Columbia
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

#define RESAMPSTATE(in,out,member,intslice,ontslice) smf_downsamp1D( &(in->member),sizeof(JCMTState),1,intslice,&(out->member), sizeof(JCMTState),1,ontslice,1,1,status );

#define FUNC_NAME "smf_downsamp_smfData"

void smf_downsamp_smfData( const smfData *idata, smfData **odata,
                           dim_t ontslice, int todouble, int *status ) {

  size_t i;                /* loop counter */
  size_t ibstride;         /* bstride of idata */
  smfData *indksquid=NULL; /* Pointer to input dksquid data */
  JCMTState *instate=NULL; /* Pointer to input JCMTState */
  dim_t intslice;          /* ntslice of idata */
  size_t itstride;         /* tstride of idata */
  dim_t nbolo;             /* number of bolos */
  size_t obstride;         /* bstride of odata */
  dim_t ondata;            /* ndata of odata */
  size_t otstride;         /* tstride of odata */
  JCMTState *outstate=NULL;/* Pointer to output JCMTState */
  double scale;            /* how much longer new samples are */

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

  /* Dimensions of input */
  smf_get_dims( idata, NULL, NULL, &nbolo, &intslice, NULL, &ibstride,
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

  *odata = smf_deepcopy_smfData( idata, 0, SMF__NOCREATE_DATA |
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

  /* Down-sample the bolometer data */
  if( *status == SAI__OK ) {
    if( (*odata)->isTordered ) {
      (*odata)->dims[2] = ontslice;
    } else {
      (*odata)->dims[0] = ontslice;
    }
  }

  smf_get_dims( *odata, NULL, NULL, NULL, NULL, &ondata, &obstride,
                &otstride, status );

  if( *status == SAI__OK ) {
    int size;

    if( todouble ) size = sizeof(double);
    else size = smf_dtype_size(*odata,status);

    (*odata)->pntr[0] = astCalloc( ondata, size );
  }

  if( (*status==SAI__OK) && (*odata) ) {

    if( (*odata)->dtype == SMF__DOUBLE ) {
      /* Input data are doubles */

      double *idat = idata->pntr[0];
      double *odat = (*odata)->pntr[0];

      for( i=0; (*status==SAI__OK) && i<nbolo; i++ ) {
        smf_downsamp1D( idat+i*ibstride, itstride, 0, intslice,
                        odat+i*obstride, otstride, 0, ontslice, 1, 0, status );
      }
    } else if( (*odata)->dtype == SMF__INTEGER ) {
      /* Input data are integers */

      int *idat = idata->pntr[0];

      if( todouble ) {
        /* converting int to double */
        double *odat = (*odata)->pntr[0];

        (*odata)->dtype = SMF__DOUBLE;

        for( i=0; (*status==SAI__OK) && i<nbolo; i++ ) {
          smf_downsamp1I( idat+i*ibstride, itstride, 0, intslice,
                          odat+i*obstride, otstride, 0, ontslice, 1, 0, status);
        }
      } else {
        /* output will also be int */
        int *odat = (*odata)->pntr[0];

        for( i=0; (*status==SAI__OK) && i<nbolo; i++ ) {
          smf_downsamp1I( idat+i*ibstride, itstride, 0, intslice,
                          odat+i*obstride, otstride, 0, ontslice, 0, 0, status);
        }
      }
    } else {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": don't know how to handle data type", status );
    }
  }

  /* Down-sample the smfHead */
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
        size_t frame;  /* index of nearest neighbour JCMTState */

        for( i=0; i<ontslice; i++ ) {
          frame = (size_t) round(((double) i + 0.5)*scale);
          memcpy( outstate + i, instate + frame, sizeof(*instate) );
        }

        /* Then go back and properly down-sample the more important
           fast-changing fields like pointing. Note that since there
           are approximate values there already we need to explicitly
           re-initialize to 0. */

        RESAMPSTATE(instate, outstate, rts_end, intslice, ontslice);

        RESAMPSTATE(instate, outstate, smu_az_jig_x, intslice, ontslice);
        RESAMPSTATE(instate, outstate, smu_az_jig_y, intslice, ontslice);
        RESAMPSTATE(instate, outstate, smu_az_chop_x, intslice, ontslice);
        RESAMPSTATE(instate, outstate, smu_az_chop_y, intslice, ontslice);
        RESAMPSTATE(instate, outstate, smu_tr_jig_x, intslice, ontslice);
        RESAMPSTATE(instate, outstate, smu_tr_jig_y, intslice, ontslice);
        RESAMPSTATE(instate, outstate, smu_tr_chop_x, intslice, ontslice);
        RESAMPSTATE(instate, outstate, smu_tr_chop_y, intslice, ontslice);

        RESAMPSTATE(instate, outstate, tcs_tai, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_airmass, intslice, ontslice);

        RESAMPSTATE(instate, outstate, tcs_az_ang, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_az_ac1, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_az_ac2, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_az_dc1, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_az_dc2, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_az_bc1, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_az_bc2, intslice, ontslice);

        RESAMPSTATE(instate, outstate, tcs_tr_ang, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_tr_ac1, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_tr_ac2, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_tr_dc1, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_tr_dc2, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_tr_bc1, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_tr_bc2, intslice, ontslice);

        RESAMPSTATE(instate, outstate, tcs_en_dc1, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_en_dc2, intslice, ontslice);

        RESAMPSTATE(instate, outstate, tcs_dm_abs, intslice, ontslice);
        RESAMPSTATE(instate, outstate, tcs_dm_rel, intslice, ontslice);

      }

    }
  }

  /* Down-sample the smfDA */
  if( (*status==SAI__OK) && (*odata) && (*odata)->da ) {
    smfDA *da = (*odata)->da;

    /* Down-sample the dark squids */
    if( indksquid ) {
      smf_downsamp_smfData( indksquid, &da->dksquid, ontslice, todouble,
                            status );
    }
  }

}

