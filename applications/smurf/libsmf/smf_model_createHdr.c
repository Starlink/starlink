/*
*+
*  Name:
*     smf_model_createHdr

*  Purpose:
*     Create (or fill existing) header for model smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_model_createHdr( smfData *model, smf_modeltype type,
*                          smfData *refdata, int *status );

*  Arguments:
*     model = smfData * (Given)
*        Pointer to smfData containing model information
*     type = smf_modeltype (Given)
*        Type of model
*     refdata = smfData * (Given)
*        Pointer to reference data containing full time-series WCS frameset
*        corresponding to this model.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate a "time series" WCS frameset compatible with the model
*     container, so that we can later call ndfPtwcs when we export. Requires
*     both the model smfData itself, and the tswcs from the original data.
*     Also copies over the FITS header from the reference header. If
*     the reference contains neither the WCS or FITS header information
*     the model header pointers are left in the initialized state (NULL).
*     Old tswcs and FITS headers are annulled.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-09-29 (EC):
*        Initial Version
*     2008-12-11 (EC):
*        Renames smf_model_createHdr from smf_model_createtswcs, and FITS header
*        added.
*     2009-03-12 (EC):
*        Added SMF__FLT
*     2010-04-08 (EC):
*        Propagate JCMTState if present.
*     2010-05-10 (EC):
*        Avoid overwriting reference header if it is the same as the model.
*     2010-05-12 (EC):
*        COM converted to 3d model from 1d (but with length 1 spatial axes)
*     2010-05-14 (TIMJ):
*        Added SMF__PLN
*     2010-05-18 (EC):
*        Write model type (3 characters) to FITS header "SMFMODEL"
*     2010-05-19 (EC):
*        Use a full smfData as the reference, and write out full data
*        dimensions for time series cube to FITS header.
*     2010-05-27 (TIMJ):
*        Add SMF__SMO
*     2010-06-08 (EC):
*        Add SMF__TWO
*     2014-12-18 (DSB):
*        Added SSN.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     Copyright (C) 2008-2010 University of British Columbia.
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "prm_par.h"
#include "par_par.h"
#include "ast.h"
#include "star/atl.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_model_createHdr"

void smf_model_createHdr( smfData *model, smf_modeltype type,
                          smfData *refdata, int *status ) {

  /* Local Variables */
  AstFitsChan *fits=NULL;       /* New FITS header */
  AstFrameSet *fset=NULL;       /* the returned framset */
  AstFitsChan *reffits=NULL;    /* Reference FITS header */
  smfHead *refhdr=NULL;         /* Reference smfHead */
  AstFrameSet *refwcs=NULL;     /* Reference time series WCS */

  /* Main routine */
  if( *status != SAI__OK ) return;

  if( !model || !refdata || !refdata->hdr ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Null input pointers", status );
    return;
  }

  refhdr = refdata->hdr;

  /* If the reference header is the same as the model header return */
  if( refhdr == model->hdr ) {
    return;
  }

  refwcs = refhdr->tswcs;
  reffits = refhdr->fitshdr;

  /* Create smfHead if needed */
  if( !model->hdr ) {
    model->hdr = smf_create_smfHead( status );
  }

  /* Calculate TSWCS */
  if( refwcs ) {
    astBegin;

    /* Create frameset for each model type. Many have same dimensions as the
       raw data, so just return a copy of refwcs */

    switch( type ) {

    case SMF__CUM:
      fset = astCopy( refwcs );
      break;

    case SMF__RES:
      fset = astCopy( refwcs );
      break;

    case SMF__AST:
      fset = astCopy( refwcs );
      break;

    case SMF__COM:
      /* This is really a 1-d data set since we've collapsed along the
         two spatial axes in the focal plane. However, these still
         appear as dimensions of length 1 -- so we can just use the
         full 3d WCS anyways. */
      fset = astCopy( refwcs );
      break;

    case SMF__NOI:
      fset = astCopy( refwcs );
      break;

    case SMF__EXT:
      fset = astCopy( refwcs );
      break;

    case SMF__LUT:
      fset = astCopy( refwcs );
      break;

    case SMF__QUA:
      fset = astCopy( refwcs );
      break;

    case SMF__DKS:
      /* Don't know how to make a particularly meaningful framset */
      fset = NULL;
      break;

    case SMF__GAI:
      /* Don't know how to make a particularly meaningful framset */
      fset = NULL;
      break;

    case SMF__FLT:
      fset = astCopy( refwcs );
      break;

    case SMF__PLN:
      fset = astCopy( refwcs );
      break;

    case SMF__SMO:
      fset = astCopy( refwcs );
      break;

    case SMF__SSN:
      fset = astCopy( refwcs );
      break;

    case SMF__TMP:
      /* Don't know how to make a particularly meaningful framset */
      fset = NULL;
      break;

    case SMF__TWO:
      /* Don't know how to make a particularly meaningful framset */
      fset = NULL;
      break;

    default:
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Invalid smf_modeltype given.", status);
    }

    if( (*status==SAI__OK) && fset ) {
      /* Export the frameset before ending the ast context */
      astExport( fset );

      /* Annul old tswcs if one exists */
      if( (*status==SAI__OK) && model->hdr->tswcs ) {
        model->hdr->tswcs = astAnnul( model->hdr->tswcs );
      }

      /* Store the new frameset in the header */
      if( *status==SAI__OK ) model->hdr->tswcs = fset;

    }

    astEnd;
  }

  /* Propagate FITS header (only if the new and reference FITS header pointers
     are different! */
  if( (*status==SAI__OK) && reffits && (reffits != model->hdr->fitshdr) ) {

    /* Annul old FITS header if one exists */
    if( model->hdr->fitshdr ) {
      model->hdr->fitshdr = astAnnul( model->hdr->fitshdr );
    }

    /* Copy in the new one */
    model->hdr->fitshdr = astCopy( reffits );
  }

  /* Add the model name to the FITS header and the bolometer dimensions */
  fits = model->hdr->fitshdr;
  if( fits ) {
    dim_t nrows, ncols, ntslice;
    int lbnd_r, lbnd_c, lbnd_t;

    smf_get_dims( refdata, &nrows, &ncols, NULL, &ntslice, NULL, NULL,
                  NULL, status );

    if( refdata->isTordered ) {
      lbnd_r = refdata->lbnd[SC2STORE__ROW_INDEX];
      lbnd_c = refdata->lbnd[SC2STORE__COL_INDEX];
      lbnd_t = refdata->lbnd[2];
    } else {
      lbnd_r = refdata->lbnd[1+SC2STORE__ROW_INDEX];
      lbnd_c = refdata->lbnd[1+SC2STORE__COL_INDEX];
      lbnd_t = refdata->lbnd[0];
    }

    atlPtfts( fits, "SMFMODEL", smf_model_getname(type,status),
              "SMURF Iterative model component", status );

    atlPtfti( fits, "SMFDIMR", nrows,
              "SMURF row dimension length for raw data", status );
    atlPtfti( fits, "SMFDIMC", ncols,
              "SMURF col dimension length for raw data", status );
    atlPtfti( fits, "SMFDIMT", ntslice,
              "SMURF time dimension length for raw data", status );

    atlPtfti( fits, "SMFLBNDR", lbnd_r,
              "SMURF row dimension lower bound for raw data", status );
    atlPtfti( fits, "SMFLBNDC", lbnd_c,
              "SMURF col dimension lower bound for raw data", status );
    atlPtfti( fits, "SMFLBNDT", lbnd_t,
              "SMURF time dimension lower bound for raw data", status );
  }

  /* Propagate JCMTState */
  if( (*status==SAI__OK) && (refhdr->allState) ) {
    model->hdr->nframes = refhdr->nframes;
    model->hdr->allState = astCalloc( refhdr->nframes,
                                      sizeof(*(model->hdr->allState)));
    if (model->hdr->allState) {
      memcpy( model->hdr->allState, refhdr->allState,
              refhdr->nframes*sizeof(*refhdr->allState) );
    }
  }
}
