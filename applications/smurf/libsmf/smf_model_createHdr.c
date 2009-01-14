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
*                            AstFrameSet *refwcs, int *status );

*  Arguments:
*     model = smfData * (Given)
*        Pointer to smfData containing model information
*     type = smf_modeltype (Given)
*        Type of model
*     refwcs = AstFrameSet * (Given)
*        Pointer to time-series WCS frameset corresponding to this model
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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2008 University of British Columbia.
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "prm_par.h"
#include "par_par.h"
#include "ast.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_model_createHdr"

void smf_model_createHdr( smfData *model, smf_modeltype type, 
                          smfHead *refhdr, int *status ) {

  /* Local Variables */
  AstMapping *cbmap=NULL;       /* Pointer to current->base mapping */
  AstFrameSet *fset=NULL;       /* the returned framset */ 
  int out[NDF__MXDIM];          /* Indices outputs of mapping */
  AstFitsChan *reffits=NULL;    /* Reference FITS header */
  AstFrameSet *refwcs=NULL;     /* Reference time series WCS */
  int taxis;                    /* Index of time axis */ 
  AstFrame *tfrm=NULL;          /* 1D frame (TimeFrame) */
  AstMapping *tmap=NULL;        /* Mapping for time axis */


  /* Main routine */
  if( *status != SAI__OK ) return;

  if( !model || !refhdr ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Null input pointers", status );
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
      /* For 1=dimensional data, assume it is the time axis which we
         extract from the 3d WCS */
      
      /* Get a pointer to the current->base Mapping (i.e. the Mapping from
         WCS coords to GRID coords). */
      cbmap = astGetMapping( refwcs, AST__CURRENT, AST__BASE );
      
      /* Use astMapSplit to split off the Mapping for the time
         axis. This assumes that the time axis is the 3rd axis
         (i.e. index 2) */
      
      taxis = 3;
      astMapSplit( cbmap, 1, &taxis, out, &tmap );
      
      /* We now check that the Mapping was split succesfully. This should
         always be the case for the time axis since the time axis is 
         independent of the others, but it is as well to check in case of 
         bugs, etc. */
      if( !tmap ) {
        /* The "tmap" mapping will have 1 input (the WCS time value) -
           astMapSplit guarantees this. But we
           should also check that it also has only one output (the
           corresponding GRID axis). */
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME ": Couldn't extract time-axis mapping",
                status );
      } else if( astGetI( tmap, "Nout" ) != 1 ) {
        *status = SAI__ERROR;
        errRep( "", FUNC_NAME 
                ": Time-axis mapping has incorrect number of outputs",
                status );
      } else {
        
        /* Create a new FrameSet containing a 1D GRID Frame. */
        fset = astFrameSet( astFrame( 1, "Domain=GRID" ), " " );
        
        /* Extract the 1D Frame (presumably a TimeFrame)
           describing time from the current (WCS) 3D Frame. */
        tfrm = astPickAxes( refwcs, 1, &taxis, NULL);
        
        /* Add the time frame into the 1D FrameSet, using the
           Mapping returned by astMapSplit. Note, this Mapping
           goes from time to grid, so we invert it first so that
           it goes from grid to time, as required by
           astAddFrame. */
        
        astInvert( tmap );
        astAddFrame( fset, AST__BASE, tmap, tfrm );
      }
      
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
      
    default:
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": Invalid smf_modeltype given.", status);        
    }
    
    /* Trap additional Ast errors */
    if( (*status == SAI__OK) && !astOK ) {
      *status = SAI__ERROR;
      msgSetc("MODEL", smf_model_getname( type, status ) );
      errRep( "", FUNC_NAME ": Ast error creating frameset for ^MODEL", status);
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

    /* Trap Ast error generated by copy */
    if( !astOK ) {
      *status = SAI__ERROR;
      msgSetc("MODEL", smf_model_getname( type, status ) );
      errRep( "", FUNC_NAME ": Ast error creating FITS header for ^MODEL", 
              status);
    }
  }
}
