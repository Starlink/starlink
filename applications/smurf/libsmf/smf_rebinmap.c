/*
*+
*  Name:
*     smf_rebinmap

*  Purpose:
*     Map-maker that simply rebins the data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebinmap( smfData *data, int index, int size, 
*                    AstFrameSet *outfset, int moving,
*                   int *lbnd_out, int *ubnd_out, 
*                   double *map, double *variance, double *weights,
*         	    int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to smfData struct
*     index = int (Given)
*        Index of element in igrp
*     size = int (Given)
*        Number of elements in igrp
*     outfset = AstFrameSet* (Given)
*        Frameset containing the sky->output map mapping
*     moving = int (Given)
*        Flag to denote whether the object is moving
*     lbnd_out = double* (Given)
*        2-element array pixel coord. for the lower bounds of the output map 
*     ubnd_out = double* (Given)
*        2-element array pixel coord. for the upper bounds of the output map 
*     map = double* (Returned)
*        The output map array 
*     variance = double* (Returned)
*        Variance of each pixel in map
*     weights = double* (Returned)
*        Relative weighting for each pixel in map
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function does a simple regridding of data into a map

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-02-02 (EC):
*        Initial version.
*     2006-02-13 (TIMJ):
*        Use astSetC rather than astSet
*        Avoid an additional dereference
*     2006-03-23 (AGG):
*        Updated API: now takes a smfData rather than a Grp
*     2006-07-26 (TIMJ):
*        sc2head not actually used.
*     2007-01-25 (AGG):
*        Rewrite to take account of moving objects
*     2007-02-27 (AGG):
*        Minor refactor for improved status handling
*     {enter_further_changes_here}

*  Notes:
*     Currently lon_0 and lat_0 are interpreted only as ra/dec of tangent point

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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
#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_rebinmap"

void smf_rebinmap( smfData *data,  int index, int size, AstFrameSet *outfset, 
		   int moving, int *lbnd_out, int *ubnd_out, double *map, 
		   double *variance, double *weights, int *status ) {

  /* Local Variables */
  double a;                     /* Longitude value */
  AstSkyFrame *abskyfrm = NULL; /* Output SkyFrame (always absolute) */
  AstMapping *azel2usesys = NULL;/* Mapping from AZEL to the output sky frame */
  double b;                     /* Latitude value */
  AstMapping *bolo2sky = NULL;  /* Mapping bolo->celestial coordinates */
  AstCmpMap *bolo2map = NULL;   /* Combined mapping bolo->map coordinates */
  double  *boldata = NULL;      /* Pointer to bolometer data */
  AstFrameSet *fs = NULL;       /* WCS FramesSet from input */           
  smfHead *hdr = NULL;          /* Pointer to data header this time slice */
  dim_t i;                      /* Loop counter */
  int ibasein;                  /* Index of base Frame in input WCS FrameSet */
  int lbnd_in[2];               /* Lower pixel bounds for input maps */
  int nbol = 0;                 /* # of bolometers in the sub-array */
  AstSkyFrame *oskyfrm = NULL;  /* SkyFrame from the output WCS Frameset */
  int rebinflags;               /* Control the rebinning procedure */
  AstFrame *sf1 = NULL;         /* Pointer to copy of input current Frame */
  AstMapping *sky2map=NULL;     /* Mapping from celestial->map coordinates */
  AstFrame *skyin = NULL;       /* Pointer to current Frame in input WCS FrameSet */
  AstFrameSet *swcsin = NULL;   /* Spatial WCS FrameSet for current time slice */
  const char *system;           /* Coordinate system */
  int ubnd_in[2];               /* Upper pixel bounds for input maps */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Check we have valid input data */
  if ( data == NULL ) {
    *status = SAI__ERROR;
    errRep( "", "Input data struct is NULL", status );
    return;
  }
  /* And a valid FrameSet */
  if ( outfset == NULL ) {
    *status = AST__OBJIN;
    errRep( "", "Supplied FrameSet is NULL", status );
    return;
  }

  /* Get the system from the outfset */
  system = astGetC( outfset, "system" );

  /* Retrieve the sky2map mapping from the output frameset (actually
     map2sky) */
  oskyfrm = astGetFrame( outfset, AST__CURRENT );
  sky2map = astGetMapping( outfset, AST__BASE, AST__CURRENT );
  /* Invert it to get Output SKY to output map coordinates */
  astInvert( sky2map );
  /* Create a SkyFrame in absolute coordinates */
  abskyfrm = astCopy( oskyfrm );
  astClear( abskyfrm, "SkyRefIs" );
  astClear( abskyfrm, "SkyRef(1)" );
  astClear( abskyfrm, "SkyRef(2)" );

  /* Calculate bounds in the input array */
  nbol = (data->dims)[0] * (data->dims)[1];
  lbnd_in[0] = 0;
  lbnd_in[1] = 0;
  ubnd_in[0] = (data->dims)[0]-1;
  ubnd_in[1] = (data->dims)[1]-1;

  boldata = (data->pntr)[0];
  if ( boldata == NULL ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "Input data to rebinmap is NULL", status );
    }
  }

  /* Loop over all time slices in the data */
  for( i=0; (i<(data->dims)[2]) && (*status == SAI__OK); i++ ) {
	
    smf_tslice_ast( data, i, 1, status);
	
    if( *status == SAI__OK ) {
      hdr = data->hdr;
      swcsin = hdr->wcs;
      /* Find out how to convert from input GRID coords to the output
	 sky frame.  Note, we want absolute sky coords here, even if
	 the target is moving.  Record the original base frame before
	 calling astConvert so that it can be re-instated later
	 (astConvert modifies the base Frame). */
      astInvert( swcsin );
      ibasein = astGetI( swcsin, "Base" );
      fs = astConvert( swcsin, abskyfrm, "SKY" );
      astSetI( swcsin, "Base", ibasein );
      astInvert( swcsin );
      if( fs == NULL ) {
         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "The spatial coordinate system in the "
                    "current input file is not compatible with the "
                    "spatial coordinate system in the first input file.", 
                    status );
         }
         break;
      }

      /* The "fs" FrameSet has input GRID coords as its base Frame,
	 and output (absolute) sky coords as its current frame. If the
	 target is moving, modify this so that the current Frame
	 represents offsets from the current telescope base pointing
	 position (the mapping in the "fs" FrameSet is also modified
	 automatically). */
      if( moving ) {
	/* Get the Mapping from AZEL (at the current input epoch) to
	   the output sky system. Use it to convert the telescope base
	   pointing position from (az,el) to the requested system. */
         skyin = astGetFrame( swcsin, AST__CURRENT );
         sf1 = astCopy( skyin );
         astSetC( sf1, "System", "AZEL" );
         azel2usesys = astConvert( sf1, abskyfrm, "" );
         astTran2( azel2usesys, 1, &(hdr->state->tcs_az_bc1),
                   &(hdr->state->tcs_az_bc2), 1, &a, &b );

	 /* Explicitly annul these objects for efficiency in this
	    tight loop. */
         azel2usesys = astAnnul( azel2usesys );
         sf1 = astAnnul( sf1 );
         skyin = astAnnul( skyin );

	 /* Modified the FrameSet to represent offsets from this
	    origin. We use the FrameSet pointer "fs" rather than a
	    pointer to the current Frame within the FrameSet. This
	    means that the Mapping in the FrameSet will be modified to
	    remap the current Frame. */
         astSetD( fs, "SkyRef(1)", a );
         astSetD( fs, "SkyRef(2)", b );
         astSet( fs, "SkyRefIs=origin" );

	 /* Get the Mapping and then clear the SkyRef attributes (this
	    is because the current Frame in "fs" may be "*skyframe"
	    and we do not want to make a permanent change to
	    *skyframe). */
         bolo2sky = astGetMapping( fs, AST__BASE, AST__CURRENT );
         astClear( fs, "SkyRef(1)" );
         astClear( fs, "SkyRef(2)" );
         astClear( fs, "SkyRefIs" );
      } else {
	 /* If the target is not moving, just get the Mapping. */
         bolo2sky = astGetMapping( fs, AST__BASE, AST__CURRENT );
      }
      /* The output from "sky2map" now corresponds to the input to
	 "sky2map", whether the target is moving or not. Combine the
	 input GRID to output SKY Mapping with the output SKY to
	 output pixel Mapping supplied in "sky2map". */
      bolo2map = astCmpMap( bolo2sky, sky2map, 1, "" );

      /* Rebin this time slice */
      rebinflags = 0;
      if( (index == 1) && (i == 0) )                    /* Flags start rebin */
	rebinflags = rebinflags | AST__REBININIT;

      if( (index == size) && (i == (data->dims)[2]-1) ) /* Flags end rebin */
	rebinflags = rebinflags | AST__REBINEND;
	  
      astRebinSeqD( bolo2map, 0.0, 2, lbnd_in, ubnd_in, &(boldata[i*nbol]),
		    NULL, AST__NEAREST, NULL, rebinflags, 0.1, 1000000, 
		    VAL__BADD, 2, lbnd_out, ubnd_out, lbnd_in, ubnd_in,
		    map, variance, weights );

      /* clean up ast objects */
      if ( bolo2sky ) bolo2sky = astAnnul( bolo2sky );
      if ( bolo2map ) bolo2map = astAnnul( bolo2map );
      if ( fs ) fs = astAnnul( fs );
    }
  }

  /* Clean Up */
  if ( sky2map ) sky2map  = astAnnul( sky2map );
  if ( bolo2sky ) bolo2sky = astAnnul( bolo2sky );
  if ( bolo2map ) bolo2map = astAnnul( bolo2map );
  if ( fs ) fs = astAnnul( fs );

  if ( abskyfrm ) abskyfrm = astAnnul( abskyfrm );
  if ( oskyfrm ) oskyfrm = astAnnul( oskyfrm );

  if ( *status != SAI__OK ) {
    errRep( "", "Rebinning step failed", status );
  }
}
