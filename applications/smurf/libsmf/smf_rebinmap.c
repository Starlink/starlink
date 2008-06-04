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
*                   AstFrameSet *outfset, int spread, const double params[], 
*                   int moving, int *lbnd_out, int *ubnd_out, 
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
*     spread = int (Given)
*        Integer code for pixel-spreading scheme
*     params[] = const double (Given)
*        Array of additional parameters for certain schemes
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
*     2007-05-3 (DSB):
*        Adapt to new astRebinSeq signature.
*     2007-07-11 (DSB):
*        - Use astConvert to convert between input and output skyframes,
*        rather than input frameset and output skyframe. Only get the
*        azel->output skyframe FrameSet for moving sources if the input 
*        skyframe is not azel.
*        - Avoid remapping the FrameSet more often than is necessary when
*        setting and clearing the SkyRef attributes.
*     2007-07-12 (EC):
*        -Replaced calculation of bolo2map with a call to smf_rebincube_totmap
*        -Changed name of smf_rebincube_totmap to smf_rebin_totmap
*     2008-02-12 (AGG):
*        Updated API to allow processing of bad bolometer masks. These
*        updates deprecate smf_bbrebinmap.
*     2008-02-13 (AGG):
*        Add parameters for pixel spreading scheme
*     2008-02-15 (AGG):
*        Enable AST__GENVAR to return variances
*     2008-03-11 (AGG):
*        Remove old bad bolometer-mask code: masking is now done with
*        quality flags
*     2008-04-16 (AGG):
*        Add genvar parameter
*     2008-04-18 (AGG):
*        Set lbnd to 1,1
*     2008-06-04 (TIMJ):
*        astRebinSeq requires GRID output bounds not PIXEL
*     {enter_further_changes_here}

*  Notes:
*     Currently lon_0 and lat_0 are interpreted only as ra/dec of tangent point

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2005-2008 University of British Columbia
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

void smf_rebinmap( smfData *data, int index, int size, 
		   AstFrameSet *outfset, int spread, const double params[], 
		   int moving, int genvar, int *lbnd_out, int *ubnd_out, double *map, 
		   double *variance, double *weights, int *status ) {

  /* Local Variables */
  AstSkyFrame *abskyfrm = NULL; /* Output SkyFrame (always absolute) */
  AstMapping *bolo2map = NULL;  /* Combined mapping bolo->map coordinates */
  double *boldata = NULL;       /* Pointer to bolometer data */
  dim_t i;                      /* Loop counter */
  int lbnd_in[2];               /* Lower pixel bounds for input maps */
  int ldim[ 2 ];                /* Output array lower GRID bounds */
  dim_t nbol;                   /* # of bolometers in the sub-array */
  int nused;                    /* No. of input values used */
  AstSkyFrame *oskyfrm = NULL;  /* SkyFrame from the output WCS Frameset */
  int rebinflags = 0;           /* Control the rebinning procedure */
  AstMapping *sky2map=NULL;     /* Mapping from celestial->map coordinates */
  int ubnd_in[2];               /* Upper pixel bounds for input maps */
  int udim[ 2 ];                /* Output array upper GRID bounds */

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

/* Fill an array with the lower grid index bounds of the output. */
   ldim[ 0 ] = 1;
   ldim[ 1 ] = 1;

/* Integer upper grid index bounds of the output. */
   udim[ 0 ] = ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1;
   udim[ 1 ] = ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1;


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

  /* Check that there really is valid data */
  boldata = (data->pntr)[0];
  if ( boldata == NULL ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "Input data to rebinmap is NULL", status );
    }
  }
  /* Calculate bounds in the input array */
  nbol = (data->dims)[0] * (data->dims)[1];
  lbnd_in[0] = 1;
  lbnd_in[1] = 1;
  ubnd_in[0] = (data->dims)[0];
  ubnd_in[1] = (data->dims)[1];

  /* Loop over all time slices in the data */
  for( i=0; (i<(data->dims)[2]) && (*status == SAI__OK); i++ ) {
       
    /* Calculate the bolometer to map-pixel transformation for this tslice */
    bolo2map = smf_rebin_totmap( data, i, abskyfrm, sky2map, moving, 
				 status );
    /* Set rebin flags */
    rebinflags = 0;

    /* There may be bad values */
    rebinflags = rebinflags | AST__USEBAD;

    /* Set flag at start of rebin */
    if( (index == 1) && (i == 0) )
      rebinflags = rebinflags | AST__REBININIT;

    /* And set flag for end of rebin */
    if( (index == size) && (i == (data->dims)[2]-1) )
      rebinflags = rebinflags | AST__REBINEND;

    /* Generate VARIANCE if requested */
    if ( genvar )
      rebinflags = rebinflags | AST__GENVAR;
 
    /* Rebin this time slice */
    astRebinSeqD( bolo2map, 0.0, 2, lbnd_in, ubnd_in, &(boldata[i*nbol]),
		  NULL, spread, params, rebinflags, 0.1, 1000000, 
		  VAL__BADD, 2, ldim, udim, lbnd_in, ubnd_in,
		  map, variance, weights, &nused );

    /* clean up ast objects */
    if ( bolo2map ) bolo2map = astAnnul( bolo2map );
  }

  /* Clean Up */
  if ( sky2map ) sky2map  = astAnnul( sky2map );
  if ( bolo2map ) bolo2map = astAnnul( bolo2map );
  if ( abskyfrm ) abskyfrm = astAnnul( abskyfrm );
  if ( oskyfrm ) oskyfrm = astAnnul( oskyfrm );

  if ( *status != SAI__OK ) {
    errRep( "", "Rebinning step failed", status );
  }
}
