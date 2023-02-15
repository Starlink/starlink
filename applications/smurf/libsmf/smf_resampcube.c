/*
*+
*  Name:
*     smf_resampcube

*  Purpose:
*     Resample a supplied 3D array into a time series cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_resampcube( smfData *data, AstSkyFrame *abskyfrm,
*                     AstMapping *iskymap, AstFrame *ispecfrm,
*                     AstMapping *ispecmap, Grp *detgrp, int moving,
*                     dim_t slbnd[ 3 ], dim_t subnd[ 3 ], int interp,
*                     const double params[], float *in_data,
*                     float *out_data, int *overlap, int *status );

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the smfData structure describing the template time
*        series file.
*     abskyfrm = AstSkyFrame * (Given)
*        A SkyFrame that specifies the coordinate system used to describe
*        the spatial axes of the input sky cube. This should represent
*        absolute sky coordinates rather than offsets even if "moving" is
*        non-zero.
*     iskymap = AstFrameSet * (Given)
*        A Mapping from 2D sky coordinates in the sky cube to 2D spatial
*        grid coordinates in the sky cube.
*     ispecfrm = AstFrame * (Given)
*        Pointer to the SpecFrame within the current Frame of the sky
*        cube's WCS Frameset.
*     ispecmap = AstMapping * (Given)
*        Pointer to the Mapping from the SpecFrame to the third GRID axis
*        within the current Frame of the sky cube's WCS Frameset.
*     detgrp = Grp * (Given)
*        A Group containing the names of the detectors to be used. All
*        detectors will be used if this group is empty.
*     moving = int (Given)
*        A flag indicating if the telescope is tracking a moving object. If
*        so, each time slice is shifted so that the position specified by
*        TCS_AZ_BC1/2 is mapped on to the same pixel position in the
*        sky cube.
*     slbnd = dim_t [ 3 ] (Given)
*        The lower pixel index bounds of the sky cube.
*     subnd = dim_t [ 3 ] (Given)
*        The upper pixel index bounds of the sky cube.
*     interp = int (Given)
*        Specifies the scheme to be used for interpolating the sky cube data
*        array to find each output detector sample value. See docs for
*        astResample (SUN/211) for the allowed values.
*     params = const double[] (Given)
*        An optional pointer to an array of double which should contain any
*        additional parameter values required by the interpolation scheme.
*        See docs for astResample (SUN/211) for further information. If no
*        additional parameters are required, this array is not used and a
*        NULL pointer may be given.
*     in_data = float * (Given)
*        The 3D data array for the input sky cube. If a NULL pointer is
*        supplied, then "out_data" and "interp" are ignored, but the
*        "overlap" value is still returned.
*     out_data = float * (Returned)
*        The 3D data array for the output time series array. Ignored if
*        "in_data" is NULL.
*     overlap = int * (Returned)
*        Returned non-zero if any spectra in the template fall within the
*        bounds of the skycube.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     The data array of the supplied sky cube is resampled at the
*     detector sample positions specified by the input template. The
*     resampled values are stored in the output time series cube.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     25-JAN-2008 (DSB):
*        Initial version.
*     7-JAN-2009 (DSB):
*        Remove unsused parameters index and size.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/ndg.h"
#include "star/atl.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_resampcube"

void smf_resampcube( smfData *data, AstSkyFrame *abskyfrm, AstMapping *iskymap,
                     AstFrame *ispecfrm, AstMapping *ispecmap,
                     Grp *detgrp, int moving, dim_t slbnd[ 3 ],
                     dim_t subnd[ 3 ], int interp, const double params[],
                     float *in_data, float *out_data, int *overlap,
                     int *status ){

/* Local Variables */
   AstCmpMap *ssmap = NULL;    /* Input GRID->output GRID Mapping for spectral axis */
   AstFitsChan *fc = NULL;     /* FitsChan used to get spectral WCS from input */
   AstFrame *specframe = NULL; /* SpecFrame in input WCS */
   AstFrameSet *fs = NULL;     /* WCS FramesSet from input */
   AstMapping *fsmap = NULL;   /* Mapping extracted from FrameSet */
   AstMapping *specmap = NULL; /* GRID->Spectral Mapping for current input file */
   dim_t ndet;                 /* No of detectors in the input */
   dim_t dim[ 3 ];             /* Output array dimensions */
   dim_t nchan;                /* Number of input spectral channels */
   dim_t nel;                  /* No. of pixels in output */
   dim_t nslice;               /* No of time slices in the input */
   dim_t nxy;                  /* No of elements in an output spatial plane */
   int pixax[ 3 ];             /* Pixel axis indices */
   int specax;                 /* The index of the input spectral axis */
   smfHead *hdr = NULL;        /* Pointer to data header for this time slice */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Begin an AST context.*/
   astBegin;

/* Store a pointer to the input templates smfHead structure. */
   hdr = data->hdr;

/* Note the number of channels, detectors and time slices in the time
   series. */
   nchan = (data->dims)[ 0 ];
   ndet = (data->dims)[ 1 ];
   nslice = (data->dims)[ 2 ];

/* Store the total number of detector sample values in the template. */
   nel = nchan*ndet*nslice;

/* Store the dimensions of the sky cube. */
   dim[ 0 ] = subnd[ 0 ] - slbnd[ 0 ] + 1;
   dim[ 1 ] = subnd[ 1 ] - slbnd[ 1 ] + 1;
   dim[ 2 ] = subnd[ 2 ] - slbnd[ 2 ] + 1;

/* Note the size of a spatial plane in the sky cube. */
   nxy = dim[ 0 ]*dim[ 1 ];

/* We want a description of the spectral WCS axis in the template. If the
   template has a WCS FrameSet containing a SpecFrame, use it, otherwise
   we will obtain it from the FITS header later. NOTE, if we knew that all
   the templates would have the same spectral axis calibration, then the
   spectral WCS need only be obtained from the first template. However, in
   the general case, I presume that data files may be combined that use
   different spectral axis calibrations, and so these differences need to
   be taken into account. */
   if( hdr->tswcs ) {
      fs = astClone( hdr->tswcs );

/* The first axis should be a SpecFrame. See if this is so. If not annull
   the specframe pointer. */
      specax = 1;
      specframe = astPickAxes( fs, 1, &specax, NULL );
      if( !astIsASpecFrame( specframe ) ) specframe = astAnnul( specframe );
   }

/* If the above did not yield a SpecFrame, use the FITS-WCS headers in the
   FITS extension of the template NDF. Take a copy of the FITS header (so that
   the contents of the header are not changed), and then read a FrameSet
   out of it. */
   if( !specframe ) {
      fc = astCopy( hdr->fitshdr );
      astClear( fc, "Card" );
      fs = astRead( fc );

/* Extract the SpecFrame that describes the spectral axis from the current
   Frame of this FrameSet. This is assumed to be the third WCS axis (NB
   the different axis number). */
      specax = 3;
      specframe = astPickAxes( fs, 1, &specax, NULL );
   }

/* Split off the 1D Mapping for this single axis from the 3D Mapping for
   the whole WCS. This results in "specmap" holding the Mapping from
   SpecFrame value to GRID value. */
   fsmap = astGetMapping( fs, AST__CURRENT, AST__BASE );
   astMapSplit( fsmap, 1, &specax, pixax, &specmap );

/* Invert the Mapping for the spectral axis so that it goes from template
   GRID coord to spectral coord. */
   astInvert( specmap );

/* Get a Mapping that converts values in the template spectral system
   to the corresponding values in the sky cube spectral system. */
   fs = astConvert( specframe, ispecfrm, "" );

/* Concatenate these Mappings with the supplied spectral Mapping to get
   a Mapping from the template spectral grid axis (pixel axis 1) to the
   sky cube spectral grid axis (pixel axis 3). */
   ssmap = astCmpMap( astCmpMap( specmap, astGetMapping( fs, AST__BASE,
                                                         AST__CURRENT ),
                                 1, " " ),
                      ispecmap, 1, " " );

/* Issue a warning if any of the detector names specified in "detgrp"
   were not found in the data. If the supplied group holds the detectors
   to be excluded, modify it so that it holds the detectors to be
   included. */
   smf_checkdets( detgrp, data, status );

/* If we are using nearest neighbour resampling, we can use specialist
   code that is faster than AST. We also use this code if we are just
   checking if the time series and sky cube have any overlap. */
   if( interp == AST__NEAREST || ! in_data ) {
      smf_resampcube_nn( data, nchan, ndet, nslice, nxy,
                         dim, (AstMapping *) ssmap, abskyfrm, iskymap,
                         detgrp, moving, in_data, out_data, overlap, status );

/* For all other interpolation schemes, we use AST. */
   } else {
      smf_resampcube_ast( data, nchan, ndet, nslice, nel, dim,
                          (AstMapping *) ssmap, abskyfrm, iskymap,
                          detgrp, moving, interp, params, in_data, out_data,
                          status );
   }

/* End the AST context. This will annul all the AST objects created
   within the context. */
   astEnd;
}
