/*
*+
*  Name:
*     smf_resampcube_ast

*  Purpose:
*     Resample a supplied 3D array into a time series cube using AST.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_resampcube_ast( smfData *data, dim_t nchan,
*                              dim_t ndet, dim_t nslice, dim_t nel,
*                              dim_t dim[3], AstMapping *ssmap,
*                              AstSkyFrame *abskyfrm, AstMapping *iskymap,
*                              Grp *detgrp, int moving, int interp,
*                              const double params[], float *in_data,
*                              float *out_data, int *status );

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the template smfData structure.
*     nchan = dim_t (Given)
*        Number of spectral channels in template.
*     ndet = dim_t (Given)
*        Number of detectors in template.
*     nslice = dim_t (Given)
*        Number of time slices in template.
*     nel = dim_t (Given)
*        Total number of elements in template.
*     nxy = dim_t (Given)
*        Number of elements in one spatial plane of the sky cube.
*     nsky = dim_t (Given)
*        Total number of elements in the sky cube.
*     dim[ 3 ] = dim_t (Given)
*        The dimensions of the sky cube.
*     ssmap = AstMapping * (Given)
*        A Mapping that goes from template spectral grid axis (pixel axis 1)
*        to the sky cube spectral grid axis (pixel axis 3).
*     abskyfrm = AstSkyFrame * (Given)
*        A SkyFrame that specifies the coordinate system used to describe
*        the spatial axes of the sky cube. This should represent
*        absolute sky coordinates rather than offsets even if "moving" is
*        non-zero.
*     iskymap = AstFrameSet * (Given)
*        A Mapping from 2D sky coordinates in the sky cube to 2D
*        spatial grid coordinates in the template.
*     detgrp = Grp * (Given)
*        A Group containing the names of the detectors to be used. All
*        detectors will be used if this group is empty.
*     moving = int (Given)
*        A flag indicating if the telescope is tracking a moving object. If
*        so, each time slice is shifted so that the position specified by
*        TCS_AZ_BC1/2 is mapped on to the same pixel position in the
*        sky cube.
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
*        The 3D data array for the input sky cube.
*     out_data = float * (Returned)
*        The 3D data array for the output time series array.
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

#define FUNC_NAME "smf_resampcube_ast"

void smf_resampcube_ast( smfData *data, dim_t nchan,
                         dim_t ndet, dim_t nslice, dim_t nel,
                         dim_t dim[3], AstMapping *ssmap,
                         AstSkyFrame *abskyfrm, AstMapping *iskymap,
                         Grp *detgrp, int moving, int interp,
                         const double params[], float *in_data,
                         float *out_data, int *status ){

/* Local Variables */
   AstCmpMap *detmap = NULL;   /* Mapping from 1D det. index to 2D "grid" coords */
   AstMapping *dtotmap = NULL; /* 1D det index-> GRID Mapping */
   AstMapping *fullmap = NULL; /* Mapping between in and out GRID coords */
   AstMapping *lutmap = NULL;  /* Mapping that identifies detectors to be used */
   AstMapping *splut = NULL;   /* Spatial LutMap */
   AstMapping *sslut = NULL;   /* Spectral LutMap */
   AstMapping *totmap = NULL;  /* Mapping between in and out spatial GRID coords */
   AstPermMap *pmap;           /* Mapping to rearrange sky cube axes */
   const char *name = NULL;    /* Pointer to current detector name */
   dim_t idet;                 /* Detector index */
   dim_t itime;                /* Index of current time slice */
   dim_t lbnd_out[ 2 ];        /* Lower bounds on receptor axis */
   dim_t ldim[ 3 ];            /* Sky cube array lower GRID bounds */
   dim_t timeslice_size;       /* Number of elements in a time slice */
   dim_t ubnd_out[ 2 ];        /* Upper time series bounds on receptor axis */
   dim_t udim[ 3 ];            /* Sky cube array upper GRID bounds */
   double *detlut = NULL;      /* Work space for detector mask */
   double con;                 /* Constant value */
   float *tdata = NULL;        /* Pointer to start of output time slice data */
   int ast_flags;              /* Basic flags to use with astResample */
   int found;                  /* Was current detector name found in detgrp? */
   int skyperm[ 3 ];           /* Sky cube axis permutation array */
   int tsperm[ 3 ];            /* Time series axis permutation array */
   smfHead *hdr = NULL;        /* Pointer to data header for this time slice */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Store a pointer to the template NDFs smfHead structure. */
   hdr = data->hdr;

/* Fill an array with the lower grid index bounds of the sky cube. */
   ldim[ 0 ] = 1;
   ldim[ 1 ] = 1;
   ldim[ 2 ] = 1;

/* Integer upper grid index bounds of the sky cube. */
   udim[ 0 ] = dim[ 0 ];
   udim[ 1 ] = dim[ 1 ];
   udim[ 2 ] = dim[ 2 ];

/* Store the size of an template time slice. */
   timeslice_size = nel/nslice;

/* Create a LutMap that holds the sky cube spectral axis GRID value at
   the centre of each template spectral axis pixel. LutMaps are faster to
   evaluate, and so astResample will go faster. We can use LutMaps without
   loosing accuracy since astResample only ever transforms the GRID
   values at input pixel centres (i.e. integer GRID values), and so the
   LutMap will always return a tabulated value rather than an
   interpolated value. */
   atlTolut( (AstMapping *) ssmap, 1.0, (double) nchan, 1.0, "LutInterp=1",
              &sslut, status );

/* Set the flags for astResample. This function will be invoked once for
   each sky cube. We therefore include the AST__NOBAD flag. This means
   that elements in the output time series array that receive no
   contribution from the current sky cube are left unchanged, rather than
   being set bad. If this were not done, the output time series array
   would be determiend solely by the final sky cube, with areas set bad
   that recieve no contribution from the final sky cube. */
   ast_flags = AST__USEBAD | AST__NOBAD;

/* If we are dealing with more than 1 detector, create a LutMap that holds
   the template GRID index of every detector to be included in the output, and
   AST__BAD for every detector that is not to be included in the output cube.
   First allocate the work space for the LUT. */
   if( ndet > 1 ) {
      detlut = astMalloc( ndet*sizeof( double ) );

/* Initialise a string to point to the name of the first detector for which
   data is available */
      name = hdr->detname;

/* Loop round all detectors for which data is available. */
      for( idet = 0; idet < ndet; idet++ ) {

/* Store the template GRID coord of this detector. GRID coords start at 1,
   not 0. */
         detlut[ idet ] = idet + 1.0;

/* If a group of detectors to be used was supplied, search the group for
   the name of the current detector. If not found, set the GRID coord bad.
   This will cause astResample to ignore data from the detector. */
         if( detgrp ) {
            found = (int) grpIndex( name, detgrp, 1, status );
            if( !found ) detlut[ idet ] = AST__BAD;
         }

/* Move on to the next available detector name. */
         name += strlen( name ) + 1;
      }

/* Create the LutMap. */
      lutmap = (AstMapping *) astLutMap( (int) ndet, detlut, 1.0, 1.0,
                                         "LutInterp=1" );

/* If we only have 1 detector, use a UnitMap instead of a LutMap (lutMaps
   must have 2 or more table entries). */
   } else {
      lutmap = (AstMapping *) astUnitMap( 1, " " );
   }

/* Combine the above LutMap with a 1-input, 2-output PermMap that copies its
   input to create its first output, and assigns a constant value of 1.0 to
   its second output. We need to do this because smf_tslice returns a 2D
   GRID system (even though the second GRID axis is not actually used). */
   tsperm[ 0 ] = 1;
   skyperm[ 0 ] = 1;
   skyperm[ 1 ] = -1;
   con = 1.0;
   detmap = astCmpMap( lutmap, astPermMap( 1, tsperm, 2, skyperm, &con, " " ),
                       1, " " );

/* Store the bounds of a single time slice grid. */
   lbnd_out[ 0 ] = 1;
   ubnd_out[ 0 ] = nchan;
   lbnd_out[ 1 ] = 1;
   ubnd_out[ 1 ] = ndet;

/* Create a PermMap that can be used to re-order the output axes so that
   channel number is axis 3. */
   skyperm[ 0 ] = 2;
   skyperm[ 1 ] = 3;
   skyperm[ 2 ] = 1;
   tsperm[ 0 ] = 3;
   tsperm[ 1 ] = 1;
   tsperm[ 2 ] = 2;
   pmap = astPermMap( 3, tsperm, 3, skyperm, NULL, " " );

/* Loop round all time slices in the input NDF. */
   for( itime = 0; itime < nslice && *status == SAI__OK; itime++ ) {

/* Store a pointer to the first output data value in this time slice. */
      tdata = out_data + itime*timeslice_size;

/* Begin an AST context. Having this context within the time slice loop
   helps keep the number of AST objects in use to a minimum. */
      astBegin;

/* Get a Mapping from the spatial GRID axes in the template to the spatial
   GRID axes in the sky cube for the current time slice. Note this has
   to be done first since it stores details of the current time slice
   in the "smfHead" structure inside "data", and this is needed by
   subsequent functions. */
      totmap = smf_rebin_totmap( data, itime, abskyfrm, iskymap, moving,
				 NO_FTS, status );
      if( !totmap ) break;

/* So "totmap" is a 2-input, 2-output Mapping that transforms the template
   spatial GRID coords into sky cube spatial GRID coords. In order to speed
   up astResample we represent this by a pair of parallel LutMaps. To do
   this (using atlTolut) we need a Mapping which only has 1 input, so we
   preceed "totmap" with "detmap" (which also has the effect of exluding
   data from unrequired detectors). We then combine this Mapping in
   parallel with the spectral LutMap to get a 2-input (channel number,
   detector index) and 3-output (output grid coords) Mapping. We finally
   add a PermMap to re-arrange the output axes so that channel number is
   axis 3 in the output. */
      dtotmap = (AstMapping *) astCmpMap( detmap, totmap, 1, " " );
      if( ndet > 1 ) {
         atlTolut( dtotmap, 1.0, (double) ndet, 1.0, "LutInterp=1", &splut,
                   status );
      } else {
         splut = astClone( dtotmap );
      }

      fullmap = astSimplify( astCmpMap( astCmpMap( sslut, splut, 0, " " ),
                                        pmap, 1, " " ) );

/* Invert this Mapping to get the mapping from sky cube grid coords to time
   series grid coords. */
      astInvert( fullmap );

/* Resample the sky cube to get data for this time slice. */
      astResample8F( fullmap, 3, ldim, udim, in_data, NULL, interp, NULL,
                    params, ast_flags, 0.0, 50, VAL__BADR, 2, lbnd_out,
                    ubnd_out, lbnd_out, ubnd_out, tdata, NULL );

/* End the AST context. */
      astEnd;
   }

/* Free resources. */
   detlut = astFree( detlut );
}

