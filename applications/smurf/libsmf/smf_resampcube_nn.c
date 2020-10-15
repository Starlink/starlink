/*
*+
*  Name:
*     smf_resampcube_nn

*  Purpose:
*     Resample a supplied 3D array into a time series cube using custom 2D
*     nearest neighbour code.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_resampcube_nn( smfData *data, dim_t nchan,
*                             dim_t ndet, dim_t nslice, dim_t nxy,
*                             dim_t dim[3], AstMapping *ssmap,
*                             AstSkyFrame *abskyfrm, AstMapping *iskymap,
*                             Grp *detgrp, int moving, float *in_data,
*                             float *out_data, int *overlap, int *status );

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the template smfData structure.
*     nchan = dim_t (Given)
*        Number of spectral channels in template.
*     ndet = dim_t (Given)
*        Number of detectors in template.
*     nslice = dim_t (Given)
*        Number of time slices in template.
*     nxy = dim_t (Given)
*        Number of elements in one spatial plane of the sky cube.
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
*     in_data = float * (Given)
*        The 3D data array for the input sky cube. If a NULL pointer is
*        supplied, then "out_data" is ignored, but the "overlap" value is
*        still returned.
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
*
*     Specialised code is used that only provides Nearest Neighbour
*     spreading when pasting each input pixel value into the output cube.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     25-JAN-2008 (DSB):
*        Initial version.
*     5-MAR-2008 (DSB):
*        Added overlap argument.
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

#define FUNC_NAME "smf_resampcube_nn"

void smf_resampcube_nn( smfData *data, dim_t nchan,
                        dim_t ndet, dim_t nslice, dim_t nxy,
                        dim_t dim[3], AstMapping *ssmap,
                        AstSkyFrame *abskyfrm, AstMapping *iskymap,
                        Grp *detgrp, int moving, float *in_data,
                        float *out_data, int *overlap, int *status ){

/* Local Variables */
   AstMapping *totmap = NULL;  /* WCS->GRID Mapping from template WCS FrameSet */
   const char *name = NULL;    /* Pointer to current detector name */
   dim_t *spectab = NULL;      /* Template->sky cube channel number conversion table */
   dim_t gxsky;                /* Sky cube X grid index */
   dim_t gysky;                /* Sky cube Y grid index */
   dim_t idet;                 /* Detector index */
   dim_t itime;                /* Index of current time slice */
   dim_t iv0;                  /* Offset for pixel in 1st sky cube spectral channel */
   dim_t timeslice_size;       /* No of detector values in one time slice */
   double *detxskycube = NULL; /* Work space for sky cube X grid coords */
   double *detxtemplt = NULL;  /* Work space for template X grid coords */
   double *detyskycube = NULL; /* Work space for sky cube Y grid coords */
   double *detytemplt = NULL;  /* Work space for template Y grid coords */
   float *ddata = NULL;        /* Pointer to start of output detector data */
   float *tdata = NULL;        /* Pointer to start of sky cube time slice data */
   int found;                  /* Was current detector name found in detgrp? */
   smfHead *hdr = NULL;        /* Pointer to data header for this time slice */

/* Initialise */
   *overlap = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Store a pointer to the template NDFs smfHead structure. */
   hdr = data->hdr;

/* Store the number of pixels in one time slice */
   timeslice_size = ndet*nchan;

/* Use the supplied mapping to get the zero-based sky cube channel number
   corresponding to each template channel number. */
   smf_rebincube_spectab( nchan, dim[ 2 ], ssmap, &spectab, status );
   if( !spectab ) goto L999;

/* Allocate work arrays to hold the template and sky cube grid coords for each
   detector. */
   detxtemplt = astMalloc( ndet*sizeof( double ) );
   detytemplt = astMalloc( ndet*sizeof( double ) );
   detxskycube = astMalloc( ndet*sizeof( double ) );
   detyskycube = astMalloc( ndet*sizeof( double ) );

/* Initialise a string to point to the name of the first detector for which
   data is to be created. */
   name = hdr->detname;

/* Fill the arrays with the grid coords of each detector. */
   for( idet = 0; idet < ndet; idet++ ) {
      detxtemplt[ idet ] = (double) idet + 1.0;
      detytemplt[ idet ] = 1.0;

/* If a group of detectors to be used was supplied, search the group for
   the name of the current detector. If not found, set the GRID coord
   bad. */
      if( detgrp ) {
         found = (int) grpIndex( name, detgrp, 1, status );
         if( !found ) {
            detxtemplt[ idet ] = AST__BAD;
            detytemplt[ idet ] = AST__BAD;
         }
      }

/* Move on to the next available detector name. */
      name += strlen( name ) + 1;
   }

/* Loop round all time slices in the template NDF. */
   for( itime = 0; itime < nslice && *status == SAI__OK; itime++ ) {

/* Store a pointer to the first output data value in this time slice. */
      tdata = in_data ? ( out_data + itime*timeslice_size ) : NULL;

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
      if( !totmap ) {
         astEnd;
         break;
      }

/* Use this Mapping to get the sky cube spatial grid coords for each
   template detector. */
      astTran2( totmap, ndet, detxtemplt, detytemplt, 1, detxskycube,
                detyskycube );

/* Loop round each detector, obtaining its output value from the sky cube. */
      for( idet = 0; idet < ndet; idet++ ) {

/* Get a pointer to the start of the output spectrum data. */
         ddata = tdata + idet*nchan;

/* Check the detector has a valid position in sky cube grid coords */
         if( detxskycube[ idet ] != AST__BAD && detyskycube[ idet ] != AST__BAD ){

/* Find the closest sky cube pixel and check it is within the bounds of the
   sky cube. */
            gxsky = floor( detxskycube[ idet ] + 0.5 );
            gysky = floor( detyskycube[ idet ] + 0.5 );
            if( gxsky >= 1 && gxsky <= dim[ 0 ] &&
                gysky >= 1 && gysky <= dim[ 1 ] ) {

/* Get the offset of the sky cube array element that corresponds to this
   pixel in the first spectral channel. */
               iv0 = ( gysky - 1 )*dim[ 0 ] + ( gxsky - 1 );

/* Copy the sky cube spectrum into the output time series cube. */
               *overlap = 1;
               if( in_data ) {
                  smf_resampcube_copy( nchan, spectab, iv0, nxy,
                                       ddata, in_data, status );
               } else {
                  break;
               }
            }
         }
      }

/* End the AST context. */
      astEnd;

/* If no input data was supplied, and we have found at least one input
   spectrum that overlaps the sky cube, we can finish early. */
      if( !in_data && *overlap ) break;
   }

/* Free non-static resources. */
L999:;
   spectab = astFree( spectab );
   detxtemplt = astFree( detxtemplt );
   detytemplt = astFree( detytemplt );
   detxskycube = astFree( detxskycube );
   detyskycube = astFree( detyskycube );

}
