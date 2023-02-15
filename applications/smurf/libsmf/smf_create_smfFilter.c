/*
*+
*  Name:
*     smf_create_smfFilter

*  Purpose:
*     Allocate an empty smfFilter structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     pntr = smf_create_smfFilter( smfData *template, int *status );

*  Arguments:
*     template = smfData * (Given)
*        Pointer to smfData to obtain dim and df
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     Pointer to newly created smfFilter. NULL on error.

*  Description: This function allocates memory for a smfFilter
*     structure. The length and frequency step size are determined,
*     but the buffer itself is not initialized. Depending on the type
*     of filter the buffer may be either real- or complex-valued.
*     Here it is initialized to NULL - it will get allocated by
*     smf_filter_* routines.

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-06-05 (EC):
*        Initial version
*     2008-06-11 (EC):
*        Enable the use of 1-D templates
*     2008-06-12 (EC):
*        -Switch to split real/imaginary arrays for smfFilter
*     2008-06-18 (EC):
*        Fixed error in calculation of df (frequency steps)
*     2008-06-23 (EC):
*        Generate WCS that can be used when writing filter to an NDF
*     2008-07-29 (TIMJ):
*        Steptime is now in smfHead.
*     2011-10-03 (EC):
*        Support 2-d map filters
*     2012-11-20 (EC):
*        Add dateobs to smfFilter
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2012 Science and Technology Facilities Council.
*     Copyright (C) 2008,2011 University of British Columbia.
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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_create_smfFilter"

smfFilter *smf_create_smfFilter( smfData *template, int *status ) {

  dim_t fdims[2];               /* Frequency space dimensions */
  AstCmpMap *fftmapping=NULL;   /* Mapping from GRID to curframe2d */
  smfFilter *filt=NULL;         /* Pointer to returned struct */
  int i;                        /* Loop counter */
  int ndims;                    /* Number of dimensions */
  dim_t rdims[2];               /* Real space dimensions */

  if( *status != SAI__OK ) return NULL;

  if( !template ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL template supplied", status );
    return NULL;
  }

  if( !template->hdr ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME
            ": no hdr in template, can't calculate frequency steps", status );
    return NULL;
  }

  filt = astCalloc( 1, sizeof(*filt) );

  (void) smf_isfft( template, rdims, NULL, fdims, filt->df, &ndims, status );

  if( *status == SAI__OK ) {

    filt->dateobs = VAL__BADD;
    filt->imag = NULL;
    filt->real = NULL;
    filt->wcs = NULL;

    filt->ndims = ndims;
    for( i=0; i<ndims; i++ ) {
      filt->fdims[i] = fdims[i];
      filt->rdims[i] = rdims[i];
    }

    if( ndims == 1 ) {
      /* --- Filter for time-series data  --- */

      AstFrame *cframe=NULL;        /* Current real/imag frame */
      AstUnitMap *cmapping=NULL;    /* Mapping from grid to cframe */
      AstCmpFrame *curframe2d=NULL; /* Current Frame for 2-d FFT */
      AstZoomMap *scalemapping=NULL;/* Scale grid coordinates by df */
      AstSpecFrame *specframe=NULL; /* Current Frame of 1-D spectrum */
      AstCmpMap *specmapping=NULL;  /* Mapping from GRID to FREQ */
      double zshift;                /* Amount by which to shift origin */
      AstShiftMap *zshiftmapping=NULL;  /* Map to shift origin of GRID */

      if( *status == SAI__OK ) {

        /* Start an AST context */
        astBegin;

        /* Create a new FrameSet containing a 2d base GRID frame */

        filt->wcs = astFrameSet( astFrame( 2, "Domain=GRID" ), " " );

        /* The current frame will have freq. along first axis, and
           real/imag coefficients along the other */

        specframe = astSpecFrame( "System=freq,Unit=Hz,StdOfRest=Topocentric" );
        cframe = astFrame( 1, "Domain=GRID" );
        curframe2d = astCmpFrame( specframe, cframe, " " );

        /* The mapping from 2d grid coordinates to (frequency, coeff) is
           accomplished with a shift and a zoommap for the 1st dimension,
           and a unit mapping for the other */

        zshift = -1;
        zshiftmapping = astShiftMap( 1, &zshift, " " );
        scalemapping = astZoomMap( 1, filt->df[0], " " );
        specmapping = astCmpMap( zshiftmapping, scalemapping, 1, " " );

        cmapping = astUnitMap( 1, " " );

        fftmapping = astCmpMap( specmapping, cmapping, 0, " " );

        /* Add the curframe2d with the fftmapping to the frameset */
        astAddFrame( filt->wcs, AST__BASE, fftmapping, curframe2d );

        /* Export the frameset before ending the AST context */
        astExport( filt->wcs );
        astEnd;

        /* We also get the MJD start time of the observation for time-series
           data in case we need it later (e.g. for setting parameters of
           the MCE filter in smf_filter_mce */

        smf_find_dateobs( template->hdr, &filt->dateobs, NULL, status );

      } else if( ndims == 2 ) {
        /* --- Filter for map data  --- */

        AstCmpFrame *curframe=NULL;
        AstFrame *curframe_c=NULL;
        AstFrame *curframe_x=NULL;
        AstFrame *curframe_y=NULL;
        AstUnitMap *scalemap_c=NULL;
        AstZoomMap *scalemap_x=NULL;
        AstZoomMap *scalemap_y=NULL;

        /* Start an AST context */
        astBegin;

        /* Create a new frameset containing a 3d base GRID frame */
        filt->wcs = astFrameSet( astFrame( 3, "Domain=GRID" ), " " );

        /* The current frame will consist of two spatial frequencies,
           and a fourier component */
        curframe_x =  astFrame( 1,"label=Spatial Frequency,Unit=1/arcsec");
        curframe_y =  astFrame( 1,"label=Spatial Frequency,Unit=1/arcsec");
        curframe_c =  astFrame( 1,"Domain=COEFF,label=Real/Imag component");

        curframe = astCmpFrame( astCmpFrame( curframe_x, curframe_y, " " ),
                                curframe_c, " " );

        /* The mapping will scale by the spatial frequency step sizes
           for the first two axes, and we just use a unit mapping for
           the component axis */

        scalemap_x = astZoomMap( 1, filt->df[0], " " );
        scalemap_y = astZoomMap( 1, filt->df[1], " " );
        scalemap_c = astUnitMap( 1, " " );

        fftmapping = astCmpMap( astCmpMap( scalemap_x, scalemap_y, 0, " " ),
                                scalemap_c, 0, " " );

        /* Add curframe with the fftmapping to the frameset */
        astAddFrame( filt->wcs, AST__BASE, fftmapping, curframe );

        /* Export and store it in the place of TSWCS in the header. We
           do this so that we can preserve the 2D map WCS, and since
           it is 3-d smf_write_smfData will use it automagically */
        astExport( filt->wcs );
        astEnd;


      } else {
        *status = SAI__ERROR;
        errRepf( "", FUNC_NAME
                 ": don't know how to handle template with %d real-space dims",
                 status, ndims );
      }
    }
  }

  /* If we have bad status free filt */
  if( (*status != SAI__OK) && filt ) {
    filt = astFree( filt );
  }

  return filt;
}
