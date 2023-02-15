/*
*+
*  Name:
*     smf_mapbounds_approx

*  Purpose:
*     Make an approximate calculation of the likely pixel bounds which
*     include all the data for a map given a projection

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_mapbounds_approx( Grp *igrp, int index, char *system, double pixsize,
*                           dim_t *lbnd_out, dim_t *ubnd_out, AstFrameSet **outframeset,
*                           int *moving, int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of timestream NDF data files to retrieve pointing
*     index = int (Given)
*        Index of the file to use for determining the map extent,
*        usually 1 to use the first file in the Grp
*     system = char* (Given)
*        String indicating the type of projection (e.g. "icrs")
*     lbnd_out = dim_t * (Returned)
*        2-element array pixel coord. for the lower bounds of the output map
*     ubnd_out = dim_t (Returned)
*        2-element array pixel coord. for the upper bounds of the output map
*     outframeset = AstFrameSet** (Returned)
*        Frameset containing the sky->output map mapping
*     moving = int* (Returned)
*        Flag to denote a moving source
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function determines the approximate map bounds based on the
*     FITS keywords MAP_WDTH, MAP_HGHT, MAP_X, MAP_Y AND MAP_PA in the
*     given file. It creates the WCS frameset (allowing for moving
*     objects if necessary), setting the tangent point to the RA, Dec
*     centre.
*
*     If the MAP_WDTH and MAP_HGHT keywords are zero then it is
*     assumed that the output map is the same size as the array
*     footprint (scaled slightly to allow for non-alignment with the
*     output coordinate frame).

* Notes:
*     It is important to note that the map size is defined only by the
*     FITS header entries. An output map that is made from mulitple
*     observations of a moving target may not include all of the data
*     if created in a stationary coordinate frame (e.g. RA/Dec).
*
*     This routine works sufficiently well for roughly square maps,
*     but fails for maps with an aspect ratio more than about 1.5.
*
*     For DREAM/STARE observations, smf_mapbounds should be called with
*     the fast flag enabled.

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-03-21 (AGG):
*        Initial version, based on smf_mapbounds.
*     2007-01-30 (AGG):
*        Add support for moving objects, remove lon_0/lat_0 from API,
*        WCS now constructed in same manner as smf_mapbounds
*     2007-02-21 (AGG):
*        Apply update from smf_mapbounds for object movement to take
*        account of the difference in epoch between the first and last
*        time slices.
*     2007-02-26 (AGG):
*        New calculation of map bounds including when the input map
*        sizes are zero (assumed to be from stare/dream data)
*     2007-07-05 (TIMJ):
*        Protect a strcmp AZEL comparison because astGetC can return
*        NULL pointer.
*     2007-07-06 (AGG):
*        Remove attempts to retrieve non-existent FITS headers
*     2007-10-29 (EC):
*        Modified interface to smf_open_file.
*     2007-12-14 (EC):
*        Call smf_open_file with SMF__NOCREATE_DATA
*     2008-02-29 (AGG):
*        Explicitly set SkyRef position, ensure SkyRefIs and
*        AlignOffset attributes are also set accordingly
*     2008-04-18 (AGG):
*        Set lbnd to 1,1
*     2008-05-01 (AGG):
*        Use FP angle in calculation of map size and centre
*     2008-07-24 (AGG):
*        Use absolute value of sine/cosine in calculating map size
*     2008-07-24 (TIMJ):
*        Use smf_calc_skyframe rather than duplicating code.
*     2008-07-28 (TIMJ):
*        Use smf_get_projpar rather than calculating ourselves.
*     2008-07-29 (TIMJ):
*        Trap undef values for keywords in non-scan mode.
*        Use astIsUndefF macro. Realise that instap is already in smfHead.
*     2008-08-11 (TIMJ):
*        Remove instrument aperture checks. Refuse to reduce non-scan.
*     2008-12-2 (DSB):
*        Use smf_getfitsd instead of smf_fits_getD in order to avoid use
*        of astIsUndef.
*     2009-10-29 (TIMJ):
*        Remove pixsize from API and set a dynamic default in this routine.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Faciltiies Council.
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <stdio.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_mapbounds_approx"

void smf_mapbounds_approx( Grp *igrp,  int index, char *system,
			   dim_t *lbnd_out, dim_t *ubnd_out, AstFrameSet **outframeset,
			   int *moving, int *status ) {

  /* Local variables */
  smfData *data = NULL;        /* pointer to  SCUBA2 data struct */
  int dxpix;                   /* Map X offset in pixels */
  int dypix;                   /* Map Y offset in pixels */
  smfFile *file = NULL;        /* SCUBA2 data file information */
  AstFitsChan *fitschan = NULL;/* Fits channels to construct WCS header */
  AstFrameSet *fs = NULL;      /* A general purpose FrameSet pointer */
  smfHead *hdr = NULL;         /* Pointer to data header this time slice */
  int hghtpix;                 /* RA-Dec map height in pixels */
  int i;                       /* loop counter */
  dim_t k;                     /* Loop counter */
  double maphght = 0.0;        /* Map height in radians */
  double mappa = 0.0;          /* Map position angle in radians */
  double mapwdth = 0.0;        /* Map width in radians */
  double mapx;                 /* Map X offset in radians */
  double mapy;                 /* Map Y offset in radians */
  double par[7];               /* Projection parameters */
  double pixsize = 0.0;        /* Requested pixel size */
  double shift[ 2 ];           /* Shifts from PIXEL to GRID coords */
  AstMapping *sky2map = NULL;  /* Mapping celestial->map coordinates */
  AstSkyFrame *skyframe = NULL;/* Output SkyFrame */
  AstFrame *skyin = NULL;      /* Sky Frame in input FrameSet */
  double skyref[ 2 ];          /* Values for output SkyFrame SkyRef attribute */
  AstFrameSet *swcsin = NULL;  /* FrameSet describing input WCS */
  int temp;                    /* Temporary variable  */
  double wdthbox;              /* Map width in arcsec */
  int wdthpix;                 /* RA-Dec map width in pixels */
  double x_array_corners[4];   /* X-Indices for corner bolos in array */
  double y_array_corners[4];   /* Y-Indices for corner pixels in array */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Begin an AST context to ensure that all AST objects are annuled
     before returning to caller */
  astBegin;

  /* Initialize output frameset pointer to NULL */
  *outframeset = NULL;
  for( i = 0; i < 7; i++ ) par[ i ] = AST__BAD;

  /* Read data from the given input file in the group - note index
     should be 1 as we use the first file in the Grp to define the map
     bounds */
  smf_open_file( NULL, igrp, index, "READ", SMF__NOCREATE_DATA, &data, status );

  /* Simply abort if it is not a scan */
  if (*status == SAI__OK && data->hdr->obsmode != SMF__OBS_SCAN) {
    *status = SAI__ERROR;
    errRep(" ", "Can not call smf_mapbounds_approx with non-scan observation"
           " (possible programming error)", status);
    goto CLEANUP;
  }

  /* Retrieve file name for use feedback */
  file = data->file;
  smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
  if( *status == SAI__OK ) {
    msgOutif(MSG__VERB, " ",
	     "SMF_MAPBOUNDS_APPROX: Processing ^FILE",
	     status);
  } else {
    errRep( "smf_mapbounds_approx", "Couldn't open input file, ^FILE", status );
  }

  /* Check that the data dimensions are 3 (for time ordered data) */
  if( *status == SAI__OK ) {
    if( data->ndims != 3 ) {
      smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
      msgSeti("THEDIMS", data->ndims);
      *status = SAI__ERROR;
      errRep("smf_mapbounds_approx",
	     "^FILE data has ^THEDIMS dimensions, should be 3.",
	     status);
    }
  }

  /* Construct the WCS for the first time slice in this file */
  smf_tslice_ast( data, 1, 1, NO_FTS, status);

  /* Retrieve header for later constructing output WCS */
  if( *status == SAI__OK) {
    hdr = data->hdr;
    swcsin = hdr->wcs;

    /* Calculate default pixel size */
    pixsize = smf_calc_telres( hdr->fitshdr, status );

    /* Get the user defined pixel size - we trust that smf_get_projpar will
       also read PIXSIZE and get the same answer. We pre-fill par[] to allow
       PIXSIZE=! to accept the dynamic default in both places.*/
    parGdr0d( "PIXSIZE", pixsize, 0, 60, 1, &pixsize, status );
    par[4] = pixsize*AST__DD2R/3600.0;
    par[5] = par[4];

    /* Retrieve input SkyFrame */
    skyin = astGetFrame( swcsin, AST__CURRENT );

    /* Retrieve map height and width from header - will be undef for
       non-scan so set up defaults first. */
    mapwdth = 0.0;
    maphght = 0.0;
    smf_getfitsd( hdr, "MAP_WDTH", &mapwdth, status );
    smf_getfitsd( hdr, "MAP_HGHT", &maphght, status );

    /* Make an approximation if map height and width are not set -
       note that this should ONLY apply for non-scan mode data */
    if ( !mapwdth || !maphght ) {
      if (*status == SAI__OK) {
        *status = SAI__ERROR;
        errRep(" ", "MAP_WDTH and MAP_HGHT must be > 0", status);
        goto CLEANUP;
      }
    }

    mapx = 0.0;   /* Used if the FITS keyword values are undefed */
    mapy = 0.0;
    smf_getfitsd( hdr, "MAP_X", &mapx, status );
    smf_getfitsd( hdr, "MAP_Y", &mapy, status );

    /* Convert map Position Angle to radians */
    mappa = 0.0;
    smf_fits_getD( hdr, "MAP_PA", &mappa, status );
    mappa *= AST__DD2R;

    /* Calculate size of output map in pixels */
    /* Note: this works for the simulator... */
    wdthbox = mapwdth*fabs(cos(mappa)) + maphght*fabs(sin(mappa));
    wdthpix = (int) ( wdthbox / pixsize);
    hghtpix = (int) ( wdthbox / pixsize);
    dxpix = (int) (mapx / pixsize);
    dypix = (int) (mapy / pixsize);

    /* Get the offsets for each corner of the array */
    temp = (wdthpix - 1) / 2;
    x_array_corners[0] = dxpix - temp;
    x_array_corners[1] = dxpix - temp;
    x_array_corners[2] = dxpix + temp;
    x_array_corners[3] = dxpix + temp;

    temp = (hghtpix - 1) / 2;
    y_array_corners[0] = dypix - temp;
    y_array_corners[1] = dypix + temp;
    y_array_corners[2] = dypix - temp;
    y_array_corners[3] = dypix + temp;

    lbnd_out[0] = x_array_corners[0];
    ubnd_out[0] = x_array_corners[0];
    lbnd_out[1] = y_array_corners[0];
    ubnd_out[1] = y_array_corners[0];

    /* Update min/max  */
    for( k=0; k<4; k++ ) {
      if( x_array_corners[k] < lbnd_out[0] ) lbnd_out[0] = x_array_corners[k];
      if( y_array_corners[k] < lbnd_out[1] ) lbnd_out[1] = y_array_corners[k];
      if( x_array_corners[k] > ubnd_out[0] ) ubnd_out[0] = x_array_corners[k];
      if( y_array_corners[k] > ubnd_out[1] ) ubnd_out[1] = y_array_corners[k];
    }

  } else {
    goto CLEANUP;
  }

  /* Now create the output FrameSet. */
  smf_calc_skyframe( skyin, system, hdr, 0, &skyframe, skyref, moving,
                     status );

  /* Get the orientation of the map vertical within the output celestial
     coordinate system. This is derived form the MAP_PA FITS header, which
     gives the orientation of the map vertical within the tracking system. */
  mappa = smf_calc_mappa( hdr, system, skyin, status );

  /* Calculate the projection parameters. We do not enable autogrid determination
     for SCUBA-2 so we do not need to obtain all the data before calculating
     projection parameters. */
  smf_get_projpar( skyframe, skyref, *moving, 0, 0, NULL, 0,
                   mappa, par, NULL, NULL, status );



  /* Now populate a FitsChan with FITS-WCS headers describing the
     required tan plane projection. The longitude and latitude axis
     types are set to either (RA,Dec) or (AZ,EL) to get the correct
     handedness. */
  fitschan = astFitsChan ( NULL, NULL, " " );
  smf_makefitschan( astGetC( skyframe, "System"), &(par[0]),
                    &(par[2]), &(par[4]), par[6], fitschan, status );
  astClear( fitschan, "Card" );
  fs = astRead( fitschan );

  /* Extract the output PIXEL->SKY Mapping - note this is will be
     inverted later to create the sk2map mapping */
  sky2map = astGetMapping( fs, AST__BASE, AST__CURRENT );

  /* Create the output FrameSet */
  *outframeset = astFrameSet( astFrame(2, "Domain=GRID"), " " );

  /* Now add the SkyFrame to it */
  astAddFrame( *outframeset, AST__BASE, sky2map, skyframe );

  /* Apply a ShiftMap to the output FrameSet to re-align the GRID
     coordinates */
  shift[0] = -lbnd_out[0];
  shift[1] = -lbnd_out[1];
  astRemapFrame( *outframeset, AST__BASE, astShiftMap( 2, shift, " " ) );

  astExport( *outframeset );

/* Report the pixel bounds of the cube. */
   if( *status == SAI__OK ) {
      msgOutif( MSG__NORM, " ", " ", status );
      msgSetk( "XL", lbnd_out[ 0 ] );
      msgSetk( "YL", lbnd_out[ 1 ] );
      msgSetk( "XU", ubnd_out[ 0 ] );
      msgSetk( "YU", ubnd_out[ 1 ] );
      msgOutif( MSG__NORM, " ", "   Output map pixel bounds: ( ^XL:^XU, ^YL:^YU )",
                status );
   }


  /* Change the pixel bounds to be consistent with the new CRPIX */
  ubnd_out[0] -= lbnd_out[0]-1;
  lbnd_out[0] = 1;

  ubnd_out[1] -= lbnd_out[1]-1;
  lbnd_out[1] = 1;

  /* Clean Up */
 CLEANUP:
  if (*status != SAI__OK) {
    errRep(FUNC_NAME, "Unable to determine map bounds", status);
  }

  if( data != NULL )
    smf_close_file( NULL, &data, status);

  astEnd;

}
