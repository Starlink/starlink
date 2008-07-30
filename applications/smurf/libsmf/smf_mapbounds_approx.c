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
*                           int *lbnd_out, int *ubnd_out, AstFrameSet **outframeset,
*                           int *moving, int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of timestream NDF data files to retrieve pointing
*     index = size_t (Given)
*        Index of the file to use for determining the map extent,
*        usually 1 to use the first file in the Grp
*     system = char* (Given)
*        String indicating the type of projection (e.g. "icrs")
*     pixsize = double (Given)
*        Linear size of a map pixel (arcsec)
*     lbnd_out = double* (Returned)
*        2-element array pixel coord. for the lower bounds of the output map 
*     ubnd_out = double* (Returned)
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
*
*     For cases where the instrument aperture is non-zero, the pixel
*     bounds are shifted by an amount calculated from the INSTAP_X/_Y
*     FITS headers and knowledge of the angle between the focal plane
*     and tracking coordinate systems (TCS_TR_ANG).

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

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
#include "star/slalib.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_mapbounds_approx"

void smf_mapbounds_approx( Grp *igrp,  size_t index, char *system, double pixsize, 
			   int *lbnd_out, int *ubnd_out, AstFrameSet **outframeset,
			   int *moving, int *status ) {

  /* Local variables */
  double bolospacing = 6.28;   /* Bolometer spacing in arcsec */
  double c;                    /* cos theta */
  smfData *data = NULL;        /* pointer to  SCUBA2 data struct */
  int dxpix;                   /* Map X offset in pixels */
  int dypix;                   /* Map Y offset in pixels */
  smfFile *file = NULL;        /* SCUBA2 data file information */
  AstFitsChan *fitschan = NULL;/* Fits channels to construct WCS header */
  AstFrameSet *fs = NULL;      /* A general purpose FrameSet pointer */
  smfHead *hdr = NULL;         /* Pointer to data header this time slice */
  double hghtbox;              /* Map height in arcsec */
  int hghtpix;                 /* RA-Dec map height in pixels */
  int i;                       /* loop counter */
  int instap = 0;              /* Flag to denote whether the
				  instrument aperture is non-zero */
  double instapx = 0.0;        /* Effective X offset in tracking frame (arcsec) */
  double instapy = 0.0;        /* Effective Y offset in tracking frame (arcsec) */
  dim_t k;                     /* Loop counter */
  double maphght = 0.0;        /* Map height in radians */
  double mappa = 0.0;          /* Map position angle in radians */
  double mapwdth = 0.0;        /* Map width in radians */
  double mapx;                 /* Map X offset in radians */
  double mapy;                 /* Map Y offset in radians */
  double origval = 0.0;        /* A temporary double variable */
  double par[7];               /* Projection parameters */
  char *pname = NULL;          /* Name of currently opened data file */
  double s;                    /* sin theta */
  double shift[ 2 ];           /* Shifts from PIXEL to GRID coords */
  AstMapping *sky2map = NULL;  /* Mapping celestial->map coordinates */
  AstSkyFrame *skyframe = NULL;/* Output SkyFrame */
  AstFrame *skyin = NULL;      /* Sky Frame in input FrameSet */
  double skyref[ 2 ];          /* Values for output SkyFrame SkyRef attribute */
  AstFrameSet *swcsin = NULL;  /* FrameSet describing input WCS */
  int temp;                    /* Temporary variable  */
  double theta = 0.0;          /* Angle between FP up and tracking up */
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
  smf_open_file( igrp, index, "READ", SMF__NOCREATE_DATA, &data, status );

  /* Retrieve file name for use feedback */
  file = data->file;
  pname =  file->name;
  if( *status == SAI__OK ) {
    msgSetc("FILE", pname);
    msgOutif(MSG__VERB, " ", 
	     "SMF_MAPBOUNDS_APPROX: Processing ^FILE",
	     status);
  } else {
    msgSetc("FILE", pname);
    errRep( "smf_mapbounds_approx", "Couldn't open input file, ^FILE", status );
  }

  /* Check that the data dimensions are 3 (for time ordered data) */
  if( *status == SAI__OK ) {
    if( data->ndims != 3 ) {
      msgSetc("FILE", pname);
      msgSeti("THEDIMS", data->ndims);
      *status = SAI__ERROR;
      errRep("smf_mapbounds_approx", 
	     "^FILE data has ^THEDIMS dimensions, should be 3.", 
	     status);
    }
  }

  /* Construct the WCS for the first time slice in this file */
  smf_tslice_ast( data, 1, 1, status);
    
  /* Retrieve header for later constructing output WCS */
  if( *status == SAI__OK) {
    hdr = data->hdr;
    swcsin = hdr->wcs;
    /* Retrieve input SkyFrame */
    skyin = astGetFrame( swcsin, AST__CURRENT );

    /* Retrieve map height and width from header - will be undef for non-scan */
    smf_fits_getD( hdr, "MAP_WDTH", &mapwdth, status );
    smf_fits_getD( hdr, "MAP_HGHT", &maphght, status );

    if (astIsUndefF(mapwdth)) mapwdth = 0.0;
    if (astIsUndefF(maphght)) maphght = 0.0;

    /* Retrieve the angle between the focal plane and the tracking
       coordinate system */
    theta = hdr->state->tcs_tr_ang;

    /* Make an approximation if map height and width are not set -
       note that this should ONLY apply for non-scan mode data */
    if ( !mapwdth && !maphght ) {
      /* 84 comes from 2 x 40 detectors + 4 inter-sub-array gap */
      mapwdth = sqrt(2.0) * 84 * bolospacing * cos( (AST__DPIBY2/2.0) - theta);
      maphght = mapwdth;
    }    
    smf_fits_getD( hdr, "MAP_X", &mapx, status );
    smf_fits_getD( hdr, "MAP_Y", &mapy, status );
    /* Undefs are a problem for non-scan maps if we have got this far */
    if (astIsUndefF(mapx)) mapx = 0.0;
    if (astIsUndefF(mapy)) mapy = 0.0;

    /* If the instrument aperture is set, calculate the projected
       values in the tracking coordinate frame */
    c = fabs(cos(theta));
    s = fabs(sin(theta));
    instapx = hdr->instap[0] * DR2AS;
    instapy = hdr->instap[1] * DR2AS;
    if ( instapx || instapy ) {
      instap = 1;
      origval = instapx;
      instapx = instapx*c - instapy*s;
      instapy = origval*s + instapy*c;
    }

    /* Convert map Position Angle to radians */
    smf_fits_getD( hdr, "MAP_PA", &mappa, status );
    if (astIsUndefF(mappa)) mappa = 0.0;
    mappa *= AST__DD2R;

    /* Calculate size of output map in pixels */
    /* Note: this works for the simulator... */
    wdthbox = mapwdth*fabs(cos(mappa)) + maphght*fabs(sin(mappa));
    hghtbox = maphght*fabs(cos(mappa)) + mapwdth*fabs(sin(mappa));
    wdthpix = (int) ( ( wdthbox*s + hghtbox*c ) / pixsize);
    hghtpix = (int) ( ( wdthbox*c + hghtbox*s ) / pixsize);
    origval = mapx;
    mapx = mapx*s + mapy*c;
    mapy = mapy*s - origval*c;
    dxpix = (int) ((mapx + instapx) / pixsize);
    dypix = (int) ((mapy + instapy) / pixsize);

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
  smf_calc_skyframe( skyin, system, hdr, 0, &skyframe, skyref, moving, status );

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
  fitschan = astFitsChan ( NULL, NULL, "" );
  smf_makefitschan( astGetC( skyframe, "System"), &(par[0]),
                    &(par[2]), &(par[4]), par[6], fitschan, status );
  astClear( fitschan, "Card" );
  fs = astRead( fitschan );

  /* Extract the output PIXEL->SKY Mapping - note this is will be
     inverted later to create the sk2map mapping */
  sky2map = astGetMapping( fs, AST__BASE, AST__CURRENT );

  /* Create the output FrameSet */
  *outframeset = astFrameSet( astFrame(2, "Domain=GRID"), "");

  /* Now add the SkyFrame to it */
  astAddFrame( *outframeset, AST__BASE, sky2map, skyframe );
  
  /* Apply a ShiftMap to the output FrameSet to re-align the GRID
     coordinates */
  shift[0] = -lbnd_out[0];
  shift[1] = -lbnd_out[1];
  astRemapFrame( *outframeset, AST__BASE, astShiftMap( 2, shift, "") );
 
  astExport( *outframeset );

/* Report the pixel bounds of the cube. */
   if( *status == SAI__OK ) {
      msgOutif( MSG__NORM, " ", " ", status );
      msgSeti( "XL", lbnd_out[ 0 ] );
      msgSeti( "YL", lbnd_out[ 1 ] );
      msgSeti( "XU", ubnd_out[ 0 ] );
      msgSeti( "YU", ubnd_out[ 1 ] );
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
    smf_close_file( &data, status);

  astEnd;

}
