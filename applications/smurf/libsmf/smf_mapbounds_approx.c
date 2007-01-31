/*
*+
*  Name:
*     smf_mapbounds_approx

*  Purpose:
*     Automatically calculate the pixel bounds for a map given a projection

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_mapbounds_approx( Grp *igrp,  int size, char *system, double pixsize, 
*                           int *lbnd_out, int *ubnd_out, AstFrameSet **outframeset,
*                           int *moving, int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of timestream NDF data files to retrieve pointing
*     size = int (Given)
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

* Notes:
*     Currently the simulator does not write MAP_PA: therefore this
*     routine ignores the map PA and assumes that the map is oriented
*     with the Y-axis parallel to the elevation axis.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-03-21 (AGG):
*        Initial version, based on smf_mapbounds.
*     2007-01-30 (AGG):
*        Add support for moving objects, remove lon_0/lat_0 from API,
*        WCS now constructed in same manner as smf_mapbounds
*     {enter_further_changes_here}

*  Notes:

*  Copyright:
*     Copyright (C) 2006-2007 Particle Physics and Astronomy Research
*     Council and University of British Columbia. All Rights Reserved.

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

void smf_mapbounds_approx( Grp *igrp,  int index, char *system, double pixsize, 
			   int *lbnd_out, int *ubnd_out, AstFrameSet **outframeset,
			   int *moving, int *status ) {

  /* Local variables */
  double az[ 2 ];              /* Azimuth values */
  AstMapping *azel2usesys = NULL; /* Mapping form AZEL to requested system */
  smfData *data = NULL;        /* pointer to  SCUBA2 data struct */
  double dec[ 2 ];             /* Dec values */
  double el[ 2 ];              /* Elevation values */
  int dxpix;                   /* Map X offset in pixels */
  int dypix;                   /* Map Y offset in pixels */
  smfFile *file = NULL;        /* SCUBA2 data file information */
  AstFitsChan *fitschan = NULL;/* Fits channels to construct WCS header */
  AstFrameSet *fs = NULL;      /* A general purpose FrameSet pointer */
  smfHead *hdr = NULL;         /* Pointer to data header this time slice */
  int hghtpix;                 /* Map height in pixels */
  dim_t k;                     /* Loop counter */
  double lon_0;                /* Longitude of output map reference point */
  double lat_0;                /* Latitude of output map reference point */
  double maphght;              /* Map height in radians */
  double mappa = 0;            /* Map position angle in radians */
  double mapwdth;              /* Map width in radians */
  double mapx;                 /* Map X offset in radians */
  double mapy;                 /* Map Y offset in radians */
  char *pname = NULL;          /* Name of currently opened data file */
  double ra[ 2 ];              /* RA values */
  double sep;                  /* Separation between first and last BASE positions */
  AstFrame *sf1 = NULL;        /* Frame representing AZEL system */
  AstFrame *sf2 = NULL;        /* Frame representing requested system */
  double shift[ 2 ];           /* Shifts from PIXEL to GRID coords */
  AstMapping *sky2map = NULL;  /* Mapping celestial->map coordinates */
  AstSkyFrame *skyframe = NULL;/* Output SkyFrame */
  AstFrame *skyin = NULL;      /* Sky Frame in input FrameSet */
  double skyref[ 2 ];          /* Values for output SkyFrame SkyRef attribute */
  AstFrameSet *swcsin = NULL;  /* FrameSet describing input WCS */
  int temp;                    /* Temporary variable  */
  const char *usesys = NULL;   /* AST system for output cube */
  int wdthpix;                 /* Map width in pixels */
  double x_array_corners[4];   /* X-Indices for corner bolos in array */ 
  double y_array_corners[4];   /* Y-Indices for corner pixels in array */ 

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Initialize output frameset pointer to NULL */
  *outframeset = NULL;

  /* Read data from the given input file in the group - note index
     should be 1 */
  smf_open_file( igrp, index, "READ", 1, &data, status );
  /* Construct the WCS for the first time slice in this file */
  smf_tslice_ast( data, 1, 1, status);
    
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

  if( *status == SAI__OK) {
    /* Retrieve map height and width from header */
    hdr = data->hdr;
    smf_fits_getD( hdr, "MAP_WDTH", &mapwdth, status );
    smf_fits_getD( hdr, "MAP_HGHT", &maphght, status );
    smf_fits_getD( hdr, "MAP_X", &mapx, status );
    smf_fits_getD( hdr, "MAP_Y", &mapy, status );
    /* Not yet used: must be converted to radians */
    smf_fits_getD( hdr, "MAP_PA", &mappa, status );
    mappa *= AST__DD2R;
    wdthpix = (int) ( ( mapwdth*cos(mappa) + maphght*sin(mappa) ) / pixsize);
    hghtpix = (int) ( ( maphght*cos(mappa) + mapwdth*sin(mappa) ) / pixsize);
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

    /* These might get used in the future.... */
    /*    smf_fits_getD( hdr, "RA", &lon_0, status );
	  smf_fits_getD( hdr, "DEC", &lat_0, status );*/

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

  /* Construct output WCS */
  swcsin = hdr->wcs;
  /* Retrieve input SkyFrame */
  skyin = astGetFrame( swcsin, AST__CURRENT );
  /* Determine the tracking coordinate system, and choose the
     celestial coordinate system for the output cube. */
  if( !strncmp( system, "TRACKING", 8 ) ) {
    usesys = smf_convert_system( hdr->state->tcs_tr_sys, status );
  } else {
    usesys = system;
  }
  /* Begin by taking a copy of the input SkyFrame (in order to inherit
     all the other attributes like Epoch, Equinox, ObsLat, ObsLon,
     Dut1, etc) and then set its System to the required system. */
  skyframe = astCopy( skyin );
  astSetC( skyframe, "SYSTEM", usesys );

  /* We will later record the telescope base pointing position as the
     SkyRef attribute in the output SkyFrame. To do this, we need to
     convert the stored telescope base pointing position from AZEL to
     the requested output system. Create a Mapping to do this using
     astConvert, and then use the Mapping to transform the stored
     position. */
  sf1 = astCopy( skyin );
  astSetC( sf1, "SYSTEM", "AZEL" );
  azel2usesys = astConvert( sf1, skyframe, "" );
  astTran2( azel2usesys, 1, &(hdr->state->tcs_az_bc1),
	    &(hdr->state->tcs_az_bc2), 1, skyref, skyref+1 );
  astNorm( skyframe, skyref );
  /* Determine if the telescope is tracking a moving target such as a
     planet or asteroid. This is indicated by significant change in
     the telescope base pointing position within the ICRS coordinate
     system. Here, "significant" means more than 1
     arc-second. Apparently users will only want to track moving
     objects if the output cube is in AZEL or GAPPT, so we ignoring a
     moving base pointing position unless the output system is AZEL or
     GAPPT. */
  if( !strcmp( usesys, "AZEL" ) || !strcmp( usesys, "GAPPT" ) ) {
    /* Set the "sf2" SkyFrame to represent ICRS coords ("sf1" already
       represents AZEL coords). */
    sf2 = astCopy( skyin );
    astSetC( sf2, "System", "ICRS" );

    /* Use the Mapping from `sf' (AzEl) to `sf2' (ICRS) to convert the
       telescope base pointing position for the first and last slices
       from (az,el) to ICRS. */
    az[ 0 ] = (hdr->allState)[ 0 ].tcs_az_bc1;
    el[ 0 ] = (hdr->allState)[ 0 ].tcs_az_bc2;
    az[ 1 ] = (hdr->allState)[ hdr->nframes - 1 ].tcs_az_bc1;
    el[ 1 ] = (hdr->allState)[ hdr->nframes - 1 ].tcs_az_bc2;

    astTran2( astConvert( sf1, sf2, "" ), 2, az, el, 1, ra, dec );

    /* Get the arc distance between the two positions and
       see if it is greater than 0.1 arc-sec. */
    sep = slaDsep( ra[ 0 ], dec[ 0 ], ra[ 1 ], dec[ 1 ] );
    *moving = ( sep > 0.1*AST__DD2R/3600.0 );
  } else {
    *moving = 0;
  }
  /* Just for kicks, let the user know the value of *moving */
  msgSeti("M",*moving);
  msgOutif(MSG__VERB, " ", "Moving = ^M", status);
  /* Before adding to frameset, ensure that the SkyFrame represents
     offsets from the first telescope base position, rather than
     absolute coordinates */
  if ( *moving ) {
    astSetD( skyframe, "SkyRef(1)", skyref[0] );
    astSetD( skyframe, "SkyRef(2)", skyref[1] );
    astSet( skyframe, "SkyRefIs=origin" );
    /* Also set tangent position */
    lon_0 = 0.0;
    lat_0 = 0.0;
  } else {
    lon_0 = DR2D * skyref[0];
    lat_0 = DR2D * skyref[1];
  }
  /* Now populate a FitsChan with FITS-WCS headers describing the
     required tan plane projection. The longitude and latitude axis
     types are set to either (RA,Dec) or (AZ,EL) to get the correct
     handedness. Note lon_0 and lat_0 will already be in degrees
     (unlike smf_mapbounds). */
  fitschan = astFitsChan ( NULL, NULL, "" );
  if ( !strcmp( astGetC( skyframe, "SYSTEM" ), "AZEL" ) ) {
    sc2ast_makefitschan( 0.0, 0.0, (pixsize*DAS2D), (pixsize*DAS2D),
			 lon_0, lat_0,
			 "AZ---TAN", "EL---TAN", fitschan, status );
  } else {
    sc2ast_makefitschan( 0.0, 0.0, (-pixsize*DAS2D), (pixsize*DAS2D),
			 lon_0, lat_0,
			 "RA---TAN", "DEC--TAN", fitschan, status );
  }
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

  astSetC( *outframeset, "SYSTEM", usesys );
  /* Change the pixel bounds to be consistent with the new CRPIX */
  ubnd_out[0] -= lbnd_out[0];
  lbnd_out[0] = 0;

  ubnd_out[1] -= lbnd_out[1];
  lbnd_out[1] = 0;

  /* Clean Up */
 
 CLEANUP:
  if (fs) fs = astAnnul( fs );
  if (sky2map) sky2map  = astAnnul( sky2map );
  if (fitschan) fitschan = astAnnul( fitschan );

  if( data != NULL )
    smf_close_file( &data, status);

}
