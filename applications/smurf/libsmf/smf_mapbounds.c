/*
*+
*  Name:
*     smf_mapbounds

*  Purpose:
*     Automatically calculate the pixel bounds and FrameSet for a map
*     given a projection

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_mapbounds( Grp *igrp,  int size, char *system, double par[ 7 ], 
*                   int flag, int *lbnd_out, int *ubnd_out, 
*                   AstFrameSet **outframeset, int *moving, int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of timestream NDF data files to retrieve pointing
*     size = int (Given)
*        Number of elements in igrp
*     system = char* (Given)
*        String indicating the type of projection (e.g. "icrs")
*     par = double[ 7 ] (Given and Returned)
*        An array holding the parameters describing the spatial
*        projection between celestial (longitude,latitude) in the
*        system specified by "system", and an interim GRID coordinate
*        system for the output cube (interim because the bounds of the
*        output map are not yet known - the interim grid system is
*        thus more like a PIXEL system for the output cube but with an
*        arbitrary pixel origin). These are stored in the order
*        CRPIX1, CRPIX2, CRVAL1, CRVAL2, CDELT1, CDELT2, CROTA2. The
*        supplied values are used to produce the output WCS
*        FrameSet. All the angular parameters are in units of radians,
*        and CRPIX1/2 are in units of pixels.
*     flag = int (Given)
*        If 0, use lon_0, lat_0. If non-zero BASE coordinates are adopted for
*        the reference point instead.
*     pixsize = double (Given)
*        Linear size of a map pixel (arcsec)
*     lbnd_out = double* (Returned)
*        2-element array pixel coord. for the lower bounds of the output map 
*     ubnd_out = double* (Returned)
*        2-element array pixel coord. for the upper bounds of the output map 
*     outframeset = AstFrameSet** (Returned)
*        Frameset containing the sky->output map mapping
*     moving = int* (Returned)
*        Flag to denote whether the source is moving
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function steps over a list of input files and calculates the
*     bolometer->output map pixel transformation for the corner bolometers
*     of the subarray in order to automatically determine the extent 
*     of the map.

*  Authors:
*     Edward Chapin (UBC)
*     Tim Jenness (JAC, Hawaii)
*     Andy Gibb (UBC)
*     David Berry 
*     {enter_new_authors_here}

*  History:
*     2006-02-02 (EC):
*        Initial version.
*     2006-02-13 (TIMJ):
*        Use astSetC rather than astSet
*        Avoid an additional dereference
*        Use sc2ast_makefitschan
*     2006-02-23 (EC):
*        Fixed bug in frameset generation - tangent point in pixel coordinates
*        was not getting set properly.
*     2006-07-26 (TIMJ):
*        sc2head no longer used. Use JCMTState instead.
*     2006-07-27 (TIMJ):
*        Do not use bare constant for rad to deg conversion.
*     2006-08-15 (EC):
*        Fixed off-by-one errors in GRID coordinates
*     2006-09-12 (EC):
*        Added ability to calculate mapbounds for AzTEC
*     2007-01-25 (AGG):
*        Rewrite to check for and take account of moving objects,
*        largely borrowed from smf_cubegrid & smf_cubebounds
*     2007-02-20 (DSB):
*        Modify check for object movement to take account of the
*        difference in epoch between the first and last time slices.
*     2007-03-07 (AGG):
*        Annul relevant AST objects every time slice to minimize
*        memory usage
*     2007-07-12 (EC):
*        -Replaced calculation of bolo2map with a call to smf_rebincube_totmap
*        -Changed name of smf_rebincube_totmap to smf_rebin_totmap
*     2007-10-29 (EC):
*        Modified interface to smf_open_file.
*     2007-12-14 (EC):
*        Call smf_open_file with SMF__NOCREATE_DATA
*     2008-02-29 (AGG):
*        Explicitly set SkyRef position, ensure SkyRefIs and
*        AlignOffset attributes are also set accordingly
*     08-APR-2008 (TIMJ):
*      	 Use tcs_tai instead of	rts_end	for position calculations.
*     2008-04-18 (AGG):
*        Set lbnd to 1,1
*     2008-05-14 (EC):
*        - Modified to use projection parameters in the same style as makecube
*        - use astSetFits instead of sc2ast_makefitschan
*     2008-05-15 (EC):
*        Moved user query of lbnd/ubnd into smurf_makemap. Here just set the
*        ?bound_out values, and set dynamic defaults for LBND/UBND
*     {enter_further_changes_here}

*  Notes:
*     Currently lon_0 and lat_0 are interpreted only as ra/dec of tangent point

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research Council.
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

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "star/slalib.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_mapbounds"

void smf_mapbounds( Grp *igrp,  int size, char *system, double par[ 7 ], 
		    int flag, int *lbnd_out, int *ubnd_out, 
		    AstFrameSet **outframeset, int *moving, int *status ) {

  /* Local Variables */
  AstSkyFrame *abskyframe = NULL; /* Output Absolute SkyFrame */
  double az[ 2 ];              /* Azimuth values */
  AstMapping *azel2usesys = NULL; /* Mapping form AZEL to requested system */
  AstMapping *bolo2map = NULL; /* Combined mapping bolo->map
				  coordinates, WCS->GRID Mapping from
				  input WCS FrameSet */
  smfData *data = NULL;        /* pointer to  SCUBA2 data struct */
  double dec[ 2 ];             /* Dec values */
  double el[ 2 ];              /* Elevation values */
  smfFile *file = NULL;        /* SCUBA2 data file information */
  AstFitsChan *fitschan = NULL;/* Fits channels to construct WCS header */
  AstFrameSet *fs = NULL;      /* A general purpose FrameSet pointer */
  smfHead *hdr = NULL;         /* Pointer to data header this time slice */
  int i;                       /* Loop counter */
  int j;                       /* Loop counter */
  dim_t k;                     /* Loop counter */
  int lbnd0[ 2 ];              /* Defaults for LBND parameter */
  char *pname = NULL;          /* Name of currently opened data file */
  double ra[ 2 ];              /* RA values */
  double sep = 0;              /* Separation between first and last base positions */
  AstFrame *sf1 = NULL;        /* Spatial Frame representing AZEL system */
  AstFrame *sf2 = NULL;        /* Spatial Frame representing requested system */
  double shift[ 2 ];           /* Shifts from PIXEL to GRID coords */
  AstMapping *sky2map = NULL;  /* Mapping celestial->map coordinates,
				  Sky <> PIXEL mapping in output
				  FrameSet */
  AstSkyFrame *skyframe = NULL;/* Output SkyFrame */
  AstFrame *skyin = NULL;      /* Sky Frame in input FrameSet */
  double skyref[ 2 ];          /* Values for output SkyFrame SkyRef attribute */
  int startboundcheck = 1;     /* Flag for first check of map dimensions */
  AstFrameSet *swcsin = NULL;  /* FrameSet describing input WCS */
  int ubnd0[ 2 ];              /* Defaults for UBND parameter */
  const char *usesys = NULL;   /* AST system for output cube */
  double x_array_corners[4];   /* X-Indices for corner bolos in array */ 
  double x_map[4];             /* Projected X-coordinates of corner bolos */ 
  double y_array_corners[4];   /* Y-Indices for corner pixels in array */ 
  double y_map[4];             /* Projected X-coordinates of corner bolos */ 
   
  /* Main routine */
  if (*status != SAI__OK) return;

  /* Initialize pointer to output FrameSet and moving-source flag */
  *outframeset = NULL;
  *moving = 0;

  astBegin;

  /* Loop over all files in the Grp */
  for( i=1; i<=size; i++ ) {
    /* Read data from the ith input file in the group */      
    smf_open_file( igrp, i, "READ", SMF__NOCREATE_DATA, &data, status );
    
    if( *status == SAI__OK ) {
      file = data->file;

      if( file != NULL ) {
        pname =  file->name;
        msgSetc("FILE", pname);
        msgSeti("T", i);
        msgSeti("N", size);
        msgOutif(MSG__VERB, " ", 
                 "SMF_MAPBOUNDS: Processing ^FILE (^T/^N) ",
                 status);
      } else {
        *status = SAI__ERROR;
        errRep( "smf_mapbounds", 
                "No smfFile associated with smfData.", status);
      }
    } else {
      errRep( "smf_mapbounds", "Couldn't open data file.", status );
    }

    /* Check that the data dimensions are 3 (for time ordered data) */
    if( *status == SAI__OK ) {
      if( data->ndims == 3 ) {
	
	/* If OK Decide which detectors (GRID coord) to use for
	   checking bounds, depending on the instrument in use. */
	hdr = data->hdr;  

	switch( hdr->instrument ) {
	  
	case INST__SCUBA2:
	  /* 4 corner bolometers of the subarray */
	  x_array_corners[0] = 1;
	  x_array_corners[1] = 1;
	  x_array_corners[2] = (data->dims)[0];
	  x_array_corners[3] = (data->dims)[0];
	  
	  y_array_corners[0] = 1;
	  y_array_corners[1] = (data->dims)[1];
	  y_array_corners[2] = 1;
	  y_array_corners[3] = (data->dims)[1];
	  break;
	  
	case INST__AZTEC:
	  /* Rough guess for extreme bolometers around the edge */
	  x_array_corners[0] = 22;
	  x_array_corners[1] = 65;
	  x_array_corners[2] = 73;
	  x_array_corners[3] = 98;
	  
	  y_array_corners[0] = 1; /* Always 1 for AzTEC */
	  y_array_corners[1] = 1;
	  y_array_corners[2] = 1;
	  y_array_corners[3] = 1;
	  break;
	  
	default:
	  *status = SAI__ERROR;
	  errRep(FUNC_NAME, "Don't know how to calculate mapbounds for data created with this instrument", status);	  
	}
      } else {
	/* Otherwise report error */
	msgSetc("FILE", pname);
	msgSeti("THEDIMS", data->ndims);
	*status = SAI__ERROR;
	errRep("smf_mapbounds", 
	       "^FILE data has ^THEDIMS dimensions, should be 3.", 
	       status);
      }
    }

    if( *status == SAI__OK) {

      /* Get the astrometry for all the time slices in this data file */
      for( j=0; j<(data->dims)[2]; j++ ) {

	/* smf_tslice_ast only needs to get called once to set up framesets */

	if( data->hdr->wcs == NULL ) {
	  smf_tslice_ast( data, j, 1, status);
	}

        swcsin = hdr->wcs;

	/* Retrieve input SkyFrame */
        skyin = astGetFrame( swcsin, AST__CURRENT );

	/* Create output SkyFrame */
        if ( skyframe == NULL ) {
          /* Determine the tracking coordinate system, and choose
             the celestial coordinate system for the output cube. */
          if( !strncmp( system, "TRACKING", 8 ) ) {
            usesys = smf_convert_system( hdr->state->tcs_tr_sys, status );
          } else {
            usesys = system;
          }

	  /* Begin by taking a copy of the input SkyFrame (in order to
	     inherit all the other attributes like Epoch, Equinox,
	     ObsLat, ObsLon, Dut1, etc) and then set its System to the
	     required system. */
          skyframe = astCopy( skyin );
          astSetC( skyframe, "SYSTEM", usesys );

	  /* We will later record the telescope base pointing position
	     as the SkyRef attribute in the output SkyFrame. To do
	     this, we need to convert the stored telescope base
	     pointing position from AZEL to the requested output
	     system. Create a Mapping to do this using astConvert, and
	     then use the Mapping to transform the stored position. */
          sf1 = astCopy( skyin );
          astSetC( sf1, "SYSTEM", "AZEL" );
          azel2usesys = astConvert( sf1, skyframe, "" );
          astTran2( azel2usesys, 1, &(hdr->state->tcs_az_bc1),
                    &(hdr->state->tcs_az_bc2), 1, skyref, skyref+1 );
          azel2usesys = astAnnul( azel2usesys );
          astNorm( skyframe, skyref );

	  /* Determine if the telescope is tracking a moving target
	     such as a planet or asteroid. This is indicated by
	     significant change in the telescope base pointing
	     position within the ICRS coordinate system. Here,
	     "significant" means more than 1 arc-second. Apparently
	     users will only want to track moving objects if the
	     output cube is in AZEL or GAPPT, so we ignoring a moving
	     base pointing position unless the output system is AZEL
	     or GAPPT. */
          if( !strcmp( usesys, "AZEL" ) || !strcmp( usesys, "GAPPT" ) ) {
            /* Set the "sf2" SkyFrame to represent ICRS coords
               ("sf1" already represents AZEL coords). */
            sf2 = astCopy( skyin );
            astSetC( sf2, "System", "ICRS" );

            /* Set the Epoch for `sf1' andf `sf2' to the epoch of the
	    first time slice, then use the Mapping from `sf1' (AzEl) to
	    `sf2' (ICRS) to convert the telescope base pointing position
	    for the first time slices from (az,el) to ICRS. */

            astSet( sf1, "Epoch=MJD %.*g", DBL_DIG, 
                    (hdr->allState)[ 0 ].tcs_tai + 32.184/86400.0 );
            astSet( sf2, "Epoch=MJD %.*g", DBL_DIG, 
                    (hdr->allState)[ 0 ].tcs_tai + 32.184/86400.0 );
            az[ 0 ] = (hdr->allState)[ 0 ].tcs_az_bc1;
            el[ 0 ] = (hdr->allState)[ 0 ].tcs_az_bc2;
            astTran2( astConvert( sf1, sf2, "" ), 1, az, el, 1, ra, dec );


            /* Set the Epoch for `sf1' andf `sf2' to the epoch of the
	    last time slice, then use the Mapping from `sf1' (AzEl) to
	    `sf2' (ICRS) to convert the telescope base pointing position
	    for the last time slices from (az,el) to ICRS. */

            astSet( sf1, "Epoch=MJD %.*g", DBL_DIG, 
                    (hdr->allState)[ hdr->nframes - 1 ].tcs_tai + 32.184/86400.0 );
            astSet( sf2, "Epoch=MJD %.*g", DBL_DIG, 
                    (hdr->allState)[ hdr->nframes - 1 ].tcs_tai + 32.184/86400.0 );
            az[ 1 ] = (hdr->allState)[ hdr->nframes - 1 ].tcs_az_bc1;
            el[ 1 ] = (hdr->allState)[ hdr->nframes - 1 ].tcs_az_bc2;
            astTran2( astConvert( sf1, sf2, "" ), 1, az + 1, el + 1, 1, 
                                                     ra + 1, dec + 1 );

	    sf1 = astAnnul( sf1 );
	    sf2 = astAnnul( sf2 );
            /* Get the arc distance between the two positions and
               see if it is greater than 0.1 arc-sec. */
            sep = slaDsep( ra[ 0 ], dec[ 0 ], ra[ 1 ], dec[ 1 ] );
            *moving = ( sep > 0.1*AST__DD2R/3600.0 );
          } else {
            *moving = 0;
          }
	  /* Just for kicks, let the user know the value of *moving */
          msgSeti("M",*moving);
          msgSetd("R", sep*AST__DR2D*3600.0 );
          msgOutif(MSG__VERB, " ", "Moving = ^M (^R arcsec)", status);
        } /* End skyframe construction */

	/* Set the reference position to the telescope BASE position */
	astSetD( skyframe, "SkyRef(1)", skyref[0] );
	astSetD( skyframe, "SkyRef(2)", skyref[1] );
        /* Before adding to frameset, ensure that the SkyFrame
           represents offsets from the first telescope base position,
           rather than absolute coordinates */
        if ( *moving ) {
          astSet( skyframe, "SkyRefIs=origin,AlignOffset=1" );
          /* Also set tangent position */
          par[2] = 0.0;
          par[3] = 0.0;
        } else {
          astSet( skyframe, "SkyRefIs=ignored,AlignOffset=0" );
	  /* If `flag' is set, use lon_0/lat_0 values given, else set
	     them to the skyref coordinates  */
	  if( !flag ) {
	    par[2] = skyref[0];
	    par[3] = skyref[1];
	  }
	}

	/* Ensure the pixel sizes have the correct signs. */
	if( par[4] != AST__BAD ) {
	  if( usesys && !strcmp( usesys, "AZEL" ) ) {
            par[4] = fabs( par[4] );
	  } else {
            par[4] = -fabs( par[4] );
	  }
	  par[5] = fabs( par[5] );
	}

	/* Update par to contain some reasonable defaults */
	par[0] = 1.0;
	par[1] = 1.0;
	par[4] = (3./3600.)*AST__DD2R;
	par[5] = (3./3600.)*AST__DD2R;
	par[6] = 0.;

	/* Ensure the pixel sizes have the correct signs. */
	if( par[4] != AST__BAD ) {
	  if( usesys && !strcmp( usesys, "AZEL" ) ) {
            par[4] = fabs( par[4] );
	  } else {
            par[4] = -fabs( par[4] );
	  }
	  par[5] = fabs( par[5] );
	}

	/* Update par based on user-specified values */
	smf_get_projpar( skyframe, par, NULL, status );

        if ( *outframeset == NULL && skyframe != NULL && (*status == SAI__OK)){
          /* Now populate a FitsChan with FITS-WCS headers describing
             the required tan plane projection. The longitude and
             latitude axis types are set to either (RA,Dec) or (AZ,EL)
             to get the correct handedness. Convert from radians to
             degrees as required by FITS. */
          fitschan = astFitsChan ( NULL, NULL, "" );

	  if( !strcmp( astGetC( skyframe, "System" ), "AZEL" ) ){
	    astSetFitsS( fitschan, "CTYPE1", "AZ---TAN", " ", 1 );
	    astSetFitsS( fitschan, "CTYPE2", "EL---TAN", " ", 1 );
	  } else {
	    astSetFitsS( fitschan, "CTYPE1", "RA---TAN", " ", 1 );
	    astSetFitsS( fitschan, "CTYPE2", "DEC--TAN", " ", 1 );
	  }
	  astSetFitsF( fitschan, "CRPIX1", par[0], " ", 1 );
	  astSetFitsF( fitschan, "CRPIX2", par[1], " ", 1 );
	  astSetFitsF( fitschan, "CRVAL1", par[2]*AST__DR2D, " ", 1 );
	  astSetFitsF( fitschan, "CRVAL2", par[3]*AST__DR2D, " ", 1 );
	  astSetFitsF( fitschan, "CDELT1", par[4]*AST__DR2D, " ", 1 );
	  astSetFitsF( fitschan, "CDELT2", par[5]*AST__DR2D, " ", 1 );
	  astSetFitsF( fitschan, "CROTA2", par[6]*AST__DR2D, " ", 1 );

          astClear( fitschan, "Card" );
          fs = astRead( fitschan );

          /* Extract the output PIXEL->SKY Mapping. */
          sky2map = astGetMapping( fs, AST__BASE, AST__CURRENT );
          /* Get a copy of the output SkyFrame and ensure it represents
             absolute coords rather than offset coords. */
          abskyframe = astCopy( skyframe );
          astClear( abskyframe, "SkyRefIs" );
          astClear( abskyframe, "AlignOffset" );

	  /* Tidy up */
	  fs = astAnnul( fs );
	  /* Create the output FrameSet */
          *outframeset = astFrameSet( astFrame(2, "Domain=GRID"), "");

	  /* Now add the SkyFrame to it */
          astAddFrame( *outframeset, AST__BASE, sky2map, skyframe );
	  /* Invert the sky2map mapping */
          astInvert( sky2map );

        } /* End WCS FrameSet construction */

	/* Calculate the bolo to map-pixel transformation for this tslice */
	bolo2map = smf_rebin_totmap( data, j, abskyframe, sky2map, 
				     *moving, status );

        if ( *status == SAI__OK ) {
          /* Check corner pixels in the array for their projected extent
             on the sky to set the pixel bounds */
          astTran2( bolo2map, 4, x_array_corners, y_array_corners, 1,
                    x_map, y_map );

          /* If starting the bound check initialize the values */
          if( startboundcheck ) {
            startboundcheck = 0;
            lbnd_out[0] = x_map[0];
            ubnd_out[0] = x_map[0];
            lbnd_out[1] = y_map[0];
            ubnd_out[1] = y_map[0];
          }

          /* Update min/max for this time slice */
          for( k=0; k<4; k++ ) {
            if( x_map[k] < lbnd_out[0] ) lbnd_out[0] = x_map[k];
            if( y_map[k] < lbnd_out[1] ) lbnd_out[1] = y_map[k];
            if( x_map[k] > ubnd_out[0] ) ubnd_out[0] = x_map[k];
            if( y_map[k] > ubnd_out[1] ) ubnd_out[1] = y_map[k];
          }
        }
	/* Explicitly annul these mappings each time slice for reduced
	   memory usage */
	if (bolo2map) bolo2map = astAnnul( bolo2map );
	if (fs) fs = astAnnul( fs );
	if ( skyin ) skyin = astAnnul( skyin );

        /* Break out of loop over time slices if bad status */
        if (*status != SAI__OK) goto CLEANUP;
      }
      /* Annul any remaining Ast objects before moving on to the next file */
      if (fs) fs = astAnnul( fs );
      if (bolo2map) bolo2map = astAnnul( bolo2map );
    }

    /* Close the data file */
    smf_close_file( &data, status);

    /* Break out of loop over data files if bad status */
    if (*status != SAI__OK) goto CLEANUP;
  }

  /* Apply a ShiftMap to the output FrameSet to re-align the GRID
     coordinates */
  shift[0] = -lbnd_out[0];
  shift[1] = -lbnd_out[1];
  astRemapFrame( *outframeset, AST__BASE, astShiftMap( 2, shift, "") );

  astSetC( *outframeset, "SYSTEM", usesys );

  astExport( *outframeset );

  /* Change the pixel bounds to be consistent with the new CRPIX */
  ubnd_out[0] -= lbnd_out[0]-1;
  lbnd_out[0] = 1;

  ubnd_out[1] -= lbnd_out[1]-1;
  lbnd_out[1] = 1;

  /* Set the dynamic defaults for lbnd/ubnd */
  lbnd0[ 0 ] = lbnd_out[ 0 ];
  lbnd0[ 1 ] = lbnd_out[ 1 ];
  parDef1i( "LBND", 2, lbnd0, status );
  
  ubnd0[ 0 ] = ubnd_out[ 0 ];
  ubnd0[ 1 ] = ubnd_out[ 1 ];
  parDef1i( "UBND", 2, ubnd0, status );

  /* Clean Up */ 
 CLEANUP:
  if (*status != SAI__OK) {
    errRep(FUNC_NAME, "Unable to determine map bounds", status);
  }

  if (sky2map) sky2map  = astAnnul( sky2map );
  if (bolo2map) bolo2map = astAnnul( bolo2map );
  if (fitschan) fitschan = astAnnul( fitschan );

  if( data != NULL )
    smf_close_file( &data, status );

  astEnd;

}
