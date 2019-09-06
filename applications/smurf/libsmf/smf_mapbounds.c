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
*     smf_mapbounds( int fast, Grp *igrp,  int size, const char *system,
*                    AstFrameSet *refwcs, int alignsys, int *lbnd_out,
*                    int *ubnd_out, AstFrameSet **outframeset, int *moving,
*                    smfBox ** boxes, fts2Port fts_port, AstKeyMap *config,
*                    int *status );

*  Arguments:
*     fast = int (Given)
*        If true and if the input map is not a scan, the bounds will only be
*        calculated by looking at the first and last timeslice. For DREAM/STARE
*        this is usually sufficient.
*     igrp = Grp* (Given)
*        Group of timestream NDF data files to retrieve pointing
*     size = int (Given)
*        Number of elements in igrp
*     system = const char* (Given)
*        String indicating the sky coordinate system (e.g. "icrs")
*     spacerefwcs = AstFrameSet * (Given)
*        Frameset corresponding to a reference WCS that should be
*        used to define the output pixel grid. Can be NULL.
*     alignsys = int (Given)
*        If non-zero, then the input data will be aligned in the coordinate
*        system specified by "system" rather than in the default system
*        (ICRS).
*     lbnd_out = double* (Returned)
*        2-element array pixel coord. for the lower bounds of the output map
*     ubnd_out = double* (Returned)
*        2-element array pixel coord. for the upper bounds of the output map
*     outframeset = AstFrameSet** (Returned)
*        Frameset containing the sky->output map mapping
*     moving = int* (Returned)
*        Flag to denote whether the source is moving
*     boxes = smfBox ** (Returned)
*        Location at which to returned a pointer to an array of smfBox
*        structures. The length of this array is equal to the number of input
*        files in group "igrp". Each element of the array holds the bounds
*        of the spatial coverage of the corresponding input file, given as
*        pixel indices within the output cube. The array should be freed
*        using astFree when no longer needed.
*     fts_port = fts2Port (Given)
*        FTS-2 port.
*     config = AstKeyMap * (Given)
*        A KeyMap containing the user-supplied configuration parameter
*        values.
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
*     2008-05-20 (EC):
*        Set intelligent dynamic default for CROTA like smf_cubegrid
*     2008-06-03 (TIMJ):
*        Add smfBox support similar to smf_cubebounds.
*        Change API of smf_get_projpar
*        Return pixel bounds rather than grid bounds
*        Prompt for bounds here rather than in caller.
*     2008-06-04 (TIMJ):
*        - Add alignsys flag. Replaces "int flag" that was unused.
*        - Fix -Wall warnings.
*        - use smf_calc_skyframe
*     2008-06-05 (TIMJ):
*        - par[] does not need to be an argument.
*        - replace par[] with reference spatial frameset.
*     2008-07-28 (TIMJ):
*        Use smf_makefitschan.
*     2008-07-29 (TIMJ):
*        Tweak the logic to make it clear that skyin is only needed
*        to determine the projection.
*     2008-07-30 (TIMJ):
*        Significant (eg 0.6s vs 13s) improvement in the speed of this
*        routine in FAST mode. Now work out the maximum excursion of the
*        telescope relative to BASE and only ask the transformation code
*        to look at those 4 time slices.
*     2009-11-03 (TIMJ):
*        Skip bad bolo2map mappings and bad telescope data.
*     2009-12-09 (TIMJ):
*        Use DRCONTROL flags to check whether we got data from the SMU and TCS.
*        For older data also check SMU information since bad values from the
*        SMU can break sc2ast_createwcs.
*     2010-01-14 (TIMJ):
*        Include SMU offset in excursion.
*     2011-3-11 (DSB):
*        Ensure user-supplied pixel bounds do not extend beyond the
*        available data if the TRIMBAD parameter is set TRUE.
*     2011-3-14 (DSB):
*        Rename TRIMBAD parameter as TRIM (for consistency with MAKECUBE).
*     2011-3-22 (DSB):
*        - Ensure that the "moving" flag is assigned a value even if the
*        output skyframe is supplied via spacerefwcs.
*        - Report an error if the extreme positions cannot be found.
*     2011-05-19 (TIMJ):
*        Find the first valid TCS position when building up framesets.
*     2012-03-06 (TIMJ):
*        Use PAL instead of SLA.
*     2014-03-04 (DSB):
*        Set the reference position in the returned SkyFrame even if a
*        reference SkyFrame was supplied. Without this, ZERO_CIRCLE
*        masking does not know where the source is, and assumes it is at
*        (RA,Dec) = (0,0).
*     2014-03-31 (DSB):
*        - Remove const qualifier from spacerefwcs since it generates
*        loads of compiler warnings. AST has no concept of "const" objects.
*        - Fast-mode re-written to avoid assuming that the map Y axis is
*        parallel to north.
*     2014-07-7 (DSB):
*        Ensure Epoch in returned SKyFrame is set from the data rather
*        than any supplied spatial reference FrameSet.
*     2014-10-08 (DSB):
*        Report an error if either axis of the map spans more than 20000 pixels.
*     2014-11-04 (DSB):
*        Fix memory leak (ac1list and ac2list not being freed).
*     2014-12-16 (DSB):
*        Do not assume the first subscan will always be usable.
*     2015-10-16 (DSB):
*        Use smf_set_moving to assign attributes for a moving target,
*        rather than just setting SkyRefIs (smf_set_moving also sets
*        AlignOffset).
*     2015-11-11 (DSB):
*        Added argument "config".
*     2019-9-6 (DSB):
*        Ignore samples that have bad pointing info (jos_drcontrol).
*     {enter_further_changes_here}

*  Notes:
*     The par[7] array used in this routine is documented in smf_get_projpar.c

*  Copyright:
*     Copyright (C) 2008-2012 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "par.h"
#include "star/ndg.h"
#include "star/pal.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_mapbounds"
#define MAX_DIM 20000

void smf_mapbounds( int fast, Grp *igrp,  int size, const char *system,
                    AstFrameSet *spacerefwcs, int alignsys, int *lbnd_out,
                    int *ubnd_out, AstFrameSet **outframeset, int *moving,
                    smfBox ** boxes, fts2Port fts_port, AstKeyMap *config,
                    int *status ) {

  /* Local Variables */
  AstSkyFrame *abskyframe = NULL; /* Output Absolute SkyFrame */
  int actval;           /* Number of parameter values supplied */
  AstMapping *bolo2map = NULL; /* Combined mapping bolo->map
                                  coordinates, WCS->GRID Mapping from
                                  input WCS FrameSet */
  smfBox *box = NULL;          /* smfBox for current file */
  smfData *data = NULL;        /* pointer to  SCUBA2 data struct */
  double dlbnd[ 2 ];    /* Floating point lower bounds for output map */
  drcntrl_bits drcntrl_mask = 0;/* Mask to use for DRCONTROL on this instrument */
  double dubnd[ 2 ];    /* Floating point upper bounds for output map */
  AstMapping *fast_map = NULL; /* Mapping from tracking to absolute map coords */
  smfFile *file = NULL;        /* SCUBA2 data file information */
  int first;                   /* Is this the first good subscan ? */
  AstFitsChan *fitschan = NULL;/* Fits channels to construct WCS header */
  AstFrameSet *fs = NULL;      /* A general purpose FrameSet pointer */
  smfHead *hdr = NULL;         /* Pointer to data header this time slice */
  int i;                       /* Loop counter */
  dim_t j;                     /* Loop counter */
  AstSkyFrame *junksky = NULL; /* Unused SkyFrame argument */
  dim_t k;                     /* Loop counter */
  int lbnd0[ 2 ];              /* Defaults for LBND parameter */
  double map_pa=0;             /* Map PA in output coord system (rads) */
  dim_t maxloop;               /* Number of times to go round the time slice loop */
  dim_t nbadt  = 0;            /* Number of bad time slices */
  dim_t ngoodt = 0;            /* Number of good time slices */
  double par[7];               /* Projection parameters */
  double shift[ 2 ];           /* Shifts from PIXEL to GRID coords */
  AstMapping *oskymap = NULL;  /* Mapping celestial->map coordinates,
                                  Sky <> PIXEL mapping in output
                                  FrameSet */
  AstSkyFrame *oskyframe = NULL;/* Output SkyFrame */
  char *refsys = NULL;         /* Sky system from supplied reference FrameSet */
  dim_t textreme[4];           /* Time index corresponding to minmax TCS posn */
  AstFrame *skyin = NULL;      /* Sky Frame in input FrameSet */
  double skyref[ 2 ];          /* Values for output SkyFrame SkyRef attribute */
  struct timeval tv1;          /* Timer */
  struct timeval tv2;          /* Timer */
  AstMapping *tmap;            /* Temporary Mapping */
  int trim;                    /* Trim borders of bad pixels from o/p image? */
  int ubnd0[ 2 ];              /* Defaults for UBND parameter */
  double x_array_corners[4];   /* X-Indices for corner bolos in array */
  double x_map[4];             /* Projected X-coordinates of corner bolos */
  double y_array_corners[4];   /* Y-Indices for corner pixels in array */
  double y_map[4];             /* Projected X-coordinates of corner bolos */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Start a timer to see how long this takes */
  smf_timerinit( &tv1, &tv2, status );

  /* Initialize pointer to output FrameSet and moving-source flag */
  *outframeset = NULL;
  *moving = 0;

  /* initialize double precision output bounds and the proj pars */
  for( i = 0; i < 7; i++ ) par[ i ] = AST__BAD;
  dlbnd[ 0 ] = VAL__MAXD;
  dlbnd[ 1 ] = VAL__MAXD;
  dubnd[ 0 ] = VAL__MIND;
  dubnd[ 1 ] = VAL__MIND;

  /* If we have a supplied reference WCS we can use that directly
     without having to calculate it from the data. Replace the requested
     system with the system from the reference FrameSet (take a copy of the
     string since astGetC may re-use its buffer). */
  if (spacerefwcs) {
     oskyframe = astGetFrame( spacerefwcs, AST__CURRENT );
     int nc = 0;
     refsys = astAppendString( NULL, &nc, astGetC( oskyframe, "System" ) );
     system = refsys;
  }

  /* Create array of returned smfBox structures and store a pointer
     to the next one to be initialised. */
  *boxes = astMalloc( sizeof( smfBox ) * size );
  box = *boxes;

  astBegin;

  /* Loop over all files in the Grp */
  first = 1;
  for( i=1; i<=size; i++, box++ ) {

    /* Initialise the spatial bounds of section of the the output cube that is
       contributed to by the current ionput file. */
    box->lbnd[ 0 ] = VAL__MAXD;
    box->lbnd[ 1 ] = VAL__MAXD;
    box->ubnd[ 0 ] = VAL__MIND;
    box->ubnd[ 1 ] = VAL__MIND;

    /* Read data from the ith input file in the group */
    smf_open_file( NULL, igrp, i, "READ", SMF__NOCREATE_DATA, &data, status );

    if (*status != SAI__OK) {
      msgSeti( "I", i );
      errRep( "smf_mapbounds", "Could not open data file no ^I.", status );
      break;
    } else {
      if( *status == SAI__OK ) {
        if( data->file == NULL ) {
          *status = SAI__ERROR;
          errRep( FUNC_NAME, "No smfFile associated with smfData.",
                  status );
          break;

        } else if( data->hdr == NULL ) {
          *status = SAI__ERROR;
          errRep( FUNC_NAME, "No smfHead associated with smfData.",
                  status );
          break;

        } else if( data->hdr->fitshdr == NULL ) {
          *status = SAI__ERROR;
          errRep( FUNC_NAME, "No FITS header associated with smfHead.",
                  status );
          break;

        }
      }
    }

    /* convenience pointers */
    file = data->file;
    hdr = data->hdr;

    /* report name of the input file */
    smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
    msgSeti("I", i);
    msgSeti("N", size);
    msgOutif(MSG__VERB, " ",
             "SMF_MAPBOUNDS: Processing ^I/^N ^FILE",
             status);

/* Check that there are 3 pixel axes. */
    if( data->ndims != 3 ) {
      smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
      msgSeti( "NDIMS", data->ndims );
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "^FILE has ^NDIMS pixel axes, should be 3.",
              status );
      break;
    }

    /* Check that the data dimensions are 3 (for time ordered data) */
    if( *status == SAI__OK ) {

      /* If OK Decide which detectors (GRID coord) to use for
         checking bounds, depending on the instrument in use. */

      switch( hdr->instrument ) {

      case INST__SCUBA2:
        drcntrl_mask = DRCNTRL__POSITION;
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

      case INST__ACSIS:
        smf_find_acsis_corners( data, x_array_corners, y_array_corners,
                                status);
        break;

      default:
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Don't know how to calculate mapbounds for data created with this instrument", status);
      }
    }

    if( *status == SAI__OK) {
      size_t goodidx = SMF__BADSZT;

      /* Need to build up a frameset based on good telescope position.
         We can not assume that we the first step will be a good TCS position
         so we look for one. If we can not find anything we skip to the
         next file. */
      maxloop = (data->dims)[2];
      for (j=0; j<maxloop; j++) {
        JCMTState state = (hdr->allState)[j];
        if (state.jos_drcontrol >= 0 && state.jos_drcontrol & drcntrl_mask ) {
          /* bad TCS - so try again */
        } else {
          /* Good tcs */
          goodidx = j;
          break;
        }
      }

      if (goodidx == SMF__BADSZT) {
        smf_smfFile_msg( data->file, "FILE", 1, "<unknown>");
        msgOutif( MSG__QUIET, "", "No good telescope positions found in file ^FILE. Ignoring",
                  status );
        smf_close_file( NULL, &data, status );
        continue;
      }

      /* If we are dealing with the first good file, create the output
         SkyFrame. */
      if( first ) {
        first = 0;

        /* Create output SkyFrame if it has not come from a reference */
        if ( oskyframe == NULL ) {

          /* smf_tslice_ast only needs to get called once to set up framesets */
          if( hdr->wcs == NULL ) {
            smf_tslice_ast( data, goodidx, 1, fts_port, status);
          }

          /* Retrieve input SkyFrame */
          skyin = astGetFrame( hdr->wcs, AST__CURRENT );

          smf_calc_skyframe( skyin, system, hdr, alignsys, &oskyframe, skyref,
                             moving, status );

          /* Get the orientation of the map vertical within the output celestial
             coordinate system. This is derived form the MAP_PA FITS header, which
             gives the orientation of the map vertical within the tracking system. */
          map_pa = smf_calc_mappa( hdr, system, skyin, status );

          /* Provide a sensible default for the pixel size based on wavelength */
          par[4] = smf_calc_telres( hdr->fitshdr, status );
          par[4] *= AST__DD2R/3600.0;
          par[5] = par[4];

          /* Calculate the projection parameters. We do not enable autogrid determination
             for SCUBA-2 so we do not need to obtain all the data before calculating
             projection parameters. */
          smf_get_projpar( oskyframe, skyref, *moving, 0, 0, NULL, 0,
                           map_pa, par, NULL, NULL, status );

          if (skyin) skyin = astAnnul( skyin );

        /* If the output skyframe has been supplied, we still need to
           determine whether the source is moving or not, and set the
           reference position. */
        } else {

          /* smf_tslice_ast only needs to get called once to set up framesets */
          if( hdr->wcs == NULL ) {
            smf_tslice_ast( data, goodidx, 1, fts_port, status);
          }

          /* Retrieve input SkyFrame */
          skyin = astGetFrame( hdr->wcs, AST__CURRENT );
          smf_calc_skyframe( skyin, system, hdr, alignsys, &junksky, skyref,
                             moving, status );

          /* Store the sky reference position. If the target is moving,
             ensure the returned SkyFrame represents offsets from the
             reference position rather than absolute coords. */
          astSetD( oskyframe, "SkyRef(1)", skyref[ 0 ] );
          astSetD( oskyframe, "SkyRef(2)", skyref[ 1 ] );
          if( *moving ) smf_set_moving( (AstFrame *) oskyframe, NULL,
status );

          /* Ensure the Epoch attribute in the map is set to the date of
             the first data in the map, rather than the date in supplied
             reference WCS. */
          astSetD( oskyframe, "Epoch", astGetD( junksky, "Epoch" ) );
        }

        if ( *outframeset == NULL && oskyframe != NULL && (*status == SAI__OK)){
          /* Now created a spatial Mapping. Use the supplied reference frameset
             if supplied */
          if (spacerefwcs) {
            oskymap = astGetMapping( spacerefwcs, AST__BASE, AST__CURRENT );
          } else {
            /* Now populate a FitsChan with FITS-WCS headers describing
               the required tan plane projection. The longitude and
               latitude axis types are set to either (RA,Dec) or (AZ,EL)
               to get the correct handedness. */
            fitschan = astFitsChan ( NULL, NULL, " " );
            smf_makefitschan( astGetC( oskyframe, "System"), &(par[0]),
                              &(par[2]), &(par[4]), par[6], fitschan, status );
            astClear( fitschan, "Card" );
            fs = astRead( fitschan );

            /* Extract the output PIXEL->SKY Mapping. */
            oskymap = astGetMapping( fs, AST__BASE, AST__CURRENT );

            /* Tidy up */
            fs = astAnnul( fs );
          }

          /* Create the output FrameSet */
          *outframeset = astFrameSet( astFrame(2, "Domain=GRID"), " " );

          /* Now add the SkyFrame to it */
          astAddFrame( *outframeset, AST__BASE, oskymap, oskyframe );

          /* Now add a POLANAL Frame if required (i.e. if the input time
             series are POL-2 Q/U values). */
          smf_addpolanal( *outframeset, hdr, config, status );

          /* Invert the oskymap mapping */
          astInvert( oskymap );

        } /* End WCS FrameSet construction */
      }

      /* Get a copy of the output SkyFrame and ensure it represents
         absolute coords rather than offset coords. */
      abskyframe = astCopy( oskyframe );
      astClear( abskyframe, "SkyRefIs" );
      astClear( abskyframe, "AlignOffset" );

      maxloop = (data->dims)[2];
      if (fast) {
        /* For scan map we scan through looking for largest telescope moves.
           For dream/stare we just look at the start and end time slices to
           account for sky rotation. */

        if (hdr->obsmode != SMF__OBS_SCAN) {
          textreme[0] = 0;
          textreme[1] = (data->dims)[2] - 1;
          maxloop = 2;

        } else {
          const char *tracksys;
          double *ac1list, *ac2list, *bc1list, *bc2list, *p1, *p2, *p3, *p4;
          double flbnd[4], fubnd[4];
          JCMTState state;

          /* If the output and tracking systems are different, get a
             Mapping between them. */
          tracksys = sc2ast_convert_system( (hdr->allState)[goodidx].tcs_tr_sys,
                                            status );
          if( strcmp( system, tracksys ) ) {
             AstSkyFrame *tempsf = astCopy( abskyframe );
             astSetC( tempsf, "System", tracksys );
             AstFrameSet *tempfs = astConvert( tempsf, abskyframe, "" );
             tmap = astGetMapping( tempfs, AST__BASE, AST__CURRENT );
             fast_map = astSimplify( tmap );
             tmap = astAnnul( tmap );
             tempsf = astAnnul( tempsf );
             tempfs = astAnnul( tempfs );
          } else {
             fast_map = NULL;
          }

          /* Copy all ac1/2 positions into two array, and transform them
             from tracking to absolute output sky coords. */
          ac1list = astMalloc( maxloop*sizeof( *ac1list ) );
          ac2list = astMalloc( maxloop*sizeof( *ac2list ) );
          if( *status == SAI__OK ) {
             p1 = ac1list;
             p2 = ac2list;
             for( j = 0; j < maxloop; j++ ) {
                state = (hdr->allState)[ j ];
                if( state.jos_drcontrol >= 0 &&
                    state.jos_drcontrol & drcntrl_mask ) {
                   *(p1++) = AST__BAD;
                   *(p2++) = AST__BAD;
                } else {
                   *(p1++) = state.tcs_tr_ac1;
                   *(p2++) = state.tcs_tr_ac2;
                }
             }
             if( fast_map ) astTran2( fast_map, maxloop, ac1list, ac2list, 1,
                                      ac1list, ac2list );
          }

          /* If the target is moving, we need to adjust these ac1/2 values
             to represent offsets from the base position. */
          if( *moving ) {

          /* Copy all bc1/2 positions into two arrays. */
             bc1list = astMalloc( maxloop*sizeof( *bc1list ) );
             bc2list = astMalloc( maxloop*sizeof( *bc2list ) );
             if( *status == SAI__OK ) {
                p1 = bc1list;
                p2 = bc2list;

                for( j = 0; j < maxloop; j++ ) {
                   state = (hdr->allState)[ j ];
                   if( state.jos_drcontrol >= 0 &&
                       state.jos_drcontrol & drcntrl_mask ) {
                      *(p1++) = AST__BAD;
                      *(p2++) = AST__BAD;
                   } else {
                      *(p1++) = state.tcs_tr_bc1;
                      *(p2++) = state.tcs_tr_bc2;
                   }
                }

                /* Transform them from tracking to absolute output sky coords. */
                if( fast_map ) astTran2( fast_map, maxloop, bc1list, bc2list,
                                         1, bc1list, bc2list );

                /* Replace each ac1/2 position with the offsets from the
                   corresponding base position. */
                p1 = bc1list;
                p2 = bc2list;
                p3 = ac1list;
                p4 = ac2list;
                for( j = 0; j < maxloop; j++ ) {
                  smf_offsets( *(p1++), *(p2++), p3++, p4++, status );
                }
             }

             /* We no longer need the base positions. */
             bc1list = astFree( bc1list );
             bc2list = astFree( bc2list );
          }

          /* Transform the ac1/2 position from output sky coords to
             output pixel coords. */
          astTran2( oskymap, maxloop, ac1list, ac2list, 1, ac1list, ac2list );

          /* Find the bounding box containing these pixel coords and the
             time slices at which the boresight touches each edge of this
             box. */
          flbnd[ 0 ] = VAL__MAXD;
          flbnd[ 1 ] = VAL__MAXD;
          fubnd[ 0 ] = VAL__MIND;
          fubnd[ 1 ] = VAL__MIND;
          for( j = 0; j < 4; j++ ) textreme[ j ] = (dim_t) VAL__BADI;

          if( *status == SAI__OK ) {
             p1 = ac1list;
             p2 = ac2list;
             for( j = 0; j < maxloop; j++,p1++,p2++ ) {
                if( *p1 != VAL__BADD && *p2 != VAL__BADD ){

                   if ( *p1 < flbnd[0] ) { flbnd[0] = *p1; textreme[0] = j; }
                   if ( *p2 < flbnd[1] ) { flbnd[1] = *p2; textreme[1] = j; }
                   if ( *p1 > fubnd[0] ) { fubnd[0] = *p1; textreme[2] = j; }
                   if ( *p2 > fubnd[1] ) { fubnd[1] = *p2; textreme[3] = j; }
                }
             }
          }

          maxloop = 4;
          msgSetd("X1", textreme[0]);
          msgSetd("X2", textreme[1]);
          msgSetd("X3", textreme[2]);
          msgSetd("X4", textreme[3]);
          msgOutif( MSG__DEBUG, " ",
                    "Extrema time slices are ^X1, ^X2, ^X3 and ^X4",
                    status);

          ac1list = astFree( ac1list );
          ac2list = astFree( ac2list );

        }
      }

      /* Get the astrometry for all the relevant time slices in this data file */
      for( j=0; j<maxloop; j++ ) {
        dim_t ts;  /* Actual time slice to use */

        /* if we are doing the fast loop, we need to read the time slice
           index from textreme. Else we just use the index */
        if (fast) {
          /* get the index but make sure it is good */
          ts = textreme[j];
          if (ts == (dim_t)VAL__BADI) continue;
        } else {
          ts = j;
        }
        /* Calculate the bolo to map-pixel transformation for this tslice */
        bolo2map = smf_rebin_totmap( data, ts, abskyframe, oskymap,
                                     *moving, fts_port, status );

        if ( *status == SAI__OK ) {
          /* skip if we did not get a mapping this time round */
          if (!bolo2map) continue;

          /* Check corner pixels in the array for their projected extent
             on the sky to set the pixel bounds */
          astTran2( bolo2map, 4, x_array_corners, y_array_corners, 1,
                    x_map, y_map );

          /* Update min/max for this time slice */
          for( k=0; k<4; k++ ) {

            if( x_map[k] != AST__BAD && y_map[k] != AST__BAD ) {
              if( x_map[k] < dlbnd[0] ) dlbnd[0] = x_map[k];
              if( y_map[k] < dlbnd[1] ) dlbnd[1] = y_map[k];
              if( x_map[k] > dubnd[0] ) dubnd[0] = x_map[k];
              if( y_map[k] > dubnd[1] ) dubnd[1] = y_map[k];

              if( x_map[k] < box->lbnd[0] ) box->lbnd[0] = x_map[k];
              if( y_map[k] < box->lbnd[1] ) box->lbnd[1] = y_map[k];
              if( x_map[k] > box->ubnd[0] ) box->ubnd[0] = x_map[k];
              if( y_map[k] > box->ubnd[1] ) box->ubnd[1] = y_map[k];

            } else if( *status == SAI__OK ) {
              *status = SAI__ERROR;
              errRep( FUNC_NAME, "Extreme positions are bad.", status );
              break;
            }
          }
        }
        /* Explicitly annul these mappings each time slice for reduced
           memory usage */
        if (bolo2map) bolo2map = astAnnul( bolo2map );
        if (fs) fs = astAnnul( fs );

        /* Break out of loop over time slices if bad status */
        if (*status != SAI__OK) goto CLEANUP;
      }

      /* Annul any remaining Ast objects before moving on to the next file */
      if (fs) fs = astAnnul( fs );
      if (bolo2map) bolo2map = astAnnul( bolo2map );
    }

    /* Close the data file */
    smf_close_file( NULL, &data, status);

    /* Break out of loop over data files if bad status */
    if (*status != SAI__OK) goto CLEANUP;
  }

  /* make sure we got values - should not be possible with good status */
  if (dlbnd[0] == VAL__MAXD || dlbnd[1] == VAL__MAXD) {
    if (*status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( " ", "Unable to find any valid map bounds", status );
    }
  }

  if (nbadt > 0) {
    msgOutf( "", "   Processed %zu time slices to calculate bounds,"
             " of which %zu had bad telescope data and were skipped",
             status, (size_t)(ngoodt+nbadt), (size_t)nbadt );
  }

  /* If spatial reference wcs was supplied, store par values that result in
     no change to the pixel origin. */
  if( spacerefwcs ){
    par[ 0 ] = 0.5;
    par[ 1 ] = 0.5;
  }

  /* Need to re-align with the interim GRID coordinates */
  lbnd_out[0] = ceil( dlbnd[0] - par[0] + 0.5 );
  ubnd_out[0] = ceil( dubnd[0] - par[0] + 0.5 );
  lbnd_out[1] = ceil( dlbnd[1] - par[1] + 0.5 );
  ubnd_out[1] = ceil( dubnd[1] - par[1] + 0.5 );

  /* Do the same with the individual input file bounding boxes */
  box = *boxes;
  for (i = 1; i <= size; i++, box++) {
    box->lbnd[0] = ceil( box->lbnd[0] - par[0] + 0.5);
    box->ubnd[0] = ceil( box->ubnd[0] - par[0] + 0.5);
    box->lbnd[1] = ceil( box->lbnd[1] - par[1] + 0.5);
    box->ubnd[1] = ceil( box->ubnd[1] - par[1] + 0.5);
  }

  /* Apply a ShiftMap to the output FrameSet to re-align the GRID
     coordinates */
  shift[0] = 2.0 - par[0] - lbnd_out[0];
  shift[1] = 2.0 - par[1] - lbnd_out[1];
  astRemapFrame( *outframeset, AST__BASE, astShiftMap( 2, shift, " " ) );

  /* Set the dynamic defaults for lbnd/ubnd */
  lbnd0[ 0 ] = lbnd_out[ 0 ];
  lbnd0[ 1 ] = lbnd_out[ 1 ];
  parDef1i( "LBND", 2, lbnd0, status );

  ubnd0[ 0 ] = ubnd_out[ 0 ];
  ubnd0[ 1 ] = ubnd_out[ 1 ];
  parDef1i( "UBND", 2, ubnd0, status );

  parGet1i( "LBND", 2, lbnd_out, &actval, status );
  if( actval == 1 ) lbnd_out[ 1 ] = lbnd_out[ 0 ];

  parGet1i( "UBND", 2, ubnd_out, &actval, status );
  if( actval == 1 ) ubnd_out[ 1 ] = ubnd_out[ 0 ];

  /* Ensure the bounds are the right way round. */
  if( lbnd_out[ 0 ] > ubnd_out[ 0 ] ) {
    int itmp = lbnd_out[ 0 ];
    lbnd_out[ 0 ] = ubnd_out[ 0 ];
    ubnd_out[ 0 ] = itmp;
  }

  if( lbnd_out[ 1 ] > ubnd_out[ 1 ] ) {
    int itmp = lbnd_out[ 1 ];
    lbnd_out[ 1 ] = ubnd_out[ 1 ];
    ubnd_out[ 1 ] = itmp;
  }

  /* If borders of bad pixels are being trimmed from the output image,
     then do not allow the user-specified bounds to extend outside the
     default bounding box (since we know that the default bounding box
     encloses all available data). */
  parGet0l( "TRIM", &trim, status );
  if( trim ) {
     if( lbnd_out[ 0 ] < lbnd0[ 0 ] ) lbnd_out[ 0 ] = lbnd0[ 0 ];
     if( lbnd_out[ 1 ] < lbnd0[ 1 ] ) lbnd_out[ 1 ] = lbnd0[ 1 ];
     if( ubnd_out[ 0 ] > ubnd0[ 0 ] ) ubnd_out[ 0 ] = ubnd0[ 0 ];
     if( ubnd_out[ 1 ] > ubnd0[ 1 ] ) ubnd_out[ 1 ] = ubnd0[ 1 ];
  }

  /* Modify the returned FrameSet to take account of the new pixel origin. */
  shift[ 0 ] = lbnd0[ 0 ] - lbnd_out[ 0 ];
  shift[ 1 ] = lbnd0[ 1 ] - lbnd_out[ 1 ];
  if( shift[ 0 ] != 0.0 || shift[ 1 ] != 0.0 ) {
    astRemapFrame( *outframeset, AST__BASE, astShiftMap( 2, shift, " " ) );
  }

/* Report the pixel bounds of the cube. */
  if( *status == SAI__OK ) {
    msgOutif( MSG__NORM, " ", " ", status );
    msgSeti( "XL", lbnd_out[ 0 ] );
    msgSeti( "YL", lbnd_out[ 1 ] );
    msgSeti( "XU", ubnd_out[ 0 ] );
    msgSeti( "YU", ubnd_out[ 1 ] );
    msgOutif( MSG__NORM, " ", "   Output map pixel bounds: ( ^XL:^XU, ^YL:^YU )",
              status );

    if( ( ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1 ) > MAX_DIM ||
        ( ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1 ) > MAX_DIM ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": The map is too big. Check your list of input "
              "data files does not include widely separated observations.",
              status );
    }
  }

  /* If no error has occurred, export the returned FrameSet pointer from the
     current AST context so that it will not be annulled when the AST
     context is ended. Otherwise, ensure a null pointer is returned. */
  if( *status == SAI__OK ) {
    astExport( *outframeset );
  } else {
    *outframeset = astAnnul( *outframeset );
  }

  msgOutiff( SMF__TIMER_MSG, "",
             "Took %.3f s to calculate map bounds",
             status, smf_timerupdate( &tv1, &tv2, status ) );

  /* Clean Up */
 CLEANUP:
  if (*status != SAI__OK) {
    errRep(FUNC_NAME, "Unable to determine map bounds", status);
  }
  if (oskymap) oskymap  = astAnnul( oskymap );
  if (bolo2map) bolo2map = astAnnul( bolo2map );
  if (fitschan) fitschan = astAnnul( fitschan );

  if( data != NULL )
    smf_close_file( NULL, &data, status );

  refsys = astFree( refsys );

  astEnd;

}
