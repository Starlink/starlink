/*
*+
*  Name:
*     smf_cubegrid

*  Purpose:
*     Determine values for the spatial projection parameters.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_cubegrid( Grp *igrp,  int size, char *system, int usedetpos,
*                   int autogrid, int alignsys, Grp *detgrp, double par[ 7 ],
*                   int poserrfatal, float poserrmax, int *moving,
*                   AstSkyFrame **skyframe, int *sparse, int *gottsys,
*                   int *status );

*  Arguments:
*     igrp = Grp* (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     system = char* (Given)
*        Specifies the celestial coordinate system which will be used to
*        describe the spatial axes of the output cube. It should be a
*        valid value for the System attribute of an AST SkyFrame, or
*        "TRACKING".
*     usedetpos = int (Given)
*        If a non-zero value is supplied, then the detector positions for
*        a given time slice are read directly from the input NDF. Otherwise
*        the detector positions are calculated on the basis of the focal
*        plane detector positions and the telescope pointing information.
*     autogrid = int (Given)
*        Determines how the values returned in "par" are found. If autogrid
*        is non-zero, then projection parameters are determined by
*        adjusting the grid until as many data samples as possible fall
*        close to the centre of pixels in the output cube. If this automatic
*        determination fails, then the par[0] is returned holding AST__BAD.
*        If autogrid is zero, CRPIX1/2 are set to zero, CRVAL1/2 are set to
*        the first pointing BASE position, CROTA2 is set to zero, CDELT1/2
*        are set to 6 arc-seconds.
*     alignsys = int (Given)
*        If non-zero, then the input data will be aligned in the coordinate
*        system specified by "system" rather than in the default system
*        (ICRS).
*     detgrp = Grp* (Given)
*        A Group of detectors names to include in or exclude from the
*        output cube. If the first name begins with a minus sign then the
*        contents of the group are replaced on exit by the names of the
*        available detectors that were not originally included in the group.
*     par = double[ 7 ] (Returned)
*        An array holding the parameters describing the spatial projection
*        between celestial (longitude,latitude) in the system specified
*        by "system", and GRID coordinates in the output cube. These are
*        stored in the order CRPIX1, CRPIX2, CRVAL1, CRVAL2, CDELT1, CDELT2,
*        CROTA2. The CRPIX1 and CRPIX2 values are in units of pixels, and
*        all other values are in units of radians. The values refer to the
*        celestial coodinate represented by the returned SkyFrame. Returned
*        holding the values indicated by the "autogrid" argument.
*
*        If a NULL pointer is supplied for this parameter, then it is
*        assumed that the spatial projection for the output cube is
*        already known.
*     poserrfatal = int (Given)
*        If non-zero, an error is reported if there is significant
*        disagreement between the detector positions implied by the RECEPPOS
*        and FPLANEX/Y components of the ACSIS extension (otherwise, a mere
*        warning is issued).
*     poserrmax = float (Given)
*        The maximum separation allowed between RECEPPOS and FPLANEX/Y, in
*        arc-seconds.
*     moving = int* (Returned)
*        Address of an int in which to return a flag indicating if the
*        telescope is tracking a moving object. If so, the returned
*        SkyFrame will describe offsets (in the system specified by "system")
*        from the base pointing position for the first time slice.
*     skyframe = AstFrameSet ** (Returned)
*        A pointer to a location at which to return a pointer to an AST
*        SkyFrame describing the spatial axes of the output WCS FrameSet.
*        If "moving" is non-zero, the spatial axes represent (lon,lat)
*        offsets (in the requested "system") from the base telescope position
*        associated with the first time slice.
*     sparse = int* (Returned)
*        Should a sparse output cube be created?
*     gottsys = int* (Returned)
*        Were some valid Tsys values found in any of the files?
*     status = int* (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function finds the values to use for the parameters describing
*     the projection between celestial (longitude,latitude) and the
*     spatial pixel axes of the output cube. These are either obtained
*     from the user via some environment parameters, or estimated
*     automatically from the the distribution of input data points on the sky.
*
*     Also creates an output catalogue holding the sample positions. This
*     uses environment parameter OUTCAT to get the name of the catalogue.
*     In addition, the environment parameter EXTRACOLS can be used to
*     specify a set of extra columsn to add to the catalogue. The
*     parameter value should be a group of JCMTSTATE item names, and an
*     extra column will be created for each one.

*  Authors:
*     David S Berry (JAC, UCLan)
*     Tim Jenness (JAC, Hawaii)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     14-NOV-2006 (DSB):
*        Initial version.
*     20-NOV-2006 (DSB):
*        In OUTCAT, use monotonically increasing integer identifiers, and
*        store detector names as labels in the output catalogue.
*     21-NOV-2006 (DSB):
*        Set the SkyRef attribute in the returned SkyFrame to be the
*        tangent point.
*     22-NOV-2006 (DSB):
*        Correct the amount of memory allocated for "allpos".
*     30-NOV-2006 (DSB):
*        Returned AST__BAD values if the grid parameters cannot be
*        determined.
*     6-DEC-2006 (DSB):
*        Added "detgrp" parameter.
*     21-DEC-2006 (DSB):
*        - Normalise the sky coords at the ref position.
*        - Re-structure to handle moving targets correctly.
*     22-DEC-2006 (DSB):
*        Use a "regular grid" of size 1x1 if all the spectra are spatially
*        co-incident.
*     28-DEC-2006 (TIMJ):
*        Needed sf1 and sf2 even if output system was AZEL
*     8-JAN-2006 (DSB):
*        Increase pixel size used if all points are co-incident, to avoid
*        them spanning more than 1 pixel.
*     9-JAN-2006 (DSB):
*        Do not allow minimimum pixel size to be less than 0.1 arc-sec.
*     22-JAN-2007 (DSB):
*        Restructured again for better handing of moving targets.
*     25-JAN-2007 (DSB):
*        Add value for HIST parameter when calling kpg1Wrtab.
*     15-FEB-2007 (DSB):
*        Ensure SkyRef is set in returned SkyFrame, even if the target is
*        not moving.
*     20-FEB-2007 (DSB):
*        Change the check for target movement to take account of the
*        difference in epoch between the first and last time slices.
*     07-MAR-2007 (TIMJ):
*        Given pointing accuracy of JCMT change coincident check to
*        use 1 arcsec rather than 0.4.
*     07-MAR-2007 (TIMJ):
*        The check for tsys value in the input was not completed if the
*        main loop was exited early, resulting in the input tsys values
*        being ignored.
*     17-JUL-2007 (TIMJ):
*        Given pointing accuracy of JCMT change coincident check to
*        use 2 arcsec rather than 1.0.
*     9-AUG-2007 (DSB):
*        Check for co-incident positions even if the automatic grid
*        determination algorithm succeeds.
*     9-OCT-2007 (DSB):
*        Add a warning if specified detectors are not found in any input file.
*     25-OCT-2007 (DSB):
*        Use smf_checkdets to ensure detgrp holds detectors to be
*        included, not excluded.
*     29-OCT-2007 (EC):
*        Modified interface to smf_open_file.
*     18-DEC-2007 (AGG):
*        Update to use new smf_free behaviour
*     19-DEC-2007 (DSB):
*        Correct the way reference WCS is handled.
*     17-JAN-2008 (DSB):
*        Added argument alignsys.
*     27-MAR-2008 (DSB):
*        Double the pixel size used when all points are deemed to be
*        co-incident, in order to reduce the risk of points ending up in
*        adjacent pixels.
*     28-MAR-2008 (DSB):
*        Use the size of the Airy disk (which is a function of local
*        oscillator frequency) as the criterion for all points being
*        co-incident.
*     08-APR-2008 (TIMJ):
*        Use tcs_tai instead of rts_end for position calculations.
*     20-MAY-2008 (DSB):
*        If autogrid is false, use a default CROTA value determined by
*        the MAP_PA FITS header.
*     03-JUN-2008 (TIMJ):
*        Factor out printing of projection parameters.
*     04-JUN-2008 (TIMJ):
*        Remove big chunks of code into smf_get_projpar and smf_calc_skyframe
*     11-FEB-2009 (DSB):
*        Ignore negative or zero input Tsys values.
*     15-APR-2009 (DSB):
*        Issue a warning if the REFCEPPOS and FPLANEX/Y positions are
*        inconsistent.
*     2-JUL-2009 (DSB):
*        Add facility for adding extra columns to the output catalogue as
*        specified by environment parameter EXTRACOLS.
*     26-NOV-2013 (DSB):
*        Add "poserrfatal" parameter.
*     16-OCT-2015 (DSB):
*        Use smf_set_moving to assign attributes for a moving target,
*        rather than just setting SkyRefIs (smf_set_moving also sets
*        AlignOffset).
*     11-MAY-2017 (DSB):
*        Add argument poserrmax.
*     15-OCT-2022 (GSB):
*        Add check of jos_drcontrol position problem flag.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006,2007 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2008-2013 Science & Technology Facilities Council.
*     Copyright (C) 2017 East Asian Observatory.
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
#include <string.h>
#include <ctype.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "par.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/ndg.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_cubegrid"


void smf_cubegrid( Grp *igrp,  int size, char *system, int usedetpos,
                   int autogrid, int alignsys, Grp *detgrp, double par[ 7 ],
                   int poserrfatal, float poserrmax, int *moving,
                   AstSkyFrame **skyframe, int *sparse, int *gottsys,
                   int *status ){

/* Local Variables */
   AstFrame *sf1 = NULL;      /* Spatial Frame representing AZEL system */
   AstFrame *skyin = NULL;    /* Sky Frame in input FrameSet */
   AstFrameSet *fs = NULL;    /* A general purpose FrameSet pointer */
   AstFrameSet *swcsin = NULL;/* FrameSet describing spatial input WCS */
   AstKeyMap *cols_km = NULL; /* A KeyMap holding values for extra columns */
   AstMapping *azel2usesys = NULL; /* Mapping form AZEL to requested system */
   AstMapping *fsmap = NULL;  /* Mapping from the "fs" FrameSet */
   AstMapping *tmap = NULL;   /* Temporary Mapping */
   Grp *colgrp = NULL;   /* Group holding names of extra catalogue columns */
   Grp *labgrp = NULL;   /* Group holding used detector labels */
   char outcatnam[ 41 ]; /* Output catalogue name */
   const char *lab = NULL;/* Pointer to start of next detector name */
   const double *tsys;   /* Pointer to Tsys value for first detector */
   double *allpos = NULL;/* Array of all sample positions */
   double *allpos2 = NULL;/* Array of all sample positions */
   double *p;            /* Pointer to next value */
   double *px;           /* Pointer to next value */
   double *py;           /* Pointer to next value */
   double *xin = NULL;   /* Workspace for detector input grid positions */
   double *xout = NULL;  /* Workspace for detector output pixel positions */
   double *yin = NULL;   /* Workspace for detector input grid positions */
   double *yout = NULL;  /* Workspace for detector output pixel positions */
   double a;             /* Longitude value */
   double b;             /* Latitude value */
   double map_pa=0;      /* Map position angle in output coord system (rads) */
   double skyref[ 2 ];   /* Values for output SkyFrame SkyRef attribute */
   float *pdata;         /* Pointer to next data sample */
   float rtsys;          /* Tsys value */
   float telres=0;       /* Telescope resolution in radians */
   int good;             /* Are there any good detector samples? */
   int ibasein;          /* Index of base Frame in input FrameSet */
   int ifile;            /* Index of current input file */
   int ipar;             /* Parameter index */
   int ipos;             /* Position index */
   int nallpos;          /* Number of positions */
   int outcat;           /* Produce an output catalogue holding sample positions? */
   int useauto;          /* Are autogrid default projection parameters being used? */
   dim_t irec;           /* Index of current input detector */
   dim_t itime;          /* Index of current time slice */
   dim_t ispec;          /* Index of current spectral sample */
   size_t found;         /* Was current detector name found in detgrp? */
   size_t nexcol;        /* Number of extra columsn for catalogue */
   smfData *data = NULL; /* Pointer to data struct for current input file */
   smfFile *file = NULL; /* Pointer to file struct for current input file */
   smfHead *hdr = NULL;  /* Pointer to data header for this time slice */
   void *cols_info;      /* Data used inside smf_extracols */
   drcntrl_bits drcntrl_mask = DRCNTRL__TCS_POSN_BIT; /* Mask to use for DRCONTROL */

/* Initialise the returned array to hold vad values. */
   if( par ) {
      for( ipar = 0; ipar < 7; ipar++ ) par[ ipar ] = AST__BAD;
   }
   *skyframe = NULL;
   *moving = 0;
   *gottsys = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Tell the user what is happening. */
   msgOutif( MSG__VERB, " ", "SMURF_MAKECUBE: Determining projection parameters", status );

/* Begin an AST context. */
   astBegin;

/* See if an output catalogue holding the sky positions at every detector
   sample is to be produced. */
   parGet0c( "OUTCAT", outcatnam, 40, status );
   if( *status == PAR__NULL ) {
      errAnnul( status );
      outcat = 0;
   } else {
      outcat = 1;
   }

/* Initialise. */
   allpos = NULL;
   nallpos = 0;
   cols_info = NULL;

/* If we are creating an output catalogue, create a GRP group to hold the
   labels to be associated with each position, and get a group of extra
   column names from the environment. */
   if( outcat && *status == SAI__OK ) {
      labgrp = grpNew( "Detector labels", status );
      kpg1Gtgrp( "EXTRACOLS", &colgrp, &nexcol, status );
      if( *status == PAR__NULL ) errAnnul( status );
      if( nexcol == 0 && colgrp ) grpDelet( &colgrp, status );
   } else {
      labgrp = NULL;
      colgrp = NULL;
   }

/* Loop round all the input NDFs. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Obtain information about the current input NDF. */
      smf_open_file( NULL, igrp, ifile, "READ", 0, &data, status );

/* Issue a suitable message and abort if anything went wrong. */
      if( *status != SAI__OK ) {
         msgSeti( "I", ifile );
         errRep( FUNC_NAME, "Could not open input data file no. ^I.", status );
         break;

      } else {
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

/* Check that the detector sky positions implied by the RECEPPOS and FPLANEX/Y
   values in the file are consistent. Issue a warning message if not, but
   then continue. */
      (void) smf_check_detpos( data, poserrmax, poserrfatal ? -1 : 1, status );

/* Get some convenient pointers. */
      file = data->file;
      hdr = data->hdr;

/* Report the name of the input file. */
      smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
      msgSeti( "I", ifile );
      msgSeti( "N", size );
      msgOutif( MSG__VERB, " ", "SMF_CUBEGRID: Processing ^I/^N ^FILE",
                     status );

/* Make sure the input file is a suitable ACSIS cube. */
      if( hdr->instrument != INST__ACSIS ) {
         smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "^FILE does not contain ACSIS instrument data.",
                     status );
         break;
      }

/* Check that there are 3 pixel axes. */
      if( data->ndims != 3 ) {
         smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
         msgSeti( "NDIMS", data->ndims );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "^FILE has ^NDIMS pixel axes, should be 3.",
                     status );
         break;
      }

/* Calculate the telescope resolution (in radians) is this has not already
   been done. */
      if( ifile == 1 ) telres = smf_calc_telres( hdr->fitshdr, status )
                                   *AST__DD2R/3600.0;

/* If the detector positions are to calculated on the basis of FPLANEX/Y
   rather than RECEPPOS, then free the detpos array in the smfHead
   structure. This will cause smf_tslice_ast to use the fplanex/y values. */
      if( !usedetpos && hdr->detpos ) {
         hdr->detpos = astFree( (double *) hdr->detpos );
      }

/* Extend the work arrays so that they are big enough to hold the coords
   of all the detectors in the current input file. */
      xin = astGrow( xin, (data->dims)[ 1 ], sizeof( double ) );
      yin = astGrow( yin, (data->dims)[ 1 ], sizeof( double ) );
      xout = astGrow( xout, (data->dims)[ 1 ], sizeof( double ) );
      yout = astGrow( yout, (data->dims)[ 1 ], sizeof( double ) );

/* Store the input GRID coords of the detectors. */
      if( *status == SAI__OK ) {
         for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {
            xin[ irec ] = irec + 1.0;
            yin[ irec ] = 1.0;
         }
      }

/* Extend the memory used to hold the list of all receptor positions.
   Make it the maximum size that could be needed - less will be used if
   some of the receptors are bad. Each position needs 2 doubles (one for
   X and one for Y). */
      allpos = astGrow( allpos, 2*( nallpos + (data->dims)[ 2 ]*
                                                   (data->dims)[ 1 ] ),
                                 sizeof( double ) );

/* Issue a warning if any of the detector names specified in "detgrp"
   were not found in the data. If the supplied group holds the detectors
   to be excluded, modify it so that it holds the detectors to be
   included. */
      smf_checkdets( detgrp, data, status );

/* Store a pointer to the next input data value */
      pdata = ( data->pntr )[ 0 ];

/* Loop round all the time slices in the input file. */
      for( itime = 0; itime < (data->dims)[ 2 ] && *status == SAI__OK; itime++ ) {

/* Skip this time slice if flagged due to a position problem. */
          if( (hdr->allState)[itime].jos_drcontrol & drcntrl_mask ) {
             continue;
          }

/* Get a FrameSet describing the spatial coordinate systems associated with
   the current time slice of the current input data file. The base frame in
   the FrameSet will be a 2D Frame in which axis 1 is detector number and
   axis 2 is unused. The current Frame will be a SkyFrame (the SkyFrame
   System may be any of the JCMT supported systems). The Epoch will be
   set to the epoch of the time slice. */
         smf_tslice_ast( data, itime, 1, NO_FTS, status );
         swcsin = hdr->wcs;

/* Get a pointer to the current WCS Frame in the input file. */
         skyin = astGetFrame( swcsin, AST__CURRENT );

/* If we have not yet created the output SkyFrame, do so now. */
         if( ! *skyframe ) {
           smf_calc_skyframe( skyin, system, hdr, alignsys, skyframe, skyref,
                              moving, status );

/* Get the orientation of the map vertical within the output celestial
   coordinate system. This is derived form the MAP_PA FITS header, which
   gives the orientation of the map vertical within the tracking system. */
           map_pa = smf_calc_mappa( hdr, system, skyin, status );

/* If we do not need to look at any other time slices, we can leave the loop
   early. */
            if( !autogrid && !outcat && *gottsys ) break;
         }

/* Get a FrameSet ("fs") connecting the base Frame in the input WCS FrameSet
   (GRID coords) to the returned SkyFrame. */
         astInvert( swcsin );
         ibasein = astGetI( swcsin, "Base" );
         fs = astConvert( swcsin, *skyframe, "SKY" );
         astSetI( swcsin, "Base", ibasein );
         astInvert( swcsin );

         if( fs == NULL ) {
            if( *status == SAI__OK ) {
               smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "The spatial coordinate system in ^FILE "
                           "is not compatible with the spatial coordinate "
                           "system in the first input file.", status );
            }
            break;
         }

/* Get a pointer to the start of the Tsys values for this time slice. */
         tsys = hdr->tsys ? hdr->tsys + hdr->ndet*itime : NULL;

/* The "fs" FrameSet goes from GRID to absolute coords in the requested
   system. If the source is moving, we now adjust this FrameSet so that,
   instead of going to absolute coords in the requested system, it goes to
   offsets from the current telescope base pointing position in the
   current system. */
         if( *moving ){

/* Get the Mapping from AZEL (at the current input epoch) to the output
   sky system. Use it to convert the telescope base pointing position from
   (az,el) to the requested system. */
            sf1 = astCopy( skyin );
            astSetC( sf1, "System", "AZEL" );
            azel2usesys = astConvert( sf1, *skyframe, "" );
            tmap = astGetMapping( azel2usesys, AST__BASE, AST__CURRENT );
            (void) astAnnul( azel2usesys );
            azel2usesys = astSimplify( tmap );
            tmap = astAnnul( tmap );

            astTran2( azel2usesys, 1, &(hdr->state->tcs_az_bc1),
                           &(hdr->state->tcs_az_bc2), 1, &a, &b );

/* Explicitly annul these objects for efficiency in this tight loop. */
            azel2usesys = astAnnul( azel2usesys );
            sf1 = astAnnul( sf1 );

/* Modified the FrameSet to represent offsets from this origin. We use the
   FrameSet pointer "fs" rather than a pointer to the current Frame within
   the FrameSet. This means that the Mapping in the FrameSet will be
   modified to remap the current Frame. */
            astSetD( fs, "SkyRef(1)", a );
            astSetD( fs, "SkyRef(2)", b );
            smf_set_moving( (AstFrame *) fs, NULL, status );

/* Get the Mapping and then clear the SkyRef attributes (this is because
   the current Frame in "fs" may be "*skyframe" and we do not want to make a
   permanent change to *skyframe). */
            fsmap = astGetMapping( fs, AST__BASE, AST__CURRENT );
            astClear( fs, "SkyRef(1)" );
            astClear( fs, "SkyRef(2)" );
            astClear( fs, "SkyRefIs" );

/* If the target is not moving, just get the Mapping. */
         } else {
            fsmap = astGetMapping( fs, AST__BASE, AST__CURRENT );
         }

/* Simplify the Mapping, and annul the original Mapping. */
         tmap = astSimplify( fsmap );
         (void) astAnnul( fsmap );
         fsmap = tmap;

/* Transform the positions of the detectors from input GRID to output SKY
   coords (or offset coords if the target is moving). */
         astTran2( fsmap, (data->dims)[ 1 ], xin, yin, 1, xout, yout );

/* Copy usable sky positions into the array holding all positions. */
         p = allpos + 2*nallpos;
         lab = hdr->detname;
         for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {

/* See if this detector is included in the group of detectors to be used. */
            if( detgrp ) {
               found = grpIndex( lab, detgrp, 1, status );
            } else {
               found = 1;
            }

/* If it is, and if the detector has a valid position, see if it produced
   any good data values. */
            if( found && xout[ irec ] != AST__BAD &&
                  yout[ irec ] != AST__BAD ) {
               good = 0;
               for( ispec = 0; ispec < (data->dims)[ 0 ]; ispec++ ){
                  if( *(pdata++) != VAL__BADR ) {
                     good = 1;
                     pdata += (data->dims)[ 0 ] - ispec - 1;
                     break;
                  }
               }

/* If it did, store it. Also, see if this detector has a good Tsys value.
   Also, store any extra columns values requested for the outptu
   catalogue. */
               if( good ) {
                  *(p++) = xout[ irec ];
                  *(p++) = yout[ irec ];
                  if( labgrp ) grpPut1( labgrp, lab, 0, status );
                  nallpos++;

                  rtsys = tsys ? (float) tsys[ irec ] : VAL__BADR;
                  if( rtsys <= 0.0 ) rtsys = VAL__BADR;
                  if( rtsys != VAL__BADR ) *gottsys = 1;

                  smf_extracols( hdr, colgrp, &cols_info, NULL, status );
               }

/* If this detector does not have a valid position, increment the data
   pointer to point at the first sample for the next detector. */
            } else {
               pdata += (data->dims)[ 0 ];
            }

/* Move on to the start of the next detector name. */
            lab += strlen( lab ) + 1;

         }

/* For efficiency, explicitly annul the AST Objects created in this tight
   loop. */
         skyin = astAnnul( skyin );
         fs = astAnnul( fs );
         fsmap = astAnnul( fsmap );
      }

/* Close the current input data file. */
      smf_close_file( NULL, &data, status);
      data = NULL;

/* If we do not need to look at any other files, we can leave the loop
   early. */
      if( *skyframe && !autogrid && !outcat && *gottsys ) break;
   }

/* Close any data file that was left open due to an early exit from the
   above loop. */
   if( data != NULL ) smf_close_file( NULL, &data, status );

/* Calculate projection parameters. */
   smf_get_projpar( *skyframe, skyref, *moving, autogrid, nallpos,
                    allpos, telres, map_pa, par, sparse, &useauto, status );


/* If creating an output catalogue, re-order the array containing the
   positions so that all the longitude values come at the start of the
   array, followed by all the latitude values. */
   if( outcat && *status == SAI__OK ) {
      allpos2 = astMalloc( sizeof( double )*2*nallpos );
      px = allpos2;
      py = allpos2 + nallpos;
      p = allpos;

      for( ipos = 0; ipos < nallpos; ipos++ ) {
         *(px++) = *(p++);
         *(py++) = *(p++);
      }

/* Create a KeyMap holding the values for any extra requested columns,
   and free the resources used by cols_info. */
      smf_extracols( NULL, colgrp, &cols_info, &cols_km, status );

/* Create the catalogue. */
      kpg1Wrcat( "OUTCAT", nallpos, nallpos, 2, allpos2, AST__CURRENT,
                 astFrameSet( *skyframe, " " ), "Detector positions", 1,
                 NULL, cols_km, labgrp, NULL, 1, status );

/* Free resources. */
      allpos2 = astFree( allpos2 );
   }

/* Free work space. */
   allpos = astFree( allpos );
   xin = astFree( xin );
   yin = astFree( yin );
   xout = astFree( xout );
   yout = astFree( yout );
   if( labgrp ) grpDelet( &labgrp, status );
   if( colgrp ) grpDelet( &colgrp, status );

/* If no error has occurred, export the returned SkyFrame pointer from the
   current AST context so that it will not be annulled when the AST
   context is ended. Otherwise, ensure a null pointer is returned. */
   if( *status == SAI__OK ) {
      astExport( *skyframe );
   } else {
      *skyframe = astAnnul( *skyframe );
   }

/* End the AST context. This will annul all AST objects created within the
   context (except for those that have been exported from the context). */
   astEnd;

/* Issue a context message if anything went wrong. */
   if( *status != SAI__OK ) errRep( FUNC_NAME, "Unable to determine grid "
                                                     "parameters", status );
}
