/*
*+
*  Name:
*     smf_cubebounds

*  Purpose:
*     Calculate the pixel index bounds for a cube 

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_cubebounds( Grp *igrp,  int size, AstSkyFrame *oskyframe, 
*                     int autogrid, int usedetpos, double par[ 7 ], 
*                     int moving, int lbnd[ 3 ], int ubnd[ 3 ], 
*                     AstFrameSet **wcsout, int *npos, int *status );

*  Arguments:
*     igrp = Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     oskyframe = AstSkyFrame * (Given)
*        A SkyFrame that specifies the coordinate system used to describe the 
*        spatial axes of the output cube. 
*     autogrid = int (Given)
*        If non-zero then the fractional pixel shift implied by elements
*        0 and 1 of "par" are retained. Otherwise, they are ignored and 
*        the used shift puts the central axis value at the centre of a
*        pixel.
*     usedetpos = int (Given)
*        If a non-zero value is supplied, then the detector positions for
*        a given time slice are read directly from the input NDF. Otherwise 
*        the detector positions are calculated on the basis of the focal
*        plane detector positions and the telescope pointing information.
*     par = double[ 7 ] (Given)
*        An array holding the parameters describing the spatial projection
*        between celestial (longitude,latitude) in the system specified
*        by "oskyframe", and GRID coordinates in the output cube. These are
*        stored in the order CRPIX1, CRPIX2, CRVAL1, CRVAL2, CDELT1, CDELT2, 
*        CROTA2. The supplied values are used to produce the output WCS 
*        FrameSet. All the angular parameters are in units of radians,
*        and CRPIX1/2 are in units of pixels. 
*     moving = int (Given)
*        A flag indicating if the telescope is tracking a moving object. If 
*        so, each time slice is shifted so that the position specified by 
*        TCS_AZ_BC1/2 is mapped on to the same pixel position in the
*        output cube.
*     lbnd = int [ 3 ] (Returned)
*        The lower pixel index bounds of the output cube.
*     ubnd = int [ 3 ] (Returned)
*        The upper pixel index bounds of the output cube.
*     wcsout = AstFrameSet ** (Returned)
*        A pointer to a location at which to return a pointer to an AST 
*        Frameset describing the WCS to be associated with the output cube.
*     npos = int * (Returned)
*        Address of an int in which to return the number of good spatial data
*        positions that will be used in the output cube.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function finds the pixel index bounds of the 3D output cube that 
*     will just encompass all the data in the supplied group of input NDFs. 
*     Each input NDF should be an ACSIS archive file. A WCS FrameSet is also
*     returned for the output cube. The base Frame in this FrameSet is 3D
*     GRID coords in the cube, and the current Frame is a CmpFrame holding
*     (lon,lat,freq) axes, where "lon,lat" are (if "moving" is zero) celestial 
*     longitude and latitude in the system specified by "system". The spatial
*     projection in the cube is a tangent plane projection defined by 
*     "par". The spectral axis system and projection are inherited from the 
*     first supplied input data file.
*
*     If "moving" is non-zero, the spatial axes represent (lon,lat) offsets 
*     in the requested output frame from the base telescope position associated
*     with the first time slice.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     19-SEP-2006 (DSB):
*        Initial version.
*     13-OCT-2006 (DSB):
*        Changed to get the input spectral WCS from the WCS FrameSet rather 
*        than the FITS header.
*     1-NOV-2006 (DSB):
*        Use new smf_makefitschan interface.
*     14-NOV-2006 (DSB):
*        - Exclude bad data values from the bounding box. 
*        - Move catalogue creation to smf_cubegrid.
*        - New interface.
*     21-NOV-2006 (DSB):
*        Correct inversion of ospecmap so that it combines correctly with
*        specmap.
*     23-NOV-2006 (DSB):
*        Correct indexing of "specin" array.
*     29-NOV-2006 (DSB):
*        Allow user to restrict the spectral range of the output cube
*        using parameter SPECBOUNDS.
*     1-DEC-2006 (DSB):
*        Correct memory leak.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

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
#include <string.h>
#include <ctype.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/ndg.h"
#include "star/slalib.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_cubebounds"

/* Returns nearest integer to "x" */
#define NINT(x) ( ( x > 0 ) ? (int)( x + 0.5 ) : (int)( x - 0.5 ) )

void smf_cubebounds( Grp *igrp,  int size, AstSkyFrame *oskyframe, 
                     int autogrid, int usedetpos, double par[ 7 ], 
                     int moving, int lbnd[ 3 ], int ubnd[ 3 ], 
                     AstFrameSet **wcsout, int *npos, int *status ){

/* Local Variables */
   AstCmpFrame *cmpfrm = NULL;  /* Current Frame for output FrameSet */
   AstCmpMap *cmpmap = NULL;    /* Base -> Current Mapping for output FrameSet */
   AstCmpMap *ssmap = NULL;     /* I/p GRID-> o/p PIXEL Mapping for spectral axis */
   AstFitsChan *fc = NULL;      /* FitsChan used to construct spectral WCS */
   AstFitsChan *fct = NULL;     /* FitsChan used to construct time slice WCS */
   AstFrame *oskyframe2 = NULL; /* Copy of output skyframe */
   AstFrame *ospecframe = NULL; /* Spectral Frame in output FrameSet */
   AstFrame *specframe = NULL;  /* Spectral Frame in input FrameSet */
   AstFrameSet *fs = NULL;      /* A general purpose FrameSet pointer */
   AstFrameSet *swcsin = NULL;  /* FrameSet describing spatial input WCS */
   AstFrameSet *swcsout = NULL; /* FrameSet describing spatial output WCS */
   AstMapping *fsmap = NULL;    /* Base->Current Mapping extracted from a FrameSet */
   AstMapping *oskymap = NULL;  /* Sky <> PIXEL mapping in output FrameSet */
   AstMapping *ospecmap = NULL; /* Spec <> PIXEL mapping in output FrameSet */
   AstMapping *specmap = NULL;  /* PIXEL -> Spec mapping in input FrameSet */
   char *pname = NULL;   /* Name of currently opened data file */
   double *xin = NULL;   /* Workspace for detector input grid positions */
   double *xout = NULL;  /* Workspace for detector output pixel positions */
   double *yin = NULL;   /* Workspace for detector input grid positions */
   double *yout = NULL;  /* Workspace for detector output pixel positions */
   double dlbnd[ 3 ];    /* Floating point lower bounds for output cube */
   double dubnd[ 3 ];    /* Floating point upper bounds for output cube */
   double ispecbounds[ 2 ];   /* Bounds of spectral axis in grid pixels */
   double shift[ 3 ];    /* Shifts from PIXEL to GRID coords */
   double specbounds[ 2 ];      /* Bounds of spectral axis in spectral WCS units */
   double specin[ 2];    /* Spectral values to be transformed */
   double specout[ 2];   /* Transformed spectral values */
   float *pdata;         /* Pointer to next data sample */
   int good;             /* Are there any good detector samples? */
   int ibasein;          /* Index of base Frame in input FrameSet */
   int ifile;            /* Index of current input file */
   int irec;             /* Index of current input detector */
   int ishift;           /* Shift to put pixel origin at centre */
   int ispec;            /* Index of current spectral sample */
   int itime;            /* Index of current time slice */
   int iwcsfrm;          /* Index of original output WCS Frame */
   int npix;             /* Number of pixels along axis */
   int nval;             /* Number of values supplied */
   int pixax[ 3 ];       /* The output fed by each selected mapping input */
   int specax;           /* Index of spectral axis in input FrameSet */
   smfData *data = NULL; /* Pointer to data struct for current input file */
   smfFile *file = NULL; /* Pointer to file struct for current input file */
   smfHead *hdr = NULL;  /* Pointer to data header for this time slice */

/* Initialise the number of data samples */
   *npos = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Tell the user what is happening. */
   msgOutif( MSG__VERB, " ", "SMURF_MAKECUBE: Determine cube bounds", status );

/* Begin an AST context. */
   astBegin;

/* Initialise the bounds of the output cube in floating point PIXEL coords. */
   dlbnd[ 0 ] = VAL__MAXD;
   dlbnd[ 1 ] = VAL__MAXD;
   dlbnd[ 2 ] = VAL__MAXD;
   dubnd[ 0 ] = VAL__MIND;
   dubnd[ 1 ] = VAL__MIND;
   dubnd[ 2 ] = VAL__MIND;

/* Create an empty FitsChan that can be used for creating mappings from a
   given set of FITS-WCS keyword values. */
   fct = astFitsChan ( NULL, NULL, "" );

/* Loop round all the input NDFs. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Obtain information about the current input NDF. */
      smf_open_file( igrp, ifile, "READ", 1, &data, status );

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

/* Get some convenient pointers. */
      file = data->file;
      hdr = data->hdr;

/* Report the name of the input file. */
      pname =  file->name;
      msgSetc( "FILE", pname );
      msgSeti( "I", ifile );
      msgSeti( "N", size );
      msgOutif( MSG__VERB, " ", "SMF_CUBEBOUNDS: Processing ^I/^N ^FILE", 
                status );

/* Make sure the input file is a suitable ACSIS cube. */
      if( hdr->instrument != INST__ACSIS ) {
         msgSetc( "FILE", pname );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "^FILE does not contain ACSIS instrument data.", 
                 status );
         break;
      }

/* Check that there are 3 pixel axes. */
      if( data->ndims != 3 ) {
         msgSetc( "FILE", pname );
         msgSeti( "NDIMS", data->ndims );
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "^FILE has ^NDIMS pixel axes, should be 3.", 
                 status );
         break;
      }

/* If the detector positions are to calculated on the basis of FPLANEX/Y
   rather than RECEPPOS, then free the detpos array in the smfHead
   structure. This will cause smf_tslice_ast to use the fplanex/y values. */
      if( !usedetpos && hdr->detpos ) {
         smf_free( (double *) hdr->detpos, status );      
         hdr->detpos = NULL;
      }

/* We want a description of the spectral WCS axis in the input file. If 
   the input file has a WCS FrameSet containing a SpecFrame, use it,
   otherwise we will obtain it from the FITS header later. NOTE, if we knew 
   that all the input NDFs would have the same spectral axis calibration, 
   then the spectral WCS need only be obtained from the first NDF. However, 
   in the general case, I presume that data files may be combined that use 
   different spectral axis calibrations, and so these differences need to 
   be taken into account. */
      if( hdr->tswcs ) {   
         fs = astClone( hdr->tswcs );
   
/* The first axis should be a SpecFrame. See if this is so. If not annul
   the specframe pointer. */
         specax = 1;
         specframe = astPickAxes( fs, 1, &specax, NULL );
         if( !astIsASpecFrame( specframe ) ) specframe = astAnnul( specframe );
      } 

/* If the above did not yield a SpecFrame, use the FITS-WCS headers in the 
   FITS extension of the input NDF. Take a copy of the FITS header (so that 
   the contents of the header are not changed), and then read a FrameSet 
   out of it. */
      if( !specframe ) {
         fc = astCopy( hdr->fitshdr );
         astClear( fc, "Card" );
         fs = astRead( fc );
   
         if( !fs ) {
            if( *status == SAI__OK ) {
               msgSetc( "FILE", pname );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "Cannot read WCS information from "
                       "the FITS header in ^FILE.", status );
            }
            break;
         } 

/* Extract the SpecFrame that describes the spectral axis from the current 
   Frame of this FrameSet. This is assumed to be the third WCS axis (NB
   the different axis number). */
         specax = 3;
         specframe = astPickAxes( fs, 1, &specax, NULL );
         if( !astIsASpecFrame( specframe ) ) {
            if( *status == SAI__OK ) {
               msgSetc( "FILE", pname );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "FITS-WCS axis 1 in ^FILE is not a spectral "
                       "axis.", status );
            }
            break;
         }
      }

/* Split off the 1D Mapping for this single axis from the 3D Mapping for
   the whole WCS. This results in "specmap" holding the Mapping from 
   SpecFrame value to GRID value. */
      fsmap = astGetMapping( fs, AST__CURRENT, AST__BASE );
      astMapSplit( fsmap, 1, &specax, pixax, &specmap );
      if( !specmap || astGetI( specmap, "Nout" ) != 1 ) {
         if( *status == SAI__OK ) {
            msgSetc( "FILE", pname );
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "The spectral axis in ^FILE is not "
                    "independent of the other axes.", status );
         }
         break;
      }

/* Invert the Mapping for the spectral axis so that it goes from GRID
   coord to spectral coord. */
      astInvert( specmap );

/* The spectral axis of the output cube is inherited from the spectral axis
   of the first input file. So, if this is the first input file, initialise 
   the bounds of the spectral GRID axis in the output cube. */
      if( !ospecframe ) {
         dlbnd[ 2 ] = 1.0;
         dubnd[ 2 ] = (data->dims)[ 0 ];

/* If this is not the first input file, then there is potentially a
   difference between the spectral system of this input file and the spectral
   system of the output cube. So use astConvert to get a Mapping from one
   to the other. */
      } else {
         fs = astConvert( specframe, ospecframe, "" );

/* Report an error and abort if no conversion could be found between the two 
   spectral axes. */
         if( !fs ) {
            if( *status == SAI__OK ) {
               msgSetc( "FILE", pname );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "The spectral axis in ^FILE is not "
                       "compatible with the spectral axis in the first "
                       "input file.", status );
            }
            break;

/* Otherwise, combine these Mappings to get the Mapping from the input 
   spectral GRID axis to the output spectral PIXEL axis. Note, "ospecmap"
   represents the Mapping from the output spectral WCS axis to the
   corresponding output PIXEL axis. */
         } else {
            ssmap = astCmpMap( astCmpMap( specmap, 
                                          astGetMapping( fs, AST__BASE,
                                                         AST__CURRENT ),
                                          1, "" ), 
                               ospecmap, 1, "" );

         }

/* Use this Mapping to transform the first and last spectral GRID values
   in the input into the corresponding values on the output spectral PIXEL
   axis. */
         specin[ 0 ] = 1.0;
         specin[ 1 ] = (data->dims)[ 0 ];
         astTran1( ssmap, 2, specin, 1, specout );

/* Update the bounds of the output cube on the spectral PIXEL axis. */
         if( specout[ 0 ] < dlbnd[ 2 ] ) dlbnd[ 2 ] = specout[ 0 ];
         if( specout[ 0 ] > dubnd[ 2 ] ) dubnd[ 2 ] = specout[ 0 ];
         if( specout[ 1 ] < dlbnd[ 2 ] ) dlbnd[ 2 ] = specout[ 1 ];
         if( specout[ 1 ] > dubnd[ 2 ] ) dubnd[ 2 ] = specout[ 1 ];
      }

/* Allocate work arrays big enough to hold the coords of all the
   detectors in the current input file.*/
      xin = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
      yin = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
      xout = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
      yout = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );

/* Store the input GRID coords of the detectors. */
      for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {
         xin[ irec ] = irec + 1.0;
         yin[ irec ] = 1.0;
      }

/* Store a pointer to the next input data value */
      pdata = ( data->pntr )[ 0 ];

/* We now need to determine the spatial extent of the input file, and
   then modify the spatial bounds of the output cube to accomodate it. 
   This involves finding the spatial extent of each time slice in the 
   input. Loop round all the time slices in the input file. */
      for( itime = 0; itime < (data->dims)[ 2 ] && *status == SAI__OK; itime++ ) {

/* Get a FrameSet describing the spatial coordinate systems associated with 
   the current time slice of the current input data file. The base frame in 
   the FrameSet will be a 2D Frame in which axis 1 is detector number and 
   axis 2 is unused. The current Frame will be a SkyFrame (the SkyFrame 
   System may be any of the JCMT supported systems). The Epoch will be
   set to the epoch of the time slice. */
         smf_tslice_ast( data, itime, 1, status );
         swcsin = hdr->wcs;

/* Create a FrameSet describing the WCS to be associated with the output 
   cube unless this has already be done. */
         if( *wcsout == NULL ) { 

/* The spectral Mapping (from GRID to SPECTRUM) and Frame (a SpecFrame)
   are inherited from the first input file. */
            ospecframe = astClone( specframe );
            ospecmap = astClone( specmap );

/* Ensure the specframe has the same epoch as the supplied SkyFrame. */
            if( astTest( oskyframe, "Epoch" ) ) {
               astSetC( ospecframe, "Epoch", astGetC( oskyframe, "Epoch" ) ); 
            }           

/* Now populate a FitsChan with FITS-WCS headers describing the required 
   tan plane projection. The longitude and latitude axis types are
   arbitrarily set to RA and Dec. The projection parameters are supplied 
   in "par". Convert from radians to degrees as required by FITS. */
            astSetFitsS( fct, "CTYPE1", "RA---TAN", " ", 1 );
            astSetFitsS( fct, "CTYPE2", "DEC--TAN", " ", 1 );
            astSetFitsF( fct, "CRPIX1", par[ 0 ], " ", 1 );
            astSetFitsF( fct, "CRPIX2", par[ 1 ], " ", 1 );
            astSetFitsF( fct, "CRVAL1", par[ 2 ]*AST__DR2D, " ", 1 );
            astSetFitsF( fct, "CRVAL2", par[ 3 ]*AST__DR2D, " ", 1 );
            astSetFitsF( fct, "CDELT1", par[ 4 ]*AST__DR2D, " ", 1 );
            astSetFitsF( fct, "CDELT2", par[ 5 ]*AST__DR2D, " ", 1 );
            astSetFitsF( fct, "CROTA2", par[ 6 ]*AST__DR2D, " ", 1 );

/* Read a FrameSet from this FitsChan. */
	    astClear( fct, "Card" );
            fs = astRead( fct );

/* Extract the output PIXEL->SKY Mapping. */
            oskymap = astGetMapping( fs, AST__BASE, AST__CURRENT );

/* Ensure that the returned SkyFrame represents absolute coords rather than 
   offsets. */
            astClear( oskyframe, "SkyRefIs" );
            astClear( oskyframe, "AlignOffset" );

/* Construct the CmpFrame that will be used as the current Frame in the 
   output cube WCS FrameSet. */
            cmpfrm = astCmpFrame( oskyframe, ospecframe, "" );

/* Construct the corresponding Mapping (from PIXEL coords to the above
   CmpFrame). The PIXEL origin on the spectral axis is 1, meaning that
   GRID and PIXEL coords are equivalent for the spectral axis. */
            cmpmap = astCmpMap( oskymap, ospecmap, 0, "" );

/* Create the returned output cube WCS FrameSet, initialising it to hold a 3D 
   PIXEL Frame. A GRID Frame will be added later. */
            *wcsout = astFrameSet( astFrame( 3, "Domain=PIXEL" ), "" );

/* Add the CmpFrame created above into the new FrameSet, using the above
   Mapping to join it to the 3D PIXEL Frame already in the FrameSet. */
            astAddFrame( *wcsout, AST__BASE, cmpmap, cmpfrm );

/* For later convenience, we create another FrameSet describing just the
   spatial axes of the output. We take a copy of the SkyFrame so that any
   changes we make to the SkyFrame via the "swcsout" pointer will not
   affect the "*wcsout" FrameSet. */
            swcsout = astFrameSet( astFrame( 2, "Domain=PIXEL" ), "" );
            oskyframe2 = astCopy( oskyframe );
            astAddFrame( swcsout, AST__BASE, oskymap, oskyframe2 );

/* If the target is moving make the spatial FrameSet describe offsets
   from the first base pointing position (this position has previously
   been stored in the SkyRef attribute of the SkyFrame). */
            if( moving ) {
               astSet( swcsout, "SkyRefIs=Origin" );
               astSet( swcsout, "AlignOffset=1" );
            }

/* For later convenience, we invert it so that the base Frame is the 
   SkyFrame and the current Frame is the PIXEL Frame. */
            astInvert( swcsout );

/* Invert the output spectral Mapping so that it goes from output
   spectral WCS value to output spectral grid value. */
            astInvert( ospecmap );
         }

/* If the target is moving, set the input WCS FrameSet to represent Az,El
   offsets from the base pointing position for the current time slice. */
         if( moving ) {
            astSet( swcsin, "System=AZEL" );
            astSetD( swcsin, "SkyRef(1)", hdr->state->tcs_az_bc1 );
            astSetD( swcsin, "SkyRef(2)", hdr->state->tcs_az_bc2 );
            astSetC( swcsin, "SkyRefIs", "Origin" );
            astSetI( swcsin, "AlignOffset", 1 );
         }

/* We now align the input and output WCS FrameSets. astConvert finds the
   Mapping between the current Frames of the two FrameSets (the two 
   SkyFrames in this case), but we want the Mapping between the the two
   base Frames (input GRID to output PIXEL). So we invert the input WCS
   FrameSet (the output WCS FrameSet has already been inverted). */
         astInvert( swcsin );

/* Now use astConvert to get the Mapping from input GRID to output PIXEL
   coords, aligning the coordinate systems on the sky. Note the original 
   base Frame index so it can be re-instated afterwards (astConvert changes
   it to indicate the alignment Frame).*/
         ibasein = astGetI( swcsin, "Base" );
         fs = astConvert( swcsin, swcsout, "SKY" );
         astSetI( swcsin, "Base", ibasein );
         if( fs == NULL ) {
            if( *status == SAI__OK ) {
               msgSetc( "FILE", pname );
               *status = SAI__ERROR;
               errRep( FUNC_NAME, "The spatial coordinate system in ^FILE "
                       "is not compatible with the spatial coordinate "
                       "system in the first input file.", status );
            }
            break;
         }

/* Invert the input WCS FrameSet again to bring it back into its original
   state. */
         astInvert( swcsin );

/* Transform the positions of the detectors from input GRID to output PIXEL
   coords. Then extend the bounds of the output cube on the spatial axes to 
   accomodate the new positions. */
         astTran2( fs, (data->dims)[ 1 ], xin, yin, 1, xout, yout );
         for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {

/* If the detector has a valid position, see if it produced any good
   data values. */
            if( xout[ irec ] != AST__BAD && yout[ irec ] != AST__BAD ) {
               good = 0;
               for( ispec = 0; ispec < (data->dims)[ 0 ]; ispec++ ){
                  if( *(pdata++) != VAL__BADR ) {
                     good = 1;
                     pdata += (data->dims)[ 0 ] - ispec - 1;
                     break;
                  }
               }         

/* If it did, extend the bounding box to include the detector. */
               if( good ) {
                  if( xout[ irec ] > dubnd[ 0 ] ) dubnd[ 0 ] = xout[ irec ];
                  if( xout[ irec ] < dlbnd[ 0 ] ) dlbnd[ 0 ] = xout[ irec ];
                  if( yout[ irec ] > dubnd[ 1 ] ) dubnd[ 1 ] = yout[ irec ];
                  if( yout[ irec ] < dlbnd[ 1 ] ) dlbnd[ 1 ] = yout[ irec ];
                  npos++;
               }

/* If this detector does not have a valid position, increment the data
   pointer to point at the first sameple for the next detector. */
            } else {
               pdata += (data->dims)[ 0 ];
            }
         }

/* For efficiency, explicitly annul the AST Objects created in this tight
   loop. */
         fs = astAnnul( fs );
      }   

/* Close the current input data file. */
      smf_close_file( &data, status);
      data = NULL;

/* Free work space. */
      xin = astFree( xin );
      yin = astFree( yin );
      xout = astFree( xout );
      yout = astFree( yout );
   }

/* Close any data file that was left open due to an early exit from the
   above loop. */
   if( data != NULL ) smf_close_file( &data, status );

/* Check we found some usable data. */
   if( dlbnd[ 0 ] == VAL__MAXD || 
       dlbnd[ 1 ] == VAL__MAXD || 
       dlbnd[ 2 ] == VAL__MAXD ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "No usable data positions found.", status );
      }
   }

/* See if the user wants to restrict the spectral range of the output cube.
   First get the bounds of the full frequency axis. */
   ispecbounds[ 0 ] = dlbnd[ 2 ];
   ispecbounds[ 1 ] = dubnd[ 2 ];
   astTran1( ospecmap, 2, ispecbounds, 0, specbounds );

/* Now allow the user to provide alternative values. The above values are
   used as dynamic defaults for the SPECBOUNDS parameter. */
   kpg1Gtaxv( "SPECBOUNDS", 2, 1, ospecframe, 1, specbounds, &nval, status );

/* Convert the supplied spectral values back to pixel coords. */
   astTran1( ospecmap, 2, specbounds, 1, ispecbounds );

/* Update the output bounds. */
   dlbnd[ 2 ] = ispecbounds[ 0 ] ;
   dubnd[ 2 ] = ispecbounds[ 1 ] ;

/* We now add a GRID Frame in the output WCS FrameSet and calculates the
   bounds of the cube in this frame. If an optimal grid is being used, we 
   want to retain the fractional pixel offset supplied in par[0]. */
   if( autogrid ) {

/* Find the indices of the pixels containing the DLBND and DUBND points. */
      lbnd[ 0 ] = NINT( dlbnd[ 0 ] );
      lbnd[ 1 ] = NINT( dlbnd[ 1 ] );
      lbnd[ 2 ] = NINT( dlbnd[ 2 ] );
      ubnd[ 0 ] = NINT( dubnd[ 0 ] );
      ubnd[ 1 ] = NINT( dubnd[ 1 ] );
      ubnd[ 2 ] = NINT( dubnd[ 2 ] );

/* Find the integer pixel shifts needed to put the lower bounds at pixel
   (1,1,1). */
      shift[ 0 ] = 1 - lbnd[ 0 ];
      shift[ 1 ] = 1 - lbnd[ 1 ];
      shift[ 2 ] = 1 - lbnd[ 2 ];

/* Modify the bounds to put the origin at the centre */
      ishift = 1 + ( ubnd[ 0 ] + lbnd[ 0 ] )/2;
      lbnd[ 0 ] -= ishift;
      ubnd[ 0 ] -= ishift;
   
      ishift = 1 + ( ubnd[ 1 ] + lbnd[ 1 ] )/2;
      lbnd[ 1 ] -= ishift;
      ubnd[ 1 ] -= ishift;
   
      ishift = 1 + ( ubnd[ 2 ] + lbnd[ 2 ] )/2;
      lbnd[ 2 ] -= ishift;
      ubnd[ 2 ] -= ishift;


/* Otherwise, we mimick the quick look cube creation tool. */
   } else {

/* Find the number of pixels needed to span the X pixel axis range. */
      npix = 1 + (int)( dubnd[ 0 ] - dlbnd[ 0 ] );

/* Find a fractional pixel shift which puts the mid point of the axis
   range at the mid point of a span of "npix" pixels. */
      shift[ 0 ] = 0.5*( 1 + npix - dlbnd[ 0 ] - dubnd[ 0 ] );

/* Find the upper and lower integer bounds after applying this shift. */
      lbnd[ 0 ] = NINT( dlbnd[ 0 ] + shift[ 0 ] );
      ubnd[ 0 ] = NINT( dubnd[ 0 ] + shift[ 0 ] );

/* Do the same for the other 2 axes. */
      npix = 1 + (int)( dubnd[ 1 ] - dlbnd[ 1 ] );
      shift[ 1 ] = 0.5*( 1 + npix - dlbnd[ 1 ] - dubnd[ 1 ] );
      lbnd[ 1 ] = NINT( dlbnd[ 1 ] + shift[ 1 ] );
      ubnd[ 1 ] = NINT( dubnd[ 1 ] + shift[ 1 ] );

      npix = 1 + (int)( dubnd[ 2 ] - dlbnd[ 2 ] );
      shift[ 2 ] = 0.5*( 1 + npix - dlbnd[ 2 ] - dubnd[ 2 ] );
      lbnd[ 2 ] = NINT( dlbnd[ 2 ] + shift[ 2 ] );
      ubnd[ 2 ] = NINT( dubnd[ 2 ] + shift[ 2 ] );

/* Modify the bounds to put the origin in the middle, using the same method as
   the on-line system (this relies on integer division). */
      ishift = 2 + ( ubnd[ 0 ] - lbnd[ 0 ] )/2;
      lbnd[ 0 ] -= ishift;
      ubnd[ 0 ] -= ishift;
   
      ishift = 2 + ( ubnd[ 1 ] - lbnd[ 1 ] )/2;
      lbnd[ 1 ] -= ishift;
      ubnd[ 1 ] -= ishift;
   
      ishift = 2 + ( ubnd[ 2 ] - lbnd[ 2 ] )/2;
      lbnd[ 2 ] -= ishift;
      ubnd[ 2 ] -= ishift;

   }

/* Now apply the shift to the PIXEL Frame to get the GRID Frame. Remember the 
   index of the WCS Frame so we can re-instate it after adding in the new 
   GRID Frame. */
   iwcsfrm = astGetI( *wcsout, "Current" );
   astAddFrame( *wcsout, AST__BASE, astShiftMap( 3, shift, "" ),
                                    astFrame( 3, "Domain=GRID") );

/* Make the new GRID Frame the base Frame and then re-instate the
   original current Frame. */
   astSetI( *wcsout, "Base", astGetI( *wcsout, "Current" ) );
   astSetI( *wcsout, "Current", iwcsfrm );

/* We now erase the original PIXEL Frame since it is no longer needed. */
   astRemoveFrame( *wcsout, 1 );

/* Report the coordinate systems in use in the output cube, and the pixel
   bounds of the cube. */
   if( *status == SAI__OK ) {
      msgOutif( MSG__NORM, " ", " ", status );

      msgSeti( "XL", lbnd[ 0 ] );
      msgSeti( "YL", lbnd[ 1 ] );
      msgSeti( "ZL", lbnd[ 2 ] );
      msgSeti( "XU", ubnd[ 0 ] );
      msgSeti( "YU", ubnd[ 1 ] );
      msgSeti( "ZU", ubnd[ 2 ] );
      msgOutif( MSG__NORM, " ", "   Output cube bounds: ( ^XL:^XU, ^YL:^YU, ^ZL:^ZU )", 
                status );
      
      msgSetc( "X", astGetC( *wcsout, "Label(1)" ) );
      msgSetc( "Y", astGetC( *wcsout, "Label(2)" ) );
      msgSetc( "Z", astGetC( *wcsout, "Label(3)" ) );
      msgOutif( MSG__NORM, " ", "   Output WCS axes: ( ^X, ^Y, ^Z )", 
                status );
      
      msgOutif( MSG__NORM, " ", " ", status );
   }

/* If no error has occurred, export the returned FrameSet pointer from the 
   current AST context so that it will not be annulled when the AST
   context is ended. Otherwise, ensure a null pointer is returned. */
   if( *status == SAI__OK ) {
      astExport( *wcsout );
   } else {
      *wcsout = astAnnul( *wcsout );
   }

/* End the AST context. This will annul all AST objects created within the
   context (except for those that have been exported from the context). */
   astEnd;

/* Issue a context message if anything went wrong. */
   if( *status != SAI__OK ) errRep( FUNC_NAME, "Unable to determine cube "
                                    "bounds", status );
}


