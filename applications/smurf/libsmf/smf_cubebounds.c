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
*     smf_cubebounds( Grp *igrp,  int size, char *system, double crval[2], 
*                     double cdelt[2], double crota2, int userecpos, 
*                     int *moving, int lbnd[ 3 ], int ubnd[ 3 ], 
*                     AstFrameSet **wcsout, int *status );

*  Arguments:
*     igrp = Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     system = char * (Given)
*        Specifies the celestial coordinate system which will be used to
*        describe the spatial axes of the output cube. It should be a 
*        valid value for the System attribute of an AST SkyFrame, or
*        "TRACKING".
*     crval = double[ 2 ] (Given)
*        The longitude and latitude to use as the tangent point in the output 
*        cube, in radians in the system specified by "system". Bad values
*        are replaced by the BASE coordinates for the tangent point.
*     cdelt = double[ 2 ] (Given)
*        Spatial pixel sizes for the output cube at the tangent point, in 
*        arcsec.
*     crota2 = double (Given)
*        The angle from north through east to the second pixel axis, in
*        degrees.
*     usedetpos = int (Given)
*        If a non-zero value is supplied, then the detector positions for
*        a given time slice are read directly from the input NDF. Otherwise 
*        the detector positions are calculated on the basis of the focal
*        plane detector positions and the telescope pointing information.
*     moving = int * (Returned)
*        Address of an int in which to return a flag indicating if the 
*        telescope is tracking a moving object. If so, each time slice is 
*        shifted to the position specified by TCS_AZ_BC1/2 before extending 
*        the output cube bouds to include it.
*     lbnd = int [ 3 ] (Returned)
*        The lower pixel index bounds of the output cube.
*     ubnd = int [ 3 ] (Returned)
*        The upper pixel index bounds of the output cube.
*     wcsout = AstFrameSet ** (Returned)
*        A pointer to a location at which to return a pointer to an AST 
*        Frameset describing the WCS to be associated with the output cube.
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
*     "crval", "cdelt" and "crota". The spectral axis system and projection 
*     are inherited from the first supplied input data file.
*
*     If "moving" is non-zero, the spatial axes represent (lon,lat) offsets 
*     in the tracking frame from the base telescope position associated
*     with the last time slice.

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

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
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

void smf_cubebounds( Grp *igrp,  int size, char *system, double crval[2], 
                     double cdelt[2], double crota2, int userecpos, 
                     int *moving, int lbnd[ 3 ], int ubnd[ 3 ], 
                     AstFrameSet **wcsout, int *status ){

/* Local Variables */
   AstCmpFrame *cmpfrm = NULL;  /* Current Frame for output FrameSet */
   AstCmpMap *cmpmap = NULL;    /* Base -> Current Mapping for output FrameSet */
   AstCmpMap *ssmap = NULL;     /* I/p GRID-> o/p PIXEL Mapping for spectral axis */
   AstFitsChan *fc = NULL;      /* FitsChan used to construct spectral WCS */
   AstFitsChan *fct = NULL;     /* FitsChan used to construct time slice WCS */
   AstFrame *oskyframe = NULL;  /* Sky Frame in output FrameSet */
   AstFrame *ospecframe = NULL; /* Spectral Frame in output FrameSet */
   AstFrame *sf1 = NULL;        /* Spatial Frame representing AZEL system */
   AstFrame *sf2 = NULL;        /* Spatial Frame representing requested system */
   AstFrame *skyframe = NULL;   /* Spatial Frame in input FrameSet */
   AstFrame *specframe = NULL;  /* Spectral Frame in input FrameSet */
   AstFrameSet *fs = NULL;      /* A general purpose FrameSet pointer */
   AstFrameSet *swcsin = NULL;  /* FrameSet describing spatial input WCS */
   AstFrameSet *swcsout = NULL; /* FrameSet describing spatial output WCS */
   AstMapping *fsmap = NULL;    /* Base->Current Mapping extracted from a FrameSet */
   AstMapping *oskymap = NULL;  /* Sky <> PIXEL mapping in output FrameSet */
   AstMapping *ospecmap = NULL; /* Spec <> PIXEL mapping in output FrameSet */
   AstMapping *specmap = NULL;  /* PIXEL -> Spec mapping in input FrameSet */
   char *pname = NULL;   /* Name of currently opened data file */
   const char *trsys = NULL; /* AST tracking system */
   const char *usesys = NULL;/* AST system for output cube */
   char outcatnam[ 41 ]; /* Output catalogue name */
   double *outpos = NULL;/* Array of sample positions */
   double *outpos2 = NULL;/* Array of sample positions */
   double *p;            /* Pointer to next value */
   double *px;           /* Pointer to next value */
   double *py;           /* Pointer to next value */
   double *xin = NULL;   /* Workspace for detector input grid positions */
   double *xout = NULL;  /* Workspace for detector output pixel positions */
   double *xout2 = NULL; /* Workspace for detector output pixel positions */
   double *yin = NULL;   /* Workspace for detector input grid positions */
   double *yout = NULL;  /* Workspace for detector output pixel positions */
   double *yout2 = NULL; /* Workspace for detector output pixel positions */
   double a;             /* Longitude value */
   double b;             /* Latitude value */
   double dlbnd[ 3 ];    /* Floating point lower bounds for output cube */
   double dubnd[ 3 ];    /* Floating point upper bounds for output cube */
   double sep;           /* Separation between first and last base positions */
   double shift[ 3 ];    /* Shifts from PIXEL to GRID coords */
   double specin[ 2];    /* Spectral values to be transformed */
   double specout[ 2];   /* Transformed spectral values */
   int ibasein;          /* Index of base Frame in input FrameSet */
   int ifile;            /* Index of current input file */
   int ipos;             /* Position index */
   int irec;             /* Index of current input detector */
   int ishift;           /* Shift to put pixel origin at centre */
   int itime;            /* Index of current time slice */
   int iwcsfrm;          /* Index of original output WCS Frame */
   int noutpos;          /* Number of positions to store in output catalogue */
   int npix;             /* Number of pixels along axis */
   int outcat;           /* Produce an output catalogue holding sample positions? */
   int pixax[ 3 ];       /* The output fed by each selected mapping input */
   int specax;           /* Index of spectral axis in input FrameSet */
   smfData *data = NULL; /* Pointer to data struct for current input file */
   smfFile *file = NULL; /* Pointer to file struct for current input file */
   smfHead *hdr = NULL;  /* Pointer to data header for this time slice */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Tell the user what is happening. */
   msgOutif( MSG__VERB, " ", "SMURF_MAKECUBE: Determine cube bounds", status );

/* Begin an AST context. */
   astBegin;

/* See if an output catalogue holding the sky positions at every receptor
   sample is to be produced. */
   parGet0c( "OUTCAT", outcatnam, 40, status );
   if( *status == PAR__NULL ) {
      errAnnul( status );
      outcat = 0;
   } else {
      outcat = 1;
   }
   outpos = NULL;
   noutpos = 0;

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
      if( !userecpos && hdr->detpos ) {
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
         specin[ 0 ] = (data->dims)[ 0 ];
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

      if( outcat ) {
         xout2 = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
         yout2 = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
      }

/* Store the input GRID coords of the detectors. */
      for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {
         xin[ irec ] = irec + 1.0;
         yin[ irec ] = 1.0;
      }

/* If required, extend the memory use to hold the list of receptor positions 
   for the output catalogue. */
      if( outcat ) {
         outpos = astGrow( outpos, noutpos + 2*(data->dims)[ 2 ]*(data->dims)[ 1 ],
                           sizeof( double ) );
      }                           

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

/* If we have not yet chosen the celestial coordinate system for the
   output cube, do so now. */
         if( !usesys ) {
            trsys = smf_convert_system( hdr->state->tcs_tr_sys, status );
            if( !strcmp( system, "TRACKING" ) ) {
               usesys = trsys;
            } else {
               usesys = system;
            }
         }   

/* Create a FrameSet describing the WCS to be associated with the output 
   cube unless this has already be done. */
         if( *wcsout == NULL ) { 

/* The spectral Mapping (from GRID to SPECTRUM) and Frame (a SpecFrame)
   are inherited from the first input file. */
            ospecframe = astClone( specframe );
            ospecmap = astClone( specmap );

/* The tangent point of the output tan plane projection is set to the 
   supplied crval position. If no position has been supplied, use
   the first pointing BASE position, converted to the required coordinate
   system. */
	    if( crval[ 0 ] == VAL__BADD || crval[ 1 ] == VAL__BADD ) {

/* Get the pointing BASE position in AZ/EL. */
               crval[ 0 ] = hdr->state->tcs_az_bc1;
               crval[ 1 ] = hdr->state->tcs_az_bc2;

/* If the required system is not AZ/EL, we need to convert the 
   axis values to the required system. */
               if( strcmp( "AZEL", usesys ) ) {

/* Create two copies of the SkyFrame in the input WCS FrameSet, and set
   their Systems to AZEL and the requested output system. */
                  skyframe = astGetFrame( swcsin, AST__CURRENT );     
                  sf1 = astCopy( skyframe );
                  sf2 = astCopy( skyframe );

                  astSetC( sf1, "System", "AZEL" );
                  astSetC( sf2, "System", usesys );

/* Get a Mapping from AZEL to the requested system, and use it to convert 
   the lon_0/lat_0 values to the requested system. */
                  astTran2( astConvert( sf1, sf2, "" ), 1, crval,
                            crval + 1, 1, &a, &b );
                  crval[ 0 ] = a;
                  crval[ 1 ] = b;
               }
            }

/* Now populate a FitsChan with FITS-WCS headers describing the required 
   tan plane projection. The longitude and latitude axis types are defined 
   by "usesys", and north in this system will be parallel to the second pixel
   axis. The tangent point is placed at pixel (0,0). */
            smf_makefitschan( usesys, crval, cdelt, crota2, fct, status );

/* Read a FrameSet from this FitsChan. */
	    astClear( fct, "Card" );
            fs = astRead( fct );

/* Get a pointer to the SkyFrame within the FrameSet, and copy the observatory 
   position from the input WCS FrameSet to the SkyFrame. Using the SkyFrame
   pointer rather than the FrameSet pointer ensures that the Mapping
   within the "fs" FrameSet is not changed as a consequence of modifying the
   ObsLon/ObsLat values. */
            oskyframe = astGetFrame( fs, AST__CURRENT );
            if( astTest( swcsin, "ObsLon" ) ) {
               astSetC( oskyframe, "ObsLon", astGetC( swcsin, "ObsLon" ) );
            }           
            if( astTest( swcsin, "ObsLat" ) ) {
               astSetC( oskyframe, "ObsLat", astGetC( swcsin, "ObsLat" ) );
            }           

/* Likewise, copy the Epoch, Equinox and Dut1 values from "swcsin". */
            if( astTest( swcsin, "Epoch" ) ) {
               astSetC( oskyframe, "Epoch", astGetC( swcsin, "Epoch" ) ); 
            }           
            if( astTest( swcsin, "Equinox" ) ) {
               astSetC( oskyframe, "Equinox", astGetC( swcsin, "Equinox" ) );
            }           
            if( astTest( swcsin, "Dut1" ) ) {
               astSetC( oskyframe, "Dut1", astGetC( swcsin, "Dut1" ) );
            }           

/* Ensure the specframe has the same epoch. */
            if( astTest( swcsin, "Epoch" ) ) {
               astSetC( ospecframe, "Epoch", astGetC( swcsin, "Epoch" ) ); 
            }           

/* Extract the output PIXEL->SKY Mapping. */
            oskymap = astGetMapping( fs, AST__BASE, AST__CURRENT );

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
            oskyframe = astCopy( oskyframe );
            astAddFrame( swcsout, AST__BASE, oskymap, oskyframe );

/* Determine if the telescope is following a moving target such as a
   planet or asteroid. This is indicated by significant changes in the
   values of TCS_TR_BC1/2 (the telescope base position in tracking
   coordinates). Here, "significant" means that the change is at least
   half a pixel in size. Some tracking systems such as AZEL will always
   indicate movement whether or not the target is moving , so for these
   system we assume that the target is not moving. */
            if( strcmp( trsys, "AZEL" ) ) {

                sep = slaDsep( (hdr->allState)[ 0 ].tcs_tr_bc1,
                        (hdr->allState)[ 0 ].tcs_tr_bc2,
                        (hdr->allState)[ hdr->nframes - 1 ].tcs_tr_bc1,
                        (hdr->allState)[ hdr->nframes - 1 ].tcs_tr_bc2 );

                *moving = ( sep > ( cdelt[ 0 ]/3600.0 )*AST__DD2R );

            } else {
               *moving = 0;
            }

/* If we are dealing with a moving target, adjust the SkyFrame in the
   output spatial FrameSet so that it represents the tracking system
   rather than the requested system. This is because the base telescope
   position is specified in the tracking system. */
            if( *moving ) astSetC( swcsout, "system", trsys );

/* For later convenience, we invert it so that the base Frame is the 
   SkyFrame and the current Frame is the PIXEL Frame. */
            astInvert( swcsout );
         }

/* If we are dealing with a moving target, adjust the SkyFrames in the
   input and output FrameSets so that they represent offsets from the
   current telescope base position. */
         if( *moving ) {

/* The telescope base position is given in tracking coords, so ensure
   that the SkyFrame in swcsin refers to the tracking system. The Mapping
   to the corresponding GRID coordinate system is modified appropriately. */
            astSetC( swcsin, "system", trsys );

/* Modify swcsin so that its SkyFrame represents offsets from the current
   telescope base position. We use the FrameSet pointer (swcsin) in this 
   call, so the Mapping from detector number to SkyFrame will be modified
   so that each detector retains its original position on the sky (but
   transformed to the offset coordinate system). Also indiucate that the
   position should be used as the origin of the offset coordinate system, 
   and that alignment should be performed in the offset coordinate system. */
            astSetD( swcsin, "SkyRef(1)", hdr->state->tcs_tr_bc1 );
            astSetD( swcsin, "SkyRef(2)", hdr->state->tcs_tr_bc2 );
            astSetC( swcsin, "SkyRefIs", "Origin" );
            astSetI( swcsin, "AlignOffset", 1 );

/* Modify swcsout so that its SkyFrame represents offsets from the current
   telescope base position. We use the SkyFrame pointer (oskyframe) here 
   rather than the FrameSet pointer (swcsout) so the Mapping from output 
   grid index to SkyFrame will not be modified. This means the each output 
   pixel will move on the sky to follow the new telescope base position. */
            astSetD( oskyframe, "SkyRef(1)", hdr->state->tcs_tr_bc1 );
            astSetD( oskyframe, "SkyRef(2)", hdr->state->tcs_tr_bc2 );
            astSetC( oskyframe, "SkyRefIs", "Origin" );
            astSetI( oskyframe, "AlignOffset", 1 );
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
            if( xout[ irec ] != AST__BAD && yout[ irec ] != AST__BAD ) {
               if( xout[ irec ] > dubnd[ 0 ] ) dubnd[ 0 ] = xout[ irec ];
               if( xout[ irec ] < dlbnd[ 0 ] ) dlbnd[ 0 ] = xout[ irec ];
               if( yout[ irec ] > dubnd[ 1 ] ) dubnd[ 1 ] = yout[ irec ];
               if( yout[ irec ] < dlbnd[ 1 ] ) dlbnd[ 1 ] = yout[ irec ];
            }
         }

/* If required, transform the receptor positions (in output grid coords) into 
   the output sky frame and copy to the array destined for the output 
   catalogue. */
         if( outcat ) {
            astTran2( swcsout, (data->dims)[ 1 ], xout, yout, 0, xout2, yout2 );
            p = outpos + 2*noutpos;
            for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {
               *(p++) = xout2[ irec ];
               *(p++) = yout2[ irec ];
            }
            noutpos += (data->dims)[ 1 ];
         }

/* For efficiency, explicitly annul the AST Objects created in this tight
   loop. */
         fs = astAnnul( fs );
      }   

/* Close the current input data file. */
      smf_close_file( &data, status);
      data = NULL;
   }

/* Close any data file that was left open due to an early exit from the
   above loop. */
   if( data != NULL ) smf_close_file( &data, status );

/* Free work space. */
   xin = astFree( xin );
   yin = astFree( yin );
   xout = astFree( xout );
   yout = astFree( yout );

/* Check we found some usable data. */
   if( dlbnd[ 0 ] == VAL__MAXD || 
       dlbnd[ 1 ] == VAL__MAXD || 
       dlbnd[ 2 ] == VAL__MAXD ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "No usable data positions found.", status );
      }
   }

/* Find the number of pixels needed to span the X pixel axis range. */
   npix = 1 + (int)( dubnd[ 0 ] - dlbnd[ 0 ] );

/* Find a fractional pixel shift which puts the mid point of the axis
   range at the mid point of a span of "npix" pixels. */
   shift[ 0 ] = 0.5*( 1 + npix - dlbnd[ 0 ] - dubnd[ 0 ] );

/* Find the upper and lower integer bounds after applying this shift. */
   lbnd[ 0 ] = NINT( dlbnd[ 0 ] + shift[ 0 ] );
   ubnd[ 0 ] = NINT( dubnd[ 0 ] + shift[ 0 ] );

/* Modify the bounds to put the origin in the middle, using the same method as
   the on-line system (this relies on integer division). */
   ishift = 2 + ( ubnd[ 0 ] - lbnd[ 0 ] )/2;
   lbnd[ 0 ] -= ishift;
   ubnd[ 0 ] -= ishift;

/* Do the same for the other 2 axes. */
   npix = 1 + (int)( dubnd[ 1 ] - dlbnd[ 1 ] );
   shift[ 1 ] = 0.5*( 1 + npix - dlbnd[ 1 ] - dubnd[ 1 ] );
   lbnd[ 1 ] = NINT( dlbnd[ 1 ] + shift[ 1 ] );
   ubnd[ 1 ] = NINT( dubnd[ 1 ] + shift[ 1 ] );

   ishift = 2 + ( ubnd[ 1 ] - lbnd[ 1 ] )/2;
   lbnd[ 1 ] -= ishift;
   ubnd[ 1 ] -= ishift;

   npix = 1 + (int)( dubnd[ 2 ] - dlbnd[ 2 ] );
   shift[ 2 ] = 0.5*( 1 + npix - dlbnd[ 2 ] - dubnd[ 2 ] );
   lbnd[ 2 ] = NINT( dlbnd[ 2 ] + shift[ 2 ] );
   ubnd[ 2 ] = NINT( dubnd[ 2 ] + shift[ 2 ] );

   ishift = 2 + ( ubnd[ 2 ] - lbnd[ 2 ] )/2;
   lbnd[ 2 ] -= ishift;
   ubnd[ 2 ] -= ishift;

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

/* If no error has occurred, export the returned FrameSet pointer from the 
   current AST context so that it will not be annulled when the AST
   context is ended. Otherwise, ensure a null pointer is returned. */
   if( *status == SAI__OK ) {
      astExport( *wcsout );
   } else {
      *wcsout = astAnnul( *wcsout );
   }

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

/* Produce the output catalogue if required. */
   if( outcat ) {

/* Re-order the array containing the positions so that all the longitude
   values come at the start of hte array, followed by all the latitude
   values. */
      outpos2 = astMalloc( sizeof( double )*2*noutpos );
      px = outpos2;
      py = outpos2 + noutpos;
      p = outpos;
      for( ipos = 0;ipos < noutpos; ipos++ ) {
         *(px++) = *(p++);
         *(py++) = *(p++);
      } 

/* Create the catalogue. */
      kpg1Wrlst( "OUTCAT", noutpos, noutpos, 2, outpos2, AST__CURRENT, 
                 astFrameSet( astGetFrame( swcsout, AST__BASE ), "" ),
                 "Detector positions", 1, NULL, 1, status );

/* Free resources. */
      outpos = astFree( outpos );
      outpos2 = astFree( outpos2 );
      xout2 = astFree( xout2 );
      yout2 = astFree( yout2 );
   }

/* End the AST context. This will annul all AST objects created within the
   context (except for those that have been exported from the context). */
   astEnd;

/* Issue a context message if anything went wrong. */
   if( *status != SAI__OK ) errRep( FUNC_NAME, "Unable to determine cube "
                                    "bounds", status );
}
