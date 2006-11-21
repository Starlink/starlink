/*
*+
*  Name:
*     smf_cubegrid

*  Purpose:
*     Calculate optimal values for the spatial projection parameters.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_cubegrid( Grp *igrp,  int size, char *system, int usedetpos, 
*                   int autogrid, double par[ 7 ], int *moving, 
*                   AstSkyFrame **skyframe, int *status );

*  Arguments:
*     igrp = Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     system = char * (Given)
*        Specifies the celestial coordinate system which will be used to
*        describe the spatial axes of the output cube. It should be a 
*        valid value for the System attribute of an AST SkyFrame, or
*        "TRACKING". This is ignored if a moving target is being tracked
*        (see "moving").
*     usedetpos = int (Given)
*        If a non-zero value is supplied, then the detector positions for
*        a given time slice are read directly from the input NDF. Otherwise 
*        the detector positions are calculated on the basis of the focal
*        plane detector positions and the telescope pointing information.
*     autogrid = int (Given)
*        Determines how any bad values in "par" are replaced. If autogrid
*        is non-zero, then new projection parameters are determined by
*        adjusting the grid until as many data samples as possible fall
*        close to the centre of pixels in the output cube. If autogrid is
*        zero, CRPIX1/2 are set to zero, CRVAL1/2 are set to the first 
*        pointing BASE position, CROTA2 is set to zero, CDELT1/2 are set to 
*        6 arc-seconds.
*     par = double[ 7 ] (Given and Returned)
*        An array holding the parameters describing the spatial projection
*        between celestial (longitude,latitude) in the system specified
*        by "system", and GRID coordinates in the output cube. These are
*        stored in the order CRPIX1, CRPIX2, CRVAL1, CRVAL2, CDELT1, CDELT2, 
*        CROTA2. The CRPIX1 and CRPIX2 values are in units of pixels, and 
*        all other values are in units of radians. The values refer to the 
*        celestial coodinate represented by the returned SkyFrame.
*     
*        Any bad values in the supplied list are replaced on exit by the 
*        actual values to be used (determined by "aitogrid"). Non-bad values 
*        in the supplied array are left unchanged on exit. 
*     moving = int * (Returned)
*        Address of an int in which to return a flag indicating if the 
*        telescope is tracking a moving object. If so, each time slice is 
*        shifted to the position specified by TCS_AZ_BC1/2 before extending 
*        the output cube bouds to include it.
*     skyframe = AstFrameSet ** (Returned)
*        A pointer to a location at which to return a pointer to an AST 
*        SkyFrame describing the spatial axes of the output WCS FrameSet.
*        If "moving" is non-zero, the spatial axes represent (lon,lat) 
*        offsets in the tracking frame from the base telescope position 
*        associated with the last time slice.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function finds the best values for the parameters describing
*     the projection between celestial (longitude,latitude) and the
*     spatial pixel axes of the output cube.
*
*     Also creates an output catalogue holding the sample positions. This
*     uses ADAM parameter OUTCAT to get the name of the catalogue.

*  Authors:
*     David S Berry (JAC, UCLan)
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
#include "par.h"
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

#define FUNC_NAME "smf_cubegrid"

void smf_cubegrid( Grp *igrp,  int size, char *system, int usedetpos, 
                   int autogrid, double par[ 7 ], int *moving, 
                   AstSkyFrame **skyframe, int *status ){

/* Local Variables */
   AstFrame *sf1 = NULL;      /* Spatial Frame representing AZEL system */
   AstFrame *sf2 = NULL;      /* Spatial Frame representing requested system */
   AstFrame *skyframein = NULL; /* SkyFrame from input WCS FrameSet */
   AstFrame *skyin = NULL;    /* Sky Frame in input FrameSet */
   AstFrameSet *fs = NULL;    /* A general purpose FrameSet pointer */
   AstFrameSet *swcsin = NULL;/* FrameSet describing spatial input WCS */
   char *pname = NULL;        /* Name of currently opened data file */
   char outcatnam[ 41 ];      /* Output catalogue name */
   const char *lab = NULL;    /* Pointer to start of next detector name */
   const char *trsys = NULL;  /* AST tracking system */
   const char *usesys = NULL; /* AST system for output cube */
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
   double oppar[ 7 ];    /* Optimal parameter values */
   double sep;           /* Separation between first and last base positions */
   float *pdata;         /* Pointer to next data sample */
   Grp *labgrp;          /* GRP group holding detector labels */
   int good;             /* Are there any good detector samples? */
   int ibasein;          /* Index of base Frame in input FrameSet */
   int ifile;            /* Index of current input file */
   int ipar;             /* Parameter index */
   int ipos;             /* Position index */
   int irec;             /* Index of current input detector */
   int ispec;            /* Index of current spectral sample */
   int itime;            /* Index of current time slice */
   int nallpos;          /* Number of positions to store in output catalogue */
   int outcat;           /* Produce an output catalogue holding sample positions? */
   smfData *data = NULL; /* Pointer to data struct for current input file */
   smfFile *file = NULL; /* Pointer to file struct for current input file */
   smfHead *hdr = NULL;  /* Pointer to data header for this time slice */

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

/* If we are creating an output catalogue, create a GRP group to hold the
   labels to bne associated with each position. */
   if( outcat ) {
      labgrp = grpNew( "Detector labels", status );
   } else {
      labgrp = NULL;
   }

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
      msgOutif( MSG__VERB, " ", "SMF_CUBEGRID: Processing ^I/^N ^FILE", 
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

/* Extend the memory used to hold the list of all receptor positions. */
      allpos = astGrow( allpos, nallpos + 2*(data->dims)[ 2 ]*(data->dims)[ 1 ],
                        sizeof( double ) );

/* Store a pointer to the next input data value */
      pdata = ( data->pntr )[ 0 ];

/* Loop round all the time slices in the input file. */
      for( itime = 0; itime < (data->dims)[ 2 ] && *status == SAI__OK; itime++ ) {

/* Get a FrameSet describing the spatial coordinate systems associated with 
   the current time slice of the current input data file. The base frame in 
   the FrameSet will be a 2D Frame in which axis 1 is detector number and 
   axis 2 is unused. The current Frame will be a SkyFrame (the SkyFrame 
   System may be any of the JCMT supported systems). The Epoch will be
   set to the epoch of the time slice. */
         smf_tslice_ast( data, itime, 1, status );
         swcsin = hdr->wcs;

/* If we have not yet created the output SkyFrame, do so now. */
         if( ! *skyframe ) {

/* Choose the celestial coordinate system for the output cube. */
            trsys = smf_convert_system( hdr->state->tcs_tr_sys, status );
            if( !strcmp( system, "TRACKING" ) ) {
               usesys = trsys;
            } else {
               usesys = system;
            }

/* Create a SkyFrame describing the spatial coordinate system to be 
   used within the WCS FrameSet of the output cube. */
            *skyframe = astSkyFrame( "System=%s", usesys );

/* Copy ObsLon, ObsLat, Epoch, Equinox and Dut1 from the input WCS. */
            skyin = astGetFrame( swcsin, AST__CURRENT );
            if( astTest( skyin, "ObsLon" ) ) {
               astSetC( *skyframe, "ObsLon", astGetC( skyin, "ObsLon" ) );
            }           
            if( astTest( skyin, "ObsLat" ) ) {
               astSetC( *skyframe, "ObsLat", astGetC( skyin, "ObsLat" ) );
            }           
            if( astTest( skyin, "Epoch" ) ) {
               astSetC( *skyframe, "Epoch", astGetC( skyin, "Epoch" ) ); 
            }           
            if( astIsASkyFrame( skyin ) && astTest( skyin, "Equinox" ) ) {
               astSetC( *skyframe, "Equinox", astGetC( skyin, "Equinox" ) );
            }           
            if( astTest( skyin, "Dut1" ) ) {
               astSetC( *skyframe, "Dut1", astGetC( skyin, "Dut1" ) );
            }           

/* If autogrid is not being used, the tangent point of the output tan plane 
   projection is set to the first pointing BASE position, converted to the 
   required coordinate system. */
	    if( !autogrid && ( par[ 2 ] == AST__BAD || par[ 3 ] == AST__BAD ) ) {

/* Get the pointing BASE position in AZ/EL. */
               par[ 2 ] = hdr->state->tcs_az_bc1;
               par[ 3 ] = hdr->state->tcs_az_bc2;

/* If the required system is not AZ/EL, we need to convert the 
   axis values to the required system. */
               if( strcmp( "AZEL", usesys ) ) {

/* Create two copies of the SkyFrame in the input WCS FrameSet, and set
   their Systems to AZEL and the requested output system. */
                  skyframein = astGetFrame( swcsin, AST__CURRENT );     
                  sf1 = astCopy( skyframein );
                  sf2 = astCopy( skyframein );

                  astSetC( sf1, "System", "AZEL" );
                  astSetC( sf2, "System", usesys );

/* Get a Mapping from AZEL to the requested system, and use it to convert 
   the lon_0/lat_0 values to the requested system. */
                  astTran2( astConvert( sf1, sf2, "" ), 1, par + 2,
                            par + 3, 1, &a, &b );
                  par[ 2 ] = a;
                  par[ 3 ] = b;
               }
            }

/* Determine if the telescope is following a moving target such as a
   planet or asteroid. This is indicated by significant changes in the
   values of TCS_TR_BC1/2 (the telescope base position in tracking
   coordinates). Here, "significant" means more than 1 arc-second. Some 
   tracking systems such as AZEL will always indicate movement whether 
   or not the target is moving , so for these system we assume that the 
   target is not moving. */
            if( strcmp( trsys, "AZEL" ) ) {

                sep = slaDsep( (hdr->allState)[ 0 ].tcs_tr_bc1,
                        (hdr->allState)[ 0 ].tcs_tr_bc2,
                        (hdr->allState)[ hdr->nframes - 1 ].tcs_tr_bc1,
                        (hdr->allState)[ hdr->nframes - 1 ].tcs_tr_bc2 );

                *moving = ( sep > AST__DD2R/3600.0 );

            } else {
               *moving = 0;
            }

/* If we are dealing with a moving target, we ignore the supplied
   "system" value and instead set the returned SkyFrame so that it 
   represents the tracking system. This is because the base telescope 
   position is specified in the tracking system. */
            if( *moving ) {
               astSetC( *skyframe, "system", trsys );
               usesys = trsys;
            }

/* Set the SKyFrame SkyRef position to the tangent point. */
            astSetD( *skyframe, "SkyRef(1)", par[ 2 ] );
            astSetD( *skyframe, "SkyRef(2)", par[ 3 ] );

/* If we do not need to look at any other time slices, we can leave the loop
   early. */
            if( !autogrid && !outcat ) break;

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
   transformed to the offset coordinate system). Also indicate that the
   position should be used as the origin of the offset coordinate system, 
   and that alignment should be performed in the offset coordinate system. */
            astSetD( swcsin, "SkyRef(1)", hdr->state->tcs_tr_bc1 );
            astSetD( swcsin, "SkyRef(2)", hdr->state->tcs_tr_bc2 );
            astSetC( swcsin, "SkyRefIs", "Origin" );
            astSetI( swcsin, "AlignOffset", 1 );

/* Modify the returned skyframe so that it represents offsets from the 
   current telescope base position. */
            astSetD( *skyframe, "SkyRef(1)", hdr->state->tcs_tr_bc1 );
            astSetD( *skyframe, "SkyRef(2)", hdr->state->tcs_tr_bc2 );
            astSetC( *skyframe, "SkyRefIs", "Origin" );
            astSetI( *skyframe, "AlignOffset", 1 );
         }

/* Get the Mapping from the base Frame in the input WCS FrameSet (GRID
   coords) to the returned SkyFrame. */
         astInvert( swcsin );
         ibasein = astGetI( swcsin, "Base" );
         fs = astConvert( swcsin, *skyframe, "SKY" );
         astSetI( swcsin, "Base", ibasein );
         astInvert( swcsin );

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

/* Transform the positions of the detectors from input GRID to output SKY
   coords. */
         astTran2( fs, (data->dims)[ 1 ], xin, yin, 1, xout, yout );

/* Copy usable sky positions into the array holding all positions. */
         p = allpos + 2*nallpos;
         lab = hdr->detname;
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

/* If it did, store it. */
               if( good ) {
                  *(p++) = xout[ irec ];
                  *(p++) = yout[ irec ];
                  if( labgrp ) grpPut1( labgrp, lab, 0, status );
                  nallpos++;
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
         fs = astAnnul( fs );
      }   

/* Close the current input data file. */
      smf_close_file( &data, status);
      data = NULL;

/* If we do not need to look at any other files, we can leave the loop
   early. */
      if( *skyframe && !autogrid && !outcat ) break;
   }

/* Close any data file that was left open due to an early exit from the
   above loop. */
   if( data != NULL ) smf_close_file( &data, status );

/* If required, calculate the optimal projection parameters. */
   if( autogrid && usesys ) {
      kpg1Opgrd( nallpos, allpos, strcmp( usesys, "AZEL" ), oppar, status );

/* Replace any supplied bad projection parameters with the optimal values
   calculated above. */
     for( ipar = 0; ipar < 7; ipar++ ) {
        if( par[ ipar ] == AST__BAD ) par[ ipar ] = oppar[ ipar ];
     }        

/* Otherwise replace any remaining bad values with fixed values. */
   } else {
      if( par[ 0 ] == AST__BAD ) par[ 0 ] = 0.0;
      if( par[ 1 ] == AST__BAD ) par[ 1 ] = 0.0;
      if( par[ 4 ] == AST__BAD ) par[ 4 ] = (6.0/3600.0)*AST__DD2R;
      if( par[ 5 ] == AST__BAD ) par[ 5 ] = (6.0/3600.0)*AST__DD2R;
      if( par[ 6 ] == AST__BAD ) par[ 6 ] = 0.0;
   }

/* Ensure the pixel sizes have the correct signs. */
   if( usesys && !strcmp( usesys, "AZEL" ) ) {
      par[ 4 ] = fabs( par[ 4 ] );
   } else {
      par[ 4 ] = -fabs( par[ 4 ] );
   }
   par[ 5 ] = fabs( par[ 5 ] );

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

/* Create the catalogue. */
      kpg1Wrtab( "OUTCAT", nallpos, nallpos, 2, allpos2, AST__CURRENT, 
                 astFrameSet( *skyframe, "" ), "Detector positions", 1, 
                 NULL, labgrp, 1, status );

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
