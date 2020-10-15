/*
*+
*  Name:
*     smf_sparsebounds

*  Purpose:
*     Calculate the pixel index bounds for a sparse array of spectra.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_sparsebounds( ThrWorkForce *wf, Grp *igrp,  int size, AstSkyFrame *oskyframe,
*                       int usedetpos, Grp *detgrp, dim_t lbnd[ 3 ],
*                       dim_t ubnd[ 3 ], AstFrameSet **wcsout, int *hasoffexp,
*                       int *polobs, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     igrp = Grp * (Given)
*        Group of input NDFs.
*     size = int (Given)
*        Number of elements in igrp
*     oskyframe = AstSkyFrame * (Given)
*        A SkyFrame that specifies the coordinate system used to describe the
*        spatial axes of the output cube.
*     usedetpos = int (Given)
*        If a non-zero value is supplied, then the detector positions for
*        a given time slice are read directly from the input NDF. Otherwise
*        the detector positions are calculated on the basis of the focal
*        plane detector positions and the telescope pointing information.
*     detgrp = Grp * (Given)
*        A Group containing the names of the detectors to be used. All
*        detectors will be used if this group is empty.
*     lbnd = dim_t [ 3 ] (Returned)
*        The lower pixel index bounds of the output cube.
*     ubnd = dim_t [ 3 ] (Returned)
*        The upper pixel index bounds of the output cube.
*     wcsout = AstFrameSet ** (Returned)
*        A pointer to a location at which to return a pointer to an AST
*        Frameset describing the WCS to be associated with the output cube.
*     hasoffexp = int * (Returned)
*        Address of an int in which to return a flag indicating if any of
*        the supplied input files has a OFF_EXPOSURE component in the JCMTSTATE
*        NDF extension.
*     polobs = int * (Returned)
*        Non-zero if all the input files contain polarisation data.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function finds the pixel index bounds of the 3D output cube that
*     will just encompass all the data in the supplied group of input
*     NDFs, storing the spectra in a sparse list of spatial positions
*     rather than a regularly gridded tangent plane projection. Each input
*     NDF should be an ACSIS archive file. A WCS FrameSet is also returned
*     for the output cube. The base Frame in this FrameSet is 3D GRID coords
*     in the cube, and the current Frame is a CmpFrame holding (lon,lat,freq)
*     axes, where "lon,lat" are celestial longitude and latitude in the
*     system specified by "system". The spectral WCS axis is inherited from
*     the first supplied input data file.

*  Authors:
*     David S Berry (JAC, UCLan)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     30-NOV-2006 (DSB):
*        Initial version, based on smf_cubebounds.
*     20-DEC-2006 (DSB):
*        Correct calculation of pixel bounds for spectral axis.
*     15-FEB-2007 (DSB):
*        Report error if SPECBOUNDS values do not have any overlap with
*        the spectral range covered by the data.
*     24-APR-2007 (DSB):
*        Added hasoffexp argument.
*     29-OCT-2007 (EC):
*        Modified interface to smf_open_file.
*     18-DEC-2007 (AGG):
*        Update to use new smf_free behaviour
*     21-JAN-2008 (DSB):
*        Added argument polobs.
*     10-JAN-2014 (DSB):
*        Added argument wf.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008,2014 Science & Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/thr.h"
#include "star/ndg.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "sc2da/sc2ast.h"

#define FUNC_NAME "smf_sparsebounds"

void smf_sparsebounds( ThrWorkForce *wf, Grp *igrp,  int size, AstSkyFrame *oskyframe,
                       int usedetpos, Grp *detgrp, dim_t lbnd[ 3 ],
                       dim_t ubnd[ 3 ], AstFrameSet **wcsout, int *hasoffexp,
                       int *polobs, int *status ){

/* Local Variables */
   AstCmpMap *cmpmap1 = NULL;   /* Combined Mappings */
   AstCmpMap *cmpmap2 = NULL;   /* Combined Mappings */
   AstCmpMap *cmpmap3 = NULL;   /* Combined Mappings */
   AstCmpMap *cmpmap4 = NULL;   /* Combined Mappings */
   AstCmpMap *ssmap = NULL;     /* I/p GRID-> o/p PIXEL Mapping for spectral axis */
   AstFitsChan *fc;             /* Storage for FITS headers */
   AstFrame *ospecframe = NULL; /* Spectral Frame in output FrameSet */
   AstFrame *specframe = NULL;  /* Spectral Frame in input FrameSet */
   AstFrameSet *fs = NULL;      /* A general purpose FrameSet pointer */
   AstFrameSet *swcsin = NULL;  /* FrameSet describing spatial input WCS */
   AstLutMap *lutmap1;          /* Longitude LutMap */
   AstLutMap *lutmap2;          /* Latitude LutMap */
   AstMapping *fsmap = NULL;    /* Base->Current Mapping extracted from a FrameSet */
   AstMapping *ospecmap = NULL; /* Spec <> PIXEL mapping in output FrameSet */
   AstMapping *smap = NULL;     /* Simplified Mapping */
   AstMapping *specmap = NULL;  /* PIXEL -> Spec mapping in input FrameSet */
   AstMapping *tmap = NULL;     /* Temporary Mapping */
   AstPermMap *pmap = NULL;     /* Axis permutation */
   const char *name;     /* Pointer to current detector name */
   dim_t ishift;         /* Shift to put pixel origin at centre */
   dim_t ispec;          /* Index of current spectral sample */
   dim_t itime;          /* Index of current time slice */
   dim_t npix;           /* Number of pixels along axis */
   double *latlut = NULL;/* Workspace to hold latitude values */
   double *lonlut = NULL;/* Workspace to hold longitude values */
   double *xin = NULL;   /* Workspace for detector input grid positions */
   double *xout = NULL;  /* Workspace for detector output pixel positions */
   double *yin = NULL;   /* Workspace for detector input grid positions */
   double *yout = NULL;  /* Workspace for detector output pixel positions */
   double ispecbounds[ 2 ];/* Bounds of spectral axis in grid pixels */
   double shift;         /* Shift from PIXEL to GRID coords */
   double slbnd;         /* Floating point lower bounds for spectral axis */
   double specbounds[ 2 ];/* Bounds of spectral axis in spectral WCS units */
   double specin[ 2 ];   /* Spectral values to be transformed */
   double specout[ 2 ];  /* Transformed spectral values */
   double subnd;         /* Floating point upper bounds for spectral axis */
   double temp;          /* Temp value used when swapping other values */
   float *pdata;         /* Pointer to next data sample */
   int found;            /* Was current detector name found in detgrp? */
   int good;             /* Are there any good detector samples? */
   int ibasein;          /* Index of base Frame in input FrameSet */
   int ifile;            /* Index of current input file */
   int inperm[ 2 ];      /* Input axis permutation array */
   int irec;             /* Index of current input detector */
   int lutsize;          /* No. of values in lonlut and latlut */
   int nval;             /* Number of values supplied */
   int outperm[ 2 ];     /* Output axis permutation array */
   int pixax[ 3 ];       /* The output fed by each selected mapping input */
   int specax;           /* Index of spectral axis in input FrameSet */
   smfData *data = NULL; /* Pointer to data struct for current input file */
   smfFile *file = NULL; /* Pointer to file struct for current input file */
   smfHead *hdr = NULL;  /* Pointer to data header for this time slice */

/* Initialise */
   *hasoffexp = 0;
   *polobs = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Tell the user what is happening. */
   msgOutif( MSG__VERB, " ", "SMURF_MAKECUBE: Determine sparse cube bounds",
             status );

/* Begin an AST context. */
   astBegin;

/* Initialise the bounds of the spectral axis in the output cube in
   floating point PIXEL coords. */
   slbnd = VAL__MAXD;
   subnd = VAL__MIND;

/* Initialise the lut pointers and size. */
   lonlut = NULL;
   latlut = NULL;
   lutsize = 0;

/* Assume for the moment that all data is polarisation data. */
   *polobs = 1;

/* Loop round all the input NDFs. */
   for( ifile = 1; ifile <= size && *status == SAI__OK; ifile++ ) {

/* Obtain information about the current input NDF. */
      smf_open_file( wf, igrp, ifile, "READ", 0, &data, status );

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
      smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
      msgSeti( "I", ifile );
      msgSeti( "N", size );
      msgOutif( MSG__VERB, " ", "SMF_SPARSEBOUNDS: Processing ^I/^N ^FILE",
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

/* If the detector positions are to calculated on the basis of FPLANEX/Y
   rather than RECEPPOS, then free the detpos array in the smfHead
   structure. This will cause smf_tslice_ast to use the fplanex/y values. */
      if( !usedetpos && hdr->detpos ) {
         hdr->detpos = astFree( (double *) hdr->detpos );
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
               smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
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
               smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
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
            smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "The spectral axis in ^FILE is not "
                    "independent of the other axes.", status );
         }
         break;
      }

/* If this is the first input file, save a copy of this mapping for use as
   the spectral Mapping in the output WCS FrameSet. */
      if( !ospecmap ) ospecmap = astCopy( specmap );

/* Invert the Mapping for the spectral axis so that it goes from GRID
   coord to spectral coord. */
      astInvert( specmap );

/* The spectral axis of the output cube is inherited from the spectral axis
   of the first input file. So, if this is the first input file, initialise
   the bounds of the spectral GRID axis in the output cube. */
      if( !ospecframe ) {
         ospecframe = astCopy( specframe );
         slbnd = 1.0;
         subnd = (data->dims)[ 0 ];

/* If this is not the first input file, then there is potentially a
   difference between the spectral system of this input file and the spectral
   system of the output cube. So use astConvert to get a Mapping from one
   to the other. */
      } else {
         fs = astConvert( specframe, ospecframe, " " );

/* Report an error and abort if no conversion could be found between the two
   spectral axes. */
         if( !fs ) {
            if( *status == SAI__OK ) {
               smf_smfFile_msg( file, "FILE", 1, "<unknown>" );
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
                                          1, " " ),
                               ospecmap, 1, " " );

         }

/* Use this Mapping to transform the first and last spectral GRID values
   in the input into the corresponding values on the output spectral PIXEL
   axis. */
         specin[ 0 ] = 1.0;
         specin[ 1 ] = (data->dims)[ 0 ];
         astTran1( ssmap, 2, specin, 1, specout );

/* Update the bounds of the output cube on the spectral PIXEL axis. */
         if( specout[ 0 ] < slbnd ) slbnd = specout[ 0 ];
         if( specout[ 0 ] > subnd ) subnd = specout[ 0 ];
         if( specout[ 1 ] < slbnd ) slbnd = specout[ 1 ];
         if( specout[ 1 ] > subnd ) subnd = specout[ 1 ];
      }

/* Allocate work arrays big enough to hold the coords of all the
   detectors in the current input file.*/
      xin = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
      yin = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
      xout = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
      yout = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );

/* Initialise a string to point to the name of the first detector for which
   data is available */
      name = hdr->detname;

/* Store input coords for the detectors. Axis 1 is the detector index, and
   axis 2 is a dummy axis that always has the value 1. */
      for( irec = 0; irec < (data->dims)[ 1 ]; irec++ ) {
         xin[ irec ] = irec + 1.0;
         yin[ irec ] = 1.0;

/* If a group of detectors to be used was supplied, search the group for
   the name of the current detector. If not found, set the GRID coords bad. */
         if( detgrp ) {
            found = (int) grpIndex( name, detgrp, 1, status );
            if( !found ) {
               xin[ irec ] = AST__BAD;
               yin[ irec ] = AST__BAD;
            }
         }

/* Move on to the next available detector name. */
         name += strlen( name ) + 1;

      }

/* Store a pointer to the next input data value */
      pdata = ( data->pntr )[ 0 ];

/* Extend the arrays holding the longitude and latitude associated with
   each spectrum to be stored in the output NDF. */
      lonlut = astGrow( lonlut, lutsize + (data->dims)[ 1 ]*(data->dims)[ 2 ],
                        sizeof( double ) );
      latlut = astGrow( latlut, lutsize + (data->dims)[ 1 ]*(data->dims)[ 2 ],
                        sizeof( double ) );

/* Loop round all the time slices in the input file. */
      for( itime = 0; itime < (data->dims)[ 2 ] && *status == SAI__OK; itime++ ) {

/* Get a FrameSet describing the spatial coordinate systems associated with
   the current time slice of the current input data file. The base frame in
   the FrameSet will be a 2D Frame in which axis 1 is detector number and
   axis 2 is unused. The current Frame will be a SkyFrame (the SkyFrame
   System may be any of the JCMT supported systems). The Epoch will be
   set to the epoch of the time slice. */
         smf_tslice_ast( data, itime, 1, NO_FTS, status );
         swcsin = hdr->wcs;

/* Update the flag indicating if any OFF_EXPOSURE values are available in
   the input data. */
         if( hdr->state->acs_offexposure != VAL__BADR ) *hasoffexp = 1;

/* Update the flag indicating if all input data is polarisation data. */
         if( hdr->state->pol_ang == VAL__BADD ) *polobs = 0;

/* We now create a Mapping from detector index to position in oskyframe. */
         astInvert( swcsin );
         ibasein = astGetI( swcsin, "Base" );
         fs = astConvert( swcsin, oskyframe, "SKY" );
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

/* Get a simplified Mapping form the FrameSet. */
         tmap = astGetMapping( fs, AST__BASE, AST__CURRENT );
         smap = astSimplify( tmap );

/* Transform the positions of the detectors from input GRID to oskyframe
   coords. */
         astTran2( smap, (data->dims)[ 1 ], xin, yin, 1, xout, yout );

/* Loop round all detectors. */
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

/* If it did, add the position of the detector to the two look-up-tables. */
               if( good ) {
                  lonlut[ lutsize ] = xout[ irec ];
                  latlut[ lutsize ] = yout[ irec ];
                  lutsize++;
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
         tmap = astAnnul( tmap );
         smap = astAnnul( smap );
      }

/* Close the current input data file. */
      smf_close_file( wf, &data, status);
      data = NULL;

/* Free work space. */
      xin = astFree( xin );
      yin = astFree( yin );
      xout = astFree( xout );
      yout = astFree( yout );
   }

/* Close any data file that was left open due to an early exit from the
   above loop. */
   if( data != NULL ) smf_close_file( wf, &data, status );

/* Check we found some usable data. */
   if( lutsize == 0 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( FUNC_NAME, "No usable data positions found.", status );
      }
   }

/* See if the user wants to restrict the spectral range of the output cube.
   First get the bounds of the full frequency axis. */
   ispecbounds[ 0 ] = slbnd;
   ispecbounds[ 1 ] = subnd;
   astTran1( ospecmap, 2, ispecbounds, 0, specbounds );

/* Now allow the user to provide alternative values. The above values are
   used as dynamic defaults for the SPECBOUNDS parameter. */
   kpg1Gtaxv( "SPECBOUNDS", 2, 1, ospecframe, 1, specbounds, &nval, status );

/* Convert the supplied spectral values back to pixel coords. */
   astTran1( ospecmap, 2, specbounds, 1, ispecbounds );

/* Ensure element 0 is lower than element 1. */
   if( ispecbounds[ 0 ] > ispecbounds[ 1 ] ) {
      temp = ispecbounds[ 0 ] ;
      ispecbounds[ 0 ] = ispecbounds[ 1 ];
      ispecbounds[ 1 ] = temp;
   }

/* Check the specified spectral bounds have some overlap with the available
   spectral range. */
   if( ispecbounds[ 0 ] >= subnd ||
       ispecbounds[ 1 ] <= slbnd ){
      if( *status == SAI__OK ) {

         specin[ 0 ] = slbnd;
         specin[ 1 ] = subnd;
         astTran1( ospecmap, 2, specin, 0, specout );

         *status = SAI__ERROR;
         msgSetd( "L", specbounds[ 0 ] );
         msgSetd( "U", specbounds[ 1 ] );
         msgSetd( "LL", specout[ 0 ] );
         msgSetd( "UU", specout[ 1 ] );
         errRep( "", "Requested spectral bounds (^L,^U) do not overlap "
                 "the spectral range of the data (^LL,^UU).", status );
      }
   }

/* Update the output bounds. */
   slbnd = ispecbounds[ 0 ] ;
   subnd = ispecbounds[ 1 ] ;

/* Create the output WCS FrameSet. First create the two lutmaps. These go
   from GRID coords to celestial (lon,lat) values in oskyframe. */
   lutmap1 = astLutMap( lutsize, lonlut, 1.0, 1.0, "LutInterp=1" );
   lutmap2 = astLutMap( lutsize, latlut, 1.0, 1.0, "LutInterp=1" );

/* Free the lut arrays. */
   lonlut = astFree( lonlut );
   latlut = astFree( latlut );

/* Combine the two LutMaps in parallel. */
   cmpmap1 = astCmpMap( lutmap1, lutmap2, 0, " " );

/* Create a PermMap with a forward transformation that duplicates axis 1
   and discards axis 2. */
   inperm[ 0 ] = 1;
   inperm[ 1 ] = 0;
   outperm[ 0 ] = 1;
   outperm[ 1 ] = 1;
   pmap = astPermMap( 2, inperm, 2, outperm, NULL, " " );

/* Combine this PermMap in series with the two LutMaps. */
   cmpmap2 = astCmpMap( pmap, cmpmap1, 1, " " );

/* Store the pixel index bounds on the first two axes. */
   lbnd[ 0 ] = 1;
   ubnd[ 0 ] = lutsize;
   lbnd[ 1 ] = 1;
   ubnd[ 1 ] = 1;

/* Find the number of pixels needed to span the third pixel axis range. */
   npix = 1 + NINT( subnd - slbnd );

/* Find a fractional pixel shift which puts the mid point of the axis
   range at the mid point of a span of "npix" pixels. */
   shift = 0.5*( 1 + npix - slbnd - subnd );

/* Find the upper and lower integer bounds after applying this shift. */
   lbnd[ 2 ] = NINT( slbnd + shift );
   ubnd[ 2 ] = NINT( subnd + shift );

/* Modify the bounds to put the origin in the middle, using the same method as
   the on-line system (this relies on integer division). */
   ishift = 2 + ( ubnd[ 2 ] - lbnd[ 2 ] )/2;
   lbnd[ 2 ] -= ishift;
   ubnd[ 2 ] -= ishift;

/* Now include the shift in the spectral Mapping. */
   astInvert( ospecmap );
   cmpmap3 = astCmpMap( astShiftMap( 1, &shift, " " ), ospecmap, 1, " " );

/* Combine the spectral and spatial Mappings in parallel. */
   cmpmap4 = astCmpMap( cmpmap2, cmpmap3, 0, " " );

/* Create the required FrameSet. */
   *wcsout = astFrameSet( astFrame( 3, "Domain=GRID" ), " " );
   astAddFrame( *wcsout, AST__BASE, cmpmap4,
                astCmpFrame( oskyframe, ospecframe, " " ) );

/* Report the coordinate systems in use in the output cube, and the pixel
   bounds of the cube. */
   if( *status == SAI__OK ) {
      msgOutif( MSG__NORM, " ", " ", status );

      msgSetk( "XL", lbnd[ 0 ] );
      msgSetk( "YL", lbnd[ 1 ] );
      msgSetk( "ZL", lbnd[ 2 ] );
      msgSetk( "XU", ubnd[ 0 ] );
      msgSetk( "YU", ubnd[ 1 ] );
      msgSetk( "ZU", ubnd[ 2 ] );
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


