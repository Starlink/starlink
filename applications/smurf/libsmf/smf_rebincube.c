/*
*+
*  Name:
*     smf_rebincube

*  Purpose:
*     Paste a supplied 3D array into an existing cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebincube( smfData *data, int index, int size, AstFrameSet *swcsout, 
*                    AstFrame *ospecfrm, AstMapping *ospecmap, Grp *detgrp,
*                    int moving, dim_t lbnd_out[ 3 ], dim_t ubnd_out[ 3 ], 
*                    float *data_array, float *var_array, double *wgt_array, 
*                    int *status );

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the input smfData structure.
*     index = int (Given)
*        Index of the current input file within the group of input files.
*     size = int (Given)
*        Index of the last input file within the group of input files.
*     swcsout = AstFrameSet * (Given)
*        A FrameSet in which the current Frame respresents 2D spatial GRID 
*        coords in the output, and the base Frame represents 2D sky
*        coords in the output. Note the unusual order of base and current 
*        Frame.
*     ospecfrm = AstFrame * (Given)
*        Pointer to the SpecFrame within the current Frame of the output WCS 
*        Frameset.
*     ospecmap = AstMapping * (Given)
*        Pointer to the Mapping from the SpecFrame to the third GRID axis 
*        within the current Frame of the output WCS Frameset.
*     detgrp = Grp * (Given)
*        A Group containing the names of the detectors to be used. All
*        detectors will be used if this group is empty.
*     moving = int (Given)
*        If non-zero, the telescope is assumed to be tracking a moving
*        object. In this case, each time slice is shifted to the position
*        specified by TCS_AZ_BC1/2 before extending the output cube bouds
*        to include it.
*     lbnd_out = dim_t [ 3 ] (Given)
*        The lower pixel index bounds of the output cube.
*     ubnd_out = dim_t [ 3 ] (Given)
*        The upper pixel index bounds of the output cube.
*     data_array = float * (Given and Returned)
*        The data array for the output cube. This is updated on exit to
*        include the data from the supplied input NDF.
*     var_array = float * (Given and Returned)
*        The variance array for the output cube. This is updated on exit to
*        include the data from the supplied input NDF.
*     wgt_array = double * (Given and Returned)
*        Relative weighting for each pixel in the output cube.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     The data array of the supplied input NDF is added into the existing
*     contents of the output data array, and the variance and weights
*     arrays are updated correspondingly.
*
*     Note, few checks are performed on the validity of the input data
*     files in this function, since they have already been checked within
*     smf_cubebounds.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     20-SEP-2006 (DSB):
*        Initial version.
*     13-OCT-2006 (DSB):
*        Changed to get the input spectral WCS from the WCS FrameSet rather 
*        than the FITS header.
*     6-NOV-2006 (DSB):
*        Added "detgrp" parameter.
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
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_rebincube( smfData *data, int index, int size, AstFrameSet *swcsout,
                    AstFrame *ospecfrm, AstMapping *ospecmap, Grp *detgrp, 
                    int moving, int lbnd_out[ 3 ], int ubnd_out[ 3 ], 
                    float *data_array, float *var_array, double *wgt_array, 
                    int *status ) {

/* Local Variables */
   AstCmpMap *ssmap = NULL;    /* Input GRID->output GRID Mapping for spectral axis */
   AstFitsChan *fc = NULL;     /* FitsChan used to get spectral WCS from input */           
   AstFrame *oskyframe = NULL; /* SkyFrame in output WCS */
   AstFrame *specframe = NULL; /* SpecFrame in input WCS */
   AstFrameSet *fs = NULL;     /* WCS FramesSet from input */           
   AstFrameSet *swcsin = NULL; /* Spatial WCS FrameSet for current time slice */
   AstMapping *fsmap = NULL;   /* WCS->GRID Mapping from input WCS FrameSet */
   AstMapping *specmap = NULL; /* GRID->Spectral Mapping for current input file */
   const char *name;           /* Pointer to current detector name */
   const char *trsys = NULL;   /* AST tracking system */
   dim_t iv;                   /* Vector index into output array */
   dim_t nel;                  /* No. of pixels in output */
   dim_t nxy;                  /* Number of pixels in one output xy plane */
   double *spectab = NULL;     /* Workspace for spectral output grid positions */
   double *xin = NULL;         /* Workspace for detector input grid positions */
   double *xout = NULL;        /* Workspace for detector output grid positions */
   double *yin = NULL;         /* Workspace for detector input grid positions */
   double *yout = NULL;        /* Workspace for detector output grid positions */
   float *pdata = NULL;        /* Pointer to next input data value */
   float dval;                 /* Output data value */
   int dim[ 3 ];               /* Output array dimensions */
   int found;                  /* Was current detector name found in detgrp? */
   int ibasein;                /* Index of base Frame in input WCS FrameSet */
   int ichan;                  /* Index of current channel */
   int idet;                   /* detector index */
   int itime;                  /* Index of current time slice */
   int ix;                     /* Output grid index on axis 1 */
   int iy;                     /* Output grid index on axis 2 */
   int iz;                     /* Output grid index on axis 3 */
   int nchan;                  /* Number of spectral channels */
   int ndet;                   /* Number of detectors in "detgrp" group */
   int pixax[ 3 ];             /* Pixel axis indices */
   int specax;                 /* The index of the input spectral axis */
   smfHead *hdr = NULL;        /* Pointer to data header for this time slice */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Begin an AST context.*/
   astBegin;

/* Store a pointer to the input NDFs smfHead structure. */
   hdr = data->hdr;

/* Store the dimensions of the output array. */
   dim[ 0 ] = ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1;
   dim[ 1 ] = ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1;
   dim[ 2 ] = ubnd_out[ 2 ] - lbnd_out[ 2 ] + 1;

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

/* Extract the SpecFrame that describes the spectral axis from the current 
   Frame of this FrameSet. This is assumed to be the third WCS axis (NB
   the different axis number). */
      specax = 3;
      specframe = astPickAxes( fs, 1, &specax, NULL );
   }

/* Split off the 1D Mapping for this single axis from the 3D Mapping for
   the whole WCS. This results in "specmap" holding the Mapping from 
   SpecFrame value to GRID value. */
   fsmap = astGetMapping( fs, AST__CURRENT, AST__BASE );
   astMapSplit( fsmap, 1, &specax, pixax, &specmap );

/* Invert the Mapping for the spectral axis so that it goes from input GRID
   coord to spectral coord. */
   astInvert( specmap );

/* Get a Mapping that converts values in the input spectral system to the 
   corresponding values in the output spectral system. */
   fs = astConvert( specframe, ospecfrm, "" );

/* Concatenate these Mappings with the supplied spectral Mapping to get 
   a Mapping from the input spectral grid axis (pixel axis 1) to the
   output spectral grid axis (pixel axis 3). Simplify the Mapping. */
   ssmap = astCmpMap( astCmpMap( specmap, astGetMapping( fs, AST__BASE,
                                                         AST__CURRENT ),
                                 1, "" ), 
                      ospecmap, 1, "" );
   ssmap = astSimplify( ssmap );

/* Create a table with one element for each channel in the input array,
   holding the index of the nearest corresponding output pixel. */
   nchan = (data->dims)[ 0 ];
   spectab = astMalloc( sizeof( double )*nchan );
   if( spectab ) {
      for( ichan = 0; ichan < nchan; ichan++ ) spectab[ ichan ] = ichan + 1;
      astTran1( ssmap, nchan, spectab, 1, spectab );
      for( ichan = 0; ichan < nchan; ichan++ ) {
         if( spectab[ ichan ] != AST__BAD ) {
            iz = floor( spectab[ ichan ] + 0.5 );
            if( iz >= 1 && iz <= dim[ 2 ] ) {
               spectab[ ichan ] = iz;
            } else {
               spectab[ ichan ] = 0;
            }             
         } else {
            spectab[ ichan ] = 0;
         }
      }
   }

/* Store the number of elements in an XY plane of the output. */
   nxy = dim[ 0 ]*dim[ 1 ];

/* Store the total number of elements in the output. */
   nel = nxy*dim[ 2 ];

/* If this is the first pass through this file, initialise the arrays. */
   if( index == 1 ){
      for( iv = 0; iv < nel; iv++ ) {
         data_array[ iv ] = 0.0;
         wgt_array[ iv ] = 0.0;
         var_array[ iv ] = 0.0;
      }
   }

/* Allocate work arrays big enough to hold the coords of all the
   detectors in the current input file.*/
   xin = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
   yin = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
   xout = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );
   yout = astMalloc( (data->dims)[ 1 ] * sizeof( double ) );

/* Store the input GRID coords of the detectors to be used. If a non-empty
   group of detectors was supplied, set the GRID coords to AST__BAD for all 
   detectors not included in this group. First see if a non-empty group
   of detectors was supplied. */
   if( detgrp ) {
      grpGrpsz( detgrp, &ndet, status );
   } else {
      ndet = 0;
   }

/* Initialise a string to point to the name of the first detector for which 
   data is available */
   name = hdr->detname;

/* Loop round all detectors for which data is available. */
   for( idet = 0; idet < (data->dims)[ 1 ]; idet++ ) {

/* Store the GRID coord of this detectors. */
      xin[ idet ] = idet + 1.0;
      yin[ idet ] = 1.0;

/* If a group of detectors to be used was supplied, search the group for
   the name of hte current detector. If not found, set the GRID coords bad. */
      if( ndet ) {    
         grpIndex( name, detgrp, 1, &found, status );
         if( !found ) {
            xin[ idet ] = AST__BAD;
            yin[ idet ] = AST__BAD;
         }
      }

/* Move on to the next available detector name. */
      name += strlen( name ) + 1;
   }

/* Store a pointer to the next input data value to use. */
   pdata = (data->pntr)[ 0 ];

/* Loop round all time slices in the input NDF. */
   for( itime = 0; itime < (data->dims)[ 2 ] && *status == SAI__OK; itime++ ) {

/* Get a FrameSet describing the spatial coordinate systems associated with 
   the current time slice of the current input data file. The base frame in 
   the FrameSet will be a 2D Frame in which axis 1 is detector number and 
   axis 2 is unused. The current Frame will be a SkyFrame (the SkyFrame 
   System may be any of the JCMT supported systems). The Epoch will be
   set to the epoch of the time slice. */
      smf_tslice_ast( data, itime, 1, status );
      swcsin = hdr->wcs;

/* If we are dealing with a moving target, adjust the SkyFrames in the
   input and output FrameSets so that they represent offsets from the
   current telescope base position. */
      if( moving ) {

/* The telescope base position is given in tracking coords, so if we have 
   not yet done so, note the AST equivalent of the TCS tracking system, 
   and ensure the output FrameSet uses the same system. Note that the 
   SkyFrame is the base Frame in the swcsout FrameSet, and so the FrameSet 
   needs to be inverted (in order to make the SkyFrame the current Frame) 
   before setting the system value. The Mappings in the FrameSet will be 
   adjusted automatically to ensure that the new sky position of each 
   pixel is the tracking system equivalent of the old pixel position. Also 
   get a pointer to the SkyFrame in the output FrameSet. */
         if( !trsys ) {
            trsys = smf_convert_system( hdr->state->tcs_tr_sys, status );
            astInvert( swcsout );
            astSetC( swcsout, "system", trsys );
            oskyframe = astGetFrame( swcsout, AST__CURRENT );
            astInvert( swcsout );
         }

/* Also ensure that the SkyFrame in swcsin refers to the tracking system. The 
   Mapping to the corresponding GRID coordinate system is modified 
   appropriately. */
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
   base Frames (input GRID to output GRID). So we invert the input WCS
   FrameSet (the output WCS FrameSet has already been inverted). */
      astInvert( swcsin );

/* Now use astConvert to get the Mapping from input GRID to output GRID
   coords, aligning the coordinate systems on the sky. Note the original 
   base Frame index so it can be re-instated afterwards (astConvert changes
   it to indicate the alignment Frame).*/
      ibasein = astGetI( swcsin, "Base" );
      fs = astConvert( swcsin, swcsout, "SKY" );
      astSetI( swcsin, "Base", ibasein );

/* Invert the input WCS FrameSet again to bring it back into its original
   state. */
      astInvert( swcsin );

/* Transform the positions of the detectors from input GRID to output GRID
   coords. */
      astTran2( fs, (data->dims)[ 1 ], xin, yin, 1, xout, yout );

/* For each good position, place the input data values for a whole spectrum
   into the nearest pixel of the output array and increment the weight 
   array. */
      for( idet = 0; idet < (data->dims)[ 1 ]; idet++ ) {

         if( xout[ idet ] != AST__BAD && yout[ idet ] != AST__BAD ) {
            ix = floor( xout[ idet ] + 0.5 ) - 1;
            iy = floor( yout[ idet ] + 0.5 ) - 1;

            if( ix >= 0 && ix < dim[ 0 ] && 
                iy >= 0 && iy < dim[ 1 ] ) {

               for( ichan = 0; ichan < nchan; ichan++, pdata++ ) {
                  iz = spectab[ ichan ] - 1;
                  if( iz >= 0 && iz < dim[ 2 ] && *pdata != VAL__BADR ) { 
                     iv = ix + dim[ 0 ]*iy + nxy*iz;
                     data_array[ iv ] += *pdata;
                     wgt_array[ iv ] += 1.0;
                     var_array[ iv ] += ( *pdata )*( *pdata );

                  }
               }

            } else {
               pdata += nchan;
            }

         } else {
            pdata += nchan;
         }
      }

/* For efficiency, explicitly annul the AST Objects created in this tight
   loop. */
      fs = astAnnul( fs );
   }

/* If this is the final pass through this function, normalise the returned
   data and variance values. */
   if( index == size ) {
      for( iv = 0; iv < nel; iv++ ) {
         if( wgt_array[ iv ] > 0.0 ) {
            dval = data_array[ iv ]/wgt_array[ iv ];
            data_array[ iv ] = dval;
            var_array[ iv ] /=  wgt_array[ iv ];
            var_array[ iv ] -=  dval*dval;
         } else {
            data_array[ iv ] = VAL__BADR;
            var_array[ iv ] = VAL__BADR;
         }
      }
   }

/* Free resources. */
   spectab = astFree( spectab );
   xin = astFree( xin );
   yin = astFree( yin );
   xout = astFree( xout );
   yout = astFree( yout );

/* End the AST context. This will annul all the AST objects created
   within the context. */
   astEnd;
}
