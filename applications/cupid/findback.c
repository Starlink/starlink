#include "sae_par.h" 
#include "mers.h" 
#include "ndf.h" 
#include "star/ndg.h" 
#include "star/kaplibs.h" 
#include "star/grp.h" 
#include "par.h" 
#include "cupid.h" 
#include "prm_par.h"
#include <math.h> 
#include <string.h> 
#include <stdio.h>

void findback( int *status ){
/*
*+
*  Name:
*     FINDBACK

*  Purpose:
*     Estimate the background in an NDF by removing small scale structure.

*  Language:
*     C 

*  Type of Module:
*     ADAM A-task

*  Synopsis:
*     void findback( int *status );

*  Description:
*     This application uses spatial filtering to remove structure with a
*     scale size less than a specified size from a 1, 2, or 3 dimensional 
*     NDF, thus producing an estimate of the local background within the NDF.
*
*     The algorithm proceeds as follows. A filtered form of the input data 
*     is first produced by replacing every input pixel by the minimum of
*     the input values within a rectangular box centred on the pixel.
*     This filtered data is then filtered again, using a filter that
*     replaces every pixel value by the maximum value in a box centred on 
*     the pixel. This produces an estimate of the lower envelope of the data,
*     but usually contains unacceptable sharp edges. In addition, this 
*     filtered data has a tendency to hug the lower envelope of the
*     noise, thus under-estimating the true background of the noise-free 
*     data. The first problem is minimised by smoothing the background
*     estimate using a filter that replaces every pixel value by the mean
*     of the values in a box centred on the pixel. The second problem 
*     is minimised by estimating the difference between the input data
*     and the background estimate within regions well removed from any
*     bright areas. This difference is then extrapolated into the bright
*     source regions and used as a correction to the background estimate.
*     Specifically, the residuals between the input data and the initial
*     background estimate are first formed, and residuals which are more
*     than three times the RMS noise are set bad. The remaining residuals
*     are smoothed with a mean filter. This smoothing will replace a lot
*     of the bad values rejected above, but may not remove them all. Any
*     remaining bad values are estimated by linear interpolation between
*     the nearest good values along the first axis. The interpolated
*     residuals are then smoothed again using amean filter, to get a
*     surface representing the bias in the initial background estimate.
*     This surface is finally added onto the initial background estimate
*     to obtain the output NDF.

*  Usage:
*     findback in out box ilevel

*  ADAM Parameters:
*     BOX() = _INTEGER (Read)
*        The dimensions of each of the filtes, in pixels. Each value
*        should be odd (if an even value is supplied, the next higher odd
*        value will be used). The number of values supplied should not 
*        exceed the number of pixel axes in the input array. If any trailing 
*        values of 1 are supplied, then each pixel value on the corresponding 
*        axes will be fitted independently of its neighbours. For instance, 
*        if the data array is 3-dimensional, and the third BOX value is 1, 
*        then each x-y plane will be fitted independently of the neighbouring 
*        planes. If the NDF has more than 1 pixel axis but only 1 value is 
*        supplied, then the same value will be used for the both the first 
*        and second pixel axes (a value of 1 will be assumed for the third 
*        axis if the input array is 3-dimensional).
*     ILEVEL = _INTEGER (Read)
*        Controls the amount of diagnostic information reported. It
*        should be in the range 0 to 1. A value of zero will suppress all 
*        screen output. A value of 1 will indicate progress through the
*        various stages of the algorithm. [0]
*     IN = NDF (Read)
*        The input NDF.
*     OUT = NDF (Write)
*        The output NDF containing the fitted lower envelope.

*  Notes:
*     - Smoothing cubes in 3 dimensions can be very slow.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-SEP-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*/      

/* Local Variables: */
   Grp *grp;                 /* GRP identifier for configuration settings */
   char dtype[ 21 ];         /* HDS data type for output NDF */
   char type[ 21 ];          /* HDS data type to use when processing */
   int *old_status;          /* Pointer to original status value */
   int box[ 3 ];             /* Dimensions of each cell in pixels */
   int dim[ NDF__MXDIM ];    /* Dimensions of each NDF pixel axis */
   int el;                   /* Number of elements mapped */
   int i;                    /* Loop count */
   int ilevel;               /* Interaction level */
   int indf1;                /* Identifier for input NDF */
   int indf2;                /* Identifier for output NDF */
   int islice;               /* Slice index */
   int ndim;                 /* Total number of pixel axes in NDF */
   int nsdim;                /* Number of significant pixel axes in NDF */
   int nslice;               /* Number of slices to process */
   int nval;                 /* Number of values supplied */
   int nystep;               /* Number of independent y slices */
   int nzstep;               /* Number of slices in z direction */
   int sdim[ 3 ];            /* Dimensions of each significant NDF axis */
   int size;                 /* Size of GRP group */
   int slice_dim[ 3 ];       /* Dimensions of each significant slice axis */
   int slice_size;           /* Number of pixels in each slice */
   void *ipd1;               /* Pointer to input Data array */
   void *ipd2;               /* Pointer to output Data array */
   void *wa;                 /* Pointer to work array */
   void *wb;                 /* Pointer to work array */
 
/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Start an NDF context */
   ndfBegin();

/* Record the existing AST status pointer, and ensure AST uses the supplied 
   status pointer instead. */
   old_status = astWatch( status );

/* Initialise pointer values. */
   wa = NULL;
   wb = NULL;

/* Get an identifier for the input NDF. We use NDG (via kpg1_Rgndf)
   instead of calling ndfAssoc directly since NDF/HDS has problems with
   file names containing spaces, which NDG does not have. */
   kpg1Rgndf( "IN", 1, 1, "", &grp, &size, status );
   ndgNdfas( grp, 1, "READ", &indf1, status );
   grpDelet( &grp, status );

/* Get the dimensions of the input NDF. */
   ndfDim( indf1, NDF__MXDIM, dim, &ndim, status );

/* Identify and count the number of significant axes (i.e. axes spanning
   more than 1 pixel). Also record their dimensions. */
   nsdim = 0;
   for( i = 0; i < ndim; i++ ) {
      if( dim[ i ] > 1 ) sdim[ nsdim++ ] = dim[ i ];
   }

/* If there are too many significant axes, report an error. */
   if( nsdim > 3 && *status == SAI__OK ) {
       *status = SAI__ERROR;
       ndfMsg( "N", indf1 );
       msgSeti( "NS", nsdim );
       errRep( "", "The NDF '^N' has ^NS significant pixel axes, but this"
               "application requires 1, 2 or 3.", status );
   }

/* Ensure we have 3 values in sdim (pad with trailings 1's if required). */
   if( nsdim < 3 ) sdim[ 2 ] = 1;
   if( nsdim < 2 ) sdim[ 1 ] = 1;

/* Create the output by propagating everything except the Data and
   Variance arrays. */
   ndfProp( indf1, "UNITS,AXIS,WCS,QUALITY", "OUT", &indf2, status );

/* Get the dimensions of each of the filters, in pixels. If only one
   value is supplied, duplicate it as the second value if the second axis
   is significant. If fewer than 3 values were supplied, use 1 for the 3rd 
   value (whether or not it is significant). This results in each plane
   being fitted independently of the adjacent planes by default. */
   parGet1i( "BOX", nsdim, box, &nval, status );
   if( *status != SAI__OK ) goto L999;
   if( nval < 2 ) box[ 1 ] = ( nsdim > 1 ) ? box[ 0 ] : 1;
   if( nval < 3 ) box[ 2 ] = 1;

/* Ensure box sizes are odd. */
   box[ 0 ] = 2*( box[ 0 ] / 2 ) + 1; 
   box[ 1 ] = 2*( box[ 1 ] / 2 ) + 1; 
   box[ 2 ] = 2*( box[ 2 ] / 2 ) + 1; 

/* If any trailing axes have a cell size of 1, then we apply the algorithm 
   independently to every pixel index on the trailing axes. First of all 
   set things up assuming that there are no trailing axes with cell size 
   of 1. */
   nystep = 1;
   nzstep = 1;
   slice_dim[ 0 ] = sdim[ 0 ];
   slice_dim[ 1 ] = sdim[ 1 ];
   slice_dim[ 2 ] = sdim[ 2 ];

/* If the 3rd pixel axis has a cell size of 1, arrange that each slice
   contains a single plane. */
   if( box[ 2 ] == 1 ) {
      nzstep = sdim[ 2 ];
      slice_dim[ 2 ] = 1;

/* If the 2nd pixel axis also has a cell size of 1, arrange that each slice
   contains a single row. */
      if( box[ 1 ] == 1 ) {
         nystep = sdim[ 1 ];
         slice_dim[ 1 ] = 1;
      }
   } 

/* Determine the number of pixels in each independent slice. */
   slice_size = slice_dim[ 0 ]*slice_dim[ 1 ]*slice_dim[ 2 ];

/* Decide what numeric data type to use, and set the output NDF data type. */
   ndfMtype( "_REAL,_DOUBLE", indf1, indf1, "Data,Variance", type, 
             20, dtype, 20, status ); 
   ndfStype( dtype, indf2, "Data,Variance", status );

/* Map the input and output arrays. */
   ndfMap( indf1, "Data", type, "READ", &ipd1, &el, status );
   ndfMap( indf2, "Data", type, "WRITE", &ipd2, &el, status );

/* Allocate work arrays. */
   if( type ) {
      if( !strcmp( type, "_REAL" ) ) {
         wa = astMalloc( sizeof( float )*el );
         wb = astMalloc( sizeof( float )*el );

      } else {
         wa = astMalloc( sizeof( double )*el );
         wb = astMalloc( sizeof( double )*el );
      }
   }

/* Get the interaction level. */
   parGdr0i( "ILEVEL", 0, 0, 1, 1, &ilevel, status );
   if( ilevel > 0 ) msgBlank( status );

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Loop round all slices to be processed. */
   nslice = nystep*nzstep;
   for( islice = 0; islice < nslice; islice++ ) {

/* Report the bounds of the slice if required. */
      if( ilevel > 0 ) {
          msgSeti( "I", islice + 1 );
          msgSeti( "N", nslice );
          msgOut( "", "   Processing slice ^I of ^N...", status );
          msgBlank( status );
      }

/* Process this slice, then increment the pointer to the next slice. */
      else if( !strcmp( type, "_REAL" ) ) {
         cupidFindback1F( slice_dim, box, (float *) ipd1, (float *) ipd2, 
                        (float *) wa, (float *) wb, ilevel, status );
         ipd1 = ( (float *) ipd1 ) + slice_size;
         ipd2 = ( (float *) ipd2 ) + slice_size;
   
      } else {
         cupidFindback1D( slice_dim, box, (double *) ipd1, (double *) ipd2, 
                        (double *) wa, (double *) wb, ilevel, status  );
         ipd1 = ( (double *) ipd1 ) + slice_size;
         ipd2 = ( (double *) ipd2 ) + slice_size;
      }
   }     

/* Tidy up */
L999:;
   if( ilevel > 0 ) msgBlank( status );

/* Free workspace. */
   wa = astFree( wa );
   wb = astFree( wb );

/* Reinstate the original AST inherited status value. */
   astWatch( old_status );

/* End the NDF context */
   ndfEnd( status );

/* If an error has occurred, issue another error report identifying the 
   program which has failed (i.e. this one). */
   if( *status != SAI__OK ) {
      errRep( "FINDBACK_ERR", "FINDBACK: Failed to find the background "
              "of an NDF.", status );
   }
}

