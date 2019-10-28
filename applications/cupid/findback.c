#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/kaplibs.h"
#include "star/grp.h"
#include "star/thr.h"
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
*     residuals are then smoothed again using a mean filter, to get a
*     surface representing the bias in the initial background estimate.
*     This surface is finally added onto the initial background estimate
*     to obtain the output NDF.

*  Usage:
*     findback in out box

*  ADAM Parameters:
*     BOX() = _INTEGER (Read)
*        The dimensions of each of the filters, in pixels. Each value
*        should be odd (if an even value is supplied, the next higher odd
*        value will be used). The number of values supplied should not
*        exceed the number of significant (i.e. more than one element)
*        pixel axes in the input array. If any trailing values of 1 are
*        supplied, then each pixel value on the corresponding axes
*        will be fitted independently of its neighbours. For instance,
*        if the data array is 3-dimensional, and the third BOX value is 1,
*        then each x-y plane will be fitted independently of the neighbouring
*        planes. If the NDF has more than 1 pixel axis but only 1 value is
*        supplied, then the same value will be used for the both the first
*        and second pixel axes (a value of 1 will be assumed for the third
*        axis if the input array is 3-dimensional).
*     MSG_FILTER = _CHAR (Read)
*        Controls the amount of diagnostic information reported. This is the
*        standard messaging level. The default messaging level is NORM (2).
*        A value of NONE or 0 will suppress all screen output. VERB (3) will
*        indicate progress through the various stages of the algorithm. [NORM]
*     IN = NDF (Read)
*        The input NDF.
*     RMS = _DOUBLE (Read)
*        Specifies a value to use as the global RMS noise level in the
*        supplied data array. The suggested default value is the square root
*        of the mean of the values in the input NDF's Variance component.
*        If the NDF has no Variance component, the suggested default
*        is based on the differences between neighbouring pixel values,
*        measured over the entire input NDF. If multiple slices within the
*        NDF are to be processed independently (see parameter BOX), it
*        may be more appropriate for a separate default RMS to be calculated
*        for each slice. This will normally be the case if the noise could
*        be different in each of the slices. In such cases a null (!) can
*        be supplied for the RMS parameter, which forces a separate
*        default RMS value to be found and used for each slice. Any
*        pixel-to-pixel correlation in the noise can result in these
*        defaults being too low.
*     SUB = _LOGICAL (Read)
*        If a TRUE value is supplied, the output NDF will contain the
*        difference between the supplied input data and the estimated
*        background. If a FALSE value is supplied, the output NDF will
*        contain the estimated background itself. [FALSE]
*     OUT = NDF (Write)
*        The output NDF containing either the estimated background, or the
*        background-subtracted input data, as specified by parameter SUB.
*     WLIM = _REAL (Read)
*        If the input NDF contains bad pixels, then this parameter
*        may be used to determine the number of good pixels which must
*        be present within the filter box before a valid output
*        pixel is generated.  It can be used, for example, to prevent
*        output pixels from being generated in regions where there are
*        relatively few good pixels to contribute to the filtered
*        result.
*
*        If a null (!) value is used for WLIM, the pattern of bad pixels
*        is propagated from the input NDF to the output NDF unchanged. In
*        this case, filtered output values are only calculated for those
*        pixels which are not bad in the input NDF.
*
*        If a numerical value is given for WLIM, then it specifies the
*        minimum fraction of good pixels which must be present in the
*        filter box in order to generate a good output pixel.  If
*        this specified minimum fraction of good input pixels is not
*        present, then a bad output pixel will result, otherwise a
*        filtered output value will be calculated.  The value of this
*        parameter should lie between 0.0 and 1.0 (the actual number
*        used will be rounded up if necessary to correspond to at least
*        1 pixel). [0.3]

*  Notes:
*     - Smoothing cubes in 3 dimensions can be very slow.

*  Copyright:
*     Copyright (C) 2009,2013 Science and Technology Facilities Council.
*     Copyright (C) 2006, 2007 Particle Physics & Astronomy Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-SEP-2006 (DSB):
*        Original version.
*     19-MAR-2007 (DSB):
*        - Added parameters SUB and RMS.
*        - Fix bug that left the output NDF uninitialised if ILEVEL is set
*        non-zero.
*        - Use generic data type handling as in FINDCLUMPS.
*     14-JAN-2009 (TIMJ):
*        Use MERS for message filtering.
*     29-JUL-2009 (TIMJ):
*        Rename ILEVEL to MSG_FILTER
*     17-MAY-2011 (DSB):
*        Use sqrt rather than sqrtf when calculating RMS.
*     12-SEP-2011 (DSB):
*        Process slices in separate threads.
*     5-APR-2013 (DSB):
*        Use thrGetWorkforce instead of thrCreateWorkforce in order to
*        avoid accumulation of rsource usage associated with each new
*        created workforce, which can be a problem when running from a
*        monolith. And do not delete the singleton workforce.
*     10-JUL-2013 (DSB):
*        - Added parameter WLIM.
*        - Fixed incorrect lower bounds when any insignificant axes are
*        present (this only affected debugging tools).
*     {enter_further_changes_here}

*-
*/

/* Local Variables: */
   CupidFindback0Data *job_data; /* Pointer to data for all jobs */
   CupidFindback0Data *pdata; /* Pointer to data for current job */
   Grp *grp;                 /* GRP identifier for configuration settings */
   ThrWorkForce *wf = NULL;  /* Pool of persistent worker threads */
   char dtype[ 21 ];         /* HDS data type for output NDF */
   char itype[ 21 ];         /* HDS data type to use when processing */
   double *ipv;              /* Pointer to Variance array */
   double *pd1;              /* Pointer to double precision input data */
   double *pd2;              /* Pointer to double precision output data */
   double rms;               /* Global rms error in data */
   double sum;               /* Sum of variances */
   float *pf1;               /* Pointer to single precision input data */
   float *pf2;               /* Pointer to single precision output data */
   float wlim;               /* Min. frac of good pixels required in a filter box */
   hdsdim dim[ NDF__MXDIM ]; /* Dimensions of each NDF pixel axis */
   hdsdim islice;            /* Slice index */
   hdsdim iystep;            /* Index of slice in ydirection */
   hdsdim izstep;            /* Index of slice in z direction */
   hdsdim lbnd[ NDF__MXDIM ];/* Lower pixel bounds of slice */
   hdsdim nslice;            /* Number of slices to process */
   hdsdim nystep;            /* Number of independent y slices */
   hdsdim nzstep;            /* Number of slices in z direction */
   hdsdim sdim[ 3 ];         /* Dimensions of each significant NDF axis */
   hdsdim slbnd[ 3 ];        /* Lower bounds of each significant NDF axis */
   hdsdim slice_dim[ 3 ];    /* Dimensions of each significant slice axis */
   hdsdim slice_lbnd[ 3 ];   /* Lower bounds of each significant slice axis */
   hdsdim slice_size;        /* Number of pixels in each slice */
   hdsdim ubnd[ NDF__MXDIM ];/* Upper pixel bounds of slice */
   int *old_status;          /* Pointer to original status value */
   int box[ 3 ];             /* Dimensions of each cell in pixels */
   int i;                    /* Loop count */
   int indf1;                /* Identifier for input NDF */
   int indf2;                /* Identifier for output NDF */
   int n;                    /* Number of values summed in "sum" */
   int ndim;                 /* Total number of pixel axes in NDF */
   int newalg;               /* Use experimental algorithm variations? */
   int nsdim;                /* Number of significant pixel axes in NDF */
   int nval;                 /* Number of values supplied */
   int state;                /* Parameter state */
   int sub;                  /* Output the background-subtracted input data? */
   int type;                 /* Integer identifier for data type */
   int var;                  /* Does i/p NDF have a Variance component? */
   size_t el;                /* Number of elements mapped */
   size_t size;              /* Size of GRP group */
   void *ipd1;               /* Pointer to input Data array */
   void *ipd2;               /* Pointer to output Data array */
   void *ipdin;              /* Pointer to input Data array */
   void *ipdout;             /* Pointer to output Data array */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Start an NDF context */
   ndfBegin();

/* Record the existing AST status pointer, and ensure AST uses the supplied
   status pointer instead. */
   old_status = astWatch( status );

/* Get an identifier for the input NDF. We use NDG (via kpg1_Rgndf)
   instead of calling ndfAssoc directly since NDF/HDS has problems with
   file names containing spaces, which NDG does not have. */
   kpg1Rgndf( "IN", 1, 1, "", &grp, &size, status );
   ndgNdfas( grp, 1, "READ", &indf1, status );
   grpDelet( &grp, status );

/* Get the pixel index bounds of the input NDF. */
   ndfBound( indf1, NDF__MXDIM, lbnd, ubnd, &ndim, status );

/* Identify and count the number of significant axes (i.e. axes spanning
   more than 1 pixel). Also record their dimensions and bounds. */
   nsdim = 0;
   for( i = 0; i < ndim; i++ ) {
      dim[ i ] = ubnd[ i ] - lbnd[ i ] + 1;
      if( dim[ i ] > 1 ) {
         slbnd[ nsdim ] = lbnd[ i ];
         sdim[ nsdim++ ] = dim[ i ];
      }
   }

/* If there are too many significant axes, report an error. */
   if( nsdim > 3 && *status == SAI__OK ) {
       *status = SAI__ERROR;
       ndfMsg( "N", indf1 );
       msgSeti( "NS", nsdim );
       errRep( "", "The NDF '^N' has ^NS significant pixel axes, but this"
               "application requires 1, 2 or 3.", status );
   }

/* Ensure we have 3 values in sdim and slbnd (pad with trailings 1's
   if required). */
   if( nsdim < 3 ) {
      slbnd[ 2 ] = 1;
      sdim[ 2 ] = 1;
   }
   if( nsdim < 2 ) {
      slbnd[ 1 ] = 1;
      sdim[ 1 ] = 1;
   }

/* See if the output is to contain the background-subtracted data, or the
   background estimate itself. */
   parGet0l( "SUB", &sub, status );

/* Create the output by propagating everything except the Data and
   (if we are outputting the background itself) Variance arrays. */
   if( sub ) {
      ndfProp( indf1, "UNITS,AXIS,WCS,QUALITY,VARIANCE", "OUT", &indf2,
               status );
   } else {
      ndfProp( indf1, "UNITS,AXIS,WCS,QUALITY", "OUT", &indf2, status );
   }

   msgBlankif( MSG__VERB, status );

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

   msgOutiff( MSG__VERB, "", "Using box sizes [%d,%d,%d].", status,
              box[0], box[1], box[2]);

/* If any trailing axes have a cell size of 1, then we apply the algorithm
   independently to every pixel index on the trailing axes. First of all
   set things up assuming that there are no trailing axes with cell size
   of 1. */
   nystep = 1;
   nzstep = 1;
   slice_dim[ 0 ] = sdim[ 0 ];
   slice_dim[ 1 ] = sdim[ 1 ];
   slice_dim[ 2 ] = sdim[ 2 ];
   slice_lbnd[ 0 ] = slbnd[ 0 ];
   slice_lbnd[ 1 ] = slbnd[ 1 ];
   slice_lbnd[ 2 ] = slbnd[ 2 ];

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
   ndfMtype( "_REAL,_DOUBLE", indf1, indf1, "Data,Variance", itype,
             20, dtype, 20, status );
   if( !strcmp( itype, "_DOUBLE" ) ) {
      type = CUPID__DOUBLE;
   } else {
      type = CUPID__FLOAT;
   }

   ndfStype( dtype, indf2, "Data,Variance", status );

/* Map the input and output arrays. */
   ndfMap( indf1, "Data", itype, "READ", &ipdin, &el, status );
   ndfMap( indf2, "Data", itype, "WRITE", &ipdout, &el, status );

/* If the rms value is supplied on the command, there is no need to
   calculate a default value. */
   parState( "RMS", &state, status );
   if( state == PAR__GROUND ) {

/* Calculate the default RMS value. If the NDF has a Variance component
   it is the square root of the mean Variance value. Otherwise, it is found
   by looking at differences between adjacent pixel values in the Data
   component. */
      ndfState( indf1, "VARIANCE", &var, status );
      if( *status == SAI__OK && var ) {
         ndfMap( indf1, "VARIANCE", "_DOUBLE", "READ", (void *) &ipv, &el, status );

         sum = 0.0;
         n = 0;
         for( i = 0; i < el; i++ ) {
            if( ipv[ i ] != VAL__BADD ) {
               sum += ipv[ i ];
               n++;
            }
         }

         if( n > 0 ) {
            rms = sqrt( sum/n );

         } else {
            *status = SAI__ERROR;
            errRep( "", "The supplied data contains insufficient "
                    "good Variance values to continue.", status );
         }

      } else {
         ipv = NULL;
         rms = cupidRms( type, ipdin, el, sdim[ 0 ], status );
      }

/* Set the default RMS noise level. */
      parDef0d( "RMS", rms, status );
   }

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Get the RMS noise level. */
   parGet0d( "RMS", &rms, status );

/* Annul the error and use an RMS value of VAL__BAD if a null parameter
   value was supplied. This causes an independent default noise estimate to
   be used for each slice of the base NDF. */
   if( *status == PAR__NULL ) {
      errAnnul( status );
      rms = VAL__BADD;
   }

/* See if any experimental algorithm variations are to be used. */
   parGet0l( "NEWALG", &newalg, status );

/* Get the minimum fraction of good pixels required in a filter box to
   generate a good output value. */
   if( *status == SAI__OK ) {
      parGet0r( "WLIM", &wlim, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         wlim = -1.0;
      } else {
         if( wlim < 0.0 ) {
            wlim = 0.0;
         } else if( wlim > 1.0 ) {
            wlim = 1.0;
         }
      }
   }

/* Create a pool of worker threads. */
   wf = thrGetWorkforce( thrGetNThread( "CUPID_THREADS", status ), status );

/* Get memory to hold a description of each job passed to a worker. There
   is one job for each slice. */
   nslice = nystep*nzstep;
   job_data = astMalloc( nslice*sizeof( *job_data ) );
   if( *status == SAI__OK ) {

/* Loop round all slices to be processed. */
      ipd1 = ipdin;
      ipd2 = ipdout;
      islice = 0;
      pdata = job_data;

      for( izstep = 0; izstep < nzstep ; izstep++ ) {

         slice_lbnd[ 1 ] = slbnd[ 1 ];

         for( iystep = 0; iystep < nystep; iystep++, islice++,pdata++ ) {

/* Store the information needed by the function (cupidFindback0) that
   does the work in a thread. */
            pdata->islice = islice;
            pdata->nslice = nslice;
            pdata->type = type;
            pdata->ndim = ndim;
            pdata->box[ 0 ] = box[ 0 ];
            pdata->box[ 1 ] = box[ 1 ];
            pdata->box[ 2 ] = box[ 2 ];
            pdata->rms = rms;
            pdata->ipd1 = ipd1;
            pdata->ipd2 = ipd2;
            pdata->slice_dim[ 0 ] = slice_dim[ 0 ];
            pdata->slice_lbnd[ 0 ] = slice_lbnd[ 0 ];
            pdata->slice_dim[ 1 ] = slice_dim[ 1 ];
            pdata->slice_lbnd[ 1 ] = slice_lbnd[ 1 ];
            pdata->slice_dim[ 2 ] = slice_dim[ 2 ];
            pdata->slice_lbnd[ 2 ] = slice_lbnd[ 2 ];
            pdata->newalg = newalg;
            pdata->wlim = wlim;
            pdata->slice_size = slice_size;

/* Submit a job to the workforce to process the current slice. */
            thrAddJob( wf, 0, pdata, cupidFindback0, 0, NULL, status );

/* Update pointers to the start of the next slice in the input and output
   arrays. */
            if( type == CUPID__FLOAT ) {
               ipd1 = ( (float *) ipd1 ) + slice_size;
               ipd2 = ( (float *) ipd2 ) + slice_size;
            } else {
               ipd1 = ( (double *) ipd1 ) + slice_size;
               ipd2 = ( (double *) ipd2 ) + slice_size;
            }

/* Increment the lower bound on the 2nd pixel axis. */
            slice_lbnd[ 1 ]++;
         }

/* Increment the lower bound on the 3rd pixel axis. */
         slice_lbnd[ 2 ]++;
      }

/* Wait until all jobs have finished. */
      thrWait( wf, status );
   }

/* The output currently holds the background estimate. If the user has
   requested that the output should hold the background-subtracted input
   data, then do the arithmetic now. */
   if( sub && *status == SAI__OK ) {
      if( type == CUPID__FLOAT ) {
         pf1 = (float *) ipdin;
         pf2 = (float *) ipdout;
         for( i = 0; i < el; i++, pf1++, pf2++ ) {
            if( *pf1 != VAL__BADR && *pf2 != VAL__BADR ) {
               *pf2 = *pf1 - *pf2;
            } else {
               *pf2 = VAL__BADR;
            }
         }

      } else {
         pd1 = (double *) ipdin;
         pd2 = (double *) ipdout;
         for( i = 0; i < el; i++, pd1++, pd2++ ) {
            if( *pd1 != VAL__BADD && *pd2 != VAL__BADD ) {
               *pd2 = *pd1 - *pd2;
            } else {
               *pd2 = VAL__BADD;
            }
         }

      }
   }

/* Tidy up */
L999:;
   msgBlankif( MSG__VERB, status );

/* Free workspace. */
   job_data = astFree( job_data );

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

