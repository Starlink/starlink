/*
*+
*  Name:
*     smf_rebincube_ast

*  Purpose:
*     Paste a supplied 3D array into an existing cube using astRebinSeq.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebincube_ast( ThrWorkForce *wf, smfData *data, int first, int last,
*                        int *ptime, dim_t nchan, dim_t ndet, dim_t nslice,
*                        dim_t nel, dim_t nxy, dim_t nout, dim_t dim[3],
*                        AstMapping *ssmap, AstSkyFrame *abskyfrm,
*                        AstMapping *oskymap, Grp **detgrp, int moving,
*                        int usewgt, int spread, const double params[],
*                        int genvar, double tfac, double fcon,
*                        float *data_array, float *var_array,
*                        double *wgt_array, float *texp_array,
*                        float *teff_array, int *good_tsys, int64_t *nused,
*                        int *status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads that will do the re-binning.
*     data = smfData * (Given)
*        Pointer to the input smfData structure.
*     first = int (Given)
*        Is this the first call to this routine for the current output
*        cube?
*     last = int (Given)
*        Is this the last call to this routine for the current output
*        cube?
*     ptime = int * (Given)
*        Pointer to an array of integers, each one being the index of a
*        time slice that is to be pasted into the output cube. If this is
*        NULL, then all time slices are used. The values in the array
*        should be monotonic increasing and should be terminated by a value
*        of VAL__MAXI.
*     nchan = dim_t (Given)
*        Number of spectral channels in input cube.
*     ndet = dim_t (Given)
*        Number of detectors in input cube.
*     nslice = dim_t (Given)
*        Number of time slices in input cube.
*     nel = dim_t (Given)
*        Total number of elements in input cube.
*     nxy = dim_t (Given)
*        Number of elements in one spatial plane of the output cube.
*     nout = dim_t (Given)
*        Total number of elements in the output cube.
*     dim[ 3 ] = dim_t (Given)
*        The dimensions of the output array.
*     ssmap = AstMapping * (Given)
*        A Mapping that goes from input spectral grid axis (pixel axis 1)
*        to the output spectral grid axis (pixel axis 3).
*     abskyfrm = AstSkyFrame * (Given)
*        A SkyFrame that specifies the coordinate system used to describe
*        the spatial axes of the output cube. This should represent
*        absolute sky coordinates rather than offsets even if "moving" is
*        non-zero.
*     oskymap = AstFrameSet * (Given)
*        A Mapping from 2D sky coordinates in the output cube to 2D
*        spatial pixel coordinates in the output cube.
*     detgrp = Grp ** (Given)
*        On entry, a Group containing the names of the detectors to be
*        used. All detectors will be used if this group is empty (or NULL).
*        On exit, the supplied group (if any) is deleted, and a new group
*        is created and return holding the names of the detectors that
*        contributed any good data to the output cube.
*     moving = int (Given)
*        A flag indicating if the telescope is tracking a moving object. If
*        so, each time slice is shifted so that the position specified by
*        TCS_AZ_BC1/2 is mapped on to the same pixel position in the
*        output cube.
*     usewgt = int (Given)
*        A flag indicating if the input data should be weighted according
*        to the input variances determined from the input Tsys values.
*     spread = int (Given)
*        Specifies the scheme to be used for dividing each input data value
*        up amongst the corresponding output pixels. See docs for astRebinSeq
*        (SUN/211) for the allowed values.
*     params = const double[] (Given)
*        An optional pointer to an array of double which should contain any
*        additional parameter values required by the pixel spreading scheme.
*        See docs for astRebinSeq (SUN/211) for further information. If no
*        additional parameters are required, this array is not used and a
*        NULL pointer may be given.
*     genvar = int (Given)
*        Indicates how the output variances should be calculated:
*           0 = do not calculate any output variances
*           1 = use spread of input data values
*           2 = use system noise temperatures
*     tfac = double (Given)
*        Factor describing spectral overlap. Used to reduce the weight of
*        spectra that do not have much spectral overlap with the output.
*     fcon = double (Given)
*        The ratio of the squared backend degradation factor to the spectral
*        channel width (this is the factor needed for calculating the
*        variances from the Tsys value).
*     data_array = float * (Given and Returned)
*        The data array for the output cube. This is updated on exit to
*        include the data from the supplied input NDF.
*     var_array = float * (Given and Returned)
*        An array in which to store the variances for the output cube if
*        "genvar" is not zero (the supplied pointer is ignored if "genvar" is
*        zero). The supplied array is update on exit to include the data from
*        the supplied input NDF. This array should be the same shape and size
*        as the output data array.
*     wgt_array = double * (Given and Returned)
*        An array in which to store the relative weighting for each pixel in
*        the output cube. The supplied array is update on exit to include the
*        data from the supplied input NDF. This array should be the length of
*        "data_array", unless "genvar" is 1, in which case it should be twice
*        the length of "data_array".
*     texp_array = float * (Given and Returned)
*        A work array, which holds the total exposure time for each output
*        spectrum. It is updated on exit to include the supplied input NDF.
*        It should be big enough to hold a single spatial plane from the
*        output cube.
*     teff_array = float * (Given and Returned)
*        A work array, which holds the effective integration time for each
*        output spectrum, scaled by a factor of 4. It is updated on exit to
*        include the supplied input NDF. It should be big enough to hold a
*        single spatial plane from the output cube.
*     good_tsys = int * (Given and Returned)
*        Returned set to 1 if any good Tsys values were found in the
*        input cube.
*     nused = int64_t * (Given and Returned)
*        Use to accumulate the total number of input data samples that
*        have been pasted into the output cube.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     The data array of the supplied input NDF is added into the existing
*     contents of the output data array, and the variance and weights
*     arrays are updated correspondingly.
*
*     Since astRebinSeq is used, various spreading schemes are available
*     when pasting each input pixel value into the output cube. The
*     arrays that are used to record the output weights and variances are
*     3-dimensional, meaning that each output pixel has its own weight and
*     variance.  A pixel is bad in the output only if the total weight of
*     the good input pixels that contribute to it is more than 0.
*
*     Note, few checks are performed on the validity of the input data
*     files in this function, since they have already been checked within
*     smf_cubebounds.

*  Authors:
*     David S Berry (JAC, UClan)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     18-APR-2006 (DSB):
*        Initial version.
*     18-MAY-2006 (DSB):
*        Corrections to handle cases where there is only 1 detector.
*     12-JUL-2007 (EC):
*        -Changed name of smf_rebincube_totmap to smf_rebin_totmap
*     2-OCT-2007 (DSB):
*        Use nearest neighbour interpolation with the "detlut" LutMap.
*     11-OCT-2007 (DSB):
*        Added parameter "ptime".
*     8-MAY-2008 (DSB):
*        Use AST__USEVAR flag with astRebinSeq if output variances are
*        based on input noise temperatures.
*     11-FEB-2009 (DSB):
*        Ignore negative or zero input Tsys values.
*     1-OCT-2012 (DSB):
*        Retain all flags when calling astRebinSeq for the last time to
*        normalise the returned values. Previously, lack of the AST__USEVAR
*        flag was causing all output varianes to be set bad within astRebinseq.
*     14-MAY-2014 (DSB):
*        Do not attempt to normalise empty cubes, but issue a warning
*        instead.
*     29-OCT-2020 (DSB):
*        "detgrp" is now used to return the names of the detectors that
*        contributed good data to the cube.
*     15-OCT-2022 (GSB):
*        Add check of jos_drcontrol position problem flag.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2009 Science & Technology Facilities Council.
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
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/ndg.h"
#include "star/atl.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_rebincube_ast"

#define MAXTHREADS 20

/* Local data types */
typedef struct smfRebinCubeAstData {
   float *data;
   size_t stride;
   int lbnd;
   int ubnd;
   char used;
} SmfRebinCubeAstData;

/* Prototypes for local functions */
static void smf1_rebincube_ast( void *job_data_ptr, int *status );


void smf_rebincube_ast( ThrWorkForce *wf, smfData *data, int first, int last,
                      int *ptime, dim_t nchan, dim_t ndet, dim_t nslice,
                      dim_t nel, dim_t nxy, dim_t nout, dim_t dim[3],
                      AstMapping *ssmap, AstSkyFrame *abskyfrm,
                      AstMapping *oskymap, Grp **detgrp, int moving,
                      int usewgt, int spread, const double params[],
                      int genvar, double tfac, double fcon,
                      float *data_array, float *var_array,
                      double *wgt_array, float *texp_array,
                      float *teff_array, int *good_tsys, int64_t *nused,
                      int *status ){

/* Local Variables */
   SmfRebinCubeAstData *pdata;
   SmfRebinCubeAstData *job_data = NULL;
   AstCmpMap *detmap = NULL;   /* Mapping from 1D det. index to 2D i/p "grid" coords */
   AstMapping *dtotmap = NULL; /* 1D det index->o/p GRID Mapping */
   AstMapping *fullmap = NULL; /* WCS->GRID LutMap from input WCS FrameSet */
   AstMapping *lutmap = NULL;  /* Mapping that identifies detectors to be used */
   AstMapping *splut = NULL;   /* Spatial LutMap */
   AstMapping *sslut = NULL;   /* Spectral LutMap */
   AstMapping *totmap = NULL;  /* WCS->GRID Mapping from input WCS FrameSet */
   AstPermMap *pmap;           /* Mapping to rearrange output axes */
   Grp *usedetgrp = NULL;      /* Returned group holding used detectors. */
   char *detflags;             /* Flags indicating if each detector was used */
   const char *name = NULL;    /* Pointer to current detector name */
   const double *tsys = NULL;  /* Pointer to Tsys value for first detector */
   dim_t iv;                   /* Vector index into output 3D array */
   double *detlut = NULL;      /* Work space for detector mask */
   double blk_bot[ 2*MAXTHREADS + 1 ]; /* First o/p channel no. in each block */
   double con;                 /* Constant value */
   double dtemp;               /* Temporary value */
   double gin[ 2 ];            /* Spectral grid index bounds in input */
   double gout[ 2 ];           /* Spectral grid index bounds in output */
   double tcon;                /* Variance factor for whole time slice */
   float *detwork = NULL;      /* Work array for detector values */
   float *tdata = NULL;        /* Pointer to start of input time slice data */
   float *varwork = NULL;      /* Work array holding variances for 1 slice/channel */
   float *vp = NULL;           /* Pointer to next "varwork" element */
   float invar;                /* Input variance */
   float rtsys;                /* Tsys value */
   float teff;                 /* Effective integration time */
   float texp;                 /* Total time ( = ton + toff ) */
   int *nexttime;              /* Pointer to next time slice index to use */
   int ast_flags;              /* Basic flags to use with astRebinSeq */
   int blk_size;               /* Number of channels processed by a single thread */
   int found;                  /* Was current detector name found in detgrp? */
   int iblock;                 /* Index of current spectral block */
   dim_t ichan;                /* Index of current channel */
   dim_t idet;                 /* detector index */
   int ignore;                 /* Ignore this time slice? */
   int inperm[ 3 ];            /* Input axis permutation array */
   dim_t itime;                /* Index of current time slice */
   int64_t junk;               /* Unused parameter */
   int lbnd_in[ 2 ];           /* Lower input bounds on receptor axis */
   int ldim[ 3 ];              /* Output array lower GRID bounds */
   int maxthreads;             /* Max no. of threads to use when re-binning */
   int nblock;                 /* Number of spectral blocks */
   int nthreads;               /* Number of threads to use when re-binning */
   int outperm[ 3 ];           /* Output axis permutation array */
   int timeslice_size;         /* Number of elements in a time slice */
   int ubnd_in[ 2 ];           /* Upper input bounds on receptor axis */
   int uddim[ 1 ];             /* Detector array upper GRID bounds */
   int udim[ 3 ];              /* Output array upper GRID bounds */
   smfHead *hdr = NULL;        /* Pointer to data header for this time slice */
   drcntrl_bits drcntrl_mask = DRCNTRL__TCS_POSN_BIT; /* Mask to use for DRCONTROL */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Store a pointer to the input NDFs smfHead structure. */
   hdr = data->hdr;

/* Fill an array with the lower grid index bounds of the output. */
   ldim[ 0 ] = 1;
   ldim[ 1 ] = 1;
   ldim[ 2 ] = 1;

/* Integer upper grid index bounds of the output. */
   udim[ 0 ] = dim[ 0 ];
   udim[ 1 ] = dim[ 1 ];
   udim[ 2 ] = dim[ 2 ];

/* Integer upper bounds of detector array. */
   uddim[ 0 ] = ndet;

/* Store the size of an input time slice. */
   timeslice_size = nel/nslice;

/* Create a LutMap that holds the output spectral axis GRID value at
   the centre of each input spectral axis pixel. LutMaps are faster to
   evaluate, and so astRebinSeq will go faster. We can use LutMaps without
   loosing accuracy since astRebinSeq only ever transforms the GRID
   values at input pixel centres (i.e. integer GRID values), and so the
   LutMap will always return a tabulated value rather than an
   interpolated value. */
   atlTolut( (AstMapping *) ssmap, 1.0, (double) nchan, 1.0, "LutInterp=1",
              &sslut, status );

/* If this is the first pass through this file, initialise the arrays. */
   if( first ) smf_rebincube_init( 0, nxy, nout, genvar, data_array, var_array,
                                   wgt_array, texp_array, teff_array, &junk, status );

/* Initialisation the flags for astRebinSeq (we do not include flag
   AST__REBININIT because the arrays have been initialised). */
   ast_flags = AST__USEBAD;
   if( usewgt ) ast_flags = ast_flags | AST__VARWGT;

   if( genvar == 1 ) {
      ast_flags = ast_flags | AST__GENVAR;
   } else if( genvar == 2 ) {
      ast_flags = ast_flags | AST__USEVAR;
   }

/* If required, allocate a work array to hold all the input variances for a
   single time slice. */
   if( usewgt || genvar == 2 ) varwork = astMalloc( timeslice_size * sizeof( float ) );

/* Allocate a work array to hold the exposure time for each detector. */
   detwork = astMalloc( ndet * sizeof( float ) );

/* Debug message */
   if( data->file ) {
      msgOutiff( MSG__DEBUG, " ", "smf_rebincube_ast: Using %zu detectors "
                 "from data file '%s'.", status, ndet, data->file->name );
   }

/* Create a group to hold the used detectors. */
   usedetgrp = grpNew( "Used detectors", status );

/* Transform the spectral grid index bounds of the output cube into the input cube. */
   gout[ 0 ] = 0.5;
   gout[ 1 ] = dim[ 2 ] + 0.5;
   astTran1( sslut, 2, gout, 0, gin );

/* Create a LutMap that holds the input GRID index of every detector to be included
   in the output, and AST__BAD for every detector that is not to be included in the
   output cube. First allocate the work space for the LUT. */
   detlut = astMalloc( ndet*sizeof( double ) );

/* Allocate memory to hold the data needed by each job used to determine
   if a detector has any good values in the input array. This will fill them
   with zeros. */
   job_data = astCalloc( ndet, sizeof( *job_data ) );

/* Check memory was allocated successfully. */
   if( *status == SAI__OK ) {

/* Initialise a string to point to the name of the first detector for which
   data is available */
      name = hdr->detname;

/* Loop round all detectors for which data is available. */
      for( idet = 0; idet < ndet; idet++ ) {

/* Store the input GRID coord of this detector. GRID coords start at 1,
   not 0. */
         detlut[ idet ] = idet + 1.0;

/* If a group of detectors to be used was supplied, search the group for
   the name of the current detector. If not found, set the GRID coord bad.
   This will cause astRebinSeq to ignore data from the detector. */
         if( *detgrp ) {
            found = grpIndex( name, *detgrp, 1, status );
            if( !found ) detlut[ idet ] = AST__BAD;
         }

/* If the detector is being used, submit a job to the workforce to
   determine if the detector has any good values in the input array. */
         if( detlut[ idet ] != AST__BAD) {
            pdata = job_data + idet;
            pdata->data = ( (float *) (data->pntr)[ 0 ] ) + idet*nchan;
            pdata->stride = timeslice_size;
            pdata->lbnd = floor( gin[ 0 ] );
            pdata->ubnd = ceil( gin[ 1 ] );
            thrAddJob( wf, 0, pdata, smf1_rebincube_ast, 0, NULL, status );
         }

/* Move on to the next available detector name. */
         name += strlen( name ) + 1;
      }

/* Wait for the workforce jobs to comnplete. */
      thrWait( wf, status );

/* Now see which detectors contain any good input data and store the
   names of such detectors in the returned group. */
      name = hdr->detname;
      for( idet = 0; idet < ndet; idet++ ) {
         pdata = job_data + idet ;
         if( pdata->used ) grpPut1( usedetgrp, name, 0, status );
         name += strlen( name ) + 1;
      }
   }

/* Free resources. */
   job_data = astFree( job_data );

/* Create the LutMap. If we only have 1 detector, use a UnitMap instead of a LutMap
   (lutMaps must have 2 or more table entries). */
   if( ndet > 1 ) {
      lutmap = (AstMapping *) astLutMap( ndet, detlut, 1.0, 1.0, "LutInterp=1" );
   } else {
      lutmap = (AstMapping *) astUnitMap( 1, " " );
   }

/* Combine the above LutMap with a 1-input, 2-output PermMap that copies its
   input to create its first output, and assigns a constant value of 1.0 to
   its second output. We need to do this because smf_tslice returns a 2D
   GRID system (even though the second GRID axis is not actually used). */
   inperm[ 0 ] = 1;
   outperm[ 0 ] = 1;
   outperm[ 1 ] = -1;
   con = 1.0;
   detmap = astCmpMap( lutmap, astPermMap( 1, inperm, 2, outperm, &con, " " ),
                       1, " " );

/* Store the bounds of a single time slice grid. */
   lbnd_in[ 0 ] = 1;
   ubnd_in[ 0 ] = nchan;
   lbnd_in[ 1 ] = 1;
   ubnd_in[ 1 ] = ndet;

/* Create a PermMap that can be used to re-order the output axes so that
   channel number is axis 3. */
   outperm[ 0 ] = 2;
   outperm[ 1 ] = 3;
   outperm[ 2 ] = 1;
   inperm[ 0 ] = 3;
   inperm[ 1 ] = 1;
   inperm[ 2 ] = 2;
   pmap = astPermMap( 3, inperm, 3, outperm, NULL, " " );

/* If we are using multiple threads to rebin spectral blocks in parallel,
   calculate the number of channels that are processed by each thread,
   and the number of threads to use. The whole output spectrum is divided
   up into blocks. The number of blocks is two times the number of
   threads, and each thread rebins two adjacent blocks. Alternate blocks
   are re-binned simultanously. First, the odd numbered blocks are re-binned
   (one by each thread). When all odd numbered blocks have been re-binned,
   the even numbered blocks are re-binned. We ensure that the number of
   threads used results in a block size that is larger than the spreading
   width produced by the requested spreading scheme. This means that no
   pair of simultanously executing threads will ever try to write to the
   same channel of the output spectrum. */
   maxthreads = wf ? wf->nworker : 1;
   if( maxthreads > MAXTHREADS ) maxthreads = MAXTHREADS;
   if( maxthreads > 1 ) {

/* Find the largest number of threads into which each output spectrum can
   be split. The limit is imposes by the requirement that each block is
   larger than the pixel spreading produced by the requested spreading
   scheme. */
      nthreads = ( ( dim[ 2 ] + 1 )/2 )/smf_spreadwidth( spread, params,
                                                         status );

/* If the spectral range is less than twice the spreading width, we
   cannot use multiple threads. */
      if( nthreads > 1 ) {

/* Restrict the number of threads to be no more than the number of workers
   available in the work force. */
         if( nthreads > maxthreads ) nthreads = maxthreads;
         if( data->file ) {
            msgOutiff( MSG__DEBUG, " ", "smf_rebincube_ast: Using %d threads "
                       "to process data file '%s'.", status, nthreads, data->file->name );
         }

/* Find the number of output channels in each spectral block. */
         blk_size = ( dim[ 2 ] - 1 )/( 2*nthreads ) + 1;

/* Set up the first output channel number within each block. */
         nblock = 2*nthreads;
         for( iblock = 0; iblock < nblock; iblock++ ) {
            blk_bot[ iblock ] = (double) ( iblock*blk_size + 1 );
         }

/* Add in the first channel number beyond the last block. */
         blk_bot[ nblock ] = blk_bot[ nblock - 1 ] + blk_size;

/* If the output spectrum is too short to guarantee that there are any
   independent blocks of output channels, we process the whole spectrum
   in a single thread. */
      } else {
         if( data->file ) {
            msgOutiff( MSG__DEBUG, " ", "smf_rebincube_ast: Using one thread "
                       "to process data file '%s'.", status, data->file->name );
         }
         nthreads = 1;
         nblock = 1;
         blk_bot[ 0 ] = 1.0;
         blk_bot[ 1 ] = (double) ( dim[ 2 ] + 1 );
      }

/* If multiple threads are not available, we process the whole spectrum
   in a single thread. */
   } else {
      nthreads = 1;
      nblock = 1;
      blk_bot[ 0 ] = 1.0;
      blk_bot[ 1 ] = (double) ( dim[ 2 ] + 1 );
   }

/* Convert the block boundaries from output channel numbers into input
   channel numbers. */
   astTran1( ssmap, nblock + 1, blk_bot, 0, blk_bot );

/* Ensure they are in increasing order, and are not outside the bounds of
   the input array. */
   if( blk_bot[ 0 ] > blk_bot[ 1 ] ) {
      for( iblock = 0; iblock < ( nblock + 1 )/2; iblock++ ) {
         dtemp = blk_bot[ nblock - iblock ];
         blk_bot[ nblock - iblock ] = blk_bot[ iblock ];
         blk_bot[ iblock ] = dtemp;
      }
   }

   for( iblock = 0; iblock <= nblock; iblock++ ) {
      if( blk_bot[ iblock ] < 1 ) {
         blk_bot[ iblock ] = 1.0;
      } else if( blk_bot[ iblock ] > nchan ) {
         blk_bot[ iblock ] = nchan;
      }
   }

/* Count the number of time slices to be processed. */
   if( ptime ) {
      itime = 0;
      while( ptime[ itime ] != VAL__MAXI ) itime++;
      if( data->file ) {
         msgOutiff( MSG__DEBUG, " ", "smf_rebincube_ast: Selecting %d time "
                    "slices from data file '%s'.", status, (int) itime,
                    data->file->name );
      }
   } else {
      itime = nslice;
      if( data->file ) {
         msgOutiff( MSG__DEBUG, " ", "smf_rebincube_ast: Using all %d time "
                    "slices from data file '%s'.", status, (int) itime,
                    data->file->name );
      }
   }

/* Allocate and initialise an array of flags, one for each detector, that
   will be used to indicate which detectors contibute data to the output
   cube. */
   detflags = astCalloc( (data->dims)[ 1 ], sizeof( *detflags ) );

/* Initialise a pointer to the next time slice index to be used. */
   nexttime = ptime;

/* Initialise the progress meter. */
   smf_reportprogress( itime, status );

/* Loop round all time slices in the input NDF. */
   for( itime = 0; itime < nslice && *status == SAI__OK; itime++ ) {

/* If this time slice is not being pasted into the output cube, pass on. */
      if( nexttime ){
         if( *nexttime != (int) itime ) continue;
         nexttime++;
      }

/* Skip this time slice if flagged due to a position problem. */
      if( (hdr->allState)[itime].jos_drcontrol & drcntrl_mask ) {
         continue;
      }

/* Store a pointer to the first input data value in this time slice. */
      tdata = ( (float *) (data->pntr)[ 0 ] ) + itime*timeslice_size;

/* Begin an AST context. Having this context within the time slice loop
   helps keep the number of AST objects in use to a minimum. */
      astBegin;

/* Get a Mapping from the spatial GRID axes in the input the spatial
   GRID axes in the output for the current time slice. Note this has
   to be done first since it stores details of the current time slice
   in the "smfHead" structure inside "data", and this is needed by
   subsequent functions. */
      totmap = smf_rebin_totmap( data, itime, abskyfrm, oskymap, moving,
				 NO_FTS, status );
      if( !totmap ) {
         if( data->file ) {
            msgOutiff( MSG__DEBUG, " ", "smf_rebincube_ast: Cannot get "
                       "Mapping for slice %d from data file '%s'.", status,
                       (int) itime, data->file->name );
         }
         break;
      }

/* Get the effective exposure time, the total exposure time, and the
   Tsys->Variance onversion factor for this time slice. Also get a
   pointer to the start of the Tsys array. */
      tsys = smf_rebincube_tcon( hdr, itime, fcon, &texp, &teff, &tcon,
                                 status );

/* So "totmap" is a 2-input, 2-output Mapping that transforms the input
   spatial GRID coords into output spatial GRID coords. In order to speed
   up astRebinSeq we represent this by a pair of parallel LutMaps. To do
   this (using atlTolut) we need a Mapping which only has 1 input, so we
   preceed "totmap" with "detmap" (which also has the effect of exluding
   data from unrequired detectors). We then combine this Mapping in
   parallel with the spectral LutMap to get a 2-input (channel number,
   detector index) and 3-output (output grid coords) Mapping. We finally
   add a PermMap to re-arrange the output axes so that channel number is
   axis 3 in the output. */
      dtotmap = (AstMapping *) astCmpMap( detmap, totmap, 1, " " );
      if( ndet > 1 ) {
         atlTolut( dtotmap, 1.0, (double) ndet, 1.0, "LutInterp=1", &splut,
                   status );
      } else {
         splut = astClone( dtotmap );
      }

      fullmap = astSimplify( astCmpMap( astCmpMap( sslut, splut, 0, " " ),
                                        pmap, 1, " " ) );

/* If required calculate the variance associated with each value in the
   current time slice. based on the input Tsys values. If they are
   needed, but not available, ignored the time slice. */
      ignore = 0;
      if( varwork ) {
         ignore = 1;
         vp = varwork;
         for( idet = 0; idet < ndet; idet++ ) {
            invar = VAL__BADR;
            rtsys = tsys ? (float) tsys[ idet ] : VAL__BADR;
            if( rtsys <= 0.0 ) rtsys = VAL__BADR;
            if( rtsys != VAL__BADR ) {
               *good_tsys = 1;
               if( tcon != VAL__BADD ) {
                  invar = tcon*rtsys*rtsys;
                  ignore = 0;
               }
            }
            for( ichan = 0; ichan < nchan; ichan++ ) *(vp++) = invar;
         }
      }

/* Unless we are ignoring this time slice, paste it into the 3D output
   cube. The smf_rebincube_seqf function is a wrapper for astRebinSeqF
   that splits the total job up between "nthreads" threads running in
   parallel. */
      if( !ignore ) {
         smf_rebincube_seqf( wf, nthreads, blk_bot, fullmap, 0.0, 2, lbnd_in,
                             ubnd_in, tdata, varwork, spread, params,
                             ast_flags, 0.0, 50, VAL__BADR, 3, ldim, udim,
                             lbnd_in, ubnd_in, data_array, var_array,
                             wgt_array, nused, status );

/* Now we update the total exposure time array. Scale the exposure time
   of this time slice in order to reduce its influence on the output
   expsoure times if it does not have much spectral overlap with the
   output cube. then fill the 1D work array with this constant value and
   paste it into the 2D texp_array using the spatial mapping. Note we
   want the simple sum of the exposure times, with no normalisation. SO
   we use the AST__NONORM flag which means we do not need to supply a
   weights array.  */
         if( texp != VAL__BADR ) {
            texp *= tfac;
            for( iv = 0; iv < ndet; iv++ ) detwork[ iv ] = texp;
            astRebinSeqF( splut, 0.0, 1, ldim, uddim, detwork, NULL,
                          spread, params, AST__NONORM, 0.0, 50,
                          VAL__BADR, 2, ldim, udim, ldim, uddim, texp_array,
                          NULL, NULL, NULL );
         }

/* Now do the same with the effective exposure time. */
         if( teff != VAL__BADR ) {
            teff *= tfac;
            for( iv = 0; iv < ndet; iv++ ) detwork[ iv ] = teff;
            astRebinSeqF( splut, 0.0, 1, ldim, uddim, detwork, NULL,
                          spread, params, AST__NONORM, 0.0, 50, VAL__BADR, 2,
                          ldim, udim, ldim, uddim, teff_array, NULL, NULL,
                          NULL );
         }

      } else if( data->file ) {
         msgOutiff( MSG__DEBUG, " ", "smf_rebincube_ast: Time slice %d "
                    "is being ignored when processing data file '%s'.",
                    status, (int) itime, data->file->name );
      }

/* Update the progress meter. */
      smf_reportprogress( 0, status );

/* End the AST context. */
      astEnd;
   }

/* If this is the final pass through this function, normalise the returned
   data and variance values. */
   if( last ) {

/* Check some data was pasted into the output. */
      if( *nused > 0 ) {

/* Create a dummy mapping that can be used with astRebinSeq (it is not
   actually used for anything since we are not adding any more data into the
   output arrays). */
         fullmap = (AstMapping *) astPermMap( 2, NULL, 3, NULL, NULL, " " );

/* Normalise the data values. We do not normalise the exposure time arrays. */
         astRebinSeqF( fullmap, 0.0, 2, lbnd_in,
                       ubnd_in, NULL, NULL, spread, params,
                       AST__REBINEND | ast_flags, 0.0, 50, VAL__BADR, 3,
                       ldim, udim, lbnd_in, ubnd_in, data_array, var_array,
                       wgt_array, nused );
         fullmap = astAnnul(fullmap);

/* If no data was pasted into the output, fill it with bad values and
   issue a warning. */
      } else {
         size_t nel = dim[0]*dim[1]*dim[2];
         size_t iel;

         float *p1 = data_array;
         for( iel = 0; iel < nel; iel++ ) *(p1++) = VAL__BADR;

         if( genvar ) {
            p1 = var_array;
            for( iel = 0; iel < nel; iel++ ) *(p1++) = VAL__BADR;
         }
         msgOut( "", "WARNING: No good values in output cube.", status );
      }
   }

/* Delete the supplied detector group and return the used detector group
   in its place. */
   if( *detgrp ) grpDelet( detgrp, status );
   *detgrp = usedetgrp;

/* Free resources. */
   detlut = astFree( detlut );
   detwork = astFree( detwork );
   varwork = astFree( varwork );
}

static void smf1_rebincube_ast( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_rebincube_ast

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_rebincube_ast.

*  Invocation:
*     smf1_rebincube_ast( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfRebinCubeAstData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfRebinCubeAstData *pdata;
   float *pd;
   size_t stride;
   int islice;
   int ubnd;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfRebinCubeAstData *) job_data_ptr;

/* Store some loal convenience values. */
   stride = pdata->stride;
   ubnd = pdata->ubnd;

/* Initialise the flag to indicate no good data values have been found
   (not really necessary since the job data structure was initialised to
   hold zeros when it was allocated). */
   pdata->used = 1;

/* Loop round all the channels in the supplied spectrum tinil a good data
   value is found. */
   pd = pdata->data;
   for( islice = pdata->lbnd; islice < ubnd; islice++ ) {
      if( *pd != VAL__BADR ) {
         pdata->used = 1;
         break;
      }
      pd += stride;
   }

}
