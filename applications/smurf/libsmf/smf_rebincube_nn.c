
/*
*+
*  Name:
*     smf_rebincube_nn

*  Purpose:
*     Paste a supplied 3D array into an existing cube using custom 2D nearest
*     neighbour code.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebincube_nn( ThrWorkForce *wf, smfData *data, int first, int last,
*                       dim_t *ptime, dim_t nchan, dim_t ndet, dim_t nslice,
*                       dim_t nxy, dim_t nout, dim_t dim[3],
*                       int badmask, int is2d, AstMapping *ssmap,
*                       AstSkyFrame *abskyfrm, AstMapping *oskymap,
*                       Grp **detgrp, int moving, int usewgt, int genvar,
*                       double tfac, double fcon, float *data_array,
*                       float *var_array, double *wgt_array,
*                       float *texp_array, float *teff_array, int64_t *nused,
*                       dim_t *nreject, dim_t *naccept, int *good_tsys,
*                       int *status );

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
*     ptime = dim_t * (Given)
*        Pointer to an array of integers, each one being the index of a
*        time slice that is to be pasted into the output cube. If this is
*        NULL, then all time slices are used. The values in the array
*        should be monotonic increasing and should be terminated by a value
*        of VAL__MAXK.
*     nchan = dim_t (Given)
*        Number of spectral channels in input cube.
*     ndet = dim_t (Given)
*        Number of detectors in input cube.
*     nslice = dim_t (Given)
*        Number of time slices in input cube.
*     nxy = dim_t (Given)
*        Number of elements in one spatial plane of the output cube.
*     nout = dim_t (Given)
*        Total number of elements in the output cube.
*     dim[ 3 ] = dim_t (Given)
*        The dimensions of the output array.
*     badmask = int (Given)
*        Indicates how the bad pixel mask for each output spectrum is
*        determined. A value of zero causes the bad pixel mask for each
*        output spectrum to be identical to the bad pixel mask of the
*        first input spectrum that contributes to the output spectrum.
*        Any subsequent input spectra that contribute to the same output
*        spectrum but have a different bad pixel mask are ignored. A
*        "badmask" value of 1 causes the bad pixel mask for each output
*        spectrum to be the union of the bad pixel masks of all input
*        spectra that contribute to the output spectrum. That is, an
*        output pixel will be bad if any of the input pixels that
*        contribute to it are bad.
*     is2d = int (Given)
*        Should a 2D weights array be used? If so, the weight and
*        variance within a single output spectrum is assumed to be the
*        same for all spectral channels, and so 2D instead of 3D arrays
*        can be used, saving lots of memory. A 2D array has a single
*        element for each spectrum in the output NDF. A 3D array has an
*        element for every element in the output NDF.
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
*        spatial grid coordinates in the output cube.
*     detgrp = Grp ** (Given)
*        On entry, a Group containing the names of the detectors to be
*        used. All detectors will be used if this group is empty (or NULL).
*        On exit, the supplied group (if any) is deleted, and a new group
*        is created and return holding the names of the detectors that
*        contributed good data to the output cube.
*     moving = int (Given)
*        A flag indicating if the telescope is tracking a moving object. If
*        so, each time slice is shifted so that the position specified by
*        TCS_AZ_BC1/2 is mapped on to the same pixel position in the
*        output cube.
*     usewgt = int (Given)
*        A flag indicating if the input data should be weighted according
*        to the input variances determined from the input Tsys values.
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
*     nused = int64_t * (Given and Returned)
*        Use to accumulate the total number of input data samples that
*        have been pasted into the output cube.
*     nreject = dim_t * (Given and Returned)
*        The number of input spectra that have been ignored becuase they
*        either do not cover the full spectral range of the output or
*        because they have a different bad pixel mask to the output.
*     naccept = dim_t * (Given and Returned)
*        The number of input spectra that have not been ignored.
*     good_tsys = int * (Given and Returned)
*        Returned set to 1 if any good Tsys values were found in the
*        input cube.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     The data array of the supplied input NDF is added into the existing
*     contents of the output data array, and the variance and weights
*     arrays are updated correspondingly.
*
*     Specialised code is used that only provides Nearest Neighbour
*     spreading when pasting each input pixel value into the output cube.
*
*     Note, few checks are performed on the validity of the input data
*     files in this function, since they have already been checked within
*     smf_cubebounds.

*  Authors:
*     David S Berry (JAC, UClan)
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     20-APR-2007 (DSB):
*        Initial version.
*     2-MAY-2007 (DSB):
*        Added parameter naccept.
*     11-MAY-2007 (DSB):
*        Correct calculation of spectral overlap factor (tfac).
*     12-JUL-2007 (EC):
*        -Changed name of smf_rebincube_totmap to smf_rebin_totmap
*     17-JUL-2007 (DSB):
*        Only update exposure time arrays if the spectrum is used.
*     11-OCT-2007 (DSB):
*        Added parameter "ptime".
*     12-FEB-2008 (DSB):
*        Modify the way that good Tsys values are cheked for so that
*        data that falls outside the otuput cube is included in the check.
*     12-FEB-2008 (DSB):
*        Update naccept when using a 3D weights array.
*     7-JAN-2009 (DSB):
*        Remove unused "nel" parameter.
*     14-JAN-2009 (DSB):
*        Fix bug in initialisation of detector data structures.
*     11-FEB-2009 (DSB):
*        Ignore negative or zero input Tsys values.
*     7-JUN-2021 (DSB):
*        Include time slice index in error messages.
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
#include <math.h>
#include <stdint.h>

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

#define FUNC_NAME "smf_rebincube_nn"

void smf_rebincube_nn( ThrWorkForce *wf, smfData *data, int first, int last,
                       dim_t *ptime, dim_t nchan, dim_t ndet, dim_t nslice,
                       dim_t nxy, dim_t nout, dim_t dim[3],
                       int badmask, int is2d, AstMapping *ssmap,
                       AstSkyFrame *abskyfrm, AstMapping *oskymap,
                       Grp **detgrp, int moving, int usewgt, int genvar,
                       double tfac, double fcon, float *data_array,
                       float *var_array, double *wgt_array,
                       float *texp_array, float *teff_array, int64_t *nused,
                       dim_t *nreject, dim_t *naccept, int *good_tsys,
                       int *status ){

/* Local Variables */
   AstMapping *totmap = NULL;  /* WCS->GRID Mapping from input WCS FrameSet */
   char *detflags = NULL;      /* Flags indicating if each detector was used */
   const char *name = NULL;    /* Pointer to current detector name */
   const double *tsys = NULL;  /* Pointer to Tsys value for first detector */
   dim_t *nexttime;            /* Pointer to next time slice index to use */
   dim_t *specpop = NULL;      /* Input channels per output channel */
   dim_t *spectab = NULL;      /* I/p->o/p channel number conversion table */
   dim_t gxout;                /* Output X grid index */
   dim_t gyout;                /* Output Y grid index */
   dim_t ichan;                /* Input channel index */
   dim_t idet;                 /* detector index */
   dim_t itime;                /* Index of current time slice */
   dim_t iv0;                  /* Offset for pixel in 1st o/p spectral channel */
   dim_t jdet;                 /* Detector index */
   dim_t naccept_old;          /* Previous number of accepted spectra */
   dim_t nchanout;             /* No of spectral channels in the output */
   dim_t ochan;                /* Output channel index */
   dim_t timeslice_size;       /* No of detector values in one time slice */
   double *detxin = NULL;      /* Work space for input X grid coords */
   double *detxout = NULL;     /* Work space for output X grid coords */
   double *detyin = NULL;      /* Work space for input Y grid coords */
   double *detyout = NULL;     /* Work space for output Y grid coords */
   double invar;               /* Input variance */
   double tcon;                /* Variance factor for whole time slice */
   double wgt;                 /* Weight for input value */
   float *ddata = NULL;        /* Pointer to start of input detector data */
   float *tdata = NULL;        /* Pointer to start of input time slice data */
   float *work = NULL;         /* Pointer to start of work array */
   float rtsys;                /* Tsys value */
   float teff;                 /* Effective integration time */
   float texp;                 /* Total time ( = ton + toff ) */
   int first_ts;               /* Is this the first time slice? */
   int found;                  /* Was current detector name found in detgrp? */
   int ignore;                 /* Ignore this time slice? */
   int init_detector_data;     /* Should detector_data be initialised? */
   int use_threads;            /* Use multiple threads? */
   smfHead *hdr = NULL;        /* Pointer to data header for this time slice */

   static smfRebincubeNNArgs1 *common_data = NULL; /* Holds data common to all detectors */
   static smfRebincubeNNArgs2 *detector_data = NULL; /* Holds data for each detector */
   static dim_t *pop_array = NULL;/* I/p spectra pasted into each output spectrum */
   static dim_t ndet_max = 0;  /* Max number of detectors */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Store a pointer to the input NDFs smfHead structure. */
   hdr = data->hdr;

/* Store the number of pixels in one time slice */
   timeslice_size = ndet*nchan;

/* Use this mapping to get the zero-based output channel number corresponding
   to each input channel number. */
   smf_rebincube_spectab( nchan, dim[ 2 ], ssmap, &spectab, status );
   if( !spectab ) goto L999;

/* The 2D weighting scheme assumes that each output channel receives
   contributions from one and only one input channel in each input file.
   Create an array with an element for each output channel, holding the
   number of input channels that contribute to the output channel. */
   nchanout = dim[ 2 ];
   if( is2d ) {
      specpop = astMalloc( nchanout*sizeof( *specpop ) );
      memset( specpop, 0, nchanout*sizeof( *specpop ) );
      for( ichan = 0; ichan < nchan; ichan++ ) {
         ochan = spectab[ ichan ];
         if( ochan != -1 ) specpop[ ochan ]++;
      }
   }

/* If this is the first pass through this file, initialise the arrays. */
   if( first ){
      smf_rebincube_init( is2d, nxy, nout, genvar, data_array, var_array,
                          wgt_array, texp_array, teff_array, nused, status );

/* Allocate an extra work array and initialise it to zero. This holds the
   total number of input spectra pasted into each output spectrum. It is
   not needed by the AST-based function and so has not been put into
   smf_rebincube_init. */
      if( is2d ) {
         pop_array = astMalloc( nxy*sizeof( *pop_array ) );
         memset( pop_array, 0, nxy*sizeof( *pop_array ) );
      }
   }

/* Allocate work arrays to hold the input and output grid coords for each
   detector. */
   detxin = astMalloc( ndet*sizeof( double ) );
   detyin = astMalloc( ndet*sizeof( double ) );
   detxout = astMalloc( ndet*sizeof( double ) );
   detyout = astMalloc( ndet*sizeof( double ) );

/* Initialise a string to point to the name of the first detector for which
   data is available */
   name = hdr->detname;

/* Fill the input arrays with the grid coords of each detector. */
   for( idet = 0; idet < ndet; idet++ ) {
      detxin[ idet ] = (double) idet + 1.0;
      detyin[ idet ] = 1.0;

/* If a group of detectors to be used was supplied, search the group for
   the name of the current detector. If not found, set the GRID coord
   bad. */
      if( *detgrp ) {
         found = (int) grpIndex( name, *detgrp, 1, status );
         if( !found ) {
            detxin[ idet ] = AST__BAD;
            detyin[ idet ] = AST__BAD;
         }
      }

/* Move on to the next available detector name. */
      name += strlen( name ) + 1;
   }

/* Allocate and initialise an array of flags, one for each detector, that
   will be used to indicate which detectors contibute data to the output
   cube. */
   detflags = astCalloc( (data->dims)[ 1 ], sizeof( *detflags ) );

/* Initialise a pointer to the ntex time slice index to be used. */
   nexttime = ptime;

/* Count the number of time slices to be processed. */
   if( ptime ) {
      itime = 0;
      while( ptime[ itime ] != VAL__MAXK ) itime++;
      if( data->file ) {
         msgOutiff( MSG__DEBUG, " ", "smf_rebincube_nn: Selecting %d time "
                    "slices from data file '%s'.", status, (int) itime,
                    data->file->name );
      }
   } else {
      itime = nslice;
      if( data->file ) {
         msgOutiff( MSG__DEBUG, " ", "smf_rebincube_nn: Using all %d time "
                    "slices from data file '%s'.", status, (int) itime,
                    data->file->name );
      }
   }

/* Initialise the progress meter. */
   smf_reportprogress( itime, status );

/* Loop round all time slices in the input NDF. */
   use_threads = 0;
   first_ts = 1;
   for( itime = 0; itime < nslice && *status == SAI__OK; itime++ ) {

/* If this time slice is not being pasted into the output cube, pass on. */
      if( nexttime ){
         if( *nexttime != itime ) continue;
         nexttime++;
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
            msgOutiff( MSG__DEBUG, " ", "smf_rebincube_nn: Cannot get "
                       "Mapping for slice %d from data file '%s'.", status,
                       (int) itime, data->file->name );
         }
         astEnd;
         break;
      }

/* Get the effective exposure time, the total exposure time, and the
   Tsys->Variance onversion factor for this time slice. Also get a
   pointer to the start of the Tsys array. */
      tsys = smf_rebincube_tcon( hdr, itime, fcon, &texp, &teff, &tcon,
                                 status );

/* Use this Mapping to get the output spatial grid coords for each input
   detector. */
      astTran2( totmap, ndet, detxin, detyin, 1, detxout, detyout );

/* If this is the first time slice from the current input file to be pasted
   into the output, see if any of the spectra will end up being pasted on
   top of each other in the output. If not we can use a separate thread to
   paste each spectrum. Otherwise, we cannot use multiple threads since they
   may end up trying to write to the same output pixel at the same time. */
      if( first_ts ) {
         first_ts = 0;
         use_threads = wf ? 1 : 0;
         for( idet = 0; idet < ndet - 1 && use_threads; idet++ ) {
            if( detxout[ idet ] != AST__BAD && detyout[ idet ] != AST__BAD ){

               gxout = floor( detxout[ idet ] + 0.5 );
               gyout = floor( detyout[ idet ] + 0.5 );

               if( gxout >= 1 && gxout <= dim[ 0 ] &&
                   gyout >= 1 && gyout <= dim[ 1 ] ) {

                  for( jdet = idet + 1; idet < ndet; idet++ ) {
                     if( detxout[ jdet ] != AST__BAD &&
                         detyout[ jdet ] != AST__BAD ){

                        if( floor( detxout[ jdet ] + 0.5 ) == gxout &&
                            floor( detyout[ jdet ] + 0.5 ) == gyout ) {
                           use_threads = 0;
                           break;
                        }
                     }
                  }

               }
            }
         }

/* If we will be using mutiple threads, do some preparation. */
         if( use_threads ) {
            if( data->file ) {
               msgOutiff( MSG__DEBUG, " ", "smf_rebincube_nn: Using multiple "
                          "threads to process data file '%s'.", status,
                          data->file->name );
            }

/* Ensure we have a structure holding information which is common to all
   detectors and time slices. */
            common_data = astGrow( common_data, sizeof( smfRebincubeNNArgs1 ), 1 );
            if( astOK ) {
               common_data->badmask = badmask;
               common_data->nchan = nchan;
               common_data->nchanout = nchanout;
               common_data->spectab = spectab;
               common_data->specpop = specpop;
               common_data->nxy = nxy;
               common_data->genvar = genvar;
               common_data->data_array = data_array;
               common_data->var_array = var_array;
               common_data->wgt_array = wgt_array;
               common_data->pop_array = pop_array;
               common_data->nout = nout;
               common_data->is2d = is2d;
            }

/* Ensure we have a structure for each detector to hold the detector-specific
   data, plus a pointer to the common data. */
            init_detector_data = ( detector_data == NULL );
            if( init_detector_data ) ndet_max = 0;
            detector_data = astGrow( detector_data, sizeof( smfRebincubeNNArgs2 ),
                                     ndet ) ;

/* Initialise pointers stored within any new elements added to the
   "detector_data" array. */
            if( ndet > ndet_max && astOK ) {
               for( idet = ndet_max; idet < ndet; idet++ ) {
                  detector_data[ idet ].common = NULL;
                  detector_data[ idet ].work = NULL;
                  detector_data[ idet ].ddata = NULL;
               }
               ndet_max = ndet;
            }

/* Allocate work space for each detector and store the common data
   pointer. */
            if( astOK ) {
               for( idet = 0; idet < ndet; idet++ ) {
                  detector_data[ idet ].common = common_data;
                  detector_data[ idet ].work = astGrow( detector_data[ idet ].work,
                                                        sizeof( float ), nchanout );
               }
            }

/* If we are using a single threads, do some alternative preparation. */
         } else {
            if( data->file ) {
               msgOutiff( MSG__DEBUG, " ", "smf_rebincube_nn: Using a single "
                          "thread to process data file '%s'.", status,
                          data->file->name );
            }

/* We need an extra work array for 2D weighting that can hold a single
   output spectrum. This is used as a staging post for each input
   spectrum prior to pasting it into the output cube. */
            work = astMalloc( nchanout*sizeof( float ) );

         }
      }

/* Loop round each detector, pasting its spectral values into the output
   cube. */
      for( idet = 0; idet < ndet; idet++ ) {

/* If multi-threaded, initialise a bad value for the detector's weight
   to indicate that it is not being used. */
         if( use_threads ) detector_data[ idet ].wgt = VAL__BADD;

/* See if any good tsys values are present. */
         rtsys = tsys ? (float) tsys[ idet ] : VAL__BADR;
         if( rtsys <= 0.0 ) rtsys = VAL__BADR;
         if( rtsys != VAL__BADR ) *good_tsys = 1;

/* Check the detector has a valid position in output grid coords */
         if( detxout[ idet ] != AST__BAD && detyout[ idet ] != AST__BAD ){

/* Find the closest output pixel and check it is within the bounds of the
   output cube. */
            gxout = floor( detxout[ idet ] + 0.5 );
            gyout = floor( detyout[ idet ] + 0.5 );
            if( gxout >= 1 && gxout <= dim[ 0 ] &&
                gyout >= 1 && gyout <= dim[ 1 ] ) {

/* Get the offset of the output array element that corresponds to this
   pixel in the first spectral channel. */
               iv0 = ( gyout - 1 )*dim[ 0 ] + ( gxout - 1 );

/* If required calculate the variance associated with this detector, based on
   the input Tsys values. */
               invar = VAL__BADR;
               if( usewgt || genvar == 2 ) {
                  if(  rtsys != VAL__BADR ) {
                     if( tcon != VAL__BADD ) invar = tcon*rtsys*rtsys;
                  }
               }

/* Calculate the weight for this detector. If we need the input variance,
   either to weight the input or to calculate output variances, but the
   input variance is not available, then ignore this detector. */
               ignore = 0;
               if( usewgt ) {
                  if( invar > 0.0 && invar != VAL__BADR ) {
                     wgt = 1.0/invar;
                  } else {
                     ignore = 1;
                  }

               } else if( genvar == 2 ) {
                  ignore = ( invar <= 0.0 || invar == VAL__BADR );
                  wgt = 1.0;

               } else {
                  wgt = 1.0;
               }

/* If we are not ignoring this input spectrum, get a pointer to the start
   of the input spectrum data and paste it into the output cube using
   either the 2D or 3D algorithm. */
               if( !ignore ) {
                  ddata = tdata + idet*nchan;

/* First deal with cases where we are using a single thread (the current
   thread). */
                  if( !use_threads ) {
                     naccept_old = *naccept;

                     if( is2d ) {
                       if( smf_rebincube_paste2d( badmask, nchan, nchanout, spectab,
                                                  specpop, iv0, nxy, wgt, genvar,
                                                  invar, ddata, data_array,
                                                  var_array, wgt_array, pop_array,
                                                  nused, nreject, naccept, work,
                                                  status ) ) detflags[ idet ] = 1;
                     } else {
                        if( smf_rebincube_paste3d( nchan, nout, spectab, iv0, nxy, wgt,
                                                   genvar, invar, ddata, data_array,
                                                   var_array, wgt_array, nused,
                                                   status ) ) detflags[ idet ] = 1;
                        (*naccept)++;
                     }

/* Now we update the total and effective exposure time arrays for the
   output spectrum that receives this input spectrum. Scale the exposure
   times of this time slice in order to reduce its influence on the
   output expsoure times if it does not have much spectral overlap with
   the output cube. Only update the exposure time arrays if the spectrum
   was used (as shown by an increase in the number of accepted spectra). */
                     if( texp != VAL__BADR && *naccept > naccept_old ) {
                        texp_array[ iv0 ] += texp*tfac;
                        teff_array[ iv0 ] += teff*tfac;
                     }

/* Now deal with cases where we are using several threads. */
                  } else {

/* Set up the detector specific data. */
                     detector_data[ idet ].iv0 = iv0;
                     detector_data[ idet ].wgt = wgt;
                     detector_data[ idet ].invar = invar;
                     detector_data[ idet ].ddata = ddata;
                     detector_data[ idet ].nused = 0;
                     detector_data[ idet ].nreject = 0;
                     detector_data[ idet ].naccept = 0;

/* Add a job to the workforce's job list. This job calls smf_rebincube_paste2d
   or smf_rebincube_paste3d to paste the detector input spectrum into the
   output cube. */
                     thrAddJob( wf, 0, detector_data + idet,
                                  smf_rebincube_paste_thread, 0, NULL, status );
                  }

               } else if( data->file ) {
                  msgOutiff( MSG__DEBUG, " ", "smf_rebincube_nn: Detector %" DIM_T_FMT
                             " at time slice %zu is being ignored when processing data file '%s'.",
                             status, idet, (size_t) itime, data->file->name );
               }

            } else if( data->file ) {
               msgOutiff( MSG__DEBUG, " ", "smf_rebincube_nn: Detector %" DIM_T_FMT
                          " at time slice %zu fell outside the output cube when processing "
                          "data file '%s'.", status, idet, (size_t) itime, data->file->name );
            }
         } else if( data->file ) {
            msgOutiff( MSG__DEBUG, " ", "smf_rebincube_nn: Detector %" DIM_T_FMT
                       " at time slice %zu has an unknown position in the output cube when processing "
                       "data file '%s'.", status, idet, (size_t) itime, data->file->name );
         }
      }

/* If using multiple threads, wait until all spectra for this time slice
   have been pasted into the output cube. Then transfer the output values
   from the detector data structures to the returned variables. */
      if( use_threads ) {
         thrWait( wf, status );
         for( idet = 0; idet < ndet; idet++ ) {
            if( detector_data[ idet ].wgt != VAL__BADD ) {
               (*nused) += detector_data[ idet ].nused;
               (*nreject) += detector_data[ idet ].nreject;
               (*naccept) += detector_data[ idet ].naccept;

               if( texp != VAL__BADR && detector_data[ idet ].naccept > 0 ) {
                  texp_array[ detector_data[ idet ].iv0 ] += texp*tfac;
                  teff_array[ detector_data[ idet ].iv0 ] += teff*tfac;
               }

               if( detector_data[ idet ].used ) detflags[ idet ] = 1;

            }
         }
      }

/* Update the progress meter. */
      smf_reportprogress( 0, status );

/* End the AST context. */
      astEnd;
   }

/* Delete the supplied detector group and create a new one to hold the names
   of the detectors for which good data was found and used. */
   if( *detgrp ) grpDelet( detgrp, status );
   *detgrp = grpNew( "Used detectors", status );

/* Initialise a string to point to the name of the first detector for which
   data is available */
   name = hdr->detname;

/* Loop round all detectors. */
   for( idet = 0; idet < ndet; idet++ ) {

/* If the current detector contributed any good data to the cube, store
   its name in the group. */
      if( detflags[ idet ] ) grpPut1( *detgrp, name, 0, status );

/* Move on to the next available detector name. */
      name += strlen( name ) + 1;
   }

/* If this is the final pass through this function, normalise the returned
   data and variance values, and release any static resources allocated
   within this function. */
   if( last ) {
      if( is2d ) {
         smf_rebincube_norm2d( nout, nxy, genvar, data_array,
                               var_array, wgt_array, pop_array, status );
      } else {
         smf_rebincube_norm3d( nout, genvar, *nused, data_array,
                               var_array, wgt_array, status );
      }

      pop_array = astFree( pop_array );

      if( use_threads ) {
         common_data = astFree( common_data );
         for( idet = 0; idet < ndet_max; idet++ ) {
            detector_data[ idet ].work = astFree( detector_data[ idet ].work );
         }
         detector_data = astFree( detector_data );
      }
   }

/* Free non-static resources. */
L999:;
   detflags = astFree( detflags );
   work = astFree( work );
   spectab = astFree( spectab );
   specpop = astFree( specpop );
   detxin = astFree( detxin );
   detyin = astFree( detyin );
   detxout = astFree( detxout );
   detyout = astFree( detyout );

}
