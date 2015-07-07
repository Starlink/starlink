/*
*+
*  Name:
*     smf_rebinsparse

*  Purpose:
*     Paste a supplied 3D array into an existing cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_rebinsparse( smfData *data, int first, int *ptime, AstFrame *ospecfrm,
*                      AstMapping *ospecmap, AstSkyFrame *oskyframe,
*                      Grp *detgrp, int lbnd_out[ 3 ], int ubnd_out[ 3 ],
*                      int genvar, float *data_array, float *var_array,
*                      int *ispec, float *texp_array, float *teff_array,
*                      double *fcon, int *status );

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the input smfData structure.
*     first = int (Given)
*        Is this the first call to this routine for the current output
*        cube?
*     ptime = int * (Given)
*        Pointer to an array of integers, each one being the index of a
*        time slice that is to be pasted into the output cube. If this is
*        NULL, then all time slices are used. The values in the array
*        should be monotonic increasing and should be terminated by a value
*        of VAL__MAXI.
*     ospecfrm = AstFrame * (Given)
*        Pointer to the SpecFrame within the current Frame of the output WCS
*        Frameset.
*     ospecmap = AstMapping * (Given)
*        Pointer to the Mapping from the SpecFrame to the third GRID axis
*        within the current Frame of the output WCS Frameset.
*     oskyframe = AstFrame * (Given)
*        Pointer to the SkyFrame in the output WCS FrameSet.
*     detgrp = Grp * (Given)
*        A Group containing the names of the detectors to be used. All
*        detectors will be used if this group is empty.
*     lbnd_out = dim_t [ 3 ] (Given)
*        The lower pixel index bounds of the output cube.
*     ubnd_out = dim_t [ 3 ] (Given)
*        The upper pixel index bounds of the output cube.
*     genvar = int (Given)
*        Indicates how the output variances should be calculated. If a
*        value of 2 is supplied, then output variances are calculated on
*        the basis of the input system noise temperatures. Any other value
*        causes no output variances to be created.
*     data_array = float * (Given and Returned)
*        The data array for the output cube. This is updated on exit to
*        include the data from the supplied input NDF.
*     var_array = float * (Given and Returned)
*        An array in which to store the variances for the output cube if
*        "genvar" is not zero (the supplied pointer is ignored if "genvar" is
*        zero). The supplied array is update on exit to include the data from
*        the supplied input NDF. This array should be big enough to hold a
*        single spatial plane from the output cube (all planes will have the
*        same variance and so only one plane need be calculated).
*     ispec = int * (Given and Returned)
*        Index of the next spectrum to be stored in the output NDF.
*     texp_array = float * (Given and Returned)
*        A work array, which holds the total exposure time for each output
*        spectrum. It is updated on exit to include the supplied input
*        NDF. Only used if "spread" is AST__NEAREST. It should be big enough
*        to hold a single spatial plane from the output cube.
*     teff_array = float * (Given and Returned)
*        A work array, which holds the effective integration time (scaled
*        by a factor of 4) "on" time for each output
*        spectrum. It is updated on exit to include the supplied input
*        NDF. Only used if "spread" is AST__NEAREST. It should be big enough
*        to hold a single spatial plane from the output cube.
*     fcon = double * (Given and Returned)
*        If "first" is supplied non-zero, then *fcon is returned holding
*        the ratio of the squared backend degradation factor to the spectral
*        channel width (this is the factor needed for calculating the
*        variances from the Tsys value). This returned value should be
*        left unchanged on subseuqnet invocations of this function. If
*        "first" is zero, the value is returned holding VAL__BADD
*        if the factor for the current file has a different value.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Description:
*     The data array of the supplied input NDF is added into the existing
*     contents of the output data array.
*
*     Note, few checks are performed on the validity of the input data
*     files in this function, since they have already been checked within
*     smf_sparsebounds.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     30-NOV-2006 (DSB):
*        Initial version.
*     13-DEC-2006 (DSB):
*        Added "genvar" argument.
*     30-JAN-2007 (DSB):
*        Added calculation of Tsys output variances.
*     7-FEB-2007 (DSB):
*        Added parameter "texp_array".
*     8-FEB-2007 (DSB):
*        Added parameter "ton_array" and "fcon".
*     9-FEB-2007 (DSB):
*        Check for bad tsys values in the input NDF.
*     21-FEB-2007 (DSB):
*        Change ton_array to teff_array.
*     12-OCT-2007 (DSB):
*        Added parameter "ptime".
*     12-FEB-2008 (DSB):
*        Replaced argument "index" with "first". This is because different
*        output tiles and/or polarisation bins will receive contributions
*        from different input files, not necessarily starting at the first
*        input file or ending at the last.
*     11-FEB-2009 (DSB):
*        Ignore negative or zero input Tsys values.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2008-2009 Science & Technology Facilities Council.
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

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/ndg.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_rebinsparse"

void smf_rebinsparse( smfData *data, int first, int *ptime, AstFrame *ospecfrm,
                      AstMapping *ospecmap, AstSkyFrame *oskyframe,
                      Grp *detgrp, int lbnd_out[ 3 ], int ubnd_out[ 3 ],
                      int genvar, float *data_array, float *var_array,
                      int *ispec, float *texp_array, float *teff_array,
                      double *fcon, int *status ){

/* Local Variables */
   AstCmpMap *fmap = NULL;      /* Mapping from spectral grid to topo freq Hz */
   AstCmpMap *ssmap = NULL;     /* I/p GRID-> o/p PIXEL Mapping for spectral axis */
   AstFitsChan *fc = NULL;      /* Storage for FITS headers */
   AstFrame *specframe = NULL;  /* Spectral Frame in input FrameSet */
   AstFrame *specframe2 = NULL; /* Temporary copy of SpecFrame in input WCS */
   AstFrameSet *fs = NULL;      /* A general purpose FrameSet pointer */
   AstFrameSet *swcsin = NULL;  /* FrameSet describing spatial input WCS */
   AstMapping *fsmap = NULL;    /* Base->Current Mapping extracted from a FrameSet */
   AstMapping *smap = NULL;     /* Simplified Mapping */
   AstMapping *tmap = NULL;     /* Temporary Mapping */
   AstMapping *specmap = NULL;  /* PIXEL -> Spec mapping in input FrameSet */
   char *fftwin = NULL;  /* Name of FFT windowing function */
   const char *name = NULL; /* Pointer to current detector name */
   const double *tsys=NULL; /* Pointer to Tsys value for first detector */
   dim_t timeslice_size; /* No of detector values in one time slice */
   double *spectab = NULL;/* Workspace for spectral output grid positions */
   double *xin = NULL;   /* Workspace for detector input grid positions */
   double *xout = NULL;  /* Workspace for detector output pixel positions */
   double *yin = NULL;   /* Workspace for detector input grid positions */
   double *yout = NULL;  /* Workspace for detector output pixel positions */
   double at;            /* Frequency at which to take the gradient */
   double dnew;          /* Channel width in Hz */
   double fcon2;         /* Variance factor for whole file */
   double k;             /* Back-end degradation factor */
   double tcon;          /* Variance factor for whole time slice */
   float *pdata = NULL;  /* Pointer to next data sample */
   float *qdata = NULL;  /* Pointer to next data sample */
   float rtsys;          /* Tsys value */
   float teff;           /* Effective integration time, times 4 */
   float texp;           /* Total time ( = ton + toff ) */
   float toff;           /* Off time */
   float ton;            /* On time */
   int *nexttime = NULL; /* Pointer to next time slice index to use */
   int dim[ 3 ];         /* Output array dimensions */
   int found;            /* Was current detector name found in detgrp? */
   int good;             /* Are there any good detector samples? */
   int ibasein;          /* Index of base Frame in input FrameSet */
   int ichan;            /* Index of current channel */
   int iv;               /* Offset to next element */
   int iz;               /* Output grid index on axis 3 */
   int nchan;            /* Number of input spectral channels */
   int pixax[ 3 ];       /* The output fed by each selected mapping input */
   int specax;           /* Index of spectral axis in input FrameSet */
   size_t irec;          /* Index of current input detector */
   size_t itime;         /* Index of current time slice */
   smfHead *hdr = NULL;  /* Pointer to data header for this time slice */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context.*/
   astBegin;

/* Store a pointer to the input NDFs smfHead structure. */
   hdr = data->hdr;

/* Store the dimensions of the output array. */
   dim[ 0 ] = ubnd_out[ 0 ] - lbnd_out[ 0 ] + 1;
   dim[ 1 ] = ubnd_out[ 1 ] - lbnd_out[ 1 ] + 1;
   dim[ 2 ] = ubnd_out[ 2 ] - lbnd_out[ 2 ] + 1;

/* Store the number of pixels in one time slice */
   timeslice_size = (data->dims)[ 0 ]*(data->dims)[ 1 ];

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
                                 1, " " ),
                      ospecmap, 1, " " );
   ssmap = astSimplify( ssmap );

/* Create a table with one element for each channel in the input array,
   holding the index of the nearest corresponding output channel. */
   nchan = (data->dims)[ 0 ];
   spectab = astMalloc( sizeof( *spectab )*nchan );
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

/* Allocate work arrays big enough to hold the coords of all the
   detectors in the current input file.*/
   xin = astMalloc( (data->dims)[ 1 ] * sizeof( *xin ) );
   yin = astMalloc( (data->dims)[ 1 ] * sizeof( *yin ) );
   xout = astMalloc( (data->dims)[ 1 ] * sizeof( *xout ) );
   yout = astMalloc( (data->dims)[ 1 ] * sizeof( *yout ) );

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
         found = grpIndex( name, detgrp, 1, status );
         if( !found ) {
            xin[ irec ] = AST__BAD;
            yin[ irec ] = AST__BAD;
         }
      }

/* Move on to the next available detector name. */
      name += strlen( name ) + 1;
   }

/* Find the constant factor associated with the current input file. This
   is the squared backend degradation factor, divided by the noise bandwidth.
   Get the required FITS headers, checking they were found. */
   if( astGetFitsF( hdr->fitshdr, "BEDEGFAC", &k ) &&
       astGetFitsS( hdr->fitshdr, "FFT_WIN", &fftwin ) ){

/* Get a Mapping that converts values in the input spectral system to
   topocentric frequency in Hz, and concatenate this Mapping with the
   Mapping from input GRID coord to the input spectral system. The result
   is a Mapping from input GRID coord to topocentric frequency in Hz. */
      specframe2 = astCopy( specframe );
      astSet( specframe2, "system=freq,stdofrest=topo,unit=Hz" );
      fmap = astCmpMap( specmap, astGetMapping( astConvert( specframe,
                                                            specframe2,
                                                            "" ),
                                                AST__BASE, AST__CURRENT ),
                        1, " " );

/* Differentiate this Mapping at the mid channel position to get the width
   of an input channel in Hz. */
      at = 0.5*nchan;
      dnew = astRate( fmap, &at, 1, 1 );

/* Modify the channel width to take account of the effect of the FFT windowing
   function. Allow undef value because FFT_WIN for old data had a broken value
   in hybrid subband modes. */
      if( dnew != AST__BAD ) {
         dnew = fabs( dnew );

         if( !strcmp( fftwin, "truncate" ) ) {
            dnew *= 1.0;

         } else if( !strcmp( fftwin, "hanning" ) ) {
            dnew *= 1.5;

	    } else if( !strcmp( fftwin, "<undefined>" ) ) {
	      /* Deal with broken data - make an assumption */
	       dnew *= 1.0;

         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            msgSetc( "W", fftwin );
            errRep( FUNC_NAME, "FITS header FFT_WIN has unknown value "
                    "'^W' (programming error).", status );
         }

/* Form the required constant. */
         fcon2 = k*k/dnew;

      } else {
         fcon2 = VAL__BADD;
      }

   } else {
      fcon2 = VAL__BADD;
   }

/* Return the factor needed for calculating Tsys from the variance. */
   if( first ) {
      *fcon = fcon2;
   } else if( fcon2 != *fcon ) {
      *fcon = VAL__BADD;
   }

/* Initialise a pointer to the next time slice index to be used. */
   nexttime = ptime;

/* Loop round all the time slices in the input file. */
   for( itime = 0; itime < (data->dims)[ 2 ] && *status == SAI__OK; itime++ ) {

/* If this time slice is not being pasted into the output cube, pass on. */
      if( nexttime ){
         if( *nexttime != itime ) continue;
         nexttime++;
      }

/* Store a pointer to the first input data value in this time slice. */
      pdata = ( (float *) (data->pntr)[ 0 ] ) + itime*timeslice_size;

/* Get a FrameSet describing the spatial coordinate systems associated with
   the current time slice of the current input data file. The base frame in
   the FrameSet will be a 2D Frame in which axis 1 is detector number and
   axis 2 is unused. The current Frame will be a SkyFrame (the SkyFrame
   System may be any of the JCMT supported systems). The Epoch will be
   set to the epoch of the time slice. */
      smf_tslice_ast( data, itime, 1, NO_FTS, status );
      swcsin = hdr->wcs;

/* Note the total exposure time (texp) for all the input spectra produced by
   this time slice. */
      ton = hdr->state->acs_exposure;
      if( ton == 0.0 ) ton = VAL__BADR;

      toff = hdr->state->acs_offexposure;
      if( toff == 0.0 ) toff = VAL__BADR;

      if( ton != VAL__BADR && toff != VAL__BADR ) {
         texp = ton + toff;
         teff = 4*ton*toff/( ton + toff );
      } else {
         texp = VAL__BADR;
         teff = VAL__BADR;
      }

/* If output variances are being calculated on the basis of Tsys values
   in the input, find the constant factor associated with the current
   time slice. */
      tcon = AST__BAD;
      if( genvar == 2 && fcon2 != AST__BAD && texp != VAL__BADR ) {
         tcon = fcon2*( 1.0/ton + 1.0/toff );

/* Get a pointer to the start of the Tsys values for this time slice. */
         tsys = hdr->tsys + hdr->ndet*itime;
      }

/* We now create a Mapping from detector index to position in oskyframe. */
      astInvert( swcsin );
      ibasein = astGetI( swcsin, "Base" );
      fs = astConvert( swcsin, oskyframe, "SKY" );
      astSetI( swcsin, "Base", ibasein );
      astInvert( swcsin );

      if( fs == NULL ) {
         if( *status == SAI__OK ) {
            if (data->file) {
               smf_smfFile_msg(data->file, "FILE", 1, "<unknown>");
            } else {
               msgSetc( "FILE", "<unknown>" );
            }
            *status = SAI__ERROR;
            errRep( FUNC_NAME, "The spatial coordinate system in ^FILE "
                    "is not compatible with the spatial coordinate "
                    "system in the first input file.", status );
         }
         break;
      }

/* Get a simplified Mapping from the FrameSet. */
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
            qdata = pdata;
            good = 0;
            for( ichan = 0; ichan < nchan; ichan++ ){
               if( *(qdata++) != VAL__BADR ) {
                  good = 1;
                  break;
               }
            }

/* If it did, calculate the variance associated with each detector
   sample (if required), based on the input Tsys values, and copy the
   spectrum to the output NDF. */
            if( good ) {
               if( *ispec < dim[ 0 ] ){
                  rtsys = tsys ? (float) tsys[ irec ] : VAL__BADR;
                  if( rtsys <= 0.0 ) rtsys = VAL__BADR;
                  if( tcon != AST__BAD && genvar == 2 && rtsys != VAL__BADR ) {
                     var_array[ *ispec ] = tcon*rtsys*rtsys;
                  } else if( var_array ) {
                     var_array[ *ispec ] = VAL__BADR;
                  }

                  if( texp != VAL__BADR ) {
                     texp_array[ *ispec ] = texp;
                     teff_array[ *ispec ] = teff;
                  }

                  for( ichan = 0; ichan < nchan; ichan++, pdata++ ) {
                     iz = spectab[ ichan ] - 1;
                     if( iz >= 0 && iz < dim[ 2 ] ) {
                        iv = *ispec + dim[ 0 ]*iz;
                        data_array[ iv ] = *pdata;
                     }
                  }

                  (*ispec)++;

               } else if( *status == SAI__OK ){
                  *status = SAI__ERROR;
                  msgSeti( "DIM", dim[ 0 ] );
                  errRep( " ", "Too many spectra (more than ^DIM) for "
                          "the output NDF (programming error).", status );
                  break;
               }

/* If this detector does not have any valid data values, increment the data
   pointer to point at the first sample for the next detector. */
            } else {
               pdata += nchan;
            }

/* If this detector does not have a valid position, increment the data
   pointer to point at the first sample for the next detector. */
         } else {
            pdata += nchan;
         }
      }

/* For efficiency, explicitly annul the AST Objects created in this tight
   loop. */
      fs = astAnnul( fs );
      smap = astAnnul( smap );
      tmap = astAnnul( tmap );
   }

/* Free resources */
   spectab = astFree( spectab );
   xin = astFree( xin );
   yin = astFree( yin );
   xout = astFree( xout );
   yout = astFree( yout );

/* End the AST context. This will annul all AST objects created within the
   context (except for those that have been exported from the context). */
   astEnd;
}
