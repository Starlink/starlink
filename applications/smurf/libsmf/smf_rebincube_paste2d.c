/*
*+
*  Name:
*     smf_rebincube_paste2d

*  Purpose:
*     Paste a single input spectrum into the output cube using nearest
*     neighbour rebinning and a 2D weights array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_rebincube_paste2d( int badmask, dim_t nchan, dim_t nchanout,
*                                int *spectab, int *specpop, dim_t iv0,
*                                dim_t nxy, double wgt, int genvar,
*                                double invar, float *ddata,
*                                float *data_array, float *var_array,
*                                double *wgt_array, dim_t *pop_array,
*                                int64_t *nused, int *nreject, int *naccept,
*                                float *work, int *status );

*  Arguments:
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
*     nchan = dim_t (Given)
*        Number of spectral channels in input cube.
*     nchanout = dim_t (Given)
*        Number of spectral channels in output cube.
*     spectab = int * (Given)
*        This array should have "nchan" elements, and each element should
*        hold the integer index (zero-based) of the nearest neighbouring
*        output channel. A value of -1 should flag input channels that do
*        not have any corresponding output channel.
*     specpop = int * (Given)
*        This array should have one element for each output channel, and each
*        element should hold the number of input channels that get pasted
*        into the output channel. If a NULL value is supplied, it is
*        assumed that every output channel is contributed to by 1 input
*        channel.
*     iv0 = dim_t (Given)
*        The index within the output cube of the pixel corresponding to
*        channel zero of the output spectrum into which the input spectrum
*        is to be pasted.
*     nxy = dim_t (Given)
*        Number of elements in one spatial plane of the output cube.
*     wgt = double (Given)
*        The weight for the input spectrum.
*     genvar = int (Given)
*        Indicates how the output variances should be calculated:
*           0 = do not calculate any output variances
*           1 = use spread of input data values
*           2 = use system noise temperatures
*     invar = double (Given)
*        The Variance for the input spectrum.
*     ddata = float * (Given)
*        A pointer to the first data value in the input spectrum.
*     data_array = float * (Given and Returned)
*        The 3D data array for the output cube. This is updated on exit to
*        include the data from the supplied input spectrum.
*     var_array = float * (Given and Returned)
*        A 2D array in which to store the variances for the output cube if
*        "genvar" is not zero (the supplied pointer is ignored if "genvar" is
*        zero). The supplied array is updated on exit to include the data from
*        the supplied input spectrum. This array should be big enough to hold
*        a single spatial plane from the output cube (all planes in the
*        output cube will have the same variance).
*     wgt_array = double * (Given and Returned)
*        An array in which to store the relative weighting for each pixel in
*        the output cube. The supplied array is update on exit to include the
*        data from the supplied input spectrum. If "genvar" is 2, this array
*        should be big enough to hold a single spatial plane from the output
*        cube (all planes in the output cube will have the same weight). If
*        "genvar" is 2, this array should be big enough to hold two spatial
*        planes from the output cube.
*     pop_array = dim_t * (Given and Returned)
*        An array in which to store the number of input spectra pasted into
*        each output spectrum. It should be the same size as "var_array".
*     nused = int64_t * (Given and Returned)
*        Use to accumulate the total number of input data samples that
*        have been pasted into the output cube.
*     nreject = int * (Given and Returned)
*        The number of input spectra that have been ignored becuase they
*        either do not cover the full spectral range of the output or
*        because they have a different bad pixel mask to the output.
*     naccept = int * (Given and Returned)
*        The number of input spectra that have not been ignored.
*     work = float * (Given and Returned)
*        A work array with nchanout elements.
*     status = int * (Given and Returned)
*        Pointer to the inherited status.

*  Returned Function Value:
*     A flag that is non-zero if and only if at least one good value from
*     the input spectrum was pasted into the output spectrum.

*  Description:
*     Paste a single input spectrum into the output cube using nearest
*     neighbour rebinning and a 2D weights array.

*  Authors:
*     David S Berry (JAC, UClan)
*     {enter_new_authors_here}

*  History:
*     23-APR-2007 (DSB):
*        Initial version.
*     2-MAY-2007 (DSB):
*        Added parameter naccept.
*     16-JUL-2007 (DSB):
*        Ignore input spectra that contain no good data.
*     29-OCT-2020 (DSB):
*        Return a flag indicating if the spectrum was used.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
#include <stdint.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_rebincube_paste2d"

int smf_rebincube_paste2d( int badmask, dim_t nchan, dim_t nchanout,
                           dim_t *spectab, dim_t *specpop, dim_t iv0,
                           dim_t nxy, double wgt, int genvar,
                           double invar, float *ddata,
                           float *data_array, float *var_array,
                           double *wgt_array, dim_t *pop_array,
                           int64_t *nused, dim_t *nreject, dim_t *naccept,
                           float *work, int *status ){

/* Local Variables */
   dim_t ichan;                /* Index of input channel */
   dim_t iv;                   /* Vector index into output 3D array */
   dim_t nspecused;            /* No of input values pasted into output spectrum */
   dim_t ochan;                /* Index of output channel */
   float *qdata = NULL;        /* Pointer to next input data value */
   float swdd;                 /* Sum of squared input data value times weight */
   int ignore;                 /* Ignore this time slice? */
   int result = 0;             /* Returned flag */

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* We first paste the input spectrum into the work array, which is used
   as a temporary staging area for the input spectrum, prior to pasting it
   into the output cube. This is done so that the number of input channels
   that correspond to each output channel can be normalised out. Initialise
   the work array to hold zero at every output channel, except for those
   output channels to which no input channels contribute. Store bad values
   for such channels. */
   for( ochan = 0; ochan < nchanout; ochan++ ) {
      if( specpop[ ochan ] == 0 ) {
         work[ ochan ] = VAL__BADR;
      } else {
         work[ ochan ] = 0;
      }
   }

/* Loop round all channels of the input spectrum, pasting them into the
   work array. */
   qdata = ddata;
   for( ichan = 0; ichan < nchan; ichan++, qdata++ ) {

/* Get the corresponding output channel and check it is within the range
   of the output cube. */
      ochan = spectab[ ichan ];
      if( ochan != -1 ) {

/* Ignore this input value if it contributes to an output channel that is
   already bad. */
         if( work[ ochan ] != VAL__BADR ) {

/* If this data value is bad, set the work array bad. */
            if( *qdata == VAL__BADR ) {
               work[ ochan ] = VAL__BADR;

/* If the input and output spectra are both good at this channel, then paste
   the input pixel value into the work array, dividing it by the number of
   input channels that contribute to the output channel. In effect, this
   causes the final output channel value to be the mean of all the input
   channels that contribute to the output channel. */
            } else {
               work[ ochan ] += ( *qdata )/specpop[ ochan ];
               result = 1;
            }
         }
      }
   }

/* Asumme the input spectrum can be used. */
   ignore = 0;

/* See if we need to ignore this detector because of it having a different
   bad pixel mask to the existing output spectrum. We only do this if
   "badmask" is "FIRST" (0). */
   if( badmask == 0 ) {

/* If this is the first spectrum to be included in the output spectrum, we
   only ignore it if it contains no good data. */
      if( wgt_array[ iv0 ] == 0.0 ) {
         qdata = work;
         for( ochan = 0; ochan < nchanout; ochan++, qdata++ ) {
            if( *qdata != VAL__BADR ) {
               qdata = NULL;
               break;
            }
         }
         if( qdata != NULL ) ignore = 1;

/* If this is not the first spectrum to be included in the output spectrum, we
   ignore it if is has a bad pixel mask that is different to the existing
   bad pixel mask in the output cube. */
      } else {

/* Loop round all channels in the output spectrum. */
         qdata = work;
         iv = iv0;
         for( ochan = 0; ochan < nchanout; ochan++, qdata++, iv += nxy ) {

/* If exactly 1 of the input and output data values is bad, then the bad
   pixel masks differ and so we cannot use this input spectrum. */
            if( ( data_array[ iv ] == VAL__BADR && *qdata != VAL__BADR ) ||
                ( data_array[ iv ] != VAL__BADR && *qdata == VAL__BADR ) ) {
               ignore = 1;
               (*nreject)++;
               break;
            }
         }
      }

/* If we are using "AND" or "OR" for BADMASK, we can ignore the spectrum
   if it contains no good data. */
   } else {
      qdata = work;
      for( ochan = 0; ochan < nchanout; ochan++, qdata++ ) {
         if( *qdata != VAL__BADR ) {
            qdata = NULL;
            break;
         }
      }
      if( qdata != NULL ) ignore = 1;
   }

/* Check the detector is still good. */
   if( !ignore ){
      (*naccept)++;

/* Loop round all output spectral channels, counting how many get updated. */
      nspecused = 0;
      swdd = 0.0;
      qdata = work;
      iv = iv0;
      for( ochan = 0; ochan < nchanout; ochan++, qdata++, iv += nxy ) {

/* If any output pixel is contributed to by a bad input pixel, then set
   the output pixel bad. We need to do this because we only have a 2D array
   to store the weights in and so we cannot retain information about the
   number of non-bad pixels contributing to each channel. This check
   implements the "BADMASK=OR" option. If "BADMASK=FIRST" has been selected,
   this check will always succeed because otherwise the input spectrum would
   already have been ignored. */
         if( *qdata == VAL__BADR ){
            data_array[ iv ] = VAL__BADR;

         } else if( data_array[ iv ] != VAL__BADR ) {
            data_array[ iv ] += wgt*( *qdata );

/* If we are creating "spread" output variances, we also need the weighted
   sum of the squared input values. */
            if( genvar == 1 ) swdd += ( *qdata )*( *qdata )*wgt;

/* Increment the total number of output channels that receive
   contributions from the current input spectrum. */
            nspecused++;
         }
      }

/* Ignore this input spectrum if it made no contribution to the output. */
      if( nspecused > 0 ) {

/* Increment the total number of input spectra pasted into this output
   spectrum. */
         pop_array[ iv0 ]++;

/* Update the total weight associated with the appropriate output spectrum.
   The weight is the same for all channels in the output spectrum, and so
   the weights array need only be a single 2D slice. */
         wgt_array[ iv0 ] += wgt;

/* Now store info needed to calculate the output variances. What this
   info is depends on whether output variances are being calculated on the
   basis of input Tsys values or on the basis of the spread of input data
   values. First deal with Tsys variances. */
         if( genvar == 2 ) {
            var_array[ iv0 ] += wgt*wgt*invar;

/* Now deal with "spread" variances. */
         } else if( genvar == 1 ) {
            var_array[ iv0 ] += wgt*wgt;
            wgt_array[ iv0 + nxy ] += swdd/nspecused;
         }

/* Increment the total number of good input pixels pasted into the output
   cube. */
         (*nused) += nspecused;
      }
   }

   return result;
}
