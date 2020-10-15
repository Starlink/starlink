/*
*+
*  Name:
*     smf_find_gains_array

*  Purpose:
*     Using a given template, find the gain and offset for each bolometer
*     in a smfArray and reject aberrant bolometers.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_find_gains_array( ThrWorkForce *wf, int flags, smfArray *data,
*                               const unsigned char *mask, smfArray *lut,
*                               double *template, AstKeyMap *keymap,
*                               smf_qual_t goodqual, smf_qual_t badqual,
*                               smfArray *gain,
*                               int *nrej, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     flags = int (Given)
*        - The first bit indicates if unusual bolometer-block should be
*        rejected. If the first bit is set, then no bad bolometer
*        blocks are rejected or flagged. If the first bit is unset, then
*        whether to reject and flag bolometer blocks is determined by the
*        "NOFLAG" item in the suppledi KeyMap.
*        - The second bit indicates whether to report information about
*        the number of converged blocks. If unset, messages indicating
*        how many blocks have converged are reported. If set, these
*        messages are not reported.
*        - The third bit indicates whether to check bolo-blocks that have
*        already converged. If set then all bolo-blocks are checked to
*        see if they have unusual gains or correlation coefficients. If
*        unset, only bolo-blocks which have not converged are checked.
*        messages are not reported.
*     data = smfArray * (Given)
*        The input data. Each bolometer time series will be compared to
*        the template.
*     mask = const unsigned char * (Given)
*        Pointer to a 2D mask of boolean values. May be NULL. If supplied,
*        the mask should have the bounds of the output map. And time
*        samples in "data" that correspond to zero-valued pixels in this
*        mask are excluded from the fitting process.
*     lut = smfData * (Given)
*        The index of the corresponding pixel within "mask" for each sample
*        in "data". Only used if "mask" is not NULL.
*     template = double * (Given)
*        The 1-dimensional template. The length of this array should
*        equal the number of time slices in "data".
*     keymap = AstKeyMap * (Given)
*        An AST KeyMap holding values for the parameters required by this
*        function:
*
*        - corr_abstol (double): A bolometer is rejected if its correlation
*        coefficient is lower corr_abstol.
*
*        - corr_tol (double): A bolometer is rejected if its correlation
*        coefficient is lower than the mean correlation coefficient by more
*        than corr_tol times the standard deviation of the correlation
*        coefficients. Note, all correlation coefficients lower than
*        "corr_abstol" are removed before finding the mean and standard
*        deviation of the correlation coefficients.
*
*        - fit_box (int): The number of adjacent time slices that are
*        to be used when fitting the data to the template. fit_box should
*        be no smaller than gain_box.
*
*        - gain_abstol (double): A bolometer is rejected if its log(gain)
*        value more than gain_abstol from the mean log(gain) value. Note, all
*        negative gains are removed before finding the mean of the log(gain)
*        values.
*
*        - gain_box (int): The number of adjacent time slices that are
*        to be described using a single gain and offset.
*
*        - gain_fgood (double): The minimum number of good blocks required
*        for a usable bolometer, expressed as fraction in the range zero
*        to one.
*
*        - gain_is_one (int): If non-zero, the bolometer gains determined
*        by the least squares linear fit are ignored, and a value of unity
*        is used for all gains. The offsets and correlation coefficients
*        are still determined.
*
*        - gain_rat (double): The square root of the ratio of the maximum
*        acceptable block gain within a bolometer to the minimum acceptable
*        block gain. It is used when checking for consistency amongst the
*        blocks of a single bolometer.
*
*        - gain_tol (double): A bolometer is rejected if its log(gain)
*        value more than gain_tol standard deviations from the mean
*        log(gain) value. Note, all negative gains are removed before
*        finding the mean and standard deviation of the log(gain) values.
*
*        - noflag (int): If non-zero, then no boomeometer blocks are
*        rejected or flagged by this function. Also see argument "flags".
*
*        - offset_is_zero (int): If non-zero, the bolometer offsets are
*        forced to zero within the least squares linear fit. The gains and
*        correlation coefficients are still determined.
*
*     goodqual = smf_qual_t (Given)
*        The quality value to be used when checking for values to be
*        included in the comparison.
*     badqual = smf_qual_t (Given)
*        The quality value to be assigned to samples that are found to be
*        aberrant. In addition, if an entire bolometer is set bad, its
*        first sample will be flagged with SMF__Q_BADB.
*     gain = smfArray * (Given & Returned)
*        This holds the gains, offsets and correlation coefficients that
*        scale the template values into the bolometer values for each block:
*
*        data ~= scale*template + offset
*
*        In this smfData, the time axis is subverted to enumerate the
*        coefficients, and should have a length of 3*gain_box. For each
*        bolometer, the first "gain_box" values on the time axis contain
*        the gain values for the bolometer, the next block of "gain_box"
*        values are the offsets for the bolometer, and the final block of
*        "gain_box" values are the correlation coefficients.
*
*        The array should be filled with zeros before calling this function
*        for the first time. New gains, offsets, and correlation coefficients
*        for each bolometer in each block are found and returned in this
*        smfData. The exception tp this is that the values for any block that
*        has already converged (i.e. any block from which no bolometers were
*        rejected on the previous invocation of this function -see argument
*        "nrej") are left unchanged. Also, any values that are VAL__BADD
*        on entry (i.e. previously rejected bolometer blocks) are left
*        unchanged
*
*        On exit, a gain of VAL__BADD is assigned to any blocks that could
*        not be fit to the template, or that were rejected.
*     nrej = int * (Given and Returned)
*        Pointer to an array with one element for each bolometer block.
*        That is, the size of this array should equal the number of
*        blocks per bolometer, (i.e. one third of the size of the "time"
*        axis in "gai"). The array should be filled with arbitrary non
*        zero values before calling this function for the first time. On
*        exit, each element holds the number of bolometers rejected from
*        the corresponding block during the current invocation of this
*        function (i.e. bolometers rejected on previous invocations are
*        not included in the returned counts). Thus if a value of zero is
*        returned for a particular block, then it means that no further
*        bolometers have been rejected from that block. The supplied
*        values are used to determine whether it is necessary to
*        re-calculate the gains and offsets for the bolometers in each
*        block - if a block has a supplied value of zero in this array,
*        and the two neighbouring blocks are also supplied as zero, then
*        the central block of these three has converged, and so the
*        bolometer gains and offsets in the central block (supplied
*        within "gai") are left unchanged on exit. Note, the supplied
*        values are treated as boolean zero/non-zero values - the specific
*        non-zero value supplied is insignificant.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned value:
*     The total number of bolometer blocks rejected by the current
*     invocation of this function (i.e. the sum of the values returned in
*     the "nrej" array).

*  Description:
*     Each sub-array in the supplied smfArray is processed using
*     smf_find_gains, and the resulting "nrej" arrays are summed.
*     Rejection of bolo-blocks in each sub-array is based on the
*     statistics of the other bolo-blocks in the same sub-array (i.e.
*     not the statistics of all bolo-blocks in all sub-arrays).

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-JUN-2010 (DSB):
*        Original version.
*     27-FEB-2012 (DSB):
*        Add args "mask" and "lut".
*     6-MAY-2012 (DSB):
*        Add argument "flags".
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

/* Main entry */
int smf_find_gains_array( ThrWorkForce *wf, int flags, smfArray *data,
                          const unsigned char *mask, smfArray *lut,
                          double *template, AstKeyMap *keymap,
                          smf_qual_t goodqual, smf_qual_t badqual,
                          smfArray *gain, int *nrej, int *status ){

/* Local Variables: */
   int *nrej_out;
   int *nrej_in;
   int iblock;
   int isub;
   int nbad;
   int nsub;
   dim_t nblock;
   dim_t nrej_size;

/* Initialise */
   nbad = 0;

/* Check the inherited status. */
   if( *status != SAI__OK ) return nbad;

/* Get the number of sub-arrays within the supplied data smfArray (it is
   assumed that the quality and gain smfArrays contain the same number of
   sub-arrays). */
   nsub = (int) data->ndat;

/* If there is only one sub-array, just call smf_find_gains. */
   if( nsub == 1 ) {
      nbad = smf_find_gains( wf, flags, data->sdata[ 0 ], mask,
                             lut ? lut->sdata[0] : NULL, template,
                             keymap, goodqual, badqual,
                             gain->sdata[ 0 ], nrej, status );

/* If there are two or more sub-arrays, we need to combine the "nrej"
   arrays so that the same supplied values are used for all sub-arrays,
   and the values returned by this function represent the sum of the sum
   array "nrej" values. */
   } else {

/* Get the number of bytes in the nrej array. */
      nrej_size = astSizeOf( nrej );

/* Get the number of elements in the nrej array (i.e. the number of
   blocks). */
      nblock = nrej_size/sizeof( *nrej );

/* Create room for two other "nrej" arrays. */
      nrej_in = astMalloc( nrej_size );
      nrej_out = astMalloc( nrej_size );

/* Check pointers can be used safely. */
      if( *status == SAI__OK ) {

/* Zero the contents of "nrej_out", which is used to accumulate the
   total number of bolometers rejected from each block, summed over all
   sub-arrays. */
         memset( nrej_out, 0, nrej_size );

/* Loop round all sub-arrays. */
         for( isub = 0; isub < nsub; isub++ ) {

/* Copy the supplied nrej values into "nrej_in". */
            memcpy( nrej_in, nrej, nrej_size );

/* Call smf_find_gains to process the sub-array. This puts the number
   of sub-array bolometers rejected from each block into "nrej_in". */
            nbad += smf_find_gains( wf, flags, data->sdata[ isub ], mask,
                                    lut ? lut->sdata[ isub ] : NULL,
                                    template, keymap, goodqual, badqual,
                                    gain->sdata[ isub ], nrej_in, status );

/* Update the total number of bolometers rejected from each block, summed
   over all sub-arrays. */
            for( iblock = 0; iblock < nblock; iblock++ ) {
               nrej_out[ iblock ] += nrej_in[ iblock ];
            }
         }

/* Copy the total number of bolometers rejected from each block, summed
   over all sub-arrays, into the returned "nrej" array. */
         memcpy( nrej, nrej_out, nrej_size );
      }

/* Free resources. */
      nrej_in = astFree( nrej_in );
      nrej_out = astFree( nrej_out );
   }

/* Return the number of rejected bolo blocks. */
   return nbad;
}

