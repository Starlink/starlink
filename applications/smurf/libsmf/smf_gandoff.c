/*
*+
*  Name:
*     smf_gandoff

*  Purpose:
*     Get the gain and offset for each time slice from the GAI model.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void smf_gandoff( dim_t ibolo, dim_t time0, dim_t time1, dim_t ntime,
*                       dim_t gbstride, dim_t gcstride, double *gai_data,
*                       dim_t nblock, dim_t gain_box, double *wg,
*                       double *woff, double *wcc, int *status )

*  Arguments:
*     ibolo = dim_t (Given)
*        Index of the bolometer for which the gain and offset values are
*        required.
*     time0 = dim_t * (Given)
*        Index of first time slice for which values should be returned.
*     time1 = dim_t * (Given)
*        Index of last time slice for which values should be returned.
*     ntime = dim_t * (Given)
*        Total number of time slices available.
*     gbstride = dim_t * (Given)
*        Stride between bolometers in the "gai_data" array.
*     gcstride = dim_t * (Given)
*        Stride between "time" values in the "gai_data" array ("time" in
*        quotes because it is not really a time axis - see below).
*     gai_data = double * (Given)
*        Array holding the gain, offset and correlation coefficient for
*        the linear fit of each block of time slices to the common mode
*        signal, or NULL. This is nominally like a normal 2D array spanned
*        by bolometer index and time index, and so would be indexed as:
*
*           gai_data[ ibolo*gbstride + itime*gcstride ];
*
*        However the "time" axis is used instead to enumerate the
*        coefficients of the linear fit associated with bolometer "ibolo".
*        The "itime" value above is thus repalced with "ipar*nblock+iblock",
*        where "iblock" is the index of a block (of time slices),
*        "nblock" is the number of blocks used to describe the entire
*        time series, and "ipar" is the index of the required fit
*        parameter (0=gain, 1=offset, 2=correlation).
*
*        If NULL, the returned gain array will be filled with the value
*        1.0 amd the returned offset array will be filled with the value 0.0.
*     nblock = dim_t * (Given)
*        The number of blocks into which the time stream is divided.
*     gain_box = dim_t * (Given)
*        The number of time slices in each block (note, the final block
*        may contain more than "gain_box" slices ).
*     wg = double * (Returned)
*        Pointer to an array in which to return the gain values for
*        bolometer "ibolo". It should have at least "time1-time0+1"
*        elements.
*     woff = double * (Returned)
*        Pointer to an array in which to return the offset values for
*        bolometer "ibolo". It should have at least "time1-time0+1"
*        elements.
*     wcc = double * (Returned)
*        Pointer to an array in which to return the correlation coefficients
*        for bolometer "ibolo". It should have at least "time1-time0+1"
*        elements. It may be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     For each time slice in a given range, this function retrieves the gain
*     and offset of the linear mapping from common mode signal to a specific
*     bolometer signal. The gain and offset for each time slice are
*     returned in two asupplied arrays.
*
*     Time slices in a bolometer time stream are grouped into blocks, and
*     each block has an associated gain and offset value. However, to avoid
*     dis-continutities associated with changing the gain suddenly at the
*     end of each block, linear interpolation is performed between blocks
*     in order to assign a smoothly varying gain and offset value with
*     each individual time slice. A linear interpolation "section" extends
*     from the mid-point of a block to the mid-point of the following
*     block. For a given block of time slices, the block's gain and offset
*     are assigned exactly to the instant in time corresponding to the
*     centre of the block. Subsequent time slices in the second half of
*     the block, and the first half of the following block, are assigned
*     gain and offset values that vary linearly so that the gain and
*     offset values associated with the following block are reached at the
*     instant in time corresponding to the centre of the following block.
*     Time slices in the first half of the first block are all given the
*     gain and offset associated with the first block. Likewise, the time
*     slices in the second half of the last block are all given the gain
*     and offset associated with the last block.

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-FEB-2009 (DSB):
*        Original version
*     9-APR-2009 (DSB):
*        Return default values if no gain data is supplied.
*     6-MAY-2012 (DSB):
*        Added argument "wcc".
*     1-JUN-2012 (DSB):
*        The gains and offsets stored in "gai" for bad blocks are now
*        interpolated between the neighbouring good blocks, so we no
*        longer need to distinguish here between good blocks and bad
*        blocks.

*  Copyright:
*     Copyright (C) 2010-2012 Science & Technology Facilities Council.
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
#include "sae_par.h"
#include "prm_par.h"
#include "mers.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_gandoff( dim_t ibolo, dim_t time0, dim_t time1, dim_t ntime,
                  dim_t gbstride, dim_t gcstride, double *gai_data,
                  dim_t nblock, dim_t gain_box, double *wg, double *woff,
                  double *wcc, int *status ){

/* Local Variables; */
   dim_t cc_offset;         /* Offset from gradient to corr. coeff value */
   dim_t dt;                /* No. of time slices in section */
   dim_t igbase;            /* Index of first gain value */
   dim_t isection;          /* Index of block at end of *next* section. */
   dim_t itime;             /* Time slice index */
   dim_t next_section;      /* Index of the first time slice in next section */
   dim_t off_offset;        /* Offset from gradient to offset value */
   double *cc;              /* Pointer to next correlation coefficient */
   double *g;               /* Pointer to next gain value */
   double *off;             /* Pointer to next offset value */
   double cc0;              /* Corr. coeff at start of interpolation section */
   double cc1;              /* Corr. coeff at end of interpolation section */
   double end_time;         /* Time at end of interpolation section */
   double gain0;            /* Gain at start of interpolation section */
   double gain1;            /* Gain at end of interpolation section */
   double k1;               /* Gain gradient */
   double k2;               /* Gain intercept */
   double k3;               /* Offset gradient */
   double k4;               /* Offset intercept */
   double k5;               /* Corr. coeff gradient */
   double k6;               /* Corr. coeff intercept */
   double off0;             /* Offset at start of interpolation section */
   double off1;             /* Offset at end of interpolation section */
   double start_time;       /* Time at start of interpolation section */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Ensure supplied values look OK. */
   if( time0 > time1 ) {
      *status = SAI__ERROR;
      msgSeti( "T", (int) time0 );
      errRep( "", "smf_gandoff: Bad value (^T) supplied for 'time0'.",
              status );
   }

   if( time1 >= ntime ) {
      *status = SAI__ERROR;
      msgSeti( "T", (int) time1 );
      errRep( "", "smf_gandoff: Bad value (^T) supplied for 'time1'.",
              status );
   }

/* If no gain values have been supplied, just fil the arrays with default
   values and return. */
   if( !gai_data ) {
      g = wg;
      off = woff;
      for( itime = time0; itime <= time1; itime++ ) {
         *(g++) = 1.0;
         *(off++) = 0.0;
      }
      if( wcc ) {
         cc = wcc;
         for( itime = time0; itime <= time1; itime++ ) {
            *(cc++) = 1.0;
         }
      }
      return;
   }

/* Offset to start of offset values within gai_data. */
   off_offset = nblock*gcstride;

/* Offset to start of correlation coefficients within gai_data. */
   cc_offset = 2*nblock*gcstride;

/* Initialise index of gain value associated with the block that occupies
   the second half of the current linear interpolation section. */
   igbase = ibolo*gbstride;

/* Initialise the gain, offset and correlation coefficient at the start and
   end of the current linearly interpolated section (i.e. the second half of
   the current block and the first half of the following block).*/
   gain1 = gai_data[ igbase ];
   off1 = gai_data[ igbase + off_offset ];
   cc1 = gai_data[ igbase + cc_offset ];
   gain0 = VAL__BADD;
   off0 = VAL__BADD;
   cc0 = VAL__BADD;

/* Initialise the (floating point) time at the start and end of the section.
   This is measured from the centre of the first time slice. */
   end_time = 0.5*( gain_box - 1.0 );
   start_time = end_time - gain_box;

/* Initialise the (integer) index of the first time slice in the next linear
   interpolation section. */
   next_section = gain_box/2;

/* Set the index of gain value associated with the next block. */
   igbase += gcstride;

/* Initialise the index of the block at the end of the *next* linear
   interpolation section. */
   isection = 1;
   k1 = VAL__BADD;
   k2 = VAL__BADD;
   k3 = VAL__BADD;
   k4 = VAL__BADD;
   k5 = VAL__BADD;
   k6 = VAL__BADD;

/* Loop over all time slices, up to the last required time slice. */
   g = wg;
   off = woff;
   cc = wcc;
   for( itime = 0; itime <= time1; itime++ ) {

/* If we have reached the start of a new linear interpolation section, find
   the constants that give the gain and offset for each time slice in the new
   section. */
      if( itime == next_section ) {

/* The gain, offset, correlation coefficient and time at the end of the
   previous section are used to define the start of the new section. */
         gain0 = gain1;
         off0 = off1;
         cc0 = cc1;
         start_time = end_time;

/* Get the gain, offset, correlation coefficient and time at the end of the
   new section. Increment the count of sections and check that a new gain and
   offset value are available (i.e. we have not already reached the end). Also
   take care because the last block may contain more than gain_box time
   slices. */
         isection++;
         if( isection < nblock ) { /* most sections have "gain_box" slices */
            gain1 = gai_data[ igbase ];
            off1 = gai_data[ igbase + off_offset ];
            cc1 = gai_data[ igbase + cc_offset ];
            dt = gain_box;

         } else if( isection == nblock ){ /* penultimate section may be long */
            gain1 = gai_data[ igbase ];
            off1 = gai_data[ igbase + off_offset ];
            cc1 = gai_data[ igbase + cc_offset ];
            dt = ( ntime - next_section )/2 + gain_box/4;

         } else {  /* last section extends beyond last time slice */
            gain1 = VAL__BADD;
            off1 = VAL__BADD;
            cc1 = VAL__BADD;
            dt = gain_box;
         }
         end_time += (double) dt;

/* Advance the index of gain value associated with the next block. */
         igbase += gcstride;

/* Advance the index of the first time slice in the next linear interpolation
   section. */
         next_section += dt;

/* If we have good values at the start and end of the section, calculate the
   linear interpolation constants for this section (if the section contains
   any required time slices). Note, smf_find_gains ensures that bad
   blocks (i.e. blocks with VAL__BADD for their correlatrion coefficient)
   have gains and offsets interpolated from the neighbouring good blocks.
   So we can use the gains and offsets stored in gai even if the
   correlation coefficient is bad. */
         if( time0 < next_section ) {
            if( gain0 != VAL__BADD && off0 != VAL__BADD &&
                gain1 != VAL__BADD && off1 != VAL__BADD && dt > 0 ) {
               k1 = ( gain1 - gain0 )/dt;
               k2 = gain0 - k1*start_time;
               k3 = ( off1 - off0 )/dt;
               k4 = off0 - k3*start_time;
            } else {
               k1 = VAL__BADD;
               k2 = VAL__BADD;
               k3 = VAL__BADD;
               k4 = VAL__BADD;
            }

            if( cc0 != VAL__BADD && cc1 != VAL__BADD && dt > 0 ) {
               k5 = ( cc1 - cc0 )/dt;
               k6 = cc0 - k5*start_time;
            } else {
               k5 = VAL__BADD;
               k6 = VAL__BADD;
            }
         }
      }

/* Check we have reached the first time slice of interest. */
      if( itime >= time0 ) {

/* Use the linear interpolation constants to determine a gain and offset
   for the current time slice. Or use the gain and offset of the adjacent
   block if the current block is bad. */
         if( k1 != VAL__BADD ) {
            *(g++) = k1*( (double) itime ) + k2;
            *(off++) = k3*( (double) itime ) + k4;

         } else if( gain0 != VAL__BADD ) {
            *(g++) = gain0;
            *(off++) = off0;

         } else if( gain1 != VAL__BADD ) {
            *(g++) = gain1;
            *(off++) = off1;

         } else {
            *(g++) = 1.0;
            *(off++) = 0.0;
         }

         if( cc ) {
            if( k5 != VAL__BADD ) {
               *(cc++) = k5*( (double) itime ) + k6;

            } else if( cc0 != VAL__BADD ) {
               *(cc++) = cc0;

            } else if( cc1 != VAL__BADD ) {
               *(cc++) = cc1;

            } else {
               *(cc++) = 1.0;
            }
         }
      }
   }
}


