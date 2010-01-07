/*
*+
*  Name:
*     smf_fillgaps

*  Purpose:
*     Fill flagged regions of data with constrained realization of noise

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_fillgaps( smfData *data, unsigned char *quality,
*                   unsigned char mask, int *status )

*  Arguments:
*     data = smfData * (Given and Returned)
*        The data that will be flagged
*     quality = unsigned char * (Given and Returned)
*        If set, use this buffer instead of QUALITY associated with data.
*        If NULL, use the QUALITY associated with data (however bad status
*        will be set if internal QUALITY is also NULL).
*     mask = unsigned char (Given)
*        Define which bits in quality indicate locations of gaps to be filled.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Identify continuous blocks of each detector time series that match
*     the given quality mask (e.g. spikes). Replace the flagged block
*     of data with a constrained realization of noise: smoothly connect the
*     before/after boundaries of the gap to avoid ringing when filters are
*     applied.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-01-06 (EC):
*        Initial code stub
*     2010-01-07 (DSB):
*        Initial full version.

*  Copyright:
*     Copyright (C) 2010 Univeristy of British Columbia.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "libsc2sim/sc2sim.h"

/* Other includes */
#define FUNC_NAME "smf_fillgaps"

/* Define the width of the patch used to determine the mean level and
   noise adjacent to each flagged block. The current value is pretty well
   arbitrary. */
#define BOX 10

void  smf_fillgaps( smfData *data, unsigned char *quality, unsigned char mask, 
                    int *status ) {

/* Local Variables */
  dim_t i;                      /* Bolometer index */
  dim_t j;                      /* Time-slice index */
  dim_t nbolo;                  /* Number of bolos */
  dim_t ntslice;                /* Number of time slices */
  double *dat=NULL;             /* Pointer to bolo data */
  double cl;                    /* Offset of fit at left end of block */
  double cr;                    /* Offset of fit at right end of block */
  double grad;                  /* Gradient of line joining patch mid-points */
  double meanl;                 /* Mean value in left patch */
  double meanr;                 /* Mean value in right patch */
  double ml;                    /* Gradient of fit at left end of block */
  double mr;                    /* Gradient of fit at right end of block */
  double offset;                /* Offset of line joining patch mid-points */
  double sigma;                 /* Mean standard deviation */
  double sigmal;                /* Standard deviation in left patch */
  double sigmar;                /* Standard deviation in right patch */
  double x[ BOX ];              /* Array of sample positions */
  double y[ BOX ];              /* Array of sample values */
  int count;                    /* No. of unflagged since last flagged sample */
  int flagged;                  /* Is the current sample flagged? */
  int inside;                   /* Was previous sample flagged? */
  int jend;                     /* Index of last flagged sample in block */
  int jfinal;                   /* Final time-slice index */
  int jj;                       /* Time-slice index */
  int jstart;                   /* Index of first flagged sample in block */
  int k;                        /* Loop count */
  int leftend;                  /* Index at end of left hand patch */
  int leftstart;                /* Index at start of left hand patch */
  int rightend;                 /* Index at end of right hand patch */
  int rightstart;               /* Index at start of right hand patch */
  size_t bstride;               /* bolo stride */
  size_t tstride;               /* time slice stride */
  unsigned char *qua=NULL;      /* Pointer to quality array */

/* Main routine */
  if (*status != SAI__OK) return;

  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status )) return;

/* Pointers to data and quality */
  dat = data->pntr[0];
  if( quality ) {
    qua = quality;
  } else {
    qua = data->pntr[2];
  }

  if( !qua ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": No valid QUALITY array was provided", status );
    return;
  }

  if( !dat ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData does not contain a DATA component",status);
    return;
  }

  if( *status == SAI__OK ) {
    /* obtain data dimensions */
    smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride, &tstride,
                  status );
  }

  /* Pre-calculate a useful constant - the final used value of "j". */
  jfinal = ntslice - 1;

  /* Loop over bolometer */
  for( i=0; i<nbolo; i++ ) if( !(qua[i*bstride] & SMF__Q_BADB) ) {

    /* Initialise a flag to indicate that the current sample is not
       inside a block of flagged samples. */
    inside = 0;

    /* Initialise the count of unflagged samples following the previous
       block of flagged samples. */
    count = 0;

    /* Initialise the index of the first sample to be replaced. This
       initial value is only used if there are fewer than BOX unflagged 
       samples before the first block of flagged samples. */
    jstart = 0;
    jend = -1;

    /* Loop over time series */
    for( j=0; j<ntslice; j++ ) {

      /* Is this sample flagged? Always condsider the last sample to be
         unflagged, so that any block of flagged samples at the end of
         the time-series is filled. Samples with bad data values are
         filled, as well as samples with the specified quality. */
      flagged = ( qua[ i*bstride + j*tstride ] & mask ) || 
                ( dat[ i*bstride + j*tstride ] == VAL__BADD );

      if( flagged && (int) j < jfinal ) {

        /* If this is the first flagged sample in a new block of flagged
           samples, set "inside" to indicate that we are now inside a 
           block of flagged samples. */
        if( ! inside ) {
          inside = 1;

          /* If the number of unflagged samples since the end of the
             previous block of flagged samples is at least BOX, then
             record the index of the first sample to be replaced in this
             new block. If there have been fewer than BOX samples since
             the end of the previous block of flagged samples, we consider 
             this new block to be an extension of the previous block,
             and so we do not change the "jstart" value (the few
             unflagged samples that were found between the two blocks 
             of flagged samples will be replaced, together with the 
             neighbouring flagged samples). We do this because we need at
             last BOX unflagged samples between adjacent pairs of flagged
             blocks. */
          if( count >= BOX ) jstart = j;
        }

      /* If this sample is not flagged (or if it is the final sample in
         the time-series)... */
      } else {

        /* If this is the first sample following a block of flagged samples, 
           record the index of the last sample in the block (which may be 
           the current sample if the current sample is the last sample), and 
           indicate that we are no longer in a block. Also reset the count 
           of unflagged samples following the end of the flagged block. */
        if( inside ) {
          if( flagged ) {
             jend = j;
          } else {
             jend = j - 1;
          }
          inside = 0;
          count = 0;
        }

        /* Increment the number of unflagged samples following the end
           of the previous flagged block. */
        count++;

        /* If we have now found BOX unflagged samples following the end of 
           the previous block of flagged samples, we can replace the block. 
           Also replace the block if we have reached the end of the time
           series. */
        if( ( count == BOX || (int) j == jfinal ) && jend >= jstart ) {

          /* If the block is only a single pixel wide, just replace it
             with the mean of the two neighbouring sample values. */
          if( jend == jstart ) {
              if( jend == 0 ) {
                 dat[ i*bstride + jend*tstride ] = 
                         dat[ i*bstride + ( jend + 1 )*tstride ];
              } else if( jend == jfinal ) {
                 dat[ i*bstride + jend*tstride ] = 
                         dat[ i*bstride + ( jend - 1 )*tstride ];
              } else {
                 dat[ i*bstride + jend*tstride ] = 0.5*( 
                         dat[ i*bstride + ( jend + 1 )*tstride ] + 
                         dat[ i*bstride + ( jend - 1 )*tstride ] );
              }

          /* Otherwise, we fill the block using a straight line plus
             noise... */
          } else {     

            /* If possible fit a straight line to the BOX samples following
               the end of the flagged block. */
            rightstart = jend + 1;
            rightend = jend + BOX;
            if( rightend >= (int) ntslice ) rightend = ntslice - 1;
            if( rightend - rightstart > BOX/2 ) {
              k = 0;
              for( jj = rightstart; jj <= rightend; jj++,k++ ) {
                x[ k ] = jj;
                y[ k ] = dat[ i*bstride + jj*tstride ];
              }
              kpg1Fit1d( 1, k, y, x, &mr, &cr, &sigmar, status );
            } else {
              mr = VAL__BADD;
              cr = VAL__BADD;
              sigmar = VAL__BADD;
            }
  
            /* If possible fit a straight line to the BOX samples preceeding
               the start of the flagged block. */
            leftend = jstart - 1;
            leftstart = jstart - BOX;
            if( leftstart < 0 ) leftstart = 0;
            if( leftend - leftstart > BOX/2 ) {
              k = 0;
              for( jj = leftstart; jj <= leftend; jj++,k++ ) {
                x[ k ] = jj;
                y[ k ] = dat[ i*bstride + jj*tstride ];
              }
              kpg1Fit1d( 1, k, y, x, &ml, &cl, &sigmal, status );
            } else {
              ml = VAL__BADD;
              cl = VAL__BADD;
              sigmal = VAL__BADD;
            }
  
            /* Find the mean noise level. */
            if( sigmal != VAL__BADD && sigmar != VAL__BADD ) {
               sigma = 0.5*( sigmal + sigmar );
            } else if( sigmal != VAL__BADD ) {
               sigma = sigmal;
            } else {
               sigma = sigmar;
            }
  
            /* Find the gradient and offset for the straight line used to
               create the replacement values for the flagged block. */
            if( jstart <= 0 ) {
              grad = mr;
              offset = cr;
  
            } else if( jend >= jfinal ) {
              grad = ml;
              offset = cl;
  
            } else {
              meanl = ml*( jstart - 1 ) + cl;
              meanr = mr*( jend + 1 ) + cr;
              grad = ( meanr - meanl )/( jend - jstart + 2 );
              offset = meanl - grad*( jstart - 1 );
            }                                       
  
            /* If at least one of the straight line fits above was
               succesful, the flagged block is replaced by a straight line
               plus noise. */
            if( sigma != VAL__BADD ) {
              for( jj = jstart; jj <= jend; jj++ ) {
                dat[ i*bstride + jj*tstride ] = grad*jj + offset +
                                                  sc2sim_drand( sigma );
              }
            } 
          }
        }
      }
    }
  }
}














