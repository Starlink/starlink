/*
*+
*  Name:
*     smf_stats1

*  Purpose:
*     Low-level routine for calculating mean and standard deviation on
*     a contiguous array of double-precision data.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_stats1( double *data, dim_t start, dim_t nsamp, 
*                 unsigned char *qual, unsigned char mask, double *mean, 
*                 double *sigma, dim_t *ngood, int *status )

*  Arguments:
*     data = double* (Given)
*        Pointer to input dataarray
*     start = dim_t (Given)
*        Offset in data to the first element of the interval to analyze
*     nsamp = dim_t (Given)
*        Length of the interval to analyze
*     qual = usigned char* (Given)
*        If specified, use this QUALITY array to decide which samples
*        to use (provided mask). Otherwise data are only ignored if set
*        to VAL__BADD.
*     mask = unsigned char (Given)
*        Use with qual to define which bits in quality are relevant to
*        ignore data in the calculation.
*     mean = double* (Given and Returned)
*        Pointer to variable that will contain the mean of the data.
*     sigma = double* (Given and Returned)
*        Pointer to variable that will contain the standard deviation of
*        the data. If NULL this routine will run faster and not calculate
*        the standard deviation.
*     ngood = dim_t* (Given and Returned)
*        Pointer to variable that will indicate how many samples were used
*        to calculate the statistics. 
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Calculate mean and standard deviation. If less than 2 good samples,
*     exists with status set to SMF__INSMP.

*  Notes: 
*     The algorithm is "naive" and may suffer roundoff problems. It may be
*     necessary to switch to a (slower) compensated method. See:
*     http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance

*  Authors:
*     Edward Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-03-06 (EC):
*        Initial version
*     2008-04-18 (EC):
*        Use SMF__MINSTATSAMP for sample length check
*     2008-08-29 (TIMJ):
*        Initialise return values even if status is bad on entry.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2008 University of British Columbia. All Rights
*     Reserved.

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

/* Standard includes */
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"
#include "libsmf/smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_stats1"

void smf_stats1( double *data, dim_t start, dim_t nsamp, 
                 unsigned char *qual, unsigned char mask, double *mean, 
                 double *sigma, dim_t *ngood, int *status ) {

  /* Local variables */
  dim_t count=0;              /* Number of samples in estimate */
  dim_t i;                    /* Loop counter */
  double mu=0;                /* Estimate of the mean */
  double sum=0;               /* Sum of the data */
  double sumsq=0;             /* Sum of the square of the data */

  /* initialise return values */
  if (sigma) *sigma = VAL__BADD;
  if (ngood) *ngood = 0;
  if (mean) *mean = VAL__BADD;

  /* Check status */
  if (*status != SAI__OK) return;

  /* Initialization */

  if( qual ) {
    /* Quality checking version */

    if( sigma ) {
      /* Standard deviation calculating version */
      for( i=start; i<(start+nsamp); i++ ) {

        if( !(qual[i] & mask) ) {
          sum += data[i];
          sumsq += data[i]*data[i];
          count++;
        } 
      }
    } else {
      /* Mean only */
      for( i=start; i<(start+nsamp); i++ ) if( !(qual[i] & mask) ) {
          sum += data[i];
          count++;
        }
    }

  } else {
    /* VAL__BADD checking version */

    if( sigma ) {
      /* Standard deviation calculating version */
      for( i=start; i<(start+nsamp); i++ ) if( data[i] != VAL__BADD ) {
          sum += data[i];
          sumsq += data[i]*data[i];
          count++;
        }
    } else {
      /* Mean only */
      for( i=start; i<(start+nsamp); i++ ) if( data[i] != VAL__BADD ) {
          sum += data[i];
          count++;
        }
    }
  }

  if( ngood ) *ngood = count;

  /* Enough samples? */
  if( count < SMF__MINSTATSAMP ) {
    *status = SMF__INSMP;
    msgSeti("MIN",SMF__MINSTATSAMP);
    msgSeti("N", count );
    errRep( FUNC_NAME, "Insufficient number of good samples (^N<^MIN) for statistics", status );
    return;
  }

  /* Finalize calculation and return result */
  mu = sum / count;

  if( mean ) *mean = mu;
  if( sigma ) *sigma = sqrt( (sumsq - count*mu*mu)/(count) );
}
