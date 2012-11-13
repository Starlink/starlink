/*
*+
*  Name:
*     smf_filter_mce

*  Purpose:
*     Apply a filter that compensates for the SCUBA-2 MCE anti-aliasing filter

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_filter_mce( smfFilter *filt, int noinverse, int *status );

*  Arguments:
*     filt = smfFilter * (Given and Returned)
*        Pointer to smfFilter to be modified
*     noinverse = int (Given)
*        If set do not take calculate the filter itself, rather than its inverse
*     status = int* (Given and Returned)
*        Pointer to global status

*  Return Value:

*  Description:
*     The SCUBA-2 Multi-Channel Electronics (MCE) apply an
*     anti-aliasing filter prior to re-sampling to 200 Hz (see
*     http://e-mode.phas.ubc.ca/mcewiki/index.php/Digital_4-pole_Butterworth_Low-pass_filter).
*     The purpose of this function is to apply the inverse of the
*     filter in Fourier space (i.e. to de-convolve). However, if the
*     noinverse flag is set, the inverse is not taken (i.e., if you
*     wish to calculate the response itself for the purpose of
*     simulation).
*     

*  Notes:

*  Authors:
*     Edward Chapin
*     {enter_new_authors_here}

*  History:
*     2012-11-10 (EC):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.

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

/* System includes */
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "fftw3.h"
#include "gsl/gsl_complex_math.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_filter_mce"

/* A bunch of parameters for the MCE 4-pole Butterworth low-pass filter */

#define MCE_FILT_B_1_1 -1.9587402   /* -2.*32092./2.^15. */
#define MCE_FILT_B_1_2 0.96130371   /*  2.*15750./2.^15. */ 
#define MCE_FILT_B_2_1 -1.9066162   /* -2.*31238./2.^15. */ 
#define MCE_FILT_B_2_2 0.90911865   /* 2.*14895./2.^15.  */

#define MCE_FILT_CLOCK_PERIOD 20E-9 /* 50 MHz clock */
#define MCE_FILT_ROW_DWELL 128.     /* time to dwell at each row (in clocks) */
#define MCE_FILT_NUM_ROWS 41.       /* number of rows addressed */
#define MCE_FILT_DELTA_TIME (MCE_FILT_CLOCK_PERIOD*MCE_FILT_ROW_DWELL*MCE_FILT_NUM_ROWS) /* sample length */
#define MCE_FILT_SRATE (1./MCE_FILT_DELTA_TIME) /* sample rate */

void smf_filter_mce( smfFilter *filt, int noinverse, int *status ) {
  size_t i;             /* Loop counter */

  if( *status != SAI__OK ) return;

  if( !filt ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "NULL smfFilter supplied.", status );
    return;
  }

  if( filt->ndims != 1 ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": function only generates filters for time-series",
            status );
    return;
  }

  if( !filt->fdims[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": 0-length smfFilter supplied.",
            status );
    return;
  }

  /* If filt->real is NULL, create a complex identity filter first. Similarly,
     if the filter is currently only real-valued, add an imaginary part. */
  if( !filt->real ) {
    smf_filter_ident( filt, 1, status );
    if( *status != SAI__OK ) return;
  } else if( !filt->imag ) {
    filt->imag = astCalloc( filt->fdims[0], sizeof(*filt->imag) );
    if( *status != SAI__OK ) return;
    filt->isComplex = 1;
  }

  /* Loop over all frequencies in the filter */
  for( i=0; i<filt->fdims[0]; i++ ) {
    double cos_m_o;
    double sin_m_o;
    double cos_m_2o;
    double sin_m_2o;
    double f;
    gsl_complex den;
    gsl_complex h1_omega;
    gsl_complex h2_omega;
    gsl_complex h_omega;
    gsl_complex num;
    gsl_complex temp;
    double omega;
    
    f = filt->df[0]*i;                       /* Frequency at this step */
    omega = (f / MCE_FILT_SRATE)*2*AST__DPI; /* Angular frequency */

    cos_m_o = cos(-omega);
    sin_m_o = sin(-omega);
    cos_m_2o = cos(-2*omega);
    sin_m_2o = sin(-2*omega);

    /*
      h1_omega=(1 + 2*complex(cos_m_o,sin_m_o) + complex(cos_m_2o,sin_m_2o)) / 
      (1 + b_1_1*complex(cos_m_o,sin_m_o) + b_1_2 * complex(cos_m_2o,sin_m_2o))
    */

    /* numerator */

    GSL_SET_COMPLEX(&num, 1, 0);

    GSL_SET_COMPLEX(&temp, cos_m_o, sin_m_o);
    num = gsl_complex_add( num, gsl_complex_mul_real(temp, 2) );

    GSL_SET_COMPLEX(&temp, cos_m_2o, sin_m_2o);
    num = gsl_complex_add( num, temp );

    /* denominator */

    GSL_SET_COMPLEX(&den, 1, 0);

    GSL_SET_COMPLEX(&temp, cos_m_o, sin_m_o);
    den = gsl_complex_add( den, gsl_complex_mul_real(temp,MCE_FILT_B_1_1) );

    GSL_SET_COMPLEX(&temp, cos_m_2o, sin_m_2o);
    den = gsl_complex_add( den, gsl_complex_mul_real(temp,MCE_FILT_B_1_2) );

    /* quotient */

    h1_omega = gsl_complex_div( num, den );
                              

    /*
      h2_omega=(1 + 2*complex(cos_m_o,sin_m_o) + complex(cos_m_2o,sin_m_2o)) / 
      (1 + b_2_1*complex(cos_m_o,sin_m_o) + b_2_2*complex(cos_m_2o,sin_m_2o))

      note: we can re-use numerator from above
    */

    /* denominator */

    GSL_SET_COMPLEX(&den, 1, 0);

    GSL_SET_COMPLEX(&temp, cos_m_o, sin_m_o);
    den = gsl_complex_add( den, gsl_complex_mul_real(temp,MCE_FILT_B_2_1) );

    GSL_SET_COMPLEX(&temp, cos_m_2o, sin_m_2o);
    den = gsl_complex_add( den, gsl_complex_mul_real(temp,MCE_FILT_B_2_2) );

    /* quotient */

    h2_omega = gsl_complex_div( num, den );


    /* And finally...

      h_omega=h1_omega*h2_omega/2048.
    */

    h_omega = gsl_complex_mul( h1_omega, gsl_complex_div_real(h2_omega,2048.) );


    /* Normally we are applying the inverse of the filter to remove
       its effect from the time-series */
    if( !noinverse ) {
      h_omega = gsl_complex_inverse( h_omega );
    }

    /* Then apply this factor to the filter */
    GSL_SET_COMPLEX( &temp, filt->real[i], filt->imag[i] );
    temp = gsl_complex_mul( temp, h_omega );

    filt->real[i] = GSL_REAL( temp );
    filt->imag[i] = GSL_IMAG( temp );
  }

}
