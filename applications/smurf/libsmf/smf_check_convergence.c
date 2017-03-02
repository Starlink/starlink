/*
*+
*  Name:
*     smf_check_convergence

*  Purpose:
*     Estimate how long it will take to reach convergence.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_check_convergence( smfDIMMData *dat, int maxiter, double maptol,
*                            int *abortedat, int *status );

*  Arguments:
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     maxiter = int (Given)
*        The maximum number of iterations being performed by makemap.
*     maptol = double (Given)
*        The normalized map change required for convergence.
*     abortedat = int * (Returned)
*        On exit, *abortedat will be non-zero if it looks likely that it
*        will take more than "maxiter" iterations to converge. The
*        specific non-zero value assigned to *abortedat is the current
*        iteration number.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Checks to see if it is likely that convergence will not be achieved
*     before the maximum number of iterations has been performed. If this
*     is the case, a wanring is issued and "*abortedat" is set to the current
*     iteration number. Otherwise, "*abortedat" is set to zero.
*
*     The prediction of how many iterations are required is based on the
*     observations that the normalised map change between iterations is
*     usually a roughly exponential function of iteration number, of the
*     form
*
*     map change = A*(iteration)^B
*
*     where A is positive and B is negative. This is equivalent to a linear
*     relationship between log(map change) and log(iteration). We record the
*     most recent NFIT log(map change) values in a static array, and do a
*     robust linear fit to determine the above constants A and B, and then
*     use this fit to predict the iteration number at which the mapchange
*     falls to "maptol".

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     2-MAR-2017 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
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

#include <gsl/gsl_multifit.h>

/* Macros */
#define NFIT 8

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

void smf_check_convergence( smfDIMMData *dat, int maxiter, double maptol, int *abortedat,
                            int *status ){

/* Local Variables: */
   double logit;
   gsl_matrix *cov;
   gsl_matrix *X;
   gsl_multifit_robust_workspace *work;
   gsl_vector *x;
   gsl_vector *c;
   gsl_vector *y;
   int i;
   int it;
   int j;
   int nexp;

   static double mapchanges[NFIT];
   static int imapchange = 0;
   static int nhi = 0;
   static int nmapchange = 0;

/* Initialise */
   *abortedat = 0;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Do some initialisation of static variables if this is the first
   iteration. */
   if( dat->mapchange == VAL__MAXD ) {
      nmapchange = 0;
      imapchange = 0;
      nhi = 0;
   }

/* Do not make any estimate until we have got past any initial iterations
   where the AST model was not used. */
   if( dat->ast_skipped ) return;

/* We maintain an array ("mapchanges") holding the log(mapchange) value for
   the NFIT most recent iterations. Values are entered into this array in
   a cyclic order, with "imapchange" holding the index of the oldest value
   in the array. Also, "nmapchange" holds the number of values currently
   stored in the array. It starts at 0, goes up to NFIT, and then stays
   at NFIT. Store the log(mapchange) value for the current iteration,
   over-writing the oldest value currently in the array. Increment the
   imapchange value to indicate that the oldest value is now the next one
   in the array. Wrap round to the start of the array when the end is
   reached.   */
   mapchanges[ imapchange++ ] = log( dat->mapchange );
   if( imapchange == NFIT ) imapchange = 0;

/* If the number of values previously in the array was less than NFIT,
   we now have one more, so increment the number of values in the array. */
   if( nmapchange < NFIT ) nmapchange++;

/* Do not produce an estimate if the mapchange array is not full. */
   if( nmapchange == NFIT ) {

/* Fit a line to the log(map change) values, using log(iteration) as the X
   axis, and use to to estimate the iteration number at convergence. First,
   allocate structures needed by the GSL robust linear regression function. */
      X = gsl_matrix_alloc( NFIT, 2);
      x = gsl_vector_alloc( NFIT );
      y = gsl_vector_alloc( NFIT );
      c = gsl_vector_alloc( 2 );
      cov = gsl_matrix_alloc( 2, 2 );

/* Store the X and Y values in the above arrays. */
      it = dat->iter - NFIT + 1;
      i = imapchange;
      for( j = 0; j < NFIT; j++ ) {
         logit = log( it );
         gsl_vector_set( x, i, logit );
         gsl_vector_set( y, i, mapchanges[i] );

/* Construct design matrix X for linear fit */
         gsl_matrix_set( X, i, 0, 1.0);
         gsl_matrix_set( X, i, 1, logit );

         it++;
         if( ++i == NFIT ) i = 0;
      }

/* Perform robust fit */
      gsl_set_error_handler_off();
      work = gsl_multifit_robust_alloc( gsl_multifit_robust_bisquare, X->size1, X->size2 );
      (void) gsl_multifit_robust( X, y, c, cov, work );
      gsl_multifit_robust_free( work );

/* Get estimated iteration number at target map change. */
      nexp = (int)( 0.5 + exp( ( log( maptol ) - gsl_vector_get(c,0) )/gsl_vector_get(c,1)));

/* Free resources */
      gsl_matrix_free( X );
      gsl_vector_free( x );
      gsl_vector_free( y );
      gsl_vector_free( c );
      gsl_matrix_free( cov );

/* Issue a debug message indicated the expected iteration number at
   convergence. */
      msgOutiff( MSG__DEBUG, "", "*** Convergence expected at iteration %d.",
                 status, nexp );

/* If the expected number of iterations rises above the maximum allowed
   value for three succesive iterations, issue a warning and return *abortat
   non-zero. */
      if( nexp > maxiter ) {
         if( ++nhi == 3 ) {
            *abortedat = dat->iter;

/* Tell the user what has happened. */
            msgBlank( status );
            msgOutf( "", "*********************************", status );
            msgOutf( "", "WARNING: Convergence is too slow!", status );
            msgOutf( "", "It looks like convergence will not occur "
                     "until iteration %d.", status, nexp );
            msgOutf( "", "So I am aborting now.", status );
            msgOutf( "", "*********************************", status );
            msgBlank( status );

         }
      } else {
         nhi = 0;
      }
   }
}




