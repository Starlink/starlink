/*
*+
*  Name:
*     smf_svd

*  Purpose:
*     Multi-threaded singular value decomposition for square matrices

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_svd( ThrWorkForce *wf, dim_t n, double *a, double *sigma,
*              double *u, double eps, int sort, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads.
*     n = dim_t * (Given)
*        The size of the nxn square matrix.
*     a = double * (Given and Returned)
*        The A matrix on entry, and V' (i.e. V transposed) matrix on exit.
*        All the elements of row 1 come first, followed by all the elements
*        of row 2, etc.
*     sigma = double * (Returned)
*        The vector of N singular values. These are *not* sorted into
*        descending order. These are the diagonal elements of the S
*        matrix (see below).
*     u = double * (Returned)
*        The U matrixt. All the elements of row 1 come first, followed by
*        all the elements of row 2, etc. May be NULL if the U matrix is
*        not required.
*     eps = double (Given)
*        The required precision.
*     sort = int (Given)
*        If non-zero, sort the returned arrays so that the singular
*        values are in descending order.
*     status = int * (Given)
*        Pointer to the inherited status value.

*  Description:
*     Finds U and S, given A where:
*
*        A = U . S . V'
*
*     (V' is V transposed). U and V are both orthogonal nxn matrices, and
*     S is an nxn diagonal matrix.
*
*     The algorithm used is multi-threaded, unlike the GSL SVD function.
*     It is the "Block JRS Algorithm", as described in "A Block JRS
*     Algorithm for Highly Parallel Computation of SVDs" (Soliman, et al,
*     DOI: 10.1007/978-3-540-75444-2_36). This function is hand-coded from
*     the pseudo-code given in that paper. See the paper for details of
*     how it works.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     15-SEP_2015 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
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
#include <stdlib.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Prototypes for local static functions. */
static void smf1_svd( void *job_data_ptr, int *status );
static void smf1_roundrobin( dim_t p, int *up, int *dn );
static int smf1_compare( const void *a, const void *b );

static double *Sigma_array = NULL;

/* Local data types */
typedef struct smfSvdData {
   dim_t dnsobhigh;
   dim_t dnsoblow;
   dim_t i1;
   dim_t i2;
   dim_t j1;
   dim_t j2;
   dim_t n;
   dim_t sobhigh;
   dim_t soblow;
   dim_t upsobhigh;
   dim_t upsoblow;
   double *a;
   double *sigma;
   double *u;
   double *aorig;
   double delta;
   int converged;
   int oper;
} SmfSvdData;

#define GAMMA 0


void smf_svd( ThrWorkForce *wf, dim_t n, double *a, double *sigma,
              double *u, double eps, int sort, int *status ) {

/* Local Variables */
   SmfSvdData *job_data = NULL;
   SmfSvdData *pdata = NULL;
   dim_t *sobhigh = NULL;
   dim_t *soblow = NULL;
   dim_t i;
   dim_t j;
   dim_t k;
   dim_t irow;
   dim_t iter;
   dim_t nbig;
   dim_t nsmall;
   dim_t nstep;
   dim_t p;
   dim_t rpb;
   dim_t s;
   double *aorig;
   double sigold;
   double delta;
   int *dn = NULL;
   int *up = NULL;
   int converged;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* The number of threads to use. */
   p = wf ? wf->nworker : 1;

/* If we have more processors than blocks in the matrix, limit the number
   of processors. */
   if( 4*p > n ) p = n/4;
   if( p == 0  ) {
      *status = SAI__ERROR;
      errRepf( "", "smf_svd: Too few rows (%zu) in matrix - must "
               "be no fewer than 4", status, n );
   }

/* Allocate required arrays. */
   sobhigh = astMalloc( 2*p*sizeof( *sobhigh ) );
   soblow = astMalloc( 2*p*sizeof( *soblow ) );
   up = astMalloc( p*sizeof( *up ) );
   dn = astMalloc( p*sizeof( *dn ) );
   job_data = astMalloc( 2*p*sizeof( *job_data ) );
   aorig = u ? astStore( NULL, a, n*n*sizeof(*a) ) : NULL;

/* Check pointers can be used safely. */
   if( *status == SAI__OK ) {

/* Decide on the first and last element to be processed by each thread,
   for "simple" tasks. At the same time, set up jobs to find the sum
   of the squares of all data values (a simple task). */
      nstep = (n*n)/p;
      for( i = 0; i < p; i++ ) {
         pdata = job_data + i;
         pdata->i1 = i*nstep;
         if( i < p - 1 ){
            pdata->i2 = pdata->i1 + nstep - 1;
         } else {
            pdata->i2 = n*n - 1;
         }
         pdata->a = a;
         pdata->oper = 0;
         thrAddJob( wf, 0, pdata, smf1_svd, 0, NULL, status );
      }

      thrWait( wf, status );

      delta = 0.0;
      for( i = 0; i < p; i++ ) {
         pdata = job_data + i;
         delta += pdata->delta;
      }
      delta *= eps;

/* Set up the "size of block" (sob) arrays: soblow holds the zero-based index
   of the first matrix row in each block, and sobhigh holds the zero-based
   index of the last matrix row in each block. We want 2*P blocks (i.e. twice
   the number of threads). Distribute any left over rows evenly amongst the
   blocks so that some blocks have n/2p rows (small blocks), and some have
   n/2p+1 rows (big blocks). */
      rpb = n/(2*p);        /* Nominal number of rows per block */
      nbig = n - 2*p*rpb;   /* Number of big blocks */
      nsmall = 2*p - nbig;  /* Number of small blocks */

/* If we have more small blocks than big blocks, we start with "nbig" pairs
   of blocks in which the first block is big and the second block is small,
   and then pad the end with the surplus number of small blocks. */
      if( nbig < nsmall ) {
         s = 0;
         irow = 0;
         for( i = 0; i < nbig; i++ ) {
            soblow[ s ] = irow;
            irow += rpb;
            sobhigh[ s ] = irow;

            irow++;
            s++;

            soblow[ s ] = irow;
            irow += rpb - 1;
            sobhigh[ s ] = irow;

            irow++;
            s++;
         }

         for( i = s; i < 2*p; i++ ) {
            soblow[ i ] = irow;
            irow += rpb - 1;
            sobhigh[ i ] = irow;
            irow++;
         }

/* If we have more big blocks than small blocks, we start with "nsmall" pairs
   of blocks in which the first block is big and the second block is small,
   and then pad the end with the surplus number of big blocks. */
      } else {
         s = 0;
         irow = 0;
         for( i = 0; i < nsmall; i++ ) {
            soblow[ s ] = irow;
            irow += rpb;
            sobhigh[ s ] = irow;

            irow++;
            s++;

            soblow[ s ] = irow;
            irow += rpb - 1;
            sobhigh[ s ] = irow;

            irow++;
            s++;
         }

         for( i = s; i < 2*p; i++ ) {
            soblow[ i ] = irow;
            irow += rpb;
            sobhigh[ i ] = irow;
            irow++;
         }
      }

/* Sanity check. */
      if( ( irow != n || i != 2*p ) && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( "", "smf_svd: Error setting up the SOB arrays.", status );
         goto L999;
      }

/* Now proceed with the "Block JRS Algorithm" algorithm, as described
   in "A Block JRS Algorithm for Highly Parallel Computation of SVDs"
   (Soliman, et al, DOI: 10.1007/978-3-540-75444-2_36). Note, integer
   counters are one-based in the pseudo-code in the paper, but here
   we use zero-based counters as is normal in C. */

      for( i = 0; i < p; i++ ) {
         up[ i ] = 2*i + 1;
         dn[ i ] = 2*i;
      }

      converged = 0;
      while( !converged ) {
         converged = 1;

         for( s = 0; s < 2*p; s++ ) {
            pdata = job_data + s;
            pdata->oper = 1;
            pdata->soblow = soblow[s];
            pdata->sobhigh = sobhigh[s];
            pdata->delta = delta;
            pdata->a = a;
            pdata->n = n;
            thrAddJob( wf, 0, pdata, smf1_svd, 0, NULL, status );
         }
         thrWait( wf, status );

         for( s = 0; s < 2*p; s++ ) {
            if( !job_data[s].converged ) converged = 0;
         }

         for( iter = 1; iter < 2*p; iter++ ) {

            for( s = 0; s < p; s++ ) {
               pdata = job_data + s;
               pdata->oper = 2;
               pdata->upsoblow = soblow[up[s]];
               pdata->upsobhigh = sobhigh[up[s]];
               pdata->dnsoblow = soblow[dn[s]];
               pdata->dnsobhigh = sobhigh[dn[s]];
               thrAddJob( wf, 0, pdata, smf1_svd, 0, NULL, status );
            }
            thrWait( wf, status );

            for( s = 0; s < 2*p; s++ ) {
               if( !job_data[s].converged ) converged = 0;
            }

            smf1_roundrobin( p, up, dn );
         }
      }

      nstep = n/p;
      for( i = 0; i < p; i++ ) {
         pdata = job_data + i;
         pdata->j1 = i*nstep;
         if( i < p - 1 ){
            pdata->j2 = pdata->j1 + nstep - 1;
         } else {
            pdata->j2 = n - 1;
         }
         pdata->sigma = sigma;
         pdata->oper  = 3;
         thrAddJob( wf, 0, pdata, smf1_svd, 0, NULL, status );
      }

      thrWait( wf, status );

/* If required, sort the singular values into descending order. */
      if( sort ) {
         Sigma_array = sigma;

         double *arowold = astMalloc( n*sizeof( *arowold ) );
         int *index = astMalloc( n*sizeof( *index ) );
         if( *status == SAI__OK ) {

            for( i = 0; i < n; i++ ) index[ i ] = i;
            qsort( index, n, sizeof(*index), smf1_compare );
            for( i = 0; i < n; i++ ) {
               sigold = sigma[ i ];
               memcpy( arowold, a + i*n, n*sizeof(*a) );
               j = i;
               while( 1 ) {
                  k = index[ j ];
                  index[ j ] = j;
                  if( k == i ) break;
                  sigma[ j ] = sigma[ k ];
                  memcpy( a + j*n, a + k*n, n*sizeof(*a) );

                  j = k;
               }
               sigma[ j ] = sigold;
               memcpy( a + j*n, arowold, n*sizeof(*a) );
            }
         }
         index = astFree( index );
         arowold = astFree( arowold );
      }

/* If required, calculate the U matrix. */
      if( u ) {
         for( i = 0; i < p; i++ ) {
            pdata = job_data + i;
            pdata->u = u;
            pdata->aorig = aorig;
            pdata->oper = 4;
            thrAddJob( wf, 0, pdata, smf1_svd, 0, NULL, status );
         }

         thrWait( wf, status );
      }

   }

/* Free resources. */
L999:
   job_data = astFree( job_data );
   up = astFree( up );
   dn = astFree( dn );
   soblow = astFree( soblow );
   sobhigh = astFree( sobhigh );
   aorig = astFree( aorig );

}



static void smf1_svd( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_svd

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_svd.

*  Invocation:
*     smf1_svd( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfSvdData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfSvdData *pdata;
   dim_t bsize2;
   dim_t bsize;
   dim_t i;
   dim_t icol;
   dim_t ind;
   dim_t irow;
   dim_t j;
   dim_t k;
   dim_t n;
   double *c;
   double *pa;
   double *paorig;
   double *pi;
   double *pj;
   double *ps;
   double *pu;
   double *s;
   double dii;
   double dij;
   double djj;
   double sum;
   double sv;
   double t;
   double tau;
   double temp;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfSvdData *) job_data_ptr;

   n = pdata->n;

   if( pdata->oper == 0 ) {
      pa = pdata->a + pdata->i1;
      pdata->delta = 0.0;
      for( i = pdata->i1; i <= pdata->i2; i++,pa++ ) {
         pdata->delta += (*pa)*(*pa);
      }

   } else if( pdata->oper == 1 ) {
      bsize = pdata->sobhigh - pdata->soblow + 1;
      c = astMalloc( sizeof(*c)*bsize*(bsize-1)/2 );
      s = astMalloc( sizeof(*s)*bsize*(bsize-1)/2 );
      if( *status == SAI__OK ) {
         pdata->converged = 1;

         ind = 0;
         for( i = pdata->soblow; i < pdata->sobhigh; i++ ) {
            for( j = i + 1; j <= pdata->sobhigh; j++ ) {
               pi = pdata->a + i*n;
               pj = pdata->a + j*n;
               dii = 0.0;
               djj = 0.0;
               dij = 0.0;
               for( k = 0; k < n; k++,pi++,pj++ ) {
                  dii += *(pi)*(*pi);
                  djj += *(pj)*(*pj);
                  dij += *(pi)*(*pj);
               }

               if( fabs( dij) > pdata->delta ) pdata->converged = 0;
               if( dij != 0.0 ) {
                  tau = ( djj - dii ) /(2*dij);
                  t = (1.0 - GAMMA)/( fabs(tau) + sqrt(tau*tau+1.0-GAMMA*GAMMA) );
                  if( tau < 0.0 ) t = -t;
                  c[ ind ] = 1.0/sqrt(t*t+1.0);
                  s[ ind ] = t*c[ ind ];
               } else {
                  c[ ind ] = 1.0;
                  s[ ind ] = 0.0;
               }
               ind++;
            }
         }

         ind = 0;
         for( i = pdata->soblow; i < pdata->sobhigh; i++ ) {
            for( j = i + 1; j <= pdata->sobhigh; j++ ) {
               pi = pdata->a + i*n;
               pj = pdata->a + j*n;
               for( k = 0; k < n; k++,pi++,pj++ ) {
                  temp = c[ind]*(*pi) - s[ind]*(*pj);
                  *pj = s[ind]*(*pi) + c[ind]*(*pj);
                  *pi = temp;
               }
               ind++;
            }
         }
      }
      c = astFree( c );
      s = astFree( s );

   } else if( pdata->oper == 2 ) {
      bsize = pdata->upsobhigh - pdata->upsoblow + 1;
      bsize2 = pdata->dnsobhigh - pdata->dnsoblow + 1;
      c = astMalloc( sizeof(*c)*bsize*bsize2 );
      s = astMalloc( sizeof(*s)*bsize*bsize2 );
      if( *status == SAI__OK ) {
         pdata->converged = 1;

         ind = 0;
         for( i = pdata->upsoblow; i <= pdata->upsobhigh; i++ ) {
            for( j = pdata->dnsoblow; j <= pdata->dnsobhigh; j++ ) {
               pi = pdata->a + i*n;
               pj = pdata->a + j*n;
               dii = 0.0;
               djj = 0.0;
               dij = 0.0;
               for( k = 0; k < n; k++,pi++,pj++ ) {
                  dii += *(pi)*(*pi);
                  djj += *(pj)*(*pj);
                  dij += *(pi)*(*pj);
               }

               if( fabs( dij) > pdata->delta ) pdata->converged = 0;
               if( dij != 0.0 ) {
                  tau = ( djj - dii ) /(2*dij);
                  t = (1.0 - GAMMA)/( fabs(tau) + sqrt(tau*tau+1.0-GAMMA*GAMMA) );
                  if( tau < 0.0 ) t = -t;
                  c[ ind ] = 1.0/sqrt(t*t+1.0);
                  s[ ind ] = t*c[ ind ];
               } else {
                  c[ ind ] = 1.0;
                  s[ ind ] = 0.0;
               }
               ind++;
            }
         }

         ind = 0;
         for( i = pdata->upsoblow; i <= pdata->upsobhigh; i++ ) {
            for( j = pdata->dnsoblow; j <= pdata->dnsobhigh; j++ ) {
               pi = pdata->a + i*n;
               pj = pdata->a + j*n;
               for( k = 0; k < n; k++,pi++,pj++ ) {
                  temp = c[ind]*(*pi) - s[ind]*(*pj);
                  *pj = s[ind]*(*pi) + c[ind]*(*pj);
                  *pi = temp;
               }
               ind++;
            }
         }
      }
      c = astFree( c );
      s = astFree( s );

   } else if( pdata->oper == 3 ) {
      pa = pdata->a + n*pdata->j1;
      for( i = pdata->j1; i <= pdata->j2; i++ ) {
         sv = 0.0;
         for( k = 0; k < n; k++,pa++ ) {
            sv += (*pa)*(*pa);
         }
         sv = ( sv > 0.0 ) ? sqrt( sv ) : 0.0;
         if(pdata->sigma) (pdata->sigma)[ i ] = sv;

         pa -= n;
         if( sv > 0.0 ){
            for( k = 0; k < n; k++,pa++ ) *pa /= sv;
         } else {
            for( k = 0; k < n; k++,pa++ ) *pa = 0.0;
         }
      }

   } else if( pdata->oper == 4 ) {
      pu = pdata->u + pdata->i1;

      for( i = pdata->i1; i <= pdata->i2; i++ ) {
         irow = i/n;
         icol = i - n*irow;

         paorig = pdata->aorig + irow*n;
         pa = pdata->a + icol*n;
         ps = pdata->sigma;

         sum = 0.0;
         for( k = 0; k < n; k++ ) {
            if( pdata->sigma[icol] != 0.0 ) {
               sum += (*(paorig++))*(*(pa++))/pdata->sigma[icol];
            } else {
               paorig++;
               pa++;
               ps++;
            }
         }

         *(pu++) = sum;
      }

   } else if( *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( "", "smf1_svd: Invalid operation (%d) supplied.",
               status, pdata->oper );
   }
}

static void smf1_roundrobin( dim_t p, int *up, int *dn ){
   double saved;
   int i;
   int pp = (int) p;

   saved = up[ 0 ];

   for( i = 0; i < pp - 2; i++ ) {
      up[ i ] = up[ i + 1];
   }
   up[ i ] = dn[ pp - 1 ];

   for( i = pp - 1; i > 0; i-- ) {
      dn[ i ] = dn[ i - 1 ];
   }

   dn[ 0 ] = saved;

}

static int smf1_compare( const void *a, const void *b ){
   double aval = Sigma_array[ *( (const int *) a ) ];
   double bval = Sigma_array[ *( (const int *) b ) ];
   if( aval < bval ){
      return 1;
   } else if( aval > bval ){
      return -1;
   } else {
      return 0;
   }
}


