/*
*+
*  Name:
*     smf_pca

*  Purpose:
*     Find and remove the most correlated PCA components within a smfData.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     dim_t smf_pca( ThrWorkForce *wf, smfData *data, smfData *lut,
*                     unsigned char *mask, double corlim, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     data = smfData * (Given and Returned)
*        Pointer to the smfData to be modified.
*     lut = smfData * (Given)
*        The smfData holding the pointing information for each sample. No
*        masking is performed if this is NULL.
*     mask = unsigned char * (Given)
*        Pointer to the mask array - zero for pixels designated as
*        "source" pixels and non-zero for "background" pixels. No
*        masking is performed if this is NULL.
*     corlim = double (Given)
*        Specifies how many principal components to remove from the data.
*        If the supplied value is positive, it should be the upper limit
*        on the correlation of acceptable principal components. Any principal
*        components that are correlated with the data to a level greater than
*        this value are removed from the data. The supplied value must be in
*        the range 0.0 to 1.0. Smaller values remove more of the data. If the
*        supplied "corlim" value is negative, it should be the number of
*        components to remove (the nearest integer value is used).
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     The number of principal components that were removed.

*  Description:
*     The Principal Component Analysis (PCA) model represents a set of
*     strong components within the time streams, each of which is
*     correlated with the data from bolometer to bolometer.
*
*     The algorithm used by this function is described further in file
*     smf_pca.tex. It finds the principal components by diagonalising the
*     correlation matrix created from the supplied data (i.e. a matrix
*     holding the correlation of each bolometer time -stream with each other
*     bolometer time-stream). Note this is different to smf_clean_pca,
*     which diagonalises the co-variance matrix instead of the
*     correlation matrix. Using correlation rather than co-variance
*     should result in the analysis being less dependent on the brightness
*     of the astronomical source.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     31-JAN-2017 (DSB):
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#define _GNU_SOURCE   // gives us feenableexcept on older gcc's
#define __USE_GNU     // gives us feenableexcept on newer gcc's
#include <fenv.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Prototypes for local static functions. */
static void smf1_pca( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfPCAData {
   dim_t b1;
   dim_t b2;
   dim_t icomp;
   dim_t nb;
   dim_t nbolo;
   dim_t ntslice;
   dim_t p1;
   dim_t p2;
   dim_t t1;
   dim_t t2;
   double *c;
   double *cnew;
   double *data;
   double *dp;
   double *means;
   double *pcomp;
   double *sigmas;
   double *v;
   dim_t *badbol;
   int *lut;
   int oper;
   dim_t bstride;
   dim_t t_first;
   dim_t t_last;
   dim_t tstride;
   smf_qual_t *qua;
   unsigned char *mask;
} SmfPCAData;


dim_t smf_pca( ThrWorkForce *wf, smfData *data, smfData *lut, unsigned char *mask,
                double corlim, int *status ){

/* Local Variables: */
   SmfPCAData *job_data = NULL;
   SmfPCAData *pdata;
   dim_t *badbol;
   dim_t bstep;
   dim_t bstride;
   dim_t i;
   dim_t ireject;
   dim_t j;
   dim_t nb;
   dim_t nbad;
   dim_t nbolo;
   dim_t ncheck;
   dim_t ndata;
   dim_t nk = 0;
   dim_t npair;
   dim_t ntslice;
   dim_t ntused;
   dim_t pstep;
   dim_t t_first;
   dim_t t_last;
   dim_t tstep;
   dim_t tstride;
   double *c;
   double *cnew;
   double *dp = NULL;
   double *ev;
   double *means;
   double *old_data;
   double *pcomp;
   double *sigmas;
   double evlim;
   int iw;
   int nw;
   smf_qual_t *qua;

/* Check inherited status. */
   if( *status != SAI__OK ) return nk;

/* Uncomment to trap NaNs etc */
/* feenableexcept(FE_DIVBYZERO| FE_INVALID|FE_OVERFLOW); */

/* Get pointer to quality array. */
   qua = smf_select_qualpntr( data, 0, status );

/* Ensure the LUT array has the same ordering as the main data array. */
   if( lut ) smf_dataOrder( wf, lut, data->isTordered, status );

/* Get the strides and lengths of the axes in the smfData. */
   smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, &ndata, &bstride, &tstride,
                 status );

/* Get the first and last time slice to use (this excludes padding and
   apodised end sections). */
   if( qua ) {
      smf_get_goodrange( qua, ntslice, tstride, (SMF__Q_PAD | SMF__Q_APOD),
                         &t_first, &t_last, status );
      ntused = t_last - t_first + 1;
   } else {
      t_first = 0;
      t_last = ntslice-1;
      ntused = ntslice;
   }

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Find how many bolometers or time-slices to process in each worker
   thread. */
   bstep = nbolo/nw;
   if( bstep == 0 ) bstep = 1;
   tstep = ntused/nw;
   if( tstep == 0 ) tstep = 1;

/* Allocate job data for threads. */
   job_data = astMalloc( nw*sizeof(*job_data) );

/* Store the range of bolos and time slices to be processed by each one.
   Ensure that the last thread picks up any left-over bolos. */
   if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->b1 = iw*bstep;
         pdata->t1 = iw*tstep + t_first;
         if( iw < nw - 1 ) {
            pdata->b2 = pdata->b1 + bstep - 1;
            pdata->t2 = pdata->t1 + tstep - 1;
         } else {
            pdata->b2 = nbolo - 1;
            pdata->t2 = t_last;
         }

/* Store other values common to all jobs. */
         pdata->ntslice = ntslice;
         pdata->nbolo = nbolo;
         pdata->bstride = bstride;
         pdata->tstride = tstride;
         pdata->data = data->pntr[0];
         pdata->lut = lut ? lut->pntr[0] : NULL;
         pdata->qua = qua;
         pdata->mask = mask;
         pdata->t_first = t_first;
         pdata->t_last = t_last;
      }
   }

/* Find the mean and standard deviation of the good, un-masked, data in
   each bolometer. */
   msgOutif( MSG__VERB, "", "smf_pca: calculating mean and standard "
             "deviation in each bolo...", status );

   means = astMalloc( nbolo*sizeof(*means) );
   sigmas = astMalloc( nbolo*sizeof(*sigmas) );
   if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->means = means;
         pdata->sigmas = sigmas;
         pdata->oper = 0;
         thrAddJob( wf, 0, pdata, smf1_pca, 0, NULL, status );
      }
      thrWait( wf, status );
   }

/* Get the number of unique bolometer pairs, and the number of pairs to
   process in each thread. */
   npair = nbolo*( nbolo + 1 )/2;
   pstep = npair/nw;
   if( pstep == 0 ) pstep = 1;

/* Submit jobs to evaluate the correlation between each pair of bolometers
   and store them in the correlation matrix "c". */
   msgOutif( MSG__VERB, "", "smf_pca: calculating correlation matrix...",
             status );
   c = astMalloc( nbolo*nbolo*sizeof( *c ) );
   if( *status == SAI__OK ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;

         pdata->p1 = iw*pstep;
         if( iw < nw - 1 ) {
            pdata->p2 = pdata->p1 + pstep - 1;
         } else {
            pdata->p2 = npair - 1 ;
         }

         pdata->c = c;
         pdata->oper = 1;
         thrAddJob( wf, 0, pdata, smf1_pca, 0, NULL, status );
      }
      thrWait( wf, status );
   }

/* Now choose which bolometers to use, so that there are no bad values in
   the correlation matrix. */
   nb = 0;
   badbol = astCalloc( nbolo, sizeof(*badbol) );
   if( *status == SAI__OK ) {
      msgOutif( MSG__VERB, "", "smf_pca: removing unusable bolos...",
                status );

/* Values of -1 in the badbol array indicate rejected bolometers. We can
   start out by rejecting any that have bad sigma values. */
      for( i = 0; i < nbolo; i++ ) {
         if(  sigmas[ i ] <= 0.0 ) {
            badbol[ i ] = -1;
         } else {
            badbol[ i ] = 0;
            nb++;
         }
      }

/* Count the number of bad correlation values for each bolometer, excluding the
   bolometers rejected above. In all probability, there will be no bad values
   left after rejecting the above bolos. But it's not guaranteed, so we need
   to do the check. */
      nbad = 0;
      for( i = 0; i < nbolo; i++ ) {
         for( j = 0; j < nbolo; j++ ) {
            if( badbol[ i ] != -1 && badbol[ j ] != -1 ) {
               if( c[ j + i*nbolo ] == VAL__BADD ) {
                  badbol[ i ]++;
                  badbol[ j ]++;
                  nbad++;
               }
            }
         }
      }

/* Loop until all bad bolometers have been rejected and the remaining
   elements of the correlation matrix thus contains no bad values. */
      while( nbad > 0 ) {

/* Find the bolometer that contributes the largest number of bad values
   to the correlation matrix. */
         nbad = 0;
         ireject = 0;
         for( i = 0; i < nbolo; i++ ) {
            if( badbol[ i ] > nbad ) {
               nbad = badbol[ i ];
               ireject = i;
            }
         }

/* If any bad values were found, flag the worst bolometer as rejected. */
         if( nbad > 0 ) {
            badbol[ ireject ] = -1;
            nb--;

/* Re-evaluate the "badbol" array, ignoring rejected bolometers. */
            for( i = 0; i < nbolo; i++ ) {
               if( badbol[ i ] != -1 ) badbol[ i ] = 0;
            }

            nbad = 0;
            for( i = 0; i < nbolo; i++ ) {
               for( j = 0; j < nbolo; j++ ) {
                  if( badbol[ i ] != -1 && badbol[ j ] != -1 ) {
                     if( c[ j + i*nbolo ] == VAL__BADD ) {
                        badbol[ i ]++;
                        badbol[ j ]++;
                        nbad++;
                     }
                  }
               }
            }
         }
      }

/* Sanity check - the badbol array should now contain "nb" zeros,
   and the rest should all be -1. At the same time, change the zeros into
   the zero-based index of each good bolometer within the list of all
   "nb" good bolometers. This means that badbol now acts as a
   look-up-table that gives the new reduced bolometer index as a function
   of the full bolometer index, and stores -1 for rejected bolometers. */
      ncheck = 0;
      for( i = 0; i < nbolo; i++ ) {
         if( badbol[ i ] == 0 ) {
            badbol[ i ] = ncheck++;
         } else if( badbol[ i ] != -1 ) {
            *status = SAI__ERROR;
            errRepf( "", "smf_pca: unexpected badbol value %d for bolo %zu "
                     "(programming error).", status, (int) badbol[ i ], i );
            break;
         }
      }
      if( ncheck != nb && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( "", "smf_pca: unexpected number of good bolos %zu - expected"
                  " %zu (programming error).", status, ncheck, nb );
      }

      msgOutiff( MSG__VERB, "", "smf_pca: %zu usable bolos left.", status,
                 nb );

/* Replace the full (nbolo,nbolo) correlation matrix with a smaller
   (nb,nb) matrix that includes only the good bolometers. */
      cnew = astMalloc( nb*nb*sizeof(*cnew) );
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->cnew = cnew;
         pdata->badbol = badbol;
         pdata->nb = nb;
         pdata->oper = 2;
         thrAddJob( wf, 0, pdata, smf1_pca, 0, NULL, status );
      }
      thrWait( wf, status );

      (void) astFree( c );
      c = cnew;
   }

/* Now use Singular Value Decomposition to find the eigenvalues and
   eigenvectors of the correlation matrix: c = v.s.v^T. On exit, the
   eigenvectors (v) are stored in the rows of array "c", replacing the
   correlation matrix. Each returned eigenvector has a length of  1.0.
   The eigenvalues are stored in the "ev" array. The array is ordered
   to so that the eigenvalues are monotonic decreasing, and the
   eigenvectors are order in the same way so that c = v.s.v^T. */
   msgOutif( MSG__VERB, "", "smf_pca: perfoming singular value "
             "decomposition...", status );
   ev = astMalloc( nb*sizeof(*ev) );
   smf_svd( wf, nb, c, ev, NULL, 10*VAL__EPSD, 1, status );

/* For some of the following operations, we must be sure there are no bad
   or flagged values in the data array, so allocate memory and store a
   copy of the supplied data array array. We will remove bad or flagged
   values from this copy. */
   msgOutif( MSG__VERB, "", "smf_pca:filling gaps in data...", status );
   dp = astStore( NULL, data->pntr[0], ndata*sizeof(*dp) );

/* Set bolometer data values bad if they fall within the masked out
   region of the map (i.e. astronomical sources). */
   if( mask && lut ) {
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->dp = dp;
         pdata->oper = 3;
         thrAddJob( wf, 0, pdata, smf1_pca, 0, NULL, status );
      }
      thrWait( wf, status );
   }

/* Temporarily hijack the supplied smfData to hold the array created
   above. */
   old_data = data->pntr[0];
   data->pntr[0] = dp;

/* Fill gaps (either flagged in quality or set bad in data) in the above
   array. */
   smf_fillgaps( wf, data, SMF__Q_GAP, status );

/* Re-instate the original data array in the smfData. */
   data->pntr[0] = old_data;

/* The eigenvectors define the directions of the principal axes. The
   values in each eigenvector are the relative bolometer values
   corresponding to a single correlated component. The first eigenvector
   (corresponding to the largest eigenvalue) points in the direction
   of maximum correlation between the bolometers. The rest point in the
   directions of decreasingly strong correlations. We remove these
   components of the data down to eigenvectors that have the specified
   correlation limit. The eigenvalues are related to the strength of the
   correlation. Empirically, it seems  that for a principal component
   with eigenvalue "ev", the RMS of the correlation coefficients between
   the principal component and all of the usable bolometers, C, is given
   by:

   ev = nb * C^2

   So given a user supplied correlation limit C, we find the corresponding
   "ev" value and then remove components in order of "ev" value until a
   component below the "ev" limit is encountered. */
   evlim = nb * corlim * corlim;

/* Find how many components to remove. */
   if( corlim >= 0.0 ) {
      for( nk = 0; nk < nb; nk++ ) {
         if( ev[ nk ] <= evlim ) break;
      }
   } else {
      nk =  (dim_t)( 0.5 - corlim );
   }

/* Remove them. First form a matrix, M, holding the dot-products of each pair
   of eigenvectors. Then remove them. */
   msgOutiff( MSG__VERB, "", "smf_pca: removing %zu principal "
              "components", status, nk );

/* Allocate an array to hold the "current" principal component during the
   following loop (a time-stream). This is a projection of the nbolo*ntslice
   data values onto the current principal axis. */
   pcomp = astMalloc( ntslice*sizeof(*pcomp) );
   if( *status == SAI__OK ) {

/* Loop round removing each required principal components. */
      for( i = 0; i < nk; i++ ) {

/* Find the i'th principal component. This is a time-stream that is
   statistically independent of the other principal components. It is
   formed as a linear combination of the original usable gap-filled
   time-streams, where the coefficients of the combination are specified
   by the values in the corresponding principal axis (eigenvector) found
   above. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->dp = dp;
            pdata->c = c + i*nb;
            pdata->pcomp = pcomp;
            pdata->oper = 4;
            thrAddJob( wf, 0, pdata, smf1_pca, 0, NULL, status );
         }
         thrWait( wf, status );

/* Remove the current principal component from the bolometer data. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            pdata->oper = 5;
            thrAddJob( wf, 0, pdata, smf1_pca, 0, NULL, status );
         }
         thrWait( wf, status );

         msgOutiff( MSG__DEBUG, "", "smf_pca: removed "
                    "component %zu (correlation %g)...",
                    status, i, sqrt(ev[i]/nb) );
      }
   }

/* Free resources. */
   ev = astFree( ev );
   dp = astFree( dp );
   badbol = astFree( badbol );
   pcomp = astFree( pcomp );
   c = astFree( c );
   means = astFree( means );
   sigmas = astFree( sigmas );
   job_data = astFree( job_data );

/* Return the number of components that were rejected. */
   return nk;
}

static void smf1_pca( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_pca

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_pca.

*  Invocation:
*     smf1_pca( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfPCAData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfPCAData *pdata;
   dim_t b1;
   dim_t b2;
   dim_t b;
   dim_t bstride;
   dim_t i;
   dim_t ibolo;
   dim_t inc;
   dim_t ipair;
   dim_t itime;
   dim_t j;
   dim_t nb;
   dim_t nbolo;
   dim_t ntslice;
   dim_t ntused;
   dim_t t1;
   dim_t t2;
   dim_t t_first;
   dim_t t_last;
   dim_t tstride;
   double *pc;
   double *pdi;
   double *pdj;
   double *pm;
   double *ps;
   double cval;
   double di;
   double dj;
   double mean;
   double meani;
   double meanj;
   double s1;
   double s2;
   double s3;
   double sigmai;
   double sigmaj;
   double var;
   int *pli;
   int *plj;
   int mask;
   smf_qual_t *pqi;
   smf_qual_t *pqj;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* feenableexcept(FE_DIVBYZERO| FE_INVALID|FE_OVERFLOW); */

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfPCAData *) job_data_ptr;

/* Save some commonly used values in local variables. */
   ntslice = pdata->ntslice;
   nbolo = pdata->nbolo;
   bstride = pdata->bstride;
   tstride = pdata->tstride;
   b1 = pdata->b1;
   b2 = pdata->b2;
   t1 = pdata->t1;
   t2 = pdata->t2;
   nb = pdata->nb;
   t_first = pdata->t_first;
   t_last = pdata->t_last;
   ntused = t_last - t_first + 1;

/* Mask the source pixels? */
   mask = pdata->mask && pdata->lut;

/* Evaluate the mean and standard deviation of each bolometer.
   =========================================================== */
   if( pdata->oper == 0 ) {

/* Loop round all bolos to be processed by this thread. */
      for( ibolo = b1; ibolo <= b2; ibolo++ ) {

/* Pointers to the first element of the arrays for the bolometer. */
         pdi = pdata->data + ibolo*bstride;
         pli = mask ? pdata->lut + ibolo*bstride : NULL;
         pqi = pdata->qua + ibolo*bstride;

/* Initialise running sums. */
         s1 = s2 = s3 = 0.0;

/* Check that the bolometer has not been flagged as bad. */
         if( !( *pqi & SMF__Q_BADB ) ) {

/* Loop round all time slices. */
            pdi += t_first*tstride;
            pli += t_first*tstride;
            pqi += t_first*tstride;
            for( itime = t_first; itime <= t_last; itime++ ) {

/* Skip flagged bolometer samples. */
               if( !(*pqi & SMF__Q_GOOD ) && *pdi != VAL__BADD ){

/* Skip bolometer samples that fall within source pixels if a mask is being used. */
                  if( !mask || ( (*pli) != VAL__BADI && pdata->mask[*pli] ) ){

/* Update sums needed to form the mean and standard deviation of the bolometer. */
                     s1 += (*pdi);
                     s2 += (*pdi)*(*pdi);
                     s3 += 1.0;
                  }
               }

/* Move pointers on to the next time slice. */
               pdi += tstride;
               if( mask ) pli += tstride;
               pqi += tstride;
            }
         }

/* Store the mean and standard deviation. Only accept bolos that have at
   least 33% good values. */

         if( s3 > ntused/3 ) {
            mean = s1/s3;
            pdata->means[ ibolo ] = mean;

            var = s2/s3 - mean*mean;
            if( var <= 0.0 ) {
               pdata->sigmas[ ibolo ] = 0.0;
            } else {
               pdata->sigmas[ ibolo ] = sqrt( var );
            }
         } else {
            pdata->means[ ibolo ] = VAL__BADD;
            pdata->sigmas[ ibolo ] = 0.0;
         }
      }

/* Evaluate the full correlation matrix.  Each bolometer has its mean
   subtracted and is divided by its standard deviation.
   =================================================================== */
   } else if( pdata->oper == 1 ) {

/* Decode the first pair index to find the row (i) and column (j) within
   the correlation matrix of the two bolometers forming the first pair
   to be processed by this thread. */
      i = 0;
      inc = nbolo;
      while( pdata->p1 >= inc ){
         i++;
         inc += nbolo - i;
      }
      j = pdata->p1 - inc + nbolo;

/* Loop round all pairs of bolos to be processed by this thread. These
   are processed in raster fashion through the bottom right half of the
   correlation matrix (high column numbers, low row numbers). The top ltef
   half is formed by reflecting the bottom right values. This means that
   for each succesive row processed, one fewer columns are used. The "pair
   index" follows this pattern. E.g. in a 4x4 matrix, the first four pairs
   (indices 0-3) correspond to the whole of the bottom row, the next three
   pairs (indices 4-6) correspond to the last three elements of the second
   row, the next two pairs (indices 7-8) correspond to the last two elements
   of the third row, and the remaining pair (index 9) corresponds to the
   last element of the fourth row. As we loop through these pair indices, we
   maintain the corresponding Cartesian (row,column) indices in variables (i,j). */
      for( ipair = pdata->p1; ipair <= pdata->p2; ipair++ ) {

/* Pointers to the first element of the arrays for the two bolometers. */
         pdi = pdata->data + i*bstride + t_first*tstride;
         pli = mask ? pdata->lut + i*bstride + t_first*tstride : NULL;
         pqi = pdata->qua + i*bstride + t_first*tstride;

         pdj = pdata->data + j*bstride + t_first*tstride;
         plj = mask ? pdata->lut + j*bstride + t_first*tstride : NULL;
         pqj = pdata->qua + j*bstride + t_first*tstride;

/* Initialise running sums. */
         s1 = s2 = 0.0;

/* Get the pre-calculated mean and sigma of the data in each of the two
   bolometers. */
         sigmai = pdata->sigmas[ i ];
         sigmaj = pdata->sigmas[ j ];
         meani = pdata->means[ i ];
         meanj = pdata->means[ j ];

/* Check that both bolometers are usable. */
         if( sigmai > 0.0 && sigmaj > 0.0 ) {

/* Loop round all time slices. */
            for( itime = t_first; itime <= t_last; itime++ ) {

/* Skip flagged bolometer samples. */
               if( !(*pqi & SMF__Q_GOOD ) && *pdi != VAL__BADD &&
                   !(*pqj & SMF__Q_GOOD ) && *pdj != VAL__BADD ) {

/* If a mask is being used, skip this pair if either bolometer sample
   falls within a source pixel. */
                  if( !mask || ( (*pli) != VAL__BADI && pdata->mask[*pli] &&
                                 (*plj) != VAL__BADI && pdata->mask[*plj] ) ){

/* Update sums needed to form the correlation of the two bolometers. Transform
   each bolometer to standard form (mean 0.0, standard deviation 1.0) before
   using them. */
                     di = ( (*pdi) - meani )/sigmai;
                     dj = ( (*pdj) - meanj )/sigmaj;
                     s1 += di * dj;
                     s2 += 1.0;
                  }
               }

/* Move pointers on to the next time slice. */
               pdi += tstride;
               if( mask ) pli += tstride;
               pqi += tstride;

               pdj += tstride;
               if( mask ) plj += tstride;
               pqj += tstride;
            }
         }

/* Calculate the correlation and store in the returned array. Store the
   same value in the diagonally opposite element, since the array is
   symmetric. Ignore pairs which have less than 30% good data. */
         if( s2 > 0.3*ntslice ) {
            cval = s1/( s2 - 1.0 );
         } else {
            cval = VAL__BADD;
         }

         pdata->c[ j + i*nbolo ] = pdata->c[ i + j*nbolo ] = cval;

/* Get the indices of the two bolometers that form the next pair.
   Increment the column index. If it goes beyond the maximum column
   index (i.e. nbolo-1 ), then increment the row number and reset the
   column number to the index of the first column to be processed on the
   next row (i.e. the diagonal element). Remember to decrement the
   maximum column index for the next row since the diagonal will be one
   element closer to the start of the new row. */
         j++;
         if( j >= nbolo ) {
            i++;
            j = i;
         }
      }


/* Reduce the size of the correlation matrix by omitting bad bolos.
   ============================================================== */
   } else if( pdata->oper == 2 ) {

/* Loop round all bolos to be processed by this thread. */
      for( ibolo = b1; ibolo <= b2 && *status == SAI__OK; ibolo++ ) {

/* Skip this bolo if it has been rejected. */
         b = pdata->badbol[ ibolo ];
         if( b >= 0 ) {

/* Copy the row corresponding to the current bolometer, copying
   only the good bolometer values. */
            pdi = pdata->c + ibolo*nbolo;
            pdj = pdata->cnew + b*nb;
            for( j = 0; j < nbolo; j++ ) {
               if( pdata->badbol[ j ] >= 0 ) {
                  *(pdj++) = *pdi;
                  if( *pdi == VAL__BADD ) {
                     *status = SAI__ERROR;
                     errRep("","smf_svd: Unexpected bad value in reduced "
                            "correlation matrix (programming error).", status );
                     break;
                  }
               }
               pdi++;
            }

/* Flag rejected bolometers using SMF__Q_PCA if they were not previously
   rejected. */
         } else {
            pqi = pdata->qua + ibolo*bstride;
            if( !( *pqi & SMF__Q_BADB ) ) {
               for( itime = 0; itime < ntslice; itime++ ) {
                  *pqi |= ( SMF__Q_BADB | SMF__Q_PCA );
                  pqi += tstride;
               }
            }
         }
      }

/* Mask the time-series data to remove astronomical sources.
   ======================================================= */
   } else if( pdata->oper == 3 ) {

/* Loop round all bolos to be processed by this thread. */
      for( ibolo = b1; ibolo <= b2 && *status == SAI__OK; ibolo++ ) {

/* Skip this bolo if it has been rejected. */
         if( pdata->badbol[ ibolo ] >= 0 ) {

/* Pointers to the first element of the arrays for the bolometer. */
            pdi = pdata->dp + ibolo*bstride + t_first*tstride;
            pli = mask ? pdata->lut + ibolo*bstride + t_first*tstride: NULL;

/* Loop round all time slices. */
            for( itime = t_first; itime <= t_last; itime++ ) {

/* Set bolometer samples bad if they fall within source pixels in the
   mask. */
               if( (*pli) != VAL__BADI && !pdata->mask[*pli] ){
                  *pdi = VAL__BADD;
               }

/* Move pointers on to the next time slice. */
               pdi += tstride;
               pli += tstride;
            }
         }
      }

/* Find the i'th principal component -  a linear combination of the
   usable gap-filled time-streams, where the coefficients of the combination
   are specified by the values in the corresponding principal axis
   (eigenvector).
   ==================================================================== */
   } else if( pdata->oper == 4 ) {

/* Loop round each time slice to be processed by this thread. */
      pc = pdata->pcomp + t1;
      for( itime = t1; itime <= t2; itime++ ) {

/* Pointer to the first gap-filled bolometer value at the current time
   slice. */
         pdi = pdata->dp + itime*tstride;

/* Loop round all bolometers. */
         pm = pdata->means;
         ps = pdata->sigmas;
         s1 = 0.0;
         for( ibolo = 0; ibolo < nbolo; ibolo++ ) {

/* Get the index of the current bolo within the list of eigenvectors and
   eigenvalues, and pass on if the current bolo is not usable. */
            b = pdata->badbol[ ibolo ];
            if( b >= 0 ) {

/* Standardise the data value for the current bolometer, and then
   increment the dot product. */
               s1 += pdata->c[ b ] * (*pdi - *pm)/(*ps);
            }

/* Move on to the next bolometer. */
            pdi += bstride;
            pm++;
            ps++;
         }

/* Store the principal component's value at the current time slice. */
         *(pc++) = s1;
      }

/* Remove the current component from the data.
   =========================================== */
   } else if( pdata->oper == 5 ) {

/* Loop round each time slice to be processed by this thread. */
      pc = pdata->pcomp + t1;
      for( itime = t1; itime <= t2; itime++,pc++ ) {

/* Pointer to the first original bolometer data and quality value at the
   current time slice. */
         pdi = pdata->data + itime*tstride;
         pqi = pdata->qua + itime*tstride;

/* Get the principal component value at the current time slice. */
         cval = *pc;

/* Loop round all bolometers. */
         ps = pdata->sigmas;
         for( ibolo = 0; ibolo < nbolo; ibolo++ ) {

/* Get the index of the current bolo within the list of eigenvectors and
   eigenvalues, and pass on if the current bolo is not usable. */
            b = pdata->badbol[ ibolo ];
            if( b >= 0 ) {

/* Skip flagged bolometer samples. */
               if( !(*pqi & SMF__Q_GOOD ) && *pdi != VAL__BADD ) {

/* Get the bolometer increment and subtract it off the bolometer valuues.
   Scale the bolometer value up so that it becomes a genuine value in pW
   rather than a standardised value in standard deviations. */
                  *pdi -= cval*(pdata->c[ b ])*(*ps);
               }
            }

/* Move on to the next bolometer. */
            pdi += bstride;
            pqi += bstride;
            ps++;
         }
      }

/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_pca: Invalid operation (%d) supplied.",
               status, pdata->oper );
   }
}



