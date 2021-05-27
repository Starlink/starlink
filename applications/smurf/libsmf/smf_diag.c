/*
*+
*  Name:
*     smf_diag

*  Purpose:
*     Dump diagnostics to disk for a single tinme stream.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_diag( ThrWorkForce *wf, HDSLoc *loc, int *ibolo, int irow,
*               int power, int time, int isub, smfDIMMData *dat,
*               smf_modeltype type, smfArray *model, int res,
*               const char *root, int mask, double mingood, int cube,
*               int map, int addqual, smfSampleTable *table,
*               double chunkfactor, const char *btable,
*               AstKeyMap *keymap, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     HDSLoc = *loc (Given)
*        Locator for the HDS container file to which the diagniostic
*        info should be appended. May be NULL if no data is to be written
*        to an HDS container file.
*     ibolo = int * (Given and Returned)
*        On entry: If larger than or equal to zero, this is the zero based
*        index of the bolometer to dump. If -1, dump the mean of all
*        bolometers. If -2 dump the weighted mean of all bolometers. If -3
*        dump an automatically chosen typical bolometer. If -4, do not
*        dump any bolometer. On exit: the zero-based index of the chosen
*        detector if ibolo was -3 on entry. Otherwise, ibolo is returned
*        unchanged.
*     irow = int (Given)
*        The zero-based row number within each NDF at which to store
*        the line of diagnostic info.
*     power = int (Given)
*        If non-zero, store the power spectrum for the data.
*     time = int (Given)
*        If non-zero, store the time series for the data.
*     isub = int (Given)
*        The zero-based index of the smfData within the supplied smfArray
*        to dump.
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation
*     type = smf_modeltype (Given)
*        Indicates which model is to be dumped.
*     model = smfArray ** (Returned)
*        A smfArray holding the model data to be dumped. If NULL, then
*        the residuals are dumped.
*     res = int (Given)
*        If non-zero dump the residuals. Otherwise dump the model. If
*        "type" is SMF__AST, the hits info is dumped (as a time stream)
*        in place of the actual AST model or residual data values if
*        "res" is -999.
*     root = const char * (Given)
*        The root name for the NDFs to hold the data within the container
*        file. The string "_time" and/or "_power" (depending on arguments
*        power and time) will be added to this root to created the full name.
*     mask = int (Given)
*        If non-zero, then the AST model will be masked using the current
*        mask before being dumped. Otherwise, the AST model will not be
*        masked before being dumped.
*     mingood = double (Given)
*        The minimum fraction of good values in a time stream for which
*        data should be dumped. An error is reported if the required minimum
*        value is not met.
*     cube = int (Given)
*        Is a full cube containing time ordered data for all bolometers
*        required? If so, it will be stored in an NDF with name
*        "<root>_cube_<irow>".
*     map = int (Given)
*        Is a 2D map containing the binned time-stream data for all
*        bolometers required? If so, it will be stored in an NDF with name
*        "<root>_map_<irow>". If "map" is positive, the map will contain
*        data for just the subarray specified by "isub".If "map" is
*        negative, the map will contain data for all available subarrays.
*     addqual = int (Given)
*        If non-zero, the output NDFs will include a quality array.
*     table = smfSampleTable * (Given)
*        Pointer to a structure to be updated with information about
*        values falling in a specified map pixel, or NULL.
*     chunkfactor = double (Given)
*        The scale factor for the current chunk.
*     btable = const char * (Given)
*        If not NULL, this is the root name for the ascii files to be
*        created holding the selected bolometer values.
*     keymap = AstKeyMap * (Given)
*        A keymap holding the makemap config in use.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Dumps diagnostic information for a single (real or mean)
*     bolometer.

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     25-JAN-2013 (DSB):
*        Original version.
*     21-OCT-2013 (DSB):
*        Added argument "addqual".
*     21-JAN-2015 (DSB):
*        Added "ibolo=-4" option.
*     10-APR-2018 (DSB):
*        Added parameter "chunkfactor".
*     4-OCT-2018 (DSB):
*        Fix memory leak associated with variable "name".
*     18-MAR-2021 (DSB):
*        - Allow "loc" to be NULL.
*        - Add argument btable.
*     6-APR-2021 (DSB):
*        Allow AST hits info to be dumped as a time-stream (see argument
*        "res" above).
*     26-MAY-2021 (DSB):
*        Dump the COM model as a full 2D map including all bolometers rather
*        than a 2D representation of a single 1D common-mode time-series.

*  Copyright:
*     Copyright (C) 2013,2015 Science and Technology Facilities Council.
*     Copyright (C) 2021 East Asian Observatory.
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
#include "mers.h"
#include "sae_par.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Prototypes for local static functions. */
static void smf1_diag( void *job_data_ptr, int *status );

/* Local data types */
typedef struct smfDiagData {
   const double *in;
   dim_t nbolo;
   dim_t ntslice;
   dim_t s1;
   dim_t s2;
   dim_t t1;
   dim_t t2;
   dim_t b1;
   dim_t b2;
   smfData *data;
   double *map;
   double *out;
   double *var;
   int *imap;
   int *lut;
   int mask;
   int oper;
   size_t bstride;
   size_t tstride;
   smf_qual_t *mapqual;
   smf_qual_t *qua;
   smf_qual_t *qua_out;
} SmfDiagData;

void smf_diag( ThrWorkForce *wf, HDSLoc *loc, int *ibolo, int irow,
               int power, int time, int isub, smfDIMMData *dat,
               smf_modeltype type, smfArray *model, int res,
               const char *root, int mask, double mingood, int cube,
               int map, int addqual, smfSampleTable *table, double chunkfactor,
               const char *btable, AstKeyMap *keymap, int *status ){

/* Local Variables: */
   AstCmpFrame *totfrm;
   AstCmpMap *totmap;
   AstFrame *ffrm;
   AstFrameSet *wcs = NULL;
   AstMapping *fmap;
   SmfDiagData *job_data = NULL;
   SmfDiagData *pdata;
   char *name = NULL;
   char attr[ 50 ];
   const char *dom;
   const char *mode;
   dim_t dsize;
   dim_t fdims[2];
   dim_t itime;
   dim_t jbolo;
   dim_t ndata;
   dim_t nbolo;
   dim_t nbolor;
   dim_t ngood;
   dim_t ntslice;
   double *buffer;
   double *ip;
   double *ipv;
   double *oldcom;
   double *oldres = NULL;
   double *pd;
   double *var;
   double *wf_map;
   double *wf_mapwgt;
   double *wf_mapwgtsq;
   double *wf_mapvar;
   double scalevar;
   int *index;
   int *wf_hitsmap;
   int bolostep;
   int el;
   int fax;
   int hits;
   int i;
   int iax;
   int idx;
   int idxhi;
   int idxlo;
   int indf;
   int iw;
   int lbnd[ 3 ];
   int nax;
   int nc;
   int ndim;
   int nw;
   int place;
   int rebinflags;
   int sorted;
   int timestep;
   int ubnd[ 3 ];
   int usebolo;
   size_t bstride;
   size_t bstrider;
   size_t nmap;
   size_t tstride;
   size_t tstrider;
   smfArray *array;
   smfData *data = NULL;
   smfData *data_tmp;
   smfData *noi;
   smfData *pow;
   smfData *sidequal;
   smf_qual_t *oldcomq;
   smf_qual_t *pq;
   smf_qual_t *pqr;
   smf_qual_t *qbuffer = NULL;
   smf_qual_t *qua = NULL;
   smf_qual_t *qual;
   smf_qual_t qval;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Start an AST context. */
   astBegin;

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Allocate job data for threads. */
   job_data = astCalloc( nw, sizeof(*job_data) );

/* Are the map hits to be dumped as a time-stream? */
   if( res == -999 ){
      res = 0;
      hits = ( type == SMF__AST );
   } else {
      hits = 0;
   }

/* Get a pointer to the smfArray containing the data to be dumped. */
   array = NULL;
   if( res || type == SMF__RES ) {
      array = dat->res[ 0 ];
   } else if( model ) {
      array = model;
   } else if( type != SMF__AST && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( "", "smf_diag: Bad value (%d) for argument \"type\".",
               status, type );
   }

/* Get a pointer to the smfData containing the data to be dumped. Deal
   with cases where we are dumping the AST model first (we need to
   generate AST from the current map since it is not stored explicitly). */
   if( ! array ) {

/* Ensure we use the RES model ordering */
      smf_model_dataOrder( wf, dat, NULL, 0, SMF__RES|SMF__LUT|SMF__QUA,
                           dat->res[0]->sdata[0]->isTordered, status );

/* We temporarily hijack the RES smfData to hold the AST model. */
      data = dat->res[ 0 ]->sdata[ isub ];
      smf_get_dims( data, NULL, NULL, NULL, NULL, &dsize, NULL, NULL, status );
      oldres = data->pntr[0];
      data->pntr[0] = astMalloc( dsize*sizeof( *oldres ) );

/* Number of samples per thread. */
      size_t sampstep = dsize/nw;
      if( sampstep == 0 ) sampstep = 1;

/* Set up info for each worker thread. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->s1 = iw*sampstep;
         if( iw < nw - 1 ) {
           pdata->s2 = pdata->s1 + sampstep - 1;
         } else {
           pdata->s2 = dsize - 1 ;
         }

         pdata->qua = dat->qua[0]->sdata[isub]->pntr[0];
         pdata->out = data->pntr[0];
         pdata->lut = dat->lut[0]->sdata[isub]->pntr[0];
         pdata->map = hits?NULL:dat->map;
         pdata->imap = hits?dat->hitsmap:NULL;
         pdata->mapqual = dat->mapqual;
         pdata->mask = mask;
         pdata->oper = 0;

/* Submit the job to the workforce. */
         thrAddJob( wf, 0, pdata, smf1_diag, 0, NULL, status );
      }

/* Wait for all jobs to complete. */
      thrWait( wf, status );

/* Some models (e.g. COM) may have only one smfData for all subarrays. */
   } else if( array->ndat == 1 ) {
      data = array->sdata[ 0 ];

   } else if( isub >= 0 && isub < (int) array->ndat ) {
      data = array->sdata[ isub ];

   } else if( *status == SAI__OK ) {
      data = NULL;
      *status = SAI__ERROR;
      errRepf( "", "smf_diag: requested subarray index (%d) is invalid - %d "
               "subarrays are available.", status, isub, (int) array->ndat );
   }

/* Get a pointer to the quality array. */
   qual = smf_select_qualpntr( data, NULL, status );

/* Get dimensions and strides for the smfData. */
   smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL,
                 &bstride, &tstride, status );

/* Prepare the worker data for later submission of jobs. Find how many time
   slices and bolometers to process in each worker thread. */
   if( *status == SAI__OK ) {
      timestep = ntslice/nw;
      if( timestep == 0 ) timestep = 1;

      bolostep = nbolo/nw;
      if( bolostep == 0 ) bolostep = 1;

/* Store the range of times slices, etc, to be processed by each thread.
   Ensure that the last thread picks up any left-over time slices.  */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->t1 = iw*timestep;
         if( iw < nw - 1 ) {
            pdata->t2 = pdata->t1 + timestep - 1;
         } else {
            pdata->t2 = ntslice - 1 ;
         }

         pdata->b1 = iw*bolostep;
         if( iw < nw - 1 ) {
            pdata->b2 = pdata->b1 + bolostep - 1;
         } else {
            pdata->b2 = nbolo - 1 ;
         }

/* Store other values common to all jobs. */
         pdata->nbolo = nbolo;
         pdata->ntslice = ntslice;
         pdata->bstride = bstride;
         pdata->tstride = tstride;
      }
   }

/* If we are dumping time-streams for a single bolometer... */
   if( *ibolo != -4 ) {

/* Allocate space for the time stream to dump. */
      buffer = astMalloc( ntslice*sizeof( *buffer ) );

/* Get the time stream containing the data to be dumped. If it contains
   only one time-stream (e.g COM), use it. */
      usebolo = *ibolo;
      if( nbolo == 1 ) usebolo = 0;

/* If a specified single bolometer was requested, copy it into
   the buffer, replacing flagged samples with VAL__BADD. */
      if( usebolo >= 0 && *status == SAI__OK ) {

         pd = ((double *) data->pntr[0]) + usebolo*bstride;
         pq = qual ? qual + usebolo*bstride : NULL;
         if( pq && addqual ) qbuffer = astMalloc( ntslice*sizeof( *qbuffer ) );

         ngood = 0;
         for( itime = 0; itime < ntslice; itime++ ){
            if( qbuffer ) qbuffer[ itime ] = *pq;
            if( ( qbuffer || ( !pq || *pq == 0 ) ) && *pd != VAL__BADD ) {
               buffer[ itime ] = *pd;
               ngood++;
            } else {
               buffer[ itime ] = VAL__BADD;
            }
            pd += tstride;
            if( pq ) pq += tstride;
         }

         if( ngood < mingood*ntslice && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( "", "smf_diag: Failed to dump diagnostic data since "
                     "selected bolometer (%d) has too few good values (%d < %d).",
                     status, usebolo, (int) ngood, (int) (mingood*ntslice) );
         }

/* Produce an ascii table of the bolometer values if required. */
         if( btable ) {
            int *oldlut = data->lut;
            smfHead *oldhead = data->hdr;
            data->lut = dat->lut[0]->sdata[isub]->pntr[0];
            data->hdr = dat->res[0]->sdata[isub]->hdr;
            smf_write_bolotable( data, btable, usebolo, dat->lbnd_out,
                                 dat->mdims, status );
            data->lut = oldlut;
            data->hdr = oldhead;
         }

/* If the mean or weighted mean bolometer, or a typical bolometer, is
   required, find it. */
      } else if( *status == SAI__OK ) {

/* Get the variances for each bolometer if needed. */
         var = NULL;
         if( usebolo == -2 || usebolo == -3 ) {
            var = astMalloc( nbolo*sizeof( *var ) );
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->oper = 2;
               pdata->out = var;
               pdata->data = data;
               thrAddJob( wf, 0, pdata, smf1_diag, 0, NULL, status );
            }
            thrWait( wf, status );
         }

/* If required, find a typical bolometer... Get a list of bolometer
   indices, sorted by variance value, and select the bolometer with the
   most good samples that is within the central 40% to 60% quartiles. */
         if( usebolo == -3 ) {
            index = smf_sortD( nbolo, var, &sorted, status );
            if( *status == SAI__OK ) {
               dim_t maxgood, itest;
               maxgood = 0;
               for( itest = 0.4*nbolo; itest <= 0.6*nbolo; itest++ ) {
                  jbolo = index[ itest ];

                  pd = ((double *) data->pntr[0]) + jbolo*bstride;
                  pq = qual ? qual + jbolo*bstride : NULL;
                  ngood = 0;
                  for( itime = 0; itime < ntslice; itime++ ){
                     if( ( !pq || *pq == 0 ) && *pd != VAL__BADD ) ngood++;
                     pd += tstride;
                     if( pq ) pq += tstride;
                  }

                  if( ngood > maxgood ) {
                     maxgood = ngood;
                     usebolo = jbolo;
                  }
               }

/* Copy the data from the selected bolometer into the buffer. */
               pd = ((double *) data->pntr[0]) + usebolo*bstride;
               pq = qual ? qual + usebolo*bstride : NULL;

               if( pq && addqual ) qbuffer = astMalloc( ntslice*sizeof( *qbuffer ) );

               ngood = 0;
               for( itime = 0; itime < ntslice; itime++ ){
                  if( qbuffer ) qbuffer[ itime ] = *pq;
                  if( ( qbuffer || ( !pq || *pq == 0 ) ) && *pd != VAL__BADD ) {
                     buffer[ itime ] = *pd;
                     ngood++;
                  } else {
                     buffer[ itime ] = VAL__BADD;
                  }
                  pd += tstride;
                  if( pq ) pq += tstride;
               }

               if( ngood < mingood*ntslice && *status == SAI__OK ) {
                  *status = SAI__ERROR;
                  errRep( "", "smf_diag: Failed to dump diagnostic data since "
                          "selected bolometer has too few good values.", status );
               }

/* Produce an ascii table of the bolometer values if required. */
               if( btable ) {
                  int *oldlut = data->lut;
                  smfHead *oldhead = data->hdr;
                  data->lut = dat->lut[0]->sdata[isub]->pntr[0];
                  data->hdr = dat->res[0]->sdata[isub]->hdr;
                  smf_write_bolotable( data, btable, usebolo, dat->lbnd_out,
                                       dat->mdims, status );
                  data->lut = oldlut;
                  data->hdr = oldhead;
               }
            }
            index = astFree( index );

/* Otherwise, calculate the mean or weighted mean bolometer time series. */
         } else {
            for( iw = 0; iw < nw; iw++ ) {
               pdata = job_data + iw;
               pdata->in = data->pntr[ 0 ];
               pdata->out = buffer;
               pdata->qua = qual;
               pdata->oper = usebolo;
               pdata->var = var;
               thrAddJob( wf, 0, pdata, smf1_diag, 0, NULL, status );
            }
            thrWait( wf, status );
         }

         var = astFree( var );

      }

/* Loop over the two possible outputs - time and power */
      for( i = 0; i < 2; i++ ) {

/* Continue if the current output is not needed. */
         if( ( i == 0 && !time ) || ( i == 1 && !power ) ) continue;

/* If we are dumping the power spectrum, replace the time series with the
   power spectrum within "buffer". Temporarily hijack the COM model for
   this (we choose COM since it has only one bolometer). */
         ndata = ntslice;
         if( i == 1 ) {
            if( dat->com ) {
               data_tmp = dat->com[ 0 ]->sdata[ isub ];

/* Check COM is a single bolo time stream. */
               if( data_tmp->ndims != 3 ||
                   data_tmp->dims[ 0 ] != ntslice ||
                   data_tmp->dims[ 1 ] != 1 ||
                   data_tmp->dims[ 2 ] != 1 ) {
                  if( *status == SAI__OK ) {
                     *status = SAI__ERROR;
                     errRep( "", "smf_diag: COM has unexpected dimensions.",
                             status );
                  }

               } else {

/* Save the original COM data pointer, and use the local buffer instead. */
                  oldcom = data_tmp->pntr[ 0 ];
                  data_tmp->pntr[ 0 ] = buffer;

/* Create a temporary quality array that flags the VAL__BADD values in
   buffer. We base it on a copy of the quality array for a used bolometer
   in the residuals, so that the new quality array inherits padding and apodisation flags. */
                  oldcomq = data_tmp->qual;
                  sidequal = data_tmp->sidequal;
                  data_tmp->qual = astMalloc( ntslice*sizeof( *data_tmp->qual ));
                  data_tmp->sidequal = NULL;

/* Examine the residuals quality array to find a bolometer that has not
   been completey rejected. Then copy the bolometer's quality values into
   the temporary array created above, retaining PAD and APOD flags but
   removing all others. Set SPIKE quality (a convenient bad value) for any
   values that are bad in the time series being dumped (the "buffer" array). */
                  smf_get_dims( dat->res[ 0 ]->sdata[isub], NULL, NULL,
                                &nbolor, NULL, NULL, &bstrider, &tstrider, status );
                  pqr = smf_select_qualpntr( dat->res[ 0 ]->sdata[isub], NULL, status );
                  pq = data_tmp->qual;
                  ngood = 0;
                  for( jbolo = 0; jbolo < nbolor; jbolo++ ) {
                     if( !( *pqr & SMF__Q_BADB ) ) {
                        for( itime = 0; itime < ntslice; itime++ ) {
                           qval = *pqr & ( SMF__Q_APOD | SMF__Q_PAD );
                           if( buffer[ itime ] == VAL__BADD ) {
                              if( !(qval & SMF__Q_PAD) ) qval = SMF__Q_SPIKE;
                           buffer[ itime ] = 0.0;    /* Bad values cause grief in smf_fft_data */
                           } else {
                              ngood++;
                           }
                           *(pq++) = qval;
                           pqr += tstrider;
                        }
                        break;
                     }
                     pqr += bstrider;
                  }

/* If too few good values, just take the FFT of a load of zerso. */
                  if( ngood < mingood*ntslice ) {
                     for( itime = 0; itime < ntslice; itime++ ){
                        buffer[ itime ] = 0.0;
                     }
                  }

/* Now do the fft */
                  pow = smf_fft_data( wf, data_tmp, NULL, 0, SMF__BADSZT,
                                      status );
                  smf_convert_bad( wf, pow, status );

/* Convert to power */
                  smf_fft_cart2pol( wf, pow, 0, 1, status );
                  smf_isfft( pow, NULL, NULL, fdims, NULL, NULL, status );

/* Get the WCS and copy the power spectrum to the buffer. */
                  ndata = fdims[ 0 ];
                  if( *status == SAI__OK ) {
                     if( pow->hdr->tswcs ) {
                        wcs = astClone( pow->hdr->tswcs );
                     } else if( pow->hdr->wcs ) {
                        wcs = astClone( pow->hdr->wcs );
                     }
                     memcpy( buffer, pow->pntr[ 0 ], ndata*sizeof( buffer ) );
                     smf_close_file( wf, &pow, status );

/* If too few good values, store a set of bad values in place of the
   power spectrum. */
                     if( ngood < mingood*ntslice ) {
                        for( itime = 0; itime < ndata; itime++ ){
                           buffer[ itime ] = VAL__BADD;
                        }
                     }
                  }

                  data_tmp->pntr[ 0 ] = oldcom;
                  (void) astFree( data_tmp->qual );
                  data_tmp->qual = oldcomq;
                  data_tmp->sidequal = sidequal;
               }

            } else if( *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRepf( "", "smf_diag: Cannot dump power spectra since "
                        "no COM model is being used.", status );
            }
         }

/* Form the name of the NDF to receive the data. */
         if( loc ) {
            name = astFree( name );
            nc = 0;
            name = astAppendString( name, &nc, root );
            name = astAppendString( name, &nc, i ? "_power" : "_time" );

/* Open the NDF, creating it if necesary. Ensure its bounds encompass the
   requested row. */
            ndfOpen( loc, name, "UPDATE", "UNKNOWN", &indf, &place, status );
            if( place != NDF__NOPL ) {
               lbnd[ 0 ] = 1;
               ubnd[ 0 ] = ndata;
               lbnd[ 1 ] = irow + 1;
               ubnd[ 1 ] = irow + 1;
               ndfNew( "_DOUBLE", 2, lbnd, ubnd, &place, &indf, status );
               mode = "Write";

/* Since a new NDF was created, add WCS if available. */
               if( wcs ) {

/* If storing power spectra, the FrameSet created by smf_fft_data is for a
   4D NDF, so we need to modify it for out 2D NDFs.  */
                  if( i == 1 ) {

/* Search for the frequency axis. */
                     nax = astGetI( wcs, "NAXES" );
                     for( iax = 1; iax <= nax; iax++ ) {
                        sprintf( attr, "Domain(%d)", iax );
                        dom = astGetC( wcs, attr );
                        if( astChrMatch( dom, "SPECTRUM" ) ) {

/* Frequency axis found. Extract the grid->freq mapping, and get the
   SpecFrame. */
                           astMapSplit( wcs, 1, &iax, &fax, &fmap );
                           if( fmap ) {
                              ffrm = astPickAxes( wcs, 1, &fax, NULL );

/* Create a 2D WCS by combining the above frequency axis with a simple 1D
   axis describing iteration number. */
                              (void) astAnnul( wcs );
                              wcs = astFrameSet( astFrame( 2, "Domain=GRID" ), " " );
                              totmap = astCmpMap( fmap, astUnitMap( 1, " " ), 0, " " );
                              totfrm = astCmpFrame( ffrm, astFrame( 1, "Domain=ITERATION"
                                ",Label(1)=Iteration number,Symbol(1)=Iter" ), " " );
                              astAddFrame( wcs, AST__BASE, totmap, totfrm );

                           } else if( *status == SAI__OK ) {
                              *status = SAI__ERROR;
                              errRep( "", "smf_diag: Cannot extract the frequency axis "
                                      "from the power spectrum wcs.", status );
                           }

/* Do not need to check any more axes so break. */
                           break;
                        }
                     }

/* Report an error if no frequency axis was found. */
                     if( iax == nax && *status == SAI__OK ) {
                        *status = SAI__ERROR;
                        errRep( "", "smf_diag: No frequency axis found in the "
                                "power spectrum wcs.", status );
                     }
                  }
                  ndfPtwcs( wcs, indf, status );
                  wcs = astAnnul( wcs );
               }

/* If using an existing NDF, modify its bounds to encompass the new row
   and indicate it should be opened in update mode. */
            } else {
               ndfBound( indf, 2, lbnd, ubnd, &ndim, status );
               lbnd[ 0 ] = 1;
               ubnd[ 0 ] = ndata;
               if( lbnd[ 1 ] > irow + 1 ) lbnd[ 1 ] = irow + 1;
               if( ubnd[ 1 ] < irow + 1 ) ubnd[ 1 ] = irow + 1;
               ndfSbnd( 2, lbnd, ubnd, indf, status );
               mode = "Update";
            }

/* Map the data array and copy the values. */
            ndfMap( indf, "DATA", "_DOUBLE", mode, (void **) &ip, &el,
                    status );
            if( *status == SAI__OK ) memcpy( ip + ( irow + 1 - lbnd[1] )*ndata,
                                             buffer, sizeof(double)*ndata );

/* If required, map the Quality array and copy the values, then unmap it. */
            if( qbuffer ) {
               qua = smf_qual_map( wf, indf, mode, NULL, &nmap, status );
               if( *status == SAI__OK ) memcpy( qua + ( irow + 1 - lbnd[1] )*ndata,
                                                qbuffer, sizeof(*qua)*ndata );
               smf_qual_unmap( wf, indf, SMF__QFAM_TSERIES, qua, data->qbits, status );

/* Set the bad bits mask so that the data array will not be masked when
   it is mapped when dumping diagnostics for the next iteration. */
               ndfSbb( 0, indf, status );
            }

/* Free resources. */
            ndfAnnul( &indf, status );
            name = astFree( name );
         }
      }

/* Return the used bolometer, if required. */
      if( *ibolo == -3 && nbolo > 1 ) *ibolo = usebolo;

/* Free resources. */
      buffer = astFree( buffer );
      qbuffer = astFree( qbuffer );
   }

/* If required, dump the time series for all bolometers to a new cube. */
   if( cube && data->ndims == 3 && (data->pntr)[0] ) {

/* Get the name of the NDF to hold the cube. */
      name = astFree( name );
      nc = 0;
      name = astAppendString( name, &nc, root );
      name = astAppendString( name, &nc, "_cube_" );
      sprintf( attr, "%d", irow );
      name = astAppendString( name, &nc, attr );

/* Get its pixel bounds. */
      for( i = 0; i < 3; i++ ) {
         lbnd[ i ] = (data->lbnd)[ i ];
         ubnd[ i ] = lbnd[ i ] + (data->dims)[ i ] - 1;
      }

/* Create it and map the Data component. */
      ndfPlace( loc, name, &place, status );
      ndfNew( "_DOUBLE", 3, lbnd, ubnd, &place, &indf, status );
      ndfMap( indf, "DATA", "_DOUBLE", "WRITE", (void **) &ip, &el,
              status );

/* If required, map the quality array. */
      if( addqual ) qua = smf_qual_map( wf, indf, "wRITE", NULL, &nmap, status );

/* Copy the data values from the smfData to the NDF Data component,
   setting flagged values to VAL__BADD if required. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         if( pdata->b1 < nbolo ) {
            pdata->oper = 3;
            pdata->out = ip;
            pdata->in = (data->pntr)[0];
            pdata->qua = qual;
            pdata->qua_out = qua;
            thrAddJob( wf, 0, pdata, smf1_diag, 0, NULL, status );
         }
      }
      thrWait( wf, status );

/* If required, unmap the quality array. */
      if( addqual ) smf_qual_unmap( wf, indf, SMF__QFAM_TSERIES, qua,
                                    data->qbits, status );

/* Annul the NDF identifier. */
      ndfAnnul( &indf, status );

   }

/* If required, create a 2D map containing the binned data from all
   bolometers. */
   if( map && data->ndims == 3 && (data->pntr)[0] ) {

/* Get the name of the NDF to hold the map. */
      name = astFree( name );
      nc = 0;
      name = astAppendString( name, &nc, root );
      name = astAppendString( name, &nc, "_map_" );
      sprintf( attr, "%d", irow );
      name = astAppendString( name, &nc, attr );

/* Create it and map the Data and Variance components. */
      ndfPlace( loc, name, &place, status );
      ndfNew( "_DOUBLE", 2, dat->lbnd_out, dat->ubnd_out, &place, &indf,
              status );
      ndfMap( indf, "DATA", "_DOUBLE", "WRITE", (void **) &ip, &el,
              status );
      ndfMap( indf, "VARIANCE", "_DOUBLE", "WRITE", (void **) &ipv, &el,
              status );

/* If we are dumping the AST model, just copy the existing map. */
      if( !array && *status == SAI__OK ) {
         memcpy( ip, dat->map, dat->msize*sizeof( *dat->map ) );
         memcpy( ipv, dat->mapvar, dat->msize*sizeof( *dat->mapvar ) );

/* Otherwise, bin the time-stream data to form the map. */
      } else {

/* Get the indices of the first and last subarray to include in the map.
   When dumping COM, the number of arrays is determined by the GAI model
   since all four subarrays will share a single COM model if com.perarray
   is zero. */
         if( map > 0 ) {
            idxlo = isub;
            idxhi = isub;
         } else {
            idxlo = 0;
            if( type == SMF__COM && !res ) {
               idxhi = dat->gai[0]->ndat - 1;
            } else {
               idxhi = array->ndat - 1;
            }
         }

/* Allocate memory for multithreaded maps. */
         wf_map = astMalloc( nw*dat->msize*sizeof( *wf_map ) );
         wf_mapwgt = astMalloc( nw*dat->msize*sizeof( *wf_mapwgt ) );
         wf_mapwgtsq = astMalloc( nw*dat->msize*sizeof( *wf_mapwgtsq ) );
         wf_mapvar = astMalloc( nw*dat->msize*sizeof( *wf_mapvar ) );
         wf_hitsmap = astMalloc( nw*dat->msize*sizeof( *wf_hitsmap ) );

/* Loop over subarray. */
         for( idx = idxlo; idx <= idxhi; idx++ ) {

/* initialise rebin flags. */
            rebinflags = 0;

/* First call to rebin clears the arrays */
            if( idx == idxlo ) rebinflags = rebinflags | AST__REBININIT;

/* Final call to rebin re-normalizes */
            if( idx == idxhi ) rebinflags = rebinflags | AST__REBINEND;

/* Variances... */
            if( ntslice > 1 && dat->noi ) {
               noi =  dat->noi[0]->sdata[idx];
            } else {
               noi = NULL;
            }

/* Rebin the residual + astronomical signal into a map. COM must be
   handled separately since the COM model is 1-dimensional.  */
            if( type == SMF__COM && !res ) {
               AstObject *obj;
               astMapGet0A( keymap, "COM", &obj );
               dim_t gain_box;
               smf_get_nsamp( (AstKeyMap *) obj, "GAIN_BOX",
                              dat->res[ 0 ]->sdata[ idx ], &gain_box, status );
               obj = astAnnul( obj );
               smf_rebincom( wf,
                             (idx < (int) array->ndat)?array->sdata[ idx ]:array->sdata[ 0 ],
                             dat->gai[ 0 ]->sdata[ idx ], noi,
                             smf_select_qualpntr( dat->res[ 0 ]->sdata[ idx ],
                                                  NULL, status ),
                             dat->lut[0]->sdata[idx], rebinflags,
                             wf_map, wf_mapwgt, wf_mapvar, dat->msize,
                             gain_box, status );
            } else {
               smf_rebinmap1( wf, array->sdata[ idx ], noi, dat->lut[0]->sdata[idx]->pntr[0],
                           0, 0, 0, NULL, 0, SMF__Q_GOOD, 1, rebinflags,
                           wf_map, wf_mapwgt, wf_mapwgtsq, wf_hitsmap,
                           wf_mapvar, dat->msize, chunkfactor, &scalevar,
                           status );
            }
         }

/* Copy the data and variance arrays to the NDF. */
         if( *status == SAI__OK ) {
            memcpy( ip, wf_map, dat->msize*sizeof( *wf_map ) );
            memcpy( ipv, wf_mapvar, dat->msize*sizeof( *wf_mapvar ) );
         }

/* Free memory. */
         wf_map = astFree( wf_map );
         wf_mapwgt = astFree( wf_mapwgt );
         wf_mapwgtsq = astFree( wf_mapwgtsq );
         wf_mapvar = astFree( wf_mapvar );
         wf_hitsmap = astFree( wf_hitsmap );
      }

/* Annul the NDF identifier. */
      ndfAnnul( &indf, status );
   }

/* If required, add new columns to the table holding information about
   each sample that falls in a given map pixel. */
   if( table && loc ) {
      int *pi0, *pi, jj, nc;
      dim_t jcol, jrow;
      dim_t ibol, ipix, itime;
      char hdsname[ DAT__SZNAM + 1 ];
      char colname[ 128 ];
      size_t iel;
      double *pvals = NULL;
      double *qvals = NULL;

/* If not already done, identify the samples that fall in the specified
   pixel, and store columns holding the corresponding time slice and
   bolometer indices. Ignore models such as COM that only have 1
   time-series. */
      if( !table->nrow && nbolo > 1 ) {
         ipix = ( table->xpix - dat->lbnd_out[ 0 ] ) +
                dat->mdims[ 0 ]*( table->ypix - dat->lbnd_out[ 1 ] );

         pi0 = dat->lut[0]->sdata[isub]->pntr[0];
         for( itime = 0; itime < ntslice; itime++ ) {
            pi = pi0;
            for( ibol = 0; ibol < nbolo; ibol++ ) {
               if( *pi == (int) ipix ) {
                  jj = (table->nrow)++;
                  table->times = astGrow( table->times, table->nrow,
                                          sizeof( *(table->times) ) );
                  table->bolos = astGrow( table->bolos, table->nrow,
                                          sizeof( *(table->bolos) ) );
                  if( *status == SAI__OK ) {
                     (table->times)[ jj ] = itime;
                     (table->bolos)[ jj ] = ibol;
                  }
               }
               pi += bstride;
            }
            pi0 += tstride;
         }
      }

/* Create the name of the new column, and append to the end of the list
   of column names. Also create an array to hold column values and append
   to the end of the list. Then do the same for quality values. */
      if( table->nrow > 0 ) {
         datName( loc, hdsname, status );
         nc = sprintf( colname, "%s_%s_iter%d", hdsname, root, dat->iter );
         jj = (table->ncol)++;
         table->colnames = astGrow( table->colnames, table->ncol,
                                    sizeof( *(table->colnames) ) );
         table->colvals = astGrow( table->colvals, table->ncol,
                                    sizeof( *(table->colvals) ) );
         pvals = astMalloc( table->nrow*sizeof( *pvals ) );

         if( *status == SAI__OK ) {
            (table->colnames)[ jj ] = astStore( NULL, colname, nc + 1 );
            (table->colvals)[ jj ] = pvals;
         }

         if( qual ) {
            nc = sprintf( colname, "%s_%s_iter%d_qual", hdsname, root, dat->iter );
            jj = (table->ncol)++;
            table->colnames = astGrow( table->colnames, table->ncol,
                                       sizeof( *(table->colnames) ) );
            table->colvals = astGrow( table->colvals, table->ncol,
                                       sizeof( *(table->colvals) ) );
            qvals = astMalloc( table->nrow*sizeof( *qvals ) );

            if( *status == SAI__OK ) {
               (table->colnames)[ jj ] = astStore( NULL, colname, nc + 1 );
               (table->colvals)[ jj ] = qvals;
            }
         }

/* Store column values. */
         if( *status == SAI__OK ) {
            double *pd = (data->pntr)[0];
            size_t *pt = table->times;
            size_t *pb = table->bolos;
            if( nbolo > 1 ) {
               for( jrow = 0; jrow < table->nrow; jrow++ ) {
                  iel = (*(pb++))*bstride + (*(pt++))*tstride;
                  *(pvals++) = pd[ iel ];
                  if( qual ) *(qvals++) = qual[ iel ];
               }
            } else {
               for( jrow = 0; jrow < table->nrow; jrow++ ) {
                  iel = *(pt++);
                  *(pvals++) = pd[ iel ];
                  if( qual ) *(qvals++) = qual[ iel ];
               }
            }

/* Now write out the whole table, over-writing any previous table. */
            FILE *fd = fopen( table->table, "w" );
            if( fd ) {
               fprintf( fd, "# xpix=%d ypix=%d\n", table->xpix, table->ypix );
               fprintf( fd, "# ibolo itime " );
               for( jcol = 0; jcol < table->ncol; jcol++ ) {
                  fprintf( fd, "%s ", table->colnames[ jcol ] );
               }
               fprintf( fd, "\n" );

               pt = table->times;
               pb = table->bolos;
               for( jrow = 0; jrow < table->nrow; jrow++ ) {
                  fprintf( fd, "%zu %zu ", *(pb++), *(pt++) );
                  for( jcol = 0; jcol < table->ncol; jcol++ ) {
                     if( table->colvals[ jcol ][jrow] != VAL__BADD ) {
                        fprintf( fd, "%g ", table->colvals[ jcol ][jrow] );
                     } else {
                        fprintf( fd, "null " );
                     }
                  }
                  fprintf( fd, "\n" );
               }
               fclose( fd );

            } else {
               errRepf( "", "smf_diag: Failed to open table file '%s'.",
                       status, table->table );
            }
         }
      }
   }

/* Free the array holding the AST model and re-instate the original RES
   values if required. */
   if( oldres ) {
      (void) astFree( dat->res[0]->sdata[isub]->pntr[0] );
      dat->res[0]->sdata[isub]->pntr[0] = oldres;
   }

/* Free remaining resources. */
   job_data = astFree( job_data );
   name = astFree( name );

/* End the AST context. */
   astEnd;
}


static void smf1_diag( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_diag

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_diag.

*  Invocation:
*     smf1_diag( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfDiagData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   SmfDiagData *pdata;
   const double *pd;
   const double *in;
   dim_t ibolo;
   dim_t idata;
   dim_t itime;
   dim_t nbolo;
   dim_t t1;
   dim_t t2;
   double *out;
   double *po;
   double *var;
   double ast_data;
   double sum;
   double w;
   double wsum;
   int *pl;
   int n;
   int oper;
   size_t bstride;
   size_t tstride;
   smf_qual_t *qua_out;
   smf_qual_t *qua;
   smf_qual_t *pq2;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfDiagData *) job_data_ptr;

/* Copy stuff into local variables for easier access. */
   nbolo = pdata->nbolo;
   in = pdata->in;
   out = pdata->out;
   qua = pdata->qua;
   qua_out = pdata->qua_out;
   var = pdata->var;
   bstride = pdata->bstride;
   tstride = pdata->tstride;
   oper = pdata->oper;
   t1 = pdata->t1;
   t2 = pdata->t2;

/* Sample the map to form the AST signal. */
   if( oper == 0 ) {
      po = pdata->out +  pdata->s1;
      pq = pdata->qua +  pdata->s1;
      pl = pdata->lut +  pdata->s1;
      for( idata = pdata->s1; idata <= pdata->s2; idata++,pq++,pl++,po++ ) {
         if( !( *pq & SMF__Q_MOD ) && *pl != VAL__BADI ) {

            if( pdata->map ) {
               ast_data = pdata->map[ *pl ];
            } else if( pdata->imap[ *pl ] != VAL__BADI ){
               ast_data = (double) pdata->imap[ *pl ];
            } else {
               ast_data = VAL__BADD;
            }

            if( ast_data != VAL__BADD &&
                ( !(pdata->mapqual[ *pl ] & SMF__MAPQ_AST ) || !pdata->mask ) ) {
               *po = ast_data;
            } else {
               *po = VAL__BADD;
            }
         } else {
            *po = VAL__BADD;
         }
      }

/* Get the variances of a range of bolometers. */
   } else if( oper == 2 ) {
      out = pdata->out + pdata->b1;
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {
         w = smf_quick_noise( pdata->data, ibolo, 20, 50, SMF__Q_GOOD, status );
         if( *status == SMF__INSMP ) {
            errAnnul( status );
            w = VAL__BADD;
         }
         *(out++) = w;
      }

/* Copy full cube of data values, masking them in the process. */
   } else if( oper == 3 ) {
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {
         out = pdata->out + ibolo*bstride;
         in = pdata->in + ibolo*bstride;
         pq = qua ? qua + ibolo*bstride : NULL;
         pq2 = qua_out ? qua_out + ibolo*bstride : NULL;
         for( itime = 0; itime < pdata->ntslice; itime++) {
            if( !pq || *pq == 0 || pq2 ) {
               *out = *in;
            } else {
               *out = VAL__BADD;
            }

            if( pq && pq2 ) *pq2 = *pq;

            in += tstride;
            out += tstride;
            if( pq ) pq += tstride;
            if( pq2 ) pq2 += tstride;
         }
      }

/* Form unweighted mean of all good bolos. */
   } else if( oper == -1 ) {
      for( itime = t1; itime <= t2; itime++ ) {
         sum = 0.0;
         n = 0;
         pd = in + itime*tstride;
         pq = qua ? qua + itime*tstride : NULL;
         for( ibolo = 0; ibolo < nbolo; ibolo++ ) {
            if( *pd != VAL__BADD && ( !pq || *pq == 0 ) ) {
               sum += *pd;
               n++;
            }
            pd += bstride;
            if( pq ) pq += bstride;
         }
         if( n > 0 ) {
            out[ itime ] = sum/n;
         } else {
            out[ itime ] = VAL__BADD;
         }
      }

/* Form weighted mean of all good bolos. */
   } else if( oper == -2 ) {
      for( itime = t1; itime <= t2; itime++ ) {
         sum = 0.0;
         wsum = 0.0;
         pd = in + itime*tstride;
         pq = qua ? qua + itime*tstride : NULL;
         for( ibolo = 0; ibolo < nbolo; ibolo++ ) {
            w = var[ ibolo ];
            if( *pd != VAL__BADD && ( !pq || *pq == 0 ) &&
                w != VAL__BADD && w > 0.0 ) {
               w = 1/w;
               sum += w*(*pd);
               wsum += w;
            }
            pd += bstride;
            if( pq ) pq += bstride;
         }
         if( wsum > 0 ) {
            out[ itime ] = sum/wsum;
         } else {
            out[ itime ] = VAL__BADD;
         }
      }

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "smf1_diag: Illegal operation %d requested.",
               status, oper );
   }
}




