/*
*+
*  Name:
*     smf_subip

*  Purpose:
*     Make correction for instrumental polarisation in POL2 data.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_subip( ThrWorkForce *wf, smfArray *res, smfArray *lut,
*                     int *lbnd_out, int *ubnd_out, AstKeyMap *keymap,
*                     AstFrameSet *outfs, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     res = smfArray * (Given)
*        Pointer to a smfArray holding the bolometer time-stream values for
*        all scuba-2 subarrays.
*     lut = smfArray * (Given)
*        Pointer to a smfArray holding the map pixel index for each
*        bolometer value in all scuba-2 subarrays.
*     lbnd_out = int * (Given)
*        A 2-element array - the lower pixel index bounds of the output map.
*     ubnd_out = int * (Given)
*        A 2-element array - the upper pixel index bounds of the output map.
*     keymap = AstKeyMap * (Given)
*        A KeyMap holding all configuration parameters.
*     outfs = AstFrameSet * (Given)
*        The WCS FrameSet for the output map.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function modifies the supplied bolometer Q/U time-stream data
*     by subtracting off estimates of the Q or U caused by instrumental
*     polarisation, based on the total intensity values specified by the
*     map associated with environment parameter IPREF. The values in this
*     map are multiplied by factors determined from the current elevation
*     and the parameters of the Johnstone/Kennedy IP model for POL2 to
*     get the instrumental Q and U values which are subtracted off the
*     supplied bolometer data. The parameters for the IP model are read
*     frpm a data file specified via configuration parameter IPDATA.

*  Authors:
*     David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     7-OCT-2015 (DSB):
*        Original version.
*     11-NOV-2015 (DSB):
*        Take account of the fact that the Q/U time streams may use
*        north in any system as the ref. direction - it need not be
*        the tracking system.
*     {enter_further_changes_here}

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

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"
#include "libsmf/smf_err.h"

/* Prototypes for local static functions. */
static void smf1_subip( void *job_data_ptr, int *status );
static double *smf1_calcang( smfData *data, const char *trsys, int *status );

/* Local data types */
typedef struct smfSubIPData {
   JCMTState *allstate;
   const char *qu;
   dim_t b1;
   dim_t b2;
   dim_t nbolo;
   dim_t ntslice;
   double *angcdata;
   double *c0data;
   double *imapdata;
   double *p0data;
   double *p1data;
   double *res_data;
   double *ipang;
   int *lut_data;
   size_t bstride;
   size_t tstride;
   smf_qual_t *qua_data;
} SmfSubIPData;


void smf_subip(  ThrWorkForce *wf, smfArray *res, smfArray *lut, int *lbnd_out,
                 int *ubnd_out, AstKeyMap *keymap, AstFrameSet *outfs, int *status ) {

/* Local Variables: */
   HDSLoc *loc = NULL;
   HDSLoc *sloc = NULL;
   SmfSubIPData *job_data = NULL;
   SmfSubIPData *pdata;
   char *polnorth;
   char ipref[200];
   char subname[10];
   const char *ipdata;
   const char *qu;
   const char *trsys;
   dim_t bolostep;
   dim_t nbolo;
   dim_t ntslice;
   double *angcdata;
   double *c0data;
   double *imapdata;
   double *ipang;
   double *p0data;
   double *p1data;
   int angcndf;
   int c0ndf;
   int imapndf;
   int iw;
   int nmap;
   int nw;
   int p0ndf;
   int p1ndf;
   size_t bstride;
   size_t idx;
   size_t tstride;
   smfData *data;
   smf_qual_t *qua_data;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Check if we have pol2 data, and see if it is Q or U. */
   qu = NULL;
   for( idx = 0; idx < res->ndat; idx++ ) {
      data = res->sdata[idx];

      if( !strcmp( data->hdr->dlabel, "Q" ) ){
         if( !qu ) {
            qu = "Q";
         } else if( strcmp( qu, "Q" ) ) {
            *status = SAI__ERROR;
            break;
         }

      } else if( !strcmp( data->hdr->dlabel, "U" ) ) {
         if( !qu ) {
            qu = "U";
         } else if( strcmp( qu, "U" ) ) {
            *status = SAI__ERROR;
            break;
         }

      } else if( qu ) {
         *status = SAI__ERROR;
         qu = NULL;
         break;
      }
   }

/* Report an error if there is a mix of pol2 and non-pol2, or a mix of Q
   and U. */
   if( *status != SAI__OK ) {
      if( qu ) {
         errRep( "", "smf_subip: Input data contains mix of Q and U "
                 "data", status );
      } else {
         errRep( "", "smf_subip: Input data contains mix of POL2 and "
                 "non-POL2 data", status );
      }

/* If we have pol2 data, get the path to the total intensity image that
   is to be used to define the level of IP correction required. If no
   value is supplied, annul the error and set "qu" NULL to indicate we
   should leave immediately. */
   } else if( qu && *status == SAI__OK ) {
      parGet0c( "IPREF", ipref, sizeof(ipref), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         qu = NULL;
      }
   }

/* If we are applying IP correction... */
   if( qu && *status == SAI__OK ) {
      msgOutf( "", "smf_subip: applying instrumental polarisation %s "
               "correction based on total intensity map `%s'", status,
               qu, ipref );

/* Get the value of the POLNORTH FITS keyword from the supplied header. */
      if( !astGetFitsS( data->hdr->fitshdr, "POLNORTH", &polnorth ) &&
           *status == SAI__OK ) {
         errRep( "", "smf_subip: Input POL2 data contains no POLNORTH "
                 "keyword in the FITS header.", status );
      }

/* Determine the AST system corresponding to polarimetric reference direction
   of the Q/U bolometer values. Set "trsys" to NULL if the focal plane Y axis
   is the reference direction. */
      if( !strcmp( polnorth, "TRACKING" ) ) {
         trsys = sc2ast_convert_system( data->hdr->state->tcs_tr_sys, status );
      } else if( !strcmp( polnorth, "FPLANE" ) ) {
         trsys = NULL;
      } else {
         trsys = polnorth;
      }

/* Get an identifier for the IPREF NDF. */
      ndfFind( NULL, ipref, &imapndf, status );

/* Resample the NDFs data values onto the output map grid. */
      imapdata = smf_alignndf( imapndf, outfs, lbnd_out, ubnd_out,
                               status );

/* Annul the NDF identifier. */
      ndfAnnul( &imapndf, status );

/* Create structures used to pass information to the worker threads. */
      nw = wf ? wf->nworker : 1;
      job_data = astMalloc( nw*sizeof( *job_data ) );

/* Get the path to the container file holding the IP model parameters. */
      ipdata = "$STARLINK_DIR/share/smurf/ipdata.sdf";
      astMapGet0C( keymap, "IPDATA", &ipdata );

/* Open the container file. */
      hdsOpen( ipdata, "READ", &loc, status );

/* Do the IP correction for each subarray (s8a, s8b, etc) in turn. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {
         data = res->sdata[idx];

/* Get an array holding the angle (rad.s) from the reference direction
   used by the Q/U bolometer values  to focal plane Y, measured positive
   in the sense of rotation from focal plane Y to focal plane X, for
   every bolometer sample in the smfData. The values are bolo ordered so
   that "bstride" is 1 and "tstsride" is nbolo. */
         ipang = smf1_calcang( data, trsys, status );

/* Get the number of bolometers and time slices for the current subarray,
   together with the strides between adjacent bolometers and adjacent
   time slices. */
         smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                       &tstride, status );

/* Get a locator for the structure holding the IP parameters for the
   current subarray */
         smf_find_subarray( data->hdr, subname, sizeof( subname ), NULL,
                            status );
         datFind( loc, subname, &sloc, status );

/* Begin an NDF context. */
         ndfBegin();

/* Get identifiers for the NDFs holding the individual parameters. Each
   NDF holds a parameter value for each bolometer. */
         ndfFind( sloc, "P0", &p0ndf, status );
         ndfFind( sloc, "P1", &p1ndf, status );
         ndfFind( sloc, "C0", &c0ndf, status );
         ndfFind( sloc, "ANGC", &angcndf, status );

/* Map them. Check each one has the expected number of elements. */
         ndfMap( p0ndf, "DATA", "_DOUBLE", "READ", (void **) &p0data,
                 &nmap, status );
         if( nmap != (int) nbolo && *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "N", p0ndf );
            errRep( "", "smf_subip: Bad dimensions for ^N - should be 32x40.", status );
         }

         ndfMap( p1ndf, "DATA", "_DOUBLE", "READ", (void **) &p1data,
                 &nmap, status );
         if( nmap != (int) nbolo && *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "N", p1ndf );
            errRep( "", "smf_subip: Bad dimensions for ^N - should be 32x40.", status );
         }

         ndfMap( c0ndf, "DATA", "_DOUBLE", "READ", (void **) &c0data,
                 &nmap, status );
         if( nmap != (int) nbolo && *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "N", c0ndf );
            errRep( "", "smf_subip: Bad dimensions for ^N - should be 32x40.", status );
         }

         ndfMap( angcndf, "DATA", "_DOUBLE", "READ", (void **) &angcdata,
                 &nmap, status );
         if( nmap != (int) nbolo && *status == SAI__OK ) {
            *status = SAI__ERROR;
            ndfMsg( "N", angcndf );
            errRep( "", "smf_subip: Bad dimensions for ^N - should be 32x40.", status );
         }

/* Get a pointer to the quality array for the residuals. */
         qua_data = smf_select_qualpntr( data, NULL, status );

/* See how many bolometers to process in each thread. */
         bolostep = nbolo/nw;
         if( bolostep == 0 ) bolostep = 1;

/* Create jobs to apply the IP correction to a range of bolometers. */
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;

/* Set the range of bolometers (b1 to b2) to be processed by the current
   job. */
            pdata->b1 = iw*bolostep;
            if( iw < nw - 1 ) {
               pdata->b2 = pdata->b1 + bolostep - 1;
            } else {
               pdata->b2 = nbolo - 1 ;
            }

/* Store the other info needed by the worker thread. */
            pdata->ntslice = ntslice;
            pdata->nbolo = nbolo;
            pdata->res_data = res->sdata[idx]->pntr[0];
            pdata->lut_data = lut->sdata[idx]->pntr[0];
            pdata->qua_data = qua_data;
            pdata->ipang = ipang;
            pdata->bstride = bstride;
            pdata->tstride = tstride;
            pdata->imapdata = imapdata;
            pdata->qu = qu;
            pdata->p0data = p0data;
            pdata->p1data = p1data;
            pdata->c0data = c0data;
            pdata->angcdata = angcdata;
            pdata->allstate = data->hdr->allState;

/* Submit the job for execution by the next available thread. */
            thrAddJob( wf, 0, pdata, smf1_subip, 0, NULL, status );
         }

/* Wait for all jobs to complete. */
         thrWait( wf, status );

/* End the NDF context, thus unmapping and freeing all NDF identifiers
   created since the context was started. */
         ndfEnd( status );

/* Free locator for subarray IP parameters. */
         datAnnul( &sloc, status );
         ipang = astFree( ipang );
      }

/* Free resources. */
      datAnnul( &loc, status );
      imapdata = astFree( imapdata );
      job_data = astFree( job_data );
   }
}


static void smf1_subip( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_subip

*  Purpose:
*     Executed in a worker thread to apply IP correction to a range of
*     bolometers.

*  Invocation:
*     smf1_subip( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfSubIPData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   JCMTState *state;
   SmfSubIPData *pdata;
   const char *qu;
   dim_t ibolo;
   dim_t itime;
   dim_t nbolo;
   dim_t ntslice;
   double *imapdata;
   double *pa;
   double *pr;
   double angc;
   double c0;
   double cosval;
   double ival;
   double p0;
   double p1;
   double qfp;
   double qtr;
   double sinval;
   double tmp1;
   double tmp2;
   double tmp3;
   double ufp;
   double utr;
   int *pl;
   size_t bstride;
   size_t tstride;
   smf_qual_t *pq;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfSubIPData *) job_data_ptr;

/* Save some local values for speed. */
   ntslice = pdata->ntslice;
   nbolo = pdata->nbolo;
   tstride = pdata->tstride;
   bstride = pdata->bstride;
   imapdata = pdata->imapdata;
   qu = pdata->qu;

/* Loop round each bolometer to be processed by this thread. */
   for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {

/* Get a pointer to the first quality value for the current bolometer. */
      pq = pdata->qua_data + ibolo*bstride;

/* Check that the whole bolometer has not been flagged as bad. */
      if( !( *pq & SMF__Q_BADB ) ) {

/* Get the IP model parameters for this bolometer. */
         p0 = pdata->p0data[ ibolo ];
         p1 = pdata->p1data[ ibolo ];
         c0 = pdata->c0data[ ibolo ];
         angc = pdata->angcdata[ ibolo ];

/* If any of them are bad, flag the whole bolometer as unusable. */
         if( p0 == VAL__BADD || p1 == VAL__BADD ||
             c0 == VAL__BADD || angc == VAL__BADD ) {

            for( itime = 0; itime < ntslice; itime++ ) {
               *pq |= ( SMF__Q_IP | SMF__Q_BADB );
               pq += tstride;
            }

/* If all the IP parameters are good, we can do the IP correction. */
         } else {

/* Cache values needed in the itime loop. */
            tmp1 = p0*cos( 2*angc*AST__DD2R );
            tmp2 = p0*sin( 2*angc*AST__DD2R );
            tmp3 = 2*( c0 + angc )*AST__DD2R;

/* Get pointers to the first residual (i.e. the uncorrected Q or U value)
   and lut value (i.e. the index of the map pixel that receives the Q/U
   value) for the current bolometer. [ipang is always bolo-ordered so
   "bstride" is 1 and "tstride" is nbolo]. */
            pr = pdata->res_data + ibolo*bstride;
            pl = pdata->lut_data + ibolo*bstride;
            pa = pdata->ipang ? pdata->ipang + ibolo : NULL;

/* Loop round each time slice, maintaining a pointer to the JCMTState
   info for the slice (we need this to get the elevation for each slice). */
            state = pdata->allstate;
            for( itime = 0; itime < ntslice; itime++,state++ ) {
               if( *pl != VAL__BADI ) {

/* Get the total intensity for the sample. If it is bad, flag the sample. */
                  ival = imapdata[ *pl ];
                  if( ival == VAL__BADD ) {
                     *pq |= SMF__Q_IP;

/* Skip this sample if the residual is bad or flagged, of the
   corresponding map pixel is undefined, or the telescope elevation is
   unknown. */
                  } else if( *pr != VAL__BADD && !( *pq & SMF__Q_MOD ) &&
                             ( !pa || *pa != VAL__BADD ) &&
                             state->tcs_az_ac2 != VAL__BADD) {

/* Find the normalised instrumental Q and U. These are with respect to the
   focal plane Y axis. */
                     qfp = tmp1 + p1*cos( tmp3 + 2*state->tcs_az_ac2 );
                     ufp = tmp2 + p1*sin( tmp3 + 2*state->tcs_az_ac2 );

/* Rotate them to match the reference frame of the supplied Q and U
   values (unless the supplied Q and U values are w.r.t focal plane Y,
   in which case they already use the required reference direction). */
                     if( pa ) {
                        cosval = cos( 2*( *pa ) );
                        sinval = sin( 2*( *pa ) );
                        qtr = qfp*cosval + ufp*sinval;
                        utr = -qfp*sinval + ufp*cosval;
                     } else {
                        qtr = qfp;
                        utr = ufp;
                     }

/* Correct the residual Q or U value. */
                     if( *qu == 'Q' ) {
                        *pr -= ival*qtr;
                     } else {
                        *pr -= ival*utr;
                     }
                  }
               }

/* Move onto the next time slice. */
               pq += tstride;
               pr += tstride;
               pl += tstride;
               pa += nbolo;
            }
         }
      }
   }
}











/* Returns an array holding the angle (rad.s) from the reference
   direction used by the Q/U bolometer values to focal plane Y,
   measured positive in the sense of rotation from focal plane Y to focal
   plane X, for every bolometer sample in a smfData. The values are bolo
   ordered so that "bstride" is 1 and "tstsride" is nbolo. The returned
   array should be freed using astFree when no longer needed. */
static double *smf1_calcang( smfData *data, const char *trsys, int *status ){

/* Local Variables: */
   AstFrameSet *fpfset;
   AstFrameSet *wcs;
   AstMapping *g2s;
   AstMapping *s2f;
   dim_t ibolo;
   dim_t itime;
   dim_t nbolo;
   dim_t ncol;
   dim_t ntslice;
   double *fx2;
   double *fx;
   double *fy2;
   double *fy;
   double *gx;
   double *gy;
   double *pr;
   double *result;
   double *sx;
   double *sy;
   int subsysnum;

/* Check the inherited status. Also return NULL if the Q/U values are
   already referenced to the focal plane Y axis (i.e. all returned angles
   would be zero). */
   if( *status != SAI__OK || !trsys ) return NULL;

/* Get the number of bolometers and time slices, together with the strides
   between adjacent bolometers and adjacent time slices. */
   smf_get_dims( data,  NULL, &ncol, &nbolo, &ntslice, NULL, NULL, NULL,
                 status );

/* Allocate the returned array. */
   result = astMalloc( nbolo*ntslice*sizeof( *result ) );

/* Allocate arrays to hold the grid coords for every bolometer. */
   gx = astMalloc( nbolo*sizeof( *gx ) );
   gy = astMalloc( nbolo*sizeof( *gy ) );

/* Allocate arrays to hold the sky coords for every bolometer. */
   sx = astMalloc( nbolo*sizeof( *sx ) );
   sy = astMalloc( nbolo*sizeof( *sy ) );

/* Allocate arrays to hold the focal plane coords for every bolometer. */
   fx = astMalloc( nbolo*sizeof( *fx ) );
   fy = astMalloc( nbolo*sizeof( *fy ) );

/* Allocate arrays to hold the focal plane coords of a point slightly to
   the north of every bolometer. */
   fx2 = astMalloc( nbolo*sizeof( *fx2 ) );
   fy2 = astMalloc( nbolo*sizeof( *fy2 ) );
   if( *status == SAI__OK ) {

/* Initialise the arrays holding the grid coords for every bolometer. */
      for( ibolo = 0; ibolo < nbolo; ibolo++ ) {
         gx[ ibolo ] = ibolo % ncol + 1;
         gy[ ibolo ] = ibolo / ncol + 1;
      }

/* Get the GRID->focal plane FrameSet (the same for every time slice). */
      smf_find_subarray( data->hdr, NULL, 0, &subsysnum, status );
      sc2ast_createwcs( subsysnum, NULL, data->hdr->instap, data->hdr->telpos,
                        NO_FTS, &fpfset, status);

/* Use this to transform the bolometrer GRID coords to focal plane. */
      astTran2( fpfset, nbolo, gx, gy, 1, fx, fy );

/* Loop over all time slices. */
      pr = result;
      for( itime = 0; itime < ntslice; itime++ ) {

/* Get the WCS FrameSet for the time slice, and set its current Frame to the
   frame used as the reference by the Q/U bolometer values. */
         smf_tslice_ast( data, itime, 1, NO_FTS, status );
         wcs = data->hdr->wcs;
         if( wcs ) {
            astSetC( wcs, "System", trsys );

/* Get the mapping from GRID to SKY. */
            astBegin;
            g2s = astSimplify( astGetMapping( wcs, AST__BASE, AST__CURRENT ));

/* Get the mapping from SKY to focal plane (x,y) (the index of the FPLANE
   Frame is fixed at 3 by file sc2ast.c). */
            s2f = astSimplify( astGetMapping( wcs, AST__CURRENT, 3 ) );

/* Transform the grid coords of all bolometers to SKY coordinates using the FrameSet. */
            astTran2( g2s, nbolo, gx, gy, 1, sx, sy );

/* Increment the sky positions slightly to the north. */
            for( ibolo = 0; ibolo < nbolo; ibolo++ ) sy[ ibolo ] += 1.0E-6;

/* Transform these modified sky coordinates to focal plane. */
            astTran2( s2f, nbolo, sx, sy, 1, fx2, fy2 );
            astEnd;

/* Loop round all bolometers. */
            for( ibolo = 0; ibolo < nbolo; ibolo++ ) {

/* Get the angle from north to focal plane Y, measured positive in the
   sense of rotation from focal plane Y to focal plane X. */
               if(  fx[ibolo] != VAL__BADD &&  fy[ibolo] != VAL__BADD &&
                   fx2[ibolo] != VAL__BADD && fy2[ibolo] != VAL__BADD ) {
                  *(pr++) = atan2( fx[ibolo] - fx2[ibolo], fy2[ibolo] - fy[ibolo] );
               } else {
                  *(pr++) = VAL__BADD;
               }
            }

         } else {
            for( ibolo = 0; ibolo < nbolo; ibolo++ ) *(pr++) = VAL__BADD;
         }
      }
   }

/* Free resources. */
   fx = astFree( fx );
   fy = astFree( fy );
   fx2 = astFree( fx2 );
   fy2 = astFree( fy2 );
   sx = astFree( sx );
   sy = astFree( sy );
   gx = astFree( gx );
   gy = astFree( gy );

/* Return the array of angle values. */
   return result;
}


