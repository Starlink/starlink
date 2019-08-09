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
*     void smf_subip( ThrWorkForce *wf, smfDIMMData *dat, AstKeyMap *keymap,
*                     int *qui, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     dat = smfDIMMData * (Given)
*        Struct of pointers to information required by model calculation.
*     keymap = AstKeyMap * (Given)
*        A KeyMap holding all configuration parameters.
*     qui = int * (Given)
*        Pointer to a returned value indicating if the data is POL-2 data
*        or not. Returned VAL__BADI for non-pol2, +1 for "Q", 0 for "I"
*        and -1 for "U".
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function modifies the supplied bolometer Q/U time-stream data
*     by subtracting off estimates of the Q or U caused by instrumental
*     polarisation, based on the total intensity values specified by the
*     map associated with environment parameter IPREF. The values in this
*     map are multiplied by factors (Qf,Uf) determined from the current
*     elevation. Three models are currently provided:
*
*     - the "JAN2018" (January 2018) model:
*
*        p = A + B*el + C*el*el
*        Qf = p*cos( -2*( el - D ) )
*        Uf = p*sin( -2*( el - D ) )
*
*        (p is the expected fractional polarisation, el is the elevation
*        in radians, (A,B,C,D) are the numerical parameters of the model).
*        See report "IP model without the wind blind" (written January
*        2018) in section "Data Reduction and Analysis" of the POL2
*        commissioning wiki.
*
*     - the "APR2019" (April 2019) model:
*
*        Qf = A + B*cos( -2*( el - C ) ) + D*cos( -2*( el - E ) )
*        Uf = F + B*sin( -2*( el - C ) ) + D*sin( -2*( el - E ) )
*
*        (el is the elevation in radians, (A,B,C,D,E,F) are the numerical
*        parameters of the model).
*
*     - the "AUG2019" (August 2019) model: this model has the same form as
*       the JAN2018 model, but different numerical constants. The constants
*       are derived using the smurf:pol2ipcor command on observations of
*       DR21, OMC1, G34, Serpens Main field2 and Orion B. See report
*       "Estimating the IP from observations of bright extended polarised
*       objects" (written August 2019) in section "Data Reduction and
*       Analysis" of the POL2 commissioning wiki.
*
*     The model to use is specified by the "ipmodel" config parameter.
*
*     The selected model gives the expected instrumental Q and U values, which
*     are then rotated to use the same reference direction as the Q/U bolometer
*     values. The rotated corrections are then subtracted off the extinction-corrected
*     bolometer data (the extinction correction is removed before returning).

*  Authors:
*     David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     7-OCT-2015 (DSB):
*        Original version.
*     11-NOV-2015 (DSB):
*        - Added option for simplified IP model defined by values supplied
*        via config parameter "ipmodel").
*        - Take account of the fact that the Q/U time streams may use
*        north in any system as the ref. direction - it need not be
*        the tracking system.
*     24-NOV-2015 (DSB):
*        - The PL1 model now uses a quadratic function of elevation to give
*        the fractional polarisation at any elevation.
*        - The supplied bolometer data is now corrected for extinction
*        before subtracting the IP. The extinction correction is finally
*        removed before returning the IP corrected bolometer data.
*     15-JAN-2016 (DSB):
*        Multi-thread the angle calculations.
*     17-MAR-2016 (DSB):
*        Fix data ordering bug in smf1_calcang.
*     21-SEP-2016 (DSB):
*        Added PL2 model.
*     14-DEC-2016 (DSB):
*        Changed "ispol2" to "qui" and use VAL__BADI to indicate non-POL2
*        so that we can user zero to indicate "I" (i.e. total intensity
*        from a POL2 observation).
*     27-JAN-2017 (DSB):
*        - The existing PL1 and PL2 models were derived using I maps created
*        without POL2 in the beam. So if the supplied I map was created
*        with POL2 in the beam it needs to be corrected for the expected POL2
*        degradation factor (1.35 at 850 um) before being used.
*        - Report an error if 450 um data supplied.
*     8-FEB-2017 (DSB):
*        Added model PL3 for use with IP reference maps that are created
*        from POL2 data and thus have the same FCF as the Q and U data
*        being corrected.
*     3-MAR-2017 (DSB):
*        Fix bug that caused makemap to abort when producing maps from
*        450 um non-POL2 data.
*     24-MAY-2017 (DSB):
*        Add IPOFFSET configuration parameter.
*     19-JAN-2018 (DSB):
*        - Remove support for the Johnson-Kennedy model
*        - Remove user-configurable IP model: the choice of IP model is
*        now made by the logic in this function.
*        - Add support for 450 um.
*        - Add support for observations without the wind blind.
*     20-MAR-2019 (DSB):
*        Allow user to modifty the IP angle using the new config parameter
*        IPANGOFF.
*     18-APR-2019 (DSB):
*        Implement Pierre's two component IP model ("APR2019"), and add
*        "ipmodel" config parameter.
*     6-AUG-2019 (DSB):
*        Added AUG2019 IP model based on using smurf:pol2ipcor.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2015-2017 East Asian Observatory.
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

/* System includes */
#include <strings.h>

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

/* Local constants */
#define JAN2018 0
#define APR2019 1
#define AUG2019 2

/* Prototypes for local static functions. */
static void smf1_subip( void *job_data_ptr, int *status );
static double *smf1_calcang( ThrWorkForce *wf, smfData *data, const char *trsys,
                             int *status );

/* Local data types */
typedef struct smfSubIPData {
   JCMTState *allstate;
   const char *qu;
   const char *trsys;
   const double *fx;
   const double *fy;
   const double *gx;
   const double *gy;
   dim_t b1;
   dim_t b2;
   dim_t nbolo;
   dim_t ntslice;
   dim_t t1;
   dim_t t2;
   double *imapdata;
   double *ipang;
   double *ippars;
   double *res_data;
   double *result;
   double degfac;
   double ipoffset;
   int *lut_data;
   int ipmodel;
   int oper;
   size_t bstride;
   size_t tstride;
   smfData *data;
   smf_qual_t *qua_data;
} SmfSubIPData;

/* A mutex used to serialise access to the smfData */
static pthread_mutex_t data_mutex = PTHREAD_MUTEX_INITIALIZER;

void smf_subip(  ThrWorkForce *wf, smfDIMMData *dat, AstKeyMap *keymap,
                 int *qui, int *status ) {

/* Local Variables: */
   AstFitsChan *fc;
   SmfSubIPData *job_data = NULL;
   SmfSubIPData *pdata;
   char *polnorth;
   char *trsys;
   char ipref[200];
   char modname[10];
   const char *cpntr;
   const char *qu;
   dim_t bolostep;
   dim_t nbolo;
   dim_t ntslice;
   double *imapdata;
   double *ipang;
   double *ippars;
   double degfac;
   double ipangoff;
   double ipoffset;
   int imapndf;
   int ipmodel;
   int iw;
   int nw;
   int polref;
   int windblind;
   size_t bstride;
   size_t idx;
   size_t tstride;
   smfArray *lut;
   smfArray *res;
   smfData *data = NULL;
   smf_qual_t *qua_data;
   smf_subinst_t waveband;

/* IP model parameters.
   -------------------- */

/* "JAN2018": See report "IP model without the wind blind" (written
   January 2018) in section "Data Reduction and Analysis" of the POL2
   commissioning wiki. */
   double model1[] = { 5.520E-3, -3.649E-4,  1.316E-3, -8.544E-2 }; /* No wind blind */
   double model2[] = { 6.188E-3,  3.650E-2, -1.978E-2, -3.240E-2 }; /* 850 um with wind blind */
   double model3[] = { 1.190E-2,  1.835E-2, -1.374E-2,  1.486 };    /* 450 um with wind blind */

/* "APR2019": See email from Pierre, sent to DSB and PF on 15th April
   2019 - subject "850 IP model". Note, this model only covers 850 data
   at the moment. */
   double model4[] = { -0.47146E-2, 0.57117E-2, 0.24078,        0.0, -2.7784E-2, 0.0 }; /* No wind blind */
   double model5[] = { -0.47146E-2, 0.57117E-2, 0.24078, 1.34820E-2, -2.7784E-2, 0.0 }; /* 850 um with wind blind */
   double *model6 = NULL;                                                         /* 450 um with wind blind */

/* "AUG2019": See report "Estimating the IP from observations of bright
   extended polarised objects" (written August 2019) in section "Data
   Reduction and Analysis" of the POL2 commissioning wiki. */
   double *model7 = model1;                                          /* No wind blind */
   double model8[] = { 1.483E-2,  2.062E-2, -1.201E-2, -5.689E-2 };  /* 850 um with wind blind */
   double model9[] = { 1.020E-2,  9.413E-3, -5.411E-3,  1.719 };     /* 450 um with wind blind */

/* Initialise */
   *qui = VAL__BADI;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get convenience pointers. */
   res = dat->res[0];
   lut = dat->lut[0];

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

      } else if( !strcmp( data->hdr->dlabel, "I" ) ) {
         if( !qu ) {
            qu = "I";
         } else if( strcmp( qu, "I" ) ) {
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
      if( qu[0] == 'Q' ) {
         *qui = 1;
      } else if( qu[0] == 'U' ) {
         *qui = -1;
      } else if( qu[0] == 'I' ) {
         *qui = 0;
      }
      parGet0c( "IPREF", ipref, sizeof(ipref), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         qu = NULL;
      }
   }

/* If we are applying IP correction... */
   if( qu && ( *qui == 1 || *qui == -1 ) && *status == SAI__OK ) {

/* Get the waveband (850 or 450), and note the nominal POL2 degradation
   factor. */
      waveband = smf_calc_subinst( res->sdata[0]->hdr, status );
      if( waveband == SMF__SUBINST_850 ) {
         degfac = 1.35;
      } else {
         degfac = 1.96;
      }

/* Get the value of the POLNORTH FITS keyword from the supplied header. */
      if( !astGetFitsS( data->hdr->fitshdr, "POLNORTH", &polnorth ) &&
           *status == SAI__OK ) {
         errRep( "", "smf_subip: Input POL2 data contains no POLNORTH "
                 "keyword in the FITS header.", status );
      }

/* Get an offset to add on to the polarisation level predicted by the IP
   model. This can be used to investigate the effects on the final maps of
   changing the IP level. The IP model is uncertain to about +/- 0.3%, so
   creating two maps with offsets of +0.3 and -0.3 and then comparing
   the two maps could give a handle on the significance of the IP
   uncertainty. The offset value is obtained as a percentage. */
      astMapGet0D( keymap, "IPOFFSET", &ipoffset );

/* Convert from percentage to fraction. */
      ipoffset /= 100.0;

/* Get the amount by which to change the elevation at which the IP is
   parallel to the focal plane Y axis (i.e. Qip is non-zero and Uip is
   zero - the "D" constant described in the prologue). This allows the
   user to experiment with the effects of changing the IP angle a bit. */
      astMapGet0D( keymap, "IPANGOFF", &ipangoff );

/* See which IP model to use. */
      cpntr = NULL;
      astMapGet0C( keymap, "IPMODEL", &cpntr );
      if( cpntr ){
         strcpy( modname, cpntr );
         if( !strcasecmp( "APR2019", cpntr ) ) {
            ipmodel = APR2019;
         } else if( !strcasecmp( "JAN2018", cpntr ) ) {
            ipmodel = JAN2018;
         } else if( !strcasecmp( "AUG2019", cpntr ) ) {
            ipmodel = AUG2019;
         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( " ", "smf_subip: Unknown IPMODEL specified: '%s'.",
                     status, cpntr );
         }
      }

/* Convert from degrees to to radians. */
      ipangoff *= AST__DD2R;

/* Determine the AST system corresponding to polarimetric reference direction
   of the Q/U bolometer values. Set "trsys" to NULL if the focal plane Y axis
   is the reference direction. */
      if( !strcmp( polnorth, "TRACKING" ) ) {
         cpntr = sc2ast_convert_system( data->hdr->allState->tcs_tr_sys, status );
      } else if( !strcmp( polnorth, "FPLANE" ) ) {
         cpntr = NULL;
      } else {
         cpntr = astStore( NULL, polnorth, strlen( polnorth ) + 1 );
      }

/* Take a copy in case the contents of the static buffer pointed to by "polnorth"
   changes. */
      trsys = cpntr ? astStore( NULL, cpntr, strlen( cpntr ) + 1 ) : NULL;

/* Get an identifier for the IPREF NDF. */
      ndfFind( NULL, ipref, &imapndf, status );

/* Resample the NDFs data values onto the output map grid. */
      imapdata = smf_alignndf( imapndf, dat->outfset, dat->lbnd_out, dat->ubnd_out,
                               status );

/* See if the reference NDF was created from POL2 data. */
      polref = 0;
      kpgGtfts( imapndf, &fc, status );
      if( astTestFits( fc, "INBEAM", NULL ) ) {
         char *cval = NULL;
         astGetFitsS( fc, "INBEAM", &cval );
         if( cval && strstr( cval, "pol" ) ) polref = 1;
      }

/* See if the JCMT wind blind was in place. The wind blind was removed from
   December 2017 to January 2018. */
      windblind = 1;
      if( astTestFits( fc, "WND_BLND", NULL ) ) {
         char *cval = NULL;
         astGetFitsS( fc, "WND_BLND", &cval );
         if( astChrMatch( cval, "NONE" ) ) windblind = 0;
      }

      fc = astAnnul( fc );

/* Annul the NDF identifier. */
      ndfAnnul( &imapndf, status );

/* Tell the user what's happening. */
      msgOutf( "", "smf_subip: applying instrumental polarisation %s "
               "correction based on total intensity map `%s' (model %s).",
               status, qu, ipref, modname );

/* Create structures used to pass information to the worker threads. */
      nw = wf ? wf->nworker : 1;
      job_data = astMalloc( nw*sizeof( *job_data ) );

/* The IP correction was determined using extinction-corrected bolometer
   values, So apply any existinction correction first before doing the IP
   correction. */
      if( dat->ext ) smf_calcmodel_ext( wf, dat, 0, keymap, dat->ext, 0,
                                        status);

/* Now do the IP correction for each subarray (s8a, s8b, etc) in turn. */
      for( idx = 0; idx < res->ndat && *status == SAI__OK; idx++ ) {
         data = res->sdata[idx];

/* Get an array holding the angle (rad.s) from the reference direction
   used by the Q/U bolometer values  to focal plane Y, measured positive
   in the sense of rotation from focal plane Y to focal plane X, for
   every bolometer sample in the smfData. */
         ipang = smf1_calcang( wf, data, trsys, status );

/* Get the number of bolometers and time slices for the current subarray,
   together with the strides between adjacent bolometers and adjacent
   time slices. */
         smf_get_dims( data,  NULL, NULL, &nbolo, &ntslice, NULL, &bstride,
                       &tstride, status );

/* Choose the parameter values to use for the IP model. If the data was
   taken without the wind blind in place, the model is the same at 450 and
   850. */
         if( !windblind ) {
            if( ipmodel == JAN2018 ) {
               ippars = model1;
            } else if( ipmodel == APR2019 ) {
               ippars = model4;
            } else {
               ippars = model7;
            }

/* If the wind blind was in place, the model is differemt at 450 and 850. */
         } else if( waveband == SMF__SUBINST_850 ) {
            if( ipmodel == JAN2018 ) {
               ippars = model2;
            } else if( ipmodel == APR2019 ) {
               ippars = model5;
            } else {
               ippars = model8;
            }

         } else {
            if( ipmodel == JAN2018 ) {
               ippars = model3;
            } else if( ipmodel == APR2019 ) {
               ippars = model6;
            } else {
               ippars = model9;
            }
         }

         if( !ippars && *status == SAI__OK ) {
            errRepf( " ", "smf_subip: The requested IP model is not yet "
                     "available for the required waveband.", status );
         }

/* Modify the elevation at which the IP is parallel to the focal plane Y axis
   (i.e. Qip is non-zero and Uip is zero) by adding on the user-specified
   offset. */
         if( ipmodel != APR2019 ) {
            ippars[ 3 ] += ipangoff;
         } else {
            ippars[ 2 ] += ipangoff;
            ippars[ 4 ] += ipangoff;
         }

/* The above IP parameters were determined using total intensity maps
   made from POL2 observations. So if the supplied IP reference map was
   *not* made from POL2 data, we need to take account of the POL2
   degradation factor. */
         if( polref ) degfac = 1.0;

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
            pdata->ippars = ippars;
            pdata->ipmodel = ipmodel;
            pdata->allstate = data->hdr->allState;
            pdata->oper = 1;
            pdata->degfac = degfac;
            pdata->ipoffset = ipoffset;

/* Submit the job for execution by the next available thread. */
            thrAddJob( wf, 0, pdata, smf1_subip, 0, NULL, status );
         }

/* Wait for all jobs to complete. */
         thrWait( wf, status );

/* Free resources. */
         ipang = astFree( ipang );
      }

/* Remove any extinction correction to the modifed bolometer data. */
      if( dat->ext ) smf_calcmodel_ext( wf, dat, 0, keymap, dat->ext,
                                        SMF__DIMM_INVERT, status);
/* Free resources. */
      imapdata = astFree( imapdata );
      job_data = astFree( job_data );
      trsys = astFree( (void *) trsys );
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
   const char *trsys;
   const double *fx;
   const double *fy;
   const double *gy;
   const double *gx;
   dim_t ibolo;
   dim_t itime;
   dim_t nbolo;
   dim_t ntslice;
   double *imapdata;
   double *pa;
   double *pr;
   double angle;
   double ca;
   double cb;
   double cc;
   double cd;
   double ce;
   double cf;
   double cosval;
   double degfac;
   double ipoffset;
   double ival;
   double p1;
   double qfp;
   double qtr;
   double sinval;
   double ufp;
   double utr;
   int *pl;
   int bad;
   size_t bstride;
   size_t tstride;
   smfData *data;
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
   data = pdata->data;
   trsys = pdata->trsys;
   gx = pdata->gx;
   gy = pdata->gy;
   fx = pdata->fx;
   fy = pdata->fy;
   degfac = pdata->degfac;
   ipoffset = pdata->ipoffset;

/* Subtract the IP from a range of bolometers. */
   if( pdata->oper == 1 ) {

/* Loop round each bolometer to be processed by this thread. */
      for( ibolo = pdata->b1; ibolo <= pdata->b2; ibolo++ ) {

/* Get a pointer to the first quality value for the current bolometer. */
         pq = pdata->qua_data + ibolo*bstride;

/* Check that the whole bolometer has not been flagged as bad. */
         if( !( *pq & SMF__Q_BADB ) ) {

/* Get the IP model parameters for this bolometer. */
            if( pdata->ipmodel != APR2019 ) {
               ca = pdata->ippars[0];
               cb = pdata->ippars[1];
               cc = pdata->ippars[2];
               cd = pdata->ippars[3];
               bad = ( ca == VAL__BADD || cb == VAL__BADD ||
                       cc == VAL__BADD || cd == VAL__BADD );
            } else {
               ca = pdata->ippars[0];
               cb = pdata->ippars[1];
               cc = pdata->ippars[2];
               cd = pdata->ippars[3];
               ce = pdata->ippars[4];
               cf = pdata->ippars[5];
               bad = ( ca == VAL__BADD || cb == VAL__BADD ||
                       cc == VAL__BADD || cd == VAL__BADD ||
                       ce == VAL__BADD || cf == VAL__BADD );
            }

/* If any parameter is bad, flag the whole bolometer as unusable. */
            if( bad ) {
               for( itime = 0; itime < ntslice; itime++ ) {
                  *pq |= ( SMF__Q_IP | SMF__Q_BADB );
                  pq += tstride;
               }

/* If all the IP parameters are good, we can do the IP correction. */
            } else {

/* Get pointers to the first residual (i.e. the uncorrected Q or U value)
   and lut value (i.e. the index of the map pixel that receives the Q/U
   value) for the current bolometer. */
               pr = pdata->res_data + ibolo*bstride;
               pl = pdata->lut_data + ibolo*bstride;
               pa = pdata->ipang ? pdata->ipang + ibolo*bstride : NULL;

/* Loop round each time slice, maintaining a pointer to the JCMTState
   info for the slice (we need this to get the elevation for each slice). */
               state = pdata->allstate;
               for( itime = 0; itime < ntslice; itime++,state++ ) {

/* If there is no total intensity value for the sample, flag the sample. */
                  if( *pl == VAL__BADI ) {
                     *pq |= SMF__Q_IP;

/* Get the total intensity for the sample. If it is bad, flag the sample. */
                  } else {
                     ival = imapdata[ *pl ];
                     if( ival == VAL__BADD ) {
                        *pq |= SMF__Q_IP;

/* Skip this sample if the residual is bad or flagged, if the focal
   plane orientation is undefined, or the telescope elevation is
   unknown. */
                     } else if( *pr != VAL__BADD && !( *pq & SMF__Q_MOD ) &&
                                ( !pa || *pa != VAL__BADD ) &&
                                state->tcs_az_ac2 != VAL__BADD) {

/* Find the normalised instrumental Q and U. These are with respect to the
   focal plane Y axis. */
                        if( pdata->ipmodel != APR2019 ) {
                           p1 = ca + cb*state->tcs_az_ac2 + cc*state->tcs_az_ac2*state->tcs_az_ac2;
                           p1 += ipoffset;
                           angle = -2*( state->tcs_az_ac2 - cd );
                           qfp = p1*cos( angle );
                           ufp = p1*sin( angle );
                        } else {
                           angle = -2*( state->tcs_az_ac2 - cc );
                           qfp = ca + cb*cos( angle );
                           ufp = cf + cb*sin( angle );
                           angle = -2*( state->tcs_az_ac2 - ce );
                           qfp += cd*cos( angle );
                           ufp += cd*sin( angle );
                        }

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

/* Correct the residual Q or U value. If the total intensity was derived from
   a non-POL2 observation we need to correct the total intensity for the
   POL2 degradation factor. */
                        if( *qu == 'Q' ) {
                           *pr -= ival*qtr/degfac;
                        } else {
                           *pr -= ival*utr/degfac;
                        }
                     }
                  }

/* Move onto the next time slice. */
                  pq += tstride;
                  pr += tstride;
                  pl += tstride;
                  if( pa ) pa += tstride;
               }
            }
         }
      }

/* Calculate the reference direction for a range of time slices. */
   } else if( pdata->oper == 2 ) {

      AstFrameSet *wcs;
      AstMapping *g2s;
      AstMapping *s2f;
      double *fx2;
      double *fy2;
      double *pr;
      double *sx;
      double *sy;

/* Allocate arrays to hold the sky coords for every bolometer. */
      sx = astMalloc( nbolo*sizeof( *sx ) );
      sy = astMalloc( nbolo*sizeof( *sy ) );

/* Allocate arrays to hold the focal plane coords of a point slightly to
   the north of every bolometer. */
      fx2 = astMalloc( nbolo*sizeof( *fx2 ) );
      fy2 = astMalloc( nbolo*sizeof( *fy2 ) );
      if( *status == SAI__OK ) {

/* Loop over all time slices. */
         for( itime = pdata->t1; itime <= pdata->t2; itime++ ) {
            pr = pdata->result + itime*pdata->tstride;

/* Get the WCS FrameSet for the time slice. Use a mutex to prevent multiple
   threads writing to the header at the same time. Take a deep copy of
   the FrameSet so that we can releasae the smfData immediately. */
            thrMutexLock( &data_mutex, status );
            smf_lock_data( data, 1, status );
            smf_tslice_ast( data, itime, 1, NO_FTS, status );
            wcs = data->hdr->wcs;
            if( wcs ) wcs = astCopy( wcs );
            smf_lock_data( data, 0, status );
            thrMutexUnlock( &data_mutex, status );

/* Get the WCS FrameSet for the time slice, and set its current Frame to the
   frame used as the reference by the Q/U bolometer values. */
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
                     *pr = atan2( fx[ibolo] - fx2[ibolo], fy2[ibolo] - fy[ibolo] );
                  } else {
                     *pr = VAL__BADD;
                  }

                  pr += pdata->bstride;
               }

            } else {
               for( ibolo = 0; ibolo < nbolo; ibolo++ ) {
                  *pr = VAL__BADD;
                   pr += pdata->bstride;
               }
            }
         }
      }

/* Free resources. */
      fx2 = astFree( fx2 );
      fy2 = astFree( fy2 );
      sx = astFree( sx );
      sy = astFree( sy );

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "smf_subip: Illegal operation %d", status, pdata->oper );
   }
}











/* Returns an array holding the angle (rad.s) from the reference
   direction used by the Q/U bolometer values to focal plane Y,
   measured positive in the sense of rotation from focal plane Y to focal
   plane X, for every bolometer sample in a smfData. The returned
   array should be freed using astFree when no longer needed. */
static double *smf1_calcang( ThrWorkForce *wf, smfData *data, const char *trsys,
                             int *status ){

/* Local Variables: */
   AstFrameSet *fpfset;
   SmfSubIPData *job_data = NULL;
   SmfSubIPData *pdata;
   dim_t ibolo;
   dim_t nbolo;
   dim_t ncol;
   dim_t ntslice;
   double *fx;
   double *fy;
   double *gx;
   double *gy;
   double *result;
   int iw;
   int nw;
   int subsysnum;
   size_t tstep;
   size_t bstride;
   size_t tstride;

/* Check the inherited status. Also return NULL if the Q/U values are
   already referenced to the focal plane Y axis (i.e. all returned angles
   would be zero). */
   if( *status != SAI__OK || !trsys ) return NULL;

/* Get the number of bolometers and time slices, together with the strides
   between adjacent bolometers and adjacent time slices. */
   smf_get_dims( data,  NULL, &ncol, &nbolo, &ntslice, NULL, &bstride,
                 &tstride, status );

/* Allocate the returned array. */
   result = astMalloc( nbolo*ntslice*sizeof( *result ) );

/* Allocate arrays to hold the grid coords for every bolometer. */
   gx = astMalloc( nbolo*sizeof( *gx ) );
   gy = astMalloc( nbolo*sizeof( *gy ) );

/* Allocate arrays to hold the focal plane coords for every bolometer. */
   fx = astMalloc( nbolo*sizeof( *fx ) );
   fy = astMalloc( nbolo*sizeof( *fy ) );

/* Create structures used to pass information to the worker threads. */
   nw = wf ? wf->nworker : 1;
   job_data = astMalloc( nw*sizeof( *job_data ) );

/* Check the pointers can be used safely. */
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

/* Unlock the smfData so that it can be locked by the threaded code. */
      smf_lock_data( data, 0, status );

/* See how many time slices to process in each thread. */
      tstep = ntslice/nw;
      if( tstep == 0 ) tstep = 1;

/* Create jobs to get the angles for a range of time slices. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;

/* Set the range of timeslices (t1 to t2) to be processed by the current
   job. */
         pdata->t1 = iw*tstep;
         if( iw < nw - 1 ) {
            pdata->t2 = pdata->t1 + tstep - 1;
         } else {
            pdata->t2 = ntslice - 1;
         }

/* Store the other info needed by the worker thread. */
         pdata->nbolo = nbolo;
         pdata->result = result;
         pdata->oper = 2;
         pdata->data = data;
         pdata->trsys = trsys;
         pdata->gx = gx;
         pdata->gy = gy;
         pdata->fx = fx;
         pdata->fy = fy;
         pdata->bstride = bstride;
         pdata->tstride = tstride;

/* Submit the job for execution by the next available thread. */
         thrAddJob( wf, 0, pdata, smf1_subip, 0, NULL, status );
      }

/* Wait for all jobs to complete. */
      thrWait( wf, status );

/* Lock the smfData so that it can be used by subsequent code in the main
   thread. */
      smf_lock_data( data, 1, status );
   }

/* Free resources. */
   gx = astFree( gx );
   gy = astFree( gy );
   fx = astFree( fx );
   fy = astFree( fy );
   job_data = astFree( job_data );

/* Return the array of angle values. */
   return result;

}











