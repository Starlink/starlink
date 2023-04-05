/* Defining the FPTRAP macro will cause floating point exceptions to
   occur whenever a NaN, inf or overflow is generated. This can make it
   easier to debug the cause of these values. */
#if defined(FPTRAP)
#define _GNU_SOURCE
#include <fenv.h>
#endif

/*
*+
*  Name:
*     POL2IPCOR

*  Purpose:
*     Create an IP model from a set of POL2 observations of a bright
*     extended source.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     smurf_pol2ipcor( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine determines a correction for Instrumental Polarisation
*     (IP) from a set of I, Q and U maps for a single field. At least nine
*     POL2 observations of the same field at the same wavelength must be
*     supplied, covering as wide a range in elevation as possible and all
*     using the same pixel grid. The maps must NOT have been produced using
*     smurf:skyloop. The correction may then be applied to the supplied Q
*     and U maps to create a set of IP-corrected output maps. The parameters
*     of the IP correction are reported.
*
*     IP correction may already have been applied to the supplied Q and U
*     map (for instance, within makemap or pol2map), in which case this
*     routine will in effect determine a secondary IP correction that aims
*     to remove any residual elevation-dependence left over by the earlier
*     primary IP correction.
*
*     The IP model measured and applied by this routine can take one of
*     two forms (selected using the MODELTYPE parameter). Firstly, the
*     "single component" form:
*
*     p = A + B*el + C*el*el
*     Qn_fp = p*cos( -2*( el - D ) )
*     Un_fp = p*sin( -2*( el - D ) )
*
*     where "el" is elevation (in radians), "p" is the fractional polarisation
*     due to IP, (Qn_fp,Un_fp) are normalised Q and U corrections with respect
*     to the focal plane Y axis, and (A,B,C,D) are the parameters of the model
*     determined from the supplied input maps as described below.
*
*     Secondly, the "double component" form:
*
*     Qn_fp = A + B*cos( -2*( el - C ) ) + D*cos( -2*( el - E ) )
*     Un_fp = F + B*sin( -2*( el - C ) ) + D*sin( -2*( el - E ) )
*
*     This form has two extra free parameters (E and F) compared to the
*     single component model.
*
*     For both model forms, the corrected output Q and U values are then given by:
*
*     Q_out = Q_in - Qn_tr*I_in
*     U_out = U_in - Un_tr*I_in
*
*     where (I_in,Q_in,U_in) are the input I, Q and U values with
*     respect to tracking north (e.g. Declination) and (Qn_tr,Un_tr) are
*     the normalised Q and U corrections with respect to tracking north.
*     These are determined by rotating the (Qn_fp,Un_fp) vector as follows:
*
*     Qn_tr = cos( 2*alpha )*Qn_fp - sin( 2*alpha )*Un_fp
*     Un_tr = sin( 2*alpha )*Qn_fp + cos( 2*alpha )*Un_fp
*
*     where alpha is the angle from tracking north to the focal plane Y
*     axis, in sense of North through East, at the central epoch of the
*     observation.
*
*     The (Q,U) values in the input maps are given with respect to
*     tracking north. So if these maps had already been correct for IP
*     with a pefect IP model, then in the absence of noise the (Q,U) of
*     an astronomical source should be constant for all  observations
*     regardless of elevation (assuming the source is not variable).
*     If the IP correction is not perfect, the (Q,U) measured in each
*     observation will be offset away from the 'true' values by offsets that
*     vary with elevation and azimuth. Thus if the (Q,U) values at a single
*     point on the sky are plotted as a scatter plot, any imperfection in
*     the IP model will be revealed by the points for different observations
*     being distributed along an arc of some centro-symetric shape centred
*     on the true (Q,U), with azimuth varying with distance along the arc.
*     The above argument relies on the input (Q,U) values using tracking north
*     as the reference direction. For instance, if they were instead to use
*     the focal plane Y axis as the reference direction, then the true
*     astronomical (Q,U) would not be constant (due to sky rotation) but
*     would itself form some arc of a circle centred on the origin of the
*     (Q,U) plane.
*
*     In practice, a separate circle is fitted to the input data at every
*     point on the sky that is within both the AST and PCA masks. At a single
*     point, each observation defines one position in the (Q,U) plane, and
*     the best fitting circle passing through the (Q,U) positions for all
*     observations is found. The centre of the circle defines the best estimate
*     of the true astronomical (Q,U), and the offsets from the centre to each
*     observation's (Q,U) position is a measure of the IP. Note, all reference
*     to (Q,U) above refer to normalised (Q,U). The IP defined by each measured
*     (Q,U) position is then rotated to use focal plane Y axis as the
*     polarimetric reference direction instead of tracking north.
*
*     The above process, taken over all pixels in the AST and PCA masks, will
*     typically produce many estimates of (Qn_fp,Un_fp) over a range of
*     elevations. The parameters of the IP model (A,B,C,D) are then found
*     by doing a weighted least squares fit to these (Qn_fp,Un_fp) values.
*     The weighting function gives greater weight to pixels for which
*     the residuals of the actual (Q,U) values from the fitted circle are
*     smaller. It also favours circles in which the azimuth of each (Q,U)
*     position is more closely correlated with its position around the circle.

*  ADAM Parameters:
*     IN = NDF (Read)
*        A group of input NDFs, each holding a 2-D map of I, Q or U for a
*        single observation. All maps must hold data for the same object at
*        the same wavelength (850 or 450), and must be aligned in pixel
*        coordinates. A complete trio of I, Q and U maps must be supplied
*        for each observation. The maps must NOT have been created by
*        smurf:skyloop. This parameter is only used if a null (!) value is
*        supplied for parameter MAPDIR. If null (!) is also supplied for
*        IN, then the fit is based solely on any data supplied via
*        parameter INLOGS. [!]
*     INLOGS = LITERAL (Read)
*        A group of existing log files created by previous runs of this
*        application. If supplied, the data in these log files is
*        included in the IP model fit. This allows the fit to be based on
*        data from multiple objects. Note, this data is NOT included in
*        the output log file specified by parameter LOGFILE. [!]
*     LOGFILE = LITERAL (Write)
*        The name of an output text file to create holding a table of
*        the values that were generated from the data supplied by
*        parameter IN or MAPDIR. These values, together with any
*        specified by parameter INLOGS, is used in the fit to determine
*        the values of model parameters (A,B,C,D). The values are stored
*        in the form of a TOPCAT "ascii" table. So if LOGFILE is set to
*        "table.asc", you can view the table using the command
*        "topcat -f ascii table.asc". [!]
*     MAPDIR = LITERAL (Write)
*        The path to a directory holding existing observation maps created
*        by the POL2MAP command (i.e. via the MAPDIR parameter). If
*        supplied, the maps in this directory are used as the input maps
*        and parameter IN is ignored. The naming convenions of
*        the POL2MAP command are assumed (i.e. auto-masked maps in
*        "*_imap.sdf", externally masked maps in "*_Imap.sdf", *_Qmap.sdf"
*        and "*_Umap.sdf"). The externally masked maps are used as the
*        input maps (see parameter IN). If a null (!) value is supplied for
*        MAPDIR, all input maps are instead obtained using parameter IN.
*     MODELFILE = LITERAL (Write)
*        The name of an output text file to create holding a table of
*        values evaluated from the fitted IP model. The file uses the
*        TOPCAT "ascii" format, and holds the fitted model parameters in
*        the header. [!]
*     MODELTYPE = LITERAL (Write)
*        Selects the mathematical form of the IP model. Can be "SINGLE"
*        (for the single component model) or "DOUBLE" (for the double
*        component model). ["SINGLE"]
*     OUT = NDF (Write)
*        An optional group of output NDFs. If null (!) is supplied, no IP
*        corrected output maps are created. If a non-null value is supplied,
*        the number of NDFs supplied must be equal to the number of input
*        NDFs supplied for parameter "IN". Each output NDF corresponding to
*        an input Q or U map receives an IP-corrected copy of the corresponding
*        input map. Output NDFs corresponding to input I maps are ignored (i.e.
*        no total intensity output NDFs are created). Thus, if the value "\*_C"
*        is supplied for parameter OUT, output Q and U maps will be created
*        with names of the form "<in>_C.sdf", where <in> is the name of the
*        corresponding input Q or U map, but no output I maps will be formed
*        (the output maps will be placed in the same directory as the input
*        maps). [!]

*  Notes:
*     - Single observation maps produced by skyloop can sometimes show IP
*     that seems to vary with total intensity. In such cases, using a
*     modified single component model in which:
*
*     p = A + B*el + C*el*el + E*total_intensity
*     q = p*cos( -2( el - D ) )
*     u = p*sin( -2( el - D ) )
*
*     results in corrected maps that show lower variation with elevation.

*  Authors:
*     DSB: David Berry (EAO)

*  History:
*     30-MAY-2019 (DSB):
*        Original version.
*     4-DEC-2019 (DSB):
*        Added support for Pierre's double component model.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/


#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Define _GNU_SOURCE to get qsort_r */
#define _GNU_SOURCE

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>

/* STARLINK includes */
#include "ast.h"
#include "mers.h"
#include "par.h"
#include "par_par.h"
#include "prm_par.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/hds.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/atl.h"
#include "star/kaplibs.h"
#include "gsl/gsl_multimin.h"

/* SMURF includes */
#include "smurf_par.h"
#include "smurflib.h"
#include "libsmf/smf.h"

/* Local constants */
#define LOGCOLS "# iobs px py I Qtr Utr dQfp dUfp Qctr Uctr el az rad wgt beta rms cor fang"
#define LOGCOLCNT 18
#define FUNC_NAME "smurf_pol2ipcor"
#define TASK_NAME "POL2IPCOR"
#define S8 0
#define S4 1
#define NONE 2
#define MXBINSIZE 50
#define SINGLE_COMP 1
#define DOUBLE_COMP 2

/* Local data types */
typedef struct SmfPol2IpcorData {
   AstKeyMap *km;
   const int *binsize;
   const int *indx;
   const dim_t *lbnd;
   const dim_t *ubnd;
   double *beta;
   double *cor;
   double *dq;
   double *du;
   double *i;
   double *qc;
   double *qtr;
   double *rad;
   double *rms;
   double *uc;
   double *utr;
   double *wgt;
   dim_t *px;
   dim_t *py;
   int *obs;
   int nbin;
   int nobs;
   int operation;
   dim_t r1;
   dim_t r2;
   int wave;
   dim_t ngood;
   dim_t npix_good;
} SmfPol2IpcorData;

typedef struct Params {
   const double *ae;    /* Array of elevation values (radians) */
   const double *aq;    /* Array of normalised Q values */
   const double *au;    /* Array of normalised U values */
   const double *aw;    /* Array of weights */
   double sw;           /* Sum of the weights */
   dim_t n;            /* Length of above arrays */
   int imodel;          /* The type of model to fit */
} Params;



/* Prototypes for local functions. */
static double smf1_f( const gsl_vector *v, void *pars );
static int smf1_madebyskyloop( int indf, int *status );
#ifdef HAVE_QSORT_R_BSD
static int smf1_qsort_bsd( void *data, const void *a, const void *d );
#endif
static int smf1_qsort( const void *a, const void *d, void *data );
static void smf1_worker( void *job_data_ptr, int *status );
static void smf1_pol2ipcor( ThrWorkForce *wf, int model, AstKeyMap *km, Grp *igrp3,
                           const dim_t *lbnd,
                           const dim_t *ubnd, const int *indx, const int *binsize,
                           int nbin, int wave, FILE *fd1, FILE *fd2,
                           const double *coslist, const double *sinlist,
                           const double *ellist, const double *alist,
                           double ippars[6], int *status );
static void smf1_ipfit( int model, dim_t n, const double *q, const double *u,
                        const double *w, const double *e,
                        int iwave, double par[6], int *status );
static void smf1_df( const gsl_vector *v, void *params, gsl_vector *df );
static void smf1_fdf( const gsl_vector *v, void *params, double *f, gsl_vector *df );
static void smf1_linfit( dim_t n, const int *xindex, const double *wlist,
                         const double *xlist, const double *ylist, double *slope,
                         double *offset, double *rms, double *cor, int *status );
static void smf1_reject( int binsize, double *vals, int *status );
static void smf1_logread( const char *path, dim_t *n, double **q, double **u,
                          double **w, double **e, int *iwave, int *status );
static void smf1_circle_fitter( int nobs, double *qn, double *un,
                                double *unw, double *qnw, int nbin,
                                const int *binsize, const int *indx,
                                const double *alist, double *qlist,
                                double *qwlist, double *ulist,
                                double *uwlist, double *malist,
                                double *rad, double *uc, double *qc,
                                double *rms, double *sigma, int *status );

/* The target number of azimuth bins */
#define NBIN 5

/* The maximum number of azimuth bins */
#define MAXBIN 40

/* Main entry */
void smurf_pol2ipcor( int *status ) {

/* Local Variables */
   AstFitsChan *fc;
   AstKeyMap *km;
   AstKeyMap *obskm;
   AstTimeFrame *tf;
   FILE *fd1;
   FILE *fd2;
   Grp *igrp1 = NULL;
   Grp *igrp2 = NULL;
   Grp *igrp3 = NULL;
   ThrWorkForce *wf = NULL;
   char *cval;
   char *pname;
   char buf[80];
   char filepath[ GRP__SZFNM + 1 ];
   char grpexp[ GRP__SZGEX + 1 ];
   char label[70];
   char log[ GRP__SZFNM + 1 ];
   char mapdir[ GRP__SZFNM + 1 ];
   char modelfile[ GRP__SZFNM + 1 ];
   char modeltype[ 10 ];
   char object[80];
   char obsid[ GRP__SZNAM ];;
   const char *key;
   const char *obj;
   const char *wave;
   dim_t lbnd[ NDF__MXDIM ];
   dim_t ubnd[ NDF__MXDIM ];
   double *alist;
   double *coslist;
   double *ellist;
   double *pid;
   double *piv;
   double *pqd;
   double *pqv;
   double *pud;
   double *puv;
   double *sinlist;
   double a0;
   double a;
   double alpha;
   double angle;
   double az0;
   double az;
   double azp;
   double b0;
   double b;
   double cosval;
   double el;
   double end;
   double epoch;
   double ippars[6];
   double p;
   double qncor;
   double qncor_fp;
   double sinval;
   double start;
   double uncor;
   double uncor_fp;
   double wvm;
   int *indx;
   int binsize[ MAXBIN ];
   int flag;
   int ibin = 0;
   int i;
   int ifile;
   int imodel;
   int indf2;
   int indf;
   int iwave;
   int ndims;
   int need_epochs;
   int nobs;
   int obs;
   int obsstep;
   int qndf2;
   int qndf;
   int there;
   int thiswindblind = 0;
   int tndf;
   int undf2;
   int undf;
   int utdate;
   int verb;
   int windblind;
   size_t iel;
   size_t isize1;
   size_t isize3;
   size_t nel;
   size_t outsize;
   void *pntrs[6];

#if defined(FPTRAP)
   feenableexcept(FE_DIVBYZERO|FE_INVALID|FE_OVERFLOW);
#endif

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Begin an AST context */
   astBegin;

/* Begin an NDF context. */
   ndfBegin();

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( SMF__THREADS, status ), status );

/* Get the type of modelt to use - single component or double component. */
   parChoic( "MODELTYPE", "SINGLE", "SINGLE,DOUBLE", 1, modeltype,
             sizeof(modeltype), status );
   imodel = strcmp( modeltype, "SINGLE" ) ? DOUBLE_COMP : SINGLE_COMP;

/* Create a UTC TimeFrame that represents time as Julian epoch. It is used
   for converting the values of DATE-OBS and DATE-END FITS keywords to
   Julian epochs that can be assigned to the SkyFrame Epoch attribute.
   This defines the mapping from (RA,Dec) to (Az,El). */
   tf = astTimeFrame( "System=JEPOCH,TimeScale=UTC" );

/* Create a KeyMap to hold information about all observations. */
   km = astKeyMap( "SortBy=KeyUp" );

/* If a value is supplied for parameter MAPDIR, the input maps will be
   located in the supplied directory and will use the naming conventions
   of the pol2map command. Create a group holding the externally masked I,
   Q and U maps. */
   if( *status == SAI__OK ) {
      parGet0c( "MAPDIR", mapdir, sizeof(mapdir), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
      } else {
         verb = ( getenv( "SMURF_VERBOSE" ) != NULL );
         igrp1 = grpNew( "IN", status );
         snprintf( grpexp, sizeof(grpexp), "%s/*_Imap", mapdir );
         ndgAsexp( grpexp, verb, GRP__NOID, &igrp1, &isize1, &flag, status );
         snprintf( grpexp, sizeof(grpexp), "%s/*_Qmap", mapdir );
         ndgAsexp( grpexp, verb, GRP__NOID, &igrp1, &isize1, &flag, status );
         snprintf( grpexp, sizeof(grpexp), "%s/*_Umap", mapdir );
         ndgAsexp( grpexp, verb, GRP__NOID, &igrp1, &isize1, &flag, status );

         if( isize1 == 0 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( " ", "No usable files found in MAPDIR directory ('%s')"
                     " - does the directory exist?", status, mapdir );
         }

         if( isize1 < 9 && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( " ", "Fewer than 9 observations found in MAPDIR directory "
                     "('%s') - this application requires at least 9 observations",
                     status, mapdir );
         }
      }
   }

/* If MAPDIR was not supplied, get a group holding the names of the
   externally masked input maps to use (must be at least 27 of them).
   Annull any null parameter error. */
   if( !igrp1 && *status == SAI__OK ) {
      kpg1Rgndf( "IN", 0, 27, "Must provide I/Q/U maps for at least 9 "
                 "observations", &igrp1, &isize1, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         isize1 = 0;
      }
   }

/* Get any input log files from previous runs of pol2ipcor. The data in
   these log files is included in the fit. */
   if( *status == SAI__OK ) {
      kpg1Gtgrp( "INLOGS", &igrp3, &isize3, status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         isize3 = 0;
         if( igrp3 ) grpDelet( &igrp3, status);
      }
   }

/* Assume we will be able to find the centre epoch from the above
   externally masked maps for every supplied observation. */
   need_epochs = 0;

/* Loop round all externally-masked input maps. This loop gets an identifier
   for each NDF, checks that it looks suitable for this application, and
   stores them in a KeyMap, keyed by observation and Stokes parameter. */
   wave = NULL;
   iwave = NONE;
   *object = 0;
   windblind = -1;
   for( i = 1; i <= (int) isize1 && *status == SAI__OK; i++ ) {

/* Get the NDF path from the group. */
      pname = filepath;
      grpGet( igrp1, i, 1, &pname, sizeof(filepath), status );

/* Open the NDF and get an identifier for it. */
      ndgNdfas( igrp1, i, "READ", &indf, status );

/* Report an error if it was created by skyloop. */
      if( smf1_madebyskyloop( indf, status ) && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( "", "It looks like '%s' was made by skyloop.", status, filepath );
         errRepf( "", "This application cannot use maps mnade by skyloop.", status );
         break;
      }

/* If this is the first NDF, get the pixel bounds of the NDF. */
      if( i == 1 ) {
         ndfBound( indf, NDF__MXDIM, lbnd, ubnd, &ndims, status );

/* Check the data is 2 dimensional, or 3 dimensional with a degenerate 3rd
   axis, it's a map. */
         if( ndims != 2 && !( ndims == 3 && lbnd[2] == 1 && ubnd[2] == 1 ) &&
             *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf("","It looks like '%s' is not a 2-D map.", status, filepath );
            break;
         }

/* If this is not the first NDF, get a section of it that matches the
   first NDF. */
      } else {
         ndfSect( indf, ndims, lbnd, ubnd, &indf2, status );
         ndfAnnul( &indf, status );
         indf = indf2;
      }

/* Get a FitsChan holding the contents of the FITS extension. */
      ndfXstat( indf, "FITS", &there, status );
      if( !there && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf("","No FITS extension found in '%s'.", status, filepath );
         break;
      }
      kpgGtfts( indf, &fc, status );

/* Check the INBEAM header exists and is "pol" (case insensitive). */
      if( ( !astTestFits( fc, "INBEAM", NULL ) ||
            !astGetFitsS( fc, "INBEAM", &cval ) ||
            strncmp( cval, "pol", 3 ) ) && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf("","It looks like '%s' does not hold POL2 data.", status, filepath );
         break;
      }

/* Check it has a Label of Q, U, or I. */
      ndfCget( indf, "Label", label, sizeof(label), status );
      if( strcmp( label, "Q" ) && strcmp( label, "U" ) &&
          strcmp( label, "I" ) && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( "","'%s' has an unexpected Label '%s' - should be 'I', "
                  "'Q' or 'U'.", status, filepath, label );
         break;
      }

/* Get the waveband. Report an error if any input map has a different
   waveband to the previous input maps. */
      cval = NULL;
      astGetFitsS( fc, "FILTER", &cval );
      if( cval && !strncmp( cval, "850", 3 ) ) {
         if( wave ){
            if( strcmp( wave, "S8" ) && *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRepf("","'%s' holds 850 um data but the previous files "
                       "hold 450 um data.", status, filepath );
               break;
            }
         } else {
            wave = "S8";
            iwave = S8;
         }
      } else if( cval && !strncmp( cval, "450", 3 ) ) {
         if( wave ){
            if( strcmp( wave, "S4" ) && *status == SAI__OK ) {
               *status = SAI__ERROR;
               errRepf("","'%s' holds 450 um data but the previous files "
                       "hold 850 um data.", status, filepath );
               break;
            }
         } else {
            wave = "S4";
            iwave = S4;
         }
      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf("","Unsupported FILTER header value "
                    "'%s' found in %s.", status, cval, filepath );
         break;
      }

/* Check for the presence of the wind-blind. Report an error if any input
   map is different to the previous input maps. */
      if( astGetFitsS( fc, "WND_BLND", &cval ) ) {
         if( *status == SAI__OK ) {
            thiswindblind = strcmp( "NONE", cval ) ? 1 : 0;
         }
      } else {
         thiswindblind = 1;
      }

      if( windblind == -1 ) {
         windblind = thiswindblind;
      } else if( windblind != thiswindblind && *status == SAI__OK ) {
         *status = SAI__ERROR;
         if( windblind ) {
            errRepf( "","'%s' was taken without the wind-blind in place, "
                     "but previous maps were taken with the wind-blind "
                     "in place.", status, filepath );
         } else {
            errRepf( "","'%s' was taken with the wind-blind in place, "
                     "but previous maps were taken without the wind-blind "
                     "in place.", status, filepath );
         }
      }

/* Get the mean WVM Tau  */
      if( astGetFitsF( fc, "WVMTAUST", &start ) &&
          astGetFitsF( fc, "WVMTAUEN", &end ) ) {
         wvm = 0.5*( start + end );
      } else if( *status == SAI__OK ) {
         errRepf( " ", "%s: Cannot get WVM TAU data from header.",
                  status, filepath );
      }

/* See if the DATE-OBS and DATE-END keywords are present (they may not be
   present if the map was created using skyloop). If so, get the Julian
   epoch at the centre of the observation. */
      if( astTestFits( fc, "DATE-OBS", NULL ) &&
          astTestFits( fc, "DATE-END", NULL ) ) {

         astGetFitsS( fc, "DATE-OBS", &cval );
         if( astUnformat( tf, 1, cval, &start ) != (int) strlen( cval ) &&
             *status == SAI__OK ) {
            errRepf( " ", "%s: Error converting DATE-OBS value '%s' to a "
                     "Julian epoch.", status, filepath, cval );
         }

         astGetFitsS( fc, "DATE-END", &cval );
         if( astUnformat( tf, 1, cval, &end ) != (int) strlen( cval ) &&
             *status == SAI__OK ) {
            errRepf( " ", "%s: Error converting DATE-END value '%s' to a "
                     "Julian epoch.", status, filepath, cval );
         }

         epoch = 0.5*( start + end );

/* If no dates were found, indicate that we need to get the epohcs from
   somewhere else (i.e. from the auto-masked maps). */
      } else {
         epoch = VAL__BADD;
         need_epochs = 1;
      }

/* Form an identifier for the observation, of the form "20190112_00012". */
      astGetFitsI( fc, "UTDATE", &utdate );
      astGetFitsI( fc, "OBSNUM", &obs );
      sprintf( obsid, "%8.8d_%5.5d", utdate, obs );

/* If the main KeyMap already contains a sub-KeyMap for this observation,
   get a pointer to it. Otherwise, create a KeyMap to hold information about
   this observation, and add it into the KeyMap for all observations, using
   the above identifier as the key. */
      if( !astMapGet0A( km, obsid, &obskm ) ){
         obskm = astKeyMap( " " );
         astMapPut0A( km, obsid, obskm, NULL );
      }

/* Store information in the observation KeyMap. The FITS extensions for
   I, Q and U should be equivalent, so just store the first. */
      astMapPut0C( obskm, "OBSID", obsid, NULL );
      sprintf( buf, "%s_NDF", label );
      if( !astMapHasKey( obskm, buf ) ) {
         astMapPut0I( obskm, buf, indf, NULL );
      } else if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( " ", "More than one %s map supplied for observation %s.",
                  status, label, obsid );
      }

      if( !astMapHasKey( obskm, "FITS" ) ) astMapPut0A( obskm, "FITS", fc, NULL );
      if( !astMapHasKey( obskm, "WVM" ) ) astMapPut0D( obskm, "WVM", wvm, NULL );
      if( epoch != VAL__BADD && !astMapHasKey( obskm, "EPOCH" ) ) {
         astMapPut0D( obskm, "EPOCH", epoch, NULL );
      }
      sprintf( buf, "%s_IFILE", label );
      astMapPut0I( obskm, buf, i, NULL );
      astGetFitsS( fc, "OBJECT", &cval );
      astMapPut0C( obskm, "OBJECT", cval, NULL );

/* Annul AST object pointers created in the loop. */
      obskm = astAnnul( obskm );
      fc = astAnnul( fc );
   }

/* If we still do not have the centre epoch for one or more observations
   (e.g. because the maps supplied for parameter IN were created by skyloop,
   which may not record the start and end dates in the FITS extension),
   then report an error. We could maybe at some later point support
   skyloop maps with a modified IP model that includes a total-intensity
   term (see Notes: in prologue). In which case we would need to get the
   epochs from the auto-masked maps.  */
   if( need_epochs && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( " ", "No EPOCH headers found in the supplied maps. Were "
              "they created using SKYLOOP?", status );
      errRep( " ", "This application cannot use maps created by SKYLOOP.",
              status );
   }

/* Get the name of any log file to create. */
   if( *status == SAI__OK ) {
      parGet0c( "LOGFILE", log, sizeof(log), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         *log = 0;
      }
   }

/* Get the name of any model file to create. */
   if( *status == SAI__OK ) {
      parGet0c( "MODELFILE", modelfile, sizeof(modelfile), status );
      if( *status == PAR__NULL ) {
         errAnnul( status );
         *modelfile = 0;
      }
   }

/* How many observations do we have? */
   nobs = astMapSize( km );

/* Allocate arrays to hold the central azimuth and elevation values (in rads)
   in each observation. Also allocate other work arrays. */
   alist = astMalloc( nobs*sizeof(*alist) );
   ellist = astMalloc( nobs*sizeof(*ellist) );
   coslist = astMalloc( nobs*sizeof(*coslist) );
   sinlist = astMalloc( nobs*sizeof(*sinlist) );

   msgOutif( MSG__DEBUG, " ", "Mapping supplied NDFs...", status );

/* Loop round each observation. */
   az0 = 0.0;
   for( i = 0; i < (int) nobs && *status == SAI__OK; i++ ) {
      msgOutiff( MSG__DEBUG, " ", "Observation %d:\n", status, i );

/* Get the KeyMap holding information about all the input NDFs related to the
   current observation. */
      key = astMapKey( km, i );
      if( key ) strcpy( obsid, key );
      astMapGet0A( km, obsid, &obskm );
      msgOutiff( MSG__DEBUG, " ", "   id: %s\n", status, obsid );

/* Get the NDF identifiers for the I, Q and U for the observation. */
      if( !astMapGet0I( obskm, "I_NDF", &indf ) ) {
         *status = SAI__ERROR;
         errRepf( " ","No I map supplied for observation %s.", status,
                  obsid );

      } else if( !astMapGet0I( obskm, "Q_NDF", &qndf ) ) {
         *status = SAI__ERROR;
         errRepf( " ","No Q map supplied for observation %s.", status,
                  obsid );

      } else if( !astMapGet0I( obskm, "U_NDF", &undf ) ) {
         *status = SAI__ERROR;
         errRepf( " ","No U map supplied for observation %s.", status,
                  obsid );
      }

/* Retrieve the centre Epoch of the observation, as a Julian epoch. */
      astMapGet0D( obskm, "EPOCH", &epoch );
      msgOutiff( MSG__DEBUG, " ", "   epoch: %.20g\n", status, epoch );

/* Retrieve the FitsChan holding the headers for the observation. */
      astMapGet0A( obskm, "FITS", &fc );

/* Get header information required for each observation - 1) the mean angle
   from the polarimetric reference direction to the focal plane Y axis, in
   sense of North through East (alpha), 2) the mean elevation (el),
   3) the WCS (current Frame) position at pixel coords (0.0,0.0). */
      smf_polang( indf, fc, epoch, &alpha, &el, &az, &a, &b, status );

      msgOutiff( MSG__DEBUG, " ", "   elevation: %g\n", status, el*AST__DR2D );
      msgOutiff( MSG__DEBUG, " ", "   azimuth: %g\n", status, az*AST__DR2D );
      msgOutiff( MSG__DEBUG, " ", "   North->FPy: %g\n", status, alpha*AST__DR2D );

/* Modify the azimuth so that it is within +/- PI of the azimuth of the
   first observation. This gets rid of any discontinuity in values caused
   by the 0-360 deg wrap-around. */
      if( i == 0 ) {
         az0 = az;
      } else {
         if( az > az0 + AST__DPI ) {
            az -= 2*AST__DPI;
         } else if( az < az0 - AST__DPI ) {
            az += 2*AST__DPI;
         }
      }

/* Store this info in the observation's KeyMap and in the work arrays. */
      astMapPut0D( obskm, "ALPHA", alpha, NULL );
      astMapPut0D( obskm, "EL", el, NULL );
      astMapPut0D( obskm, "AZ", az, NULL );

      alist[ i ] = az;
      ellist[ i ] = el;
      coslist[ i ] = cos( 2*alpha );
      sinlist[ i ] = sin( 2*alpha );

/* Check that the input maps are aligned. If this is the first observation,
   record the sky position of pixel (0,0). Otherwise, check that the sky
   position of pixel (0,0) is the same as for the first observation to
   within 2 arc-seconds. */
      if( i == 0 ) {
         a0 = a;
         b0 = b;
      } else if( fabs( a - a0 ) > (2.0/3600.0)*AST__DD2R ||
                 fabs( b - b0 ) > (2.0/3600.0)*AST__DD2R ) {
         if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRepf( " ", "Maps for observation %s are not aligned with "
                     "previous maps.", status, obsid );
            break;
         }
      }

/* Set the badbits value in each NDF to mask out the background regions,
   defined by all the masks created by makemap (AST,PCA etc). */
      ndfSbb( 255, indf, status );
      ndfSbb( 255, qndf, status );
      ndfSbb( 255, undf, status );

/* Map the data and variance values in each NDF, and store the pointers
   in the observation KeyMap. */
      ndfMap( indf, "DATA,VARIANCE", "_DOUBLE", "READ", pntrs, &nel, status );
      ndfMap( qndf, "DATA,VARIANCE", "_DOUBLE", "READ", pntrs + 2, &nel, status );
      ndfMap( undf, "DATA,VARIANCE", "_DOUBLE", "READ", pntrs + 4, &nel, status );
      astMapPut1P( obskm, "PNTRS", 6, pntrs, NULL );

/* Tell the user about the observation. */
      astMapGet0C( obskm, "OBJECT", &obj );
      msgOutf( " ", "Using (I,Q,U) maps from observation %s (%s - %d um)",
               status, obsid, obj, (iwave==S4)?450:850 );

/* Annul AST object pointers created in the loop. */
      obskm = astAnnul( obskm );
      fc = astAnnul( fc );
   }

/* Produce an index to the observations that observations them into increasing
   azimuth. The observations are stored in numerical order of "UT_OBS"
   within "km". */
   indx = astMalloc( nobs*sizeof(*indx) );
   if( *status == SAI__OK && nobs ) {
      for( i = 0; i < nobs; i++ ) indx[ i ] = i;

#ifdef HAVE_QSORT_R_BSD
      qsort_r( indx, nobs, sizeof(*indx), alist, smf1_qsort_bsd );
#else
      qsort_r( indx, nobs, sizeof(*indx), smf1_qsort, alist );
#endif

   }

/* Get the maximum number of observations in one azimuth bin. */
   obsstep = nobs/NBIN;
   if( obsstep == 0 ) obsstep = 1;

/* Assign each observation to an azimuth bin. Each bin can have no more
   than "obsstep" entries, and must cover no more than 45 degrees in
   azimuth. First assign the lowest azimuth to bin zero. */
   if( *status == SAI__OK && nobs ){
      ibin = 0;                          /* Index of current bin */
      binsize[ 0 ] = 1;                  /* Population in current bin */
      az = alist[ indx[ 0 ] ]            /* Highest azimuth allowed in current bin */
                           + 45.0*AST__DD2R;
      azp = 0.0;                         /* Highest azimuth allowed in previous bin */

/* Then loop round all remaining azimuth values, using the "indx" array
   to access them in increasing order. */
      for( i = 1; i < nobs; i++ ) {

/* If the current azimuth is more than 45 degrees above the lowest
   azimuth value in the bin, or if the current bin is full, start a new
   bin using the current azimuth value as the first value in the bin. */
         if( alist[ indx[ i ] ] > az || binsize[ ibin ] == obsstep ){

/* If the current bin has a single entry it must be followed by a big gap
   in azimuth. If the single entry is within the azimuth range of the previous
   bin, then move it into the previous bin (this means the previous bin will
   have "obsstep+1" entries, but that is better than having a single isolated
   entry). */
            if( binsize[ ibin ] == 1 && alist[ indx[ i-1 ] ] < azp ) {
               binsize[ --ibin ]++;
            }

/* Start a new bin (unless we have now got too many bins), putting the current
   azimuth value into it. */
            if( ++ibin < MAXBIN ){
               binsize[ ibin ] = 1;
               azp = az;
               az = alist[ indx[ i ] ] + 45.0*AST__DD2R;

            } else if( *status == SAI__OK ){
               *status = SAI__ERROR;
               errRep( " ", "Too many azimuth bins.", status );
               break;
            }

/* Otherwise, increment the population of the current bin. */
         } else {
            binsize[ ibin ]++;
         }
      }

/* If the final bin has only a single entry, and it is within the azimuth
   range of the penultimate bin, then move it into the penultimate bin. */
      if( binsize[ ibin ] == 1 && alist[ indx[ nobs-1 ] ] < azp ) {
         binsize[ --ibin ]++;
      }
   }

/* Open output log file, and write header line. */
   if( *log ) {
      fd1 = fopen( log, "w" );
      struct tm tm = *localtime(&(time_t){time(NULL)});
      fprintf( fd1, "# SMURF:POL2IPCOR log file created at %s", asctime(&tm));
      fprintf( fd1, "#\n" );
      fprintf( fd1, "# object = %s\n", object );
      fprintf( fd1, "# wave = %s\n", wave );
      fprintf( fd1, "#\n" );
      for( i = 0; i < (int) nobs && *status == SAI__OK; i++ ) {
         key = astMapKey( km, i );
         if( key ) {
            strcpy( obsid, key );
            astMapGet0A( km, obsid, &obskm );
            astMapGet0D( obskm, "EPOCH", &epoch );
            astMapGet0D( obskm, "ALPHA", &alpha );
            astMapGet0D( obskm, "EL", &el );
            astMapGet0D( obskm, "AZ", &az );
            astMapGet0D( obskm, "WVM", &wvm );
            obskm = astAnnul( obskm );

            fprintf( fd1, "# Observation %d:\n", i );
            fprintf( fd1, "#    id: %s\n", obsid );
            fprintf( fd1, "#    epoch: %.20g\n", epoch );
            fprintf( fd1, "#    elevation: %g\n", el*AST__DR2D );
            fprintf( fd1, "#    azimuth: %g\n", az*AST__DR2D );
            fprintf( fd1, "#    North->FPy: %g\n", alpha*AST__DR2D );
            fprintf( fd1, "#    WVM tau: %g\n", wvm );
         } else {
            fprintf( fd1, "#    <UNDEFINED>\n" );
         }
      }
      fprintf( fd1, "#\n" );
      fprintf( fd1, "# iobs: Observations index\n" );
      fprintf( fd1, "# px: X pixel index\n");
      fprintf( fd1, "# py: Y pixel index\n");
      fprintf( fd1, "# I: Total intensity\n");
      fprintf( fd1, "# Qtr: normalised Q referenced to tracking north\n");
      fprintf( fd1, "# Utr: normalised U referenced to tracking north\n");
      fprintf( fd1, "# dQfp: normalised Q offset referenced to focal plane Y axis\n");
      fprintf( fd1, "# dUfp: normalised U offset referenced to focal plane Y axis\n");
      fprintf( fd1, "# Qctr: central normalised Q referenced to tracking north\n");
      fprintf( fd1, "# Uctr: central normalised U referenced to tracking north\n");
      fprintf( fd1, "# el: Elevation\n");
      fprintf( fd1, "# az: Azimuth\n");
      fprintf( fd1, "# rad: Radius of fitted circle (fractional)\n");
      fprintf( fd1, "# wgt: Weight \n");
      fprintf( fd1, "# beta: Angle around fitted circle\n" );
      fprintf( fd1, "# rms: RMS deviation of points from fitted circle\n" );
      fprintf( fd1, "# cor: Correlation of angle around circle and azimuth\n" );
      fprintf( fd1, "# fang: Weight factor due to azimuthal angle error\n" );
      fprintf( fd1, "#\n" );
      fprintf( fd1, "%s\n", LOGCOLS );
   } else {
      fd1 = NULL;
   }

/* Open output model file, and write header line. */
   if( *modelfile ) {
      fd2 = fopen( modelfile, "w" );
      struct tm tm = *localtime(&(time_t){time(NULL)});
      fprintf( fd2, "# SMURF:POL2IPCOR model file created at %s", asctime(&tm));
      fprintf( fd2, "#\n" );
   } else {
      fd2 = NULL;
   }

/* Now we have all the information we need from the input NDFs, get the
   parameters of an IP model that corrects the input maps. */
   smf1_pol2ipcor( wf, imodel, km, igrp3, lbnd, ubnd, indx, binsize, ibin+1,
                  iwave, fd1, fd2, coslist, sinlist, ellist, alist,
                  ippars, status );

/* Close the output text files. */
   if( fd1 ) fclose( fd1 );
   if( fd2 ) fclose( fd2 );

/* Abort if an error has occurred. */
   if( *status != SAI__OK ) goto L999;

/* Get a group of exactly "size" names for the output NDFs.  Base
   modification elements on the group containing the input NDFs. */
   if( nobs ) kpg1Wgndf( "OUT", igrp1, isize1, isize1, "  Give more NDFs...",
                         &igrp2, &outsize, status );

/* Produce the IP corrected output maps for each observation. */
   for( i = 0; i < nobs && *status == SAI__OK; i++ ) {

/* Get the KeyMap holding information about all the input NDFs related to the
   current observation. */
      key = astMapKey( km, i );
      if( key ) strcpy( obsid, key );
      astMapGet0A( km, obsid, &obskm );

      msgOutiff( MSG__VERB, " ", "Producing IP corrected Q and U maps for "
                 "observation %s", status, obsid );

/* Get the NDF identifiers for the input I, Q and U for the observation. */
      if( !astMapGet0I( obskm, "I_NDF", &indf ) ) {
         *status = SAI__ERROR;
         errRepf( " ","No I map supplied for observation %s.", status,
                  obsid );

      } else if( !astMapGet0I( obskm, "Q_NDF", &qndf ) ) {
         *status = SAI__ERROR;
         errRepf( " ","No Q map supplied for observation %s.", status,
                  obsid );

      } else if( !astMapGet0I( obskm, "U_NDF", &undf ) ) {
         *status = SAI__ERROR;
         errRepf( " ","No U map supplied for observation %s.", status,
                  obsid );
      }

/* Retrieve the central elevation of the observation, in radians. */
      astMapGet0D( obskm, "EL", &el );

/* Calculate the normalised Q and U corrections for the current
   observation, with respect to the focal plane Y axis. */
      if( imodel == SINGLE_COMP ) {
         p = ippars[0] + ippars[1]*el + ippars[2]*el*el;
         qncor_fp = p*cos( -2*( el - ippars[3] ) );
         uncor_fp = p*sin( -2*( el - ippars[3] ) );
      } else {
         angle = -2*( el - ippars[2] );
         qncor_fp = ippars[0] + ippars[1]*cos( angle );
         uncor_fp = ippars[5] + ippars[1]*sin( angle );
         angle = -2*( el - ippars[4] );
         qncor_fp += ippars[3]*cos( angle );
         uncor_fp += ippars[3]*sin( angle );
      }

/* Get the constants needed to rotate the reference direction used by the
   correction to the original polarimetric reference direction. */
      astMapGet0D( obskm, "ALPHA", &alpha );
      cosval = cos( 2*alpha );
      sinval = sin( 2*alpha );

/* Unmap all mapped arrays in the input NDFs for this observation. */
      ndfUnmap( indf, "*", status );
      ndfUnmap( qndf, "*", status );
      ndfUnmap( undf, "*", status );

/* Reset the badbits value in each input NDF. */
      ndfSbb( 0, indf, status );
      ndfSbb( 0, qndf, status );
      ndfSbb( 0, undf, status );

/* Each NDF identifier retrieved above may refer to a section of an input
   NDF rather than the whole input NDF. But we want the whole of each input NDF
   to be copied to the corresponding output NDF. So for each input NDF
   identifier get an identifier for the corresponding base NDF. */
      ndfBase( indf, &tndf, status );
      indf = tndf;
      ndfBase( qndf, &tndf, status );
      qndf = tndf;
      ndfBase( undf, &tndf, status );
      undf = tndf;

/* Create the output Q and U maps, copying everything from the
   corresponding input maps. */
      astMapGet0I( obskm, "Q_IFILE", &ifile );
      ndgNdfpr( qndf, "Data,Variance,Quality,Units,Axis,WCS", igrp2, ifile,
                &qndf2, status );
      astMapGet0I( obskm, "U_IFILE", &ifile );
      ndgNdfpr( undf, "Data,Variance,Quality,Units,Axis,WCS", igrp2, ifile,
                &undf2, status );

/* Map the Data and Variance arrays in the input I map and the output Q and U
   maps. */
      ndfMap( indf, "DATA,VARIANCE", "_DOUBLE", "READ", pntrs, &nel, status );
      ndfMap( qndf2, "DATA,VARIANCE", "_DOUBLE", "UPDATE", pntrs + 2, &nel, status );
      ndfMap( undf2, "DATA,VARIANCE", "_DOUBLE", "UPDATE", pntrs + 4, &nel, status );

/* Loop over all good pixels, applying the correction. */
      pid = pntrs[ 0 ];
      piv = pntrs[ 1 ];
      pqd = pntrs[ 2 ];
      pqv = pntrs[ 3 ];
      pud = pntrs[ 4 ];
      puv = pntrs[ 5 ];

      for( iel = 0; iel < nel; iel++,pid++,pqd++,pud++,piv++,pqv++,puv++ ) {
         if( *pid != VAL__BADD && *pqd != VAL__BADD && *pud != VAL__BADD &&
             *piv != VAL__BADD && *pqv != VAL__BADD && *puv != VAL__BADD ){

/* Rotate the reference direction used by the correction to the original
   polarimetric reference direction. */
            qncor = cosval*qncor_fp - sinval*uncor_fp;
            uncor = sinval*qncor_fp + cosval*uncor_fp;

/* Apply the correction. */
            *pqd -= qncor*(*pid);
            *pud -= uncor*(*pid);
            *pqv += qncor*qncor*(*piv);
            *puv += uncor*uncor*(*piv);

         }
      }

/* Release the output maps. */
      ndfAnnul( &qndf2, status );
      ndfAnnul( &undf2, status );

/* Annul AST object pointers created in the loop. */
      obskm = astAnnul( obskm );
   }

/* If a null value was supplied for parameter "OUT", annull the error. */
   if( *status == PAR__NULL ) errAnnul( status );

/* Arrive here if an error occurrs. */
L999:

/* Free resources. */
   alist = astFree( alist );
   ellist = astFree( ellist );
   coslist = astFree( coslist );
   sinlist = astFree( sinlist );
   indx = astFree( indx );
   if( igrp1 ) grpDelet( &igrp1, status);
   if( igrp2 ) grpDelet( &igrp2, status);
   if( igrp3 ) grpDelet( &igrp3, status);

/* End the NDF context. */
   ndfEnd( status );

/* End the AST context. */
   astEnd;

/* Issue a status indication.*/
   if( *status == SAI__OK ) {
      msgOutif(MSG__VERB," ",TASK_NAME " succeeded.", status);
   } else {
      msgOutif(MSG__VERB," ",TASK_NAME " failed.", status);
   }
}









static void smf1_pol2ipcor( ThrWorkForce *wf, int imodel, AstKeyMap *km,
			    Grp *igrp, const dim_t *lbnd, const dim_t *ubnd,
			    const int *indx, const int *binsize, int nbin,
                            int wave, FILE *fd1, FILE *fd2, const double *coslist,
                            const double *sinlist, const double *ellist,
                            const double *alist, double ippars[6], int *status ){

/* Local Variables: */
   AstKeyMap *kmcopy;
   SmfPol2IpcorData *job_data = NULL;
   SmfPol2IpcorData *pdata = NULL;
   char *pname;
   char filepath[ GRP__SZFNM + 1 ];
   dim_t *pxlist;
   dim_t *pylist;
   dim_t igood;
   dim_t ngood;
   dim_t npix_good;
   dim_t nrej;
   dim_t rowstep;
   double *blist;
   double *corlist;
   double *dqlist;
   double *dulist;
   double *elist;
   double *flist;
   double *ilist;
   double *qclist;
   double *qtrlist;
   double *radlist;
   double *rmslist;
   double *uclist;
   double *utrlist;
   double *wlist;
   double angle;
   double bfit;
   double boffset;
   double bres;
   double bslope;
   double cor;
   double dq;
   double du;
   double el;
   double f;
   double mbeta;
   double p;
   double q;
   double qc;
   double rad;
   double rms;
   double u;
   double uc;
   int *olist;
   int again;
   int i;
   int iw;
   int k;
   int nobs;
   int nw;
   size_t ilog;
   size_t nlog;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Initialise */
   ngood = 0;
   dqlist = NULL;
   dulist = NULL;
   elist = NULL;
   wlist = NULL;

/* How many observations? */
   nobs = astMapSize( km );

/* Skip forward to the fitting bit if no observation maps were supplied. */
   if( nobs ) {
      msgOutif( MSG__DEBUG, " ", "Fitting circles...", status );

/* How many threads do we get to play with */
      nw = wf ? wf->nworker : 1;

/* Allocate job data for threads. */
      job_data = astCalloc( nw, sizeof(*job_data) );

/* First find how many rows to process in each worker thread. */
      rowstep = ( ubnd[ 1 ] - lbnd[ 1 ] + 1 )/nw;
      if( rowstep == 0 ) rowstep = 1;

/* Store the range of rows to be processed by each thread. Ensure that the
   last thread picks up any left-over rows. */
      for( iw = 0; iw < nw && *status == SAI__OK; iw++ ) {
         pdata = job_data + iw;
         pdata->r1 = lbnd[ 1 ] + iw*rowstep;
         if( iw < nw - 1 ) {
            pdata->r2 = pdata->r1 + rowstep - 1;
         } else {
            pdata->r2 = ubnd[ 1 ];
         }

/* Store other values common to all jobs. */
         pdata->operation = 1;
         pdata->lbnd = lbnd;
         pdata->ubnd = ubnd;
         pdata->nobs = nobs;
         pdata->indx = indx;
         pdata->binsize = binsize;
         pdata->nbin = nbin;
         pdata->wave = wave;
         kmcopy = astCopy( km );
         astUnlock( kmcopy, 0 );
         pdata->km = kmcopy;

/* Submit the job to the workforce. */
         thrAddJob( wf, 0, pdata, smf1_worker, 0, NULL, status );
      }

/* Wait for all jobs to complete. */
      thrWait( wf, status );

/* Find how many good pixels were found. */
      npix_good = 0;
      if( *status == SAI__OK ) {
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            ngood += pdata->ngood;
            npix_good += pdata->npix_good;
         }
      }

/* Report an error if we have too few. */
      if( npix_good  < 10 && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( " ", "Too few pixels (%zu) produced usable fits.", status,
                  npix_good );

      } else if( ngood  < 10 && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( " ", "Too few usable (Q,U) offsets (%zu) found.", status,
                  ngood );
      }

/* Allocate arrays to hold the information about each pixel that was used
   successfully to fit a circle. */
      blist = astMalloc( ngood*sizeof( *blist ) );
      dqlist = astMalloc( ngood*sizeof( *dqlist ) );
      dulist = astMalloc( ngood*sizeof( *dulist ) );
      elist = astMalloc( ngood*sizeof(*elist) );
      ilist = astMalloc( ngood*sizeof( *ilist ) );
      olist = astMalloc( ngood*sizeof( *olist ) );
      pxlist = astMalloc( ngood*sizeof( *pxlist ) );
      pylist = astMalloc( ngood*sizeof( *pylist ) );
      qclist = astMalloc( ngood*sizeof( *qclist ) );
      qtrlist = astMalloc( ngood*sizeof( *qtrlist ) );
      radlist = astMalloc( ngood*sizeof( *radlist ) );
      uclist = astMalloc( ngood*sizeof( *uclist ) );
      utrlist = astMalloc( ngood*sizeof( *utrlist ) );
      wlist = astMalloc( ngood*sizeof( *wlist ) );
      flist = astMalloc( ngood*sizeof( *flist ) );
      rmslist = astMalloc( ngood*sizeof( *rmslist ) );
      corlist = astMalloc( ngood*sizeof( *corlist ) );

/* Concatenate the arrays returned by each thread. */
      if( *status == SAI__OK ) {
         ngood = 0;
         for( iw = 0; iw < nw; iw++ ) {
            pdata = job_data + iw;
            memcpy( blist + ngood, pdata->beta, pdata->ngood*sizeof(*blist) );
            memcpy( dqlist + ngood, pdata->dq, pdata->ngood*sizeof(*dqlist) );
            memcpy( dulist + ngood, pdata->du, pdata->ngood*sizeof(*dulist) );
            memcpy( ilist + ngood, pdata->i, pdata->ngood*sizeof(*ilist) );
            memcpy( olist + ngood, pdata->obs, pdata->ngood*sizeof(*olist) );
            memcpy( pxlist + ngood, pdata->px, pdata->ngood*sizeof(*pxlist) );
            memcpy( pylist + ngood, pdata->py, pdata->ngood*sizeof(*pylist) );
            memcpy( qclist + ngood, pdata->qc, pdata->ngood*sizeof(*qclist) );
            memcpy( qtrlist + ngood, pdata->qtr, pdata->ngood*sizeof(*qtrlist) );
            memcpy( radlist + ngood, pdata->rad, pdata->ngood*sizeof(*radlist) );
            memcpy( uclist + ngood, pdata->uc, pdata->ngood*sizeof(*uclist) );
            memcpy( utrlist + ngood, pdata->utr, pdata->ngood*sizeof(*utrlist) );
            memcpy( wlist + ngood, pdata->wgt, pdata->ngood*sizeof(*wlist) );
            memcpy( corlist + ngood, pdata->cor, pdata->ngood*sizeof(*corlist) );
            memcpy( rmslist + ngood, pdata->rms, pdata->ngood*sizeof(*rmslist) );
            ngood += pdata->ngood;
         }

/* Find the mean beta value and then ensure all beta values are with +/-
   PI of the mean value by adding or subtracting 2.PI. */
         mbeta = 0.0;
         for( igood = 0; igood < ngood; igood++ ) {
            mbeta += blist[ igood ];
         }
         mbeta /= ngood;

         for( igood = 0; igood < ngood; igood++ ) {
            while( blist[ igood ] > mbeta + AST__DPI ) {
               blist[ igood ] -= 2*AST__DPI;
            }
            while( blist[ igood ] < mbeta - AST__DPI ) {
               blist[ igood ] += 2*AST__DPI;
            }
         }

/* Get the slope and offset of the best fitting line through the (beta,az)
   points for all pixels:

   beta = bslope*az + boffset

   az and beta in radians.
*/
         smf1_linfit( ngood, olist, wlist, alist, blist, &bslope, &boffset,
                      &rms, &cor, status );

/* Improve the fit until no changes are made, up to a maximum of 10 times. */
         for( i = 0; i < 10; i++ ) {

/* Add +/- 2.PI onto each beta value to ensure it is no more than +/- PI
   away from the fit. */
            again = 0;
            for( igood = 0; igood < ngood; igood++ ) {
               bfit = bslope*alist[ olist[ igood ] ] + boffset;
               while( blist[ igood ] > bfit + AST__DPI ) {
                  blist[ igood ] -= 2*AST__DPI;
                  again = 1;
               }
               while( blist[ igood ] < bfit - AST__DPI ) {
                  blist[ igood ] += 2*AST__DPI;
                  again = 1;
               }
            }
            if( !again ) break;

/* Do a new fit using the modified beta values. */
            smf1_linfit( ngood, olist, wlist, alist, blist, &bslope, &boffset,
                         &rms, &cor, status );
         }

         msgOutiff( MSG__DEBUG, " ", "beta = %g*az + %g (degs)", status,
                    bslope, boffset*AST__DR2D );
         msgOutiff( MSG__DEBUG, " ", "   rms = %g (degs)  corr = %g", status,
                    rms, cor );

/* Modify the weights to give lower weight to points that have a beta
   value far from the above fitted line. The weighting function is 1.0
   for residuals from zero to 0.4 rads (about 20 degrees), and then drops
   linearly to zero at a residual of 1.0 rads (about 60 degrees). */
         nrej = 0;
         for( igood = 0; igood < ngood; igood++ ) {
            bfit = bslope*alist[ olist[ igood ] ] + boffset;
            bres = fabs( blist[ igood ] - bfit );
            if( bres > 0.4 ) {
               f = ( bres - 1.0 )/( 0.4 - 1.0 );
               if( f <= 0.0 ) {
                  f = 0.0;
                  nrej++;
               }
               flist[ igood ] = f;
               wlist[ igood ] *= f;
            } else {
               flist[ igood ] = 1.0;
            }
         }

         msgOutf( " ", "Using circle fits from %zu points on the sky", status,
                  npix_good );
         msgOutf( " ", "(%zu usable (Q,U) points in total).", status, ngood - nrej );

/* Fit a circle to the (du,dq) offsets for the set of all usable pixels
   from all observations. */
         smf_fit_circle( ngood, dulist, dqlist, wlist, wlist, &rad, &uc,
                         &qc, &rms, status );
         msgOutiff( MSG__DEBUG, " ", "Secondary circle centre: u=%g q=%g",
                    status, uc, qc );
         if( rad == VAL__BADD && *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( " ", "Cannot fit circle to re-centred data - radius is "
                    "too small compared to the noise.", status );
         } else {

/* Find the (Q,U) offsets from the new improved centre in tracking
   coords, and rotate them to focal plane coords. */
            for( igood = 0; igood < ngood; igood++ ) {
               du = dulist[ igood ] - uc;
               dq = dqlist[ igood ] - qc;
               k = olist[ igood ];
               dqlist[ igood ] =  coslist[ k ]*dq + sinlist[ k ]*du;
               dulist[ igood ] = -sinlist[ k ]*dq + coslist[ k ]*du;
               elist[ igood ] = ellist[ k ];
            }

/* Dump the focal plane offsets, etc, found above to an output text file. */
            if( fd1 ) {
               for( igood = 0; igood < ngood; igood++ ) {
                  k = olist[ igood ];
                  fprintf( fd1, "%d %" DIM_T_FMT " %" DIM_T_FMT " %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g\n",
                           k, pxlist[igood], pylist[igood], ilist[igood],
                           qtrlist[igood], utrlist[igood], dqlist[igood],
                           dulist[igood], qclist[igood], uclist[igood],
                           AST__DR2D*ellist[k], AST__DR2D*alist[k], radlist[igood],
                           wlist[igood], blist[igood]*AST__DR2D,
                           rmslist[igood], corlist[igood], flist[igood] );
               }
            }
         }
      }

/* Free resources. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         astLock( pdata->km, 0 );
         pdata->km = astAnnul( pdata->km );

         pdata->beta = astFree( pdata->beta );
         pdata->dq = astFree( pdata->dq );
         pdata->du = astFree( pdata->du );
         pdata->i = astFree( pdata->i );
         pdata->obs = astFree( pdata->obs );
         pdata->px = astFree( pdata->px );
         pdata->py = astFree( pdata->py );
         pdata->qc = astFree( pdata->qc );
         pdata->qtr = astFree( pdata->qtr );
         pdata->rad = astFree( pdata->rad );
         pdata->uc = astFree( pdata->uc );
         pdata->utr = astFree( pdata->utr );
         pdata->wgt = astFree( pdata->wgt );
         pdata->cor = astFree( pdata->cor );
         pdata->rms = astFree( pdata->rms );
      }
      job_data = astFree( job_data );

      ilist = astFree( ilist );
      blist = astFree( blist );
      olist = astFree( olist );
      pxlist = astFree( pxlist );
      pylist = astFree( pylist );
      qclist = astFree( qclist );
      radlist = astFree( radlist );
      uclist = astFree( uclist );
      qtrlist = astFree( qtrlist );
      utrlist = astFree( utrlist );
      rmslist = astFree( rmslist );
      corlist = astFree( corlist );
      flist = astFree( flist );
   }

/* Add in the data from any externally supplied log files. */
   if( igrp ) {

/* Loop round all the log files in the supplied group. */
      nlog = grpGrpsz( igrp, status );
      for( ilog = 0; ilog < nlog; ilog++ ) {

/* Get the logfile path from the group. */
         pname = filepath;
         grpGet( igrp, ilog+1, 1, &pname, sizeof(filepath), status );

/* Read its contents into the arrays holding the values to use in the fit. */
         smf1_logread( pname, &ngood, &dqlist, &dulist, &wlist, &elist, &wave, status );
      }
   }

/* Do an iterative least squares fit to get the best values for the IP model parameters. */
   smf1_ipfit( imodel, ngood, dqlist, dulist, wlist, elist, wave, ippars, status );

/* If required, create an output text file holding a table of evaluated
   model values. */
   if( fd2 ) {
      fprintf( fd2, "# wavelength = %s um\n", (wave==S4)?"450":"850" );
      fprintf( fd2, "#\n" );
      if( imodel == SINGLE_COMP ) {
         fprintf( fd2, "# A = %g\n", ippars[ 0 ] );
         fprintf( fd2, "# B = %g (rads^-1)\n", ippars[ 1 ] );
         fprintf( fd2, "# C = %g (rads^-2)\n", ippars[ 2 ] );
         fprintf( fd2, "# D = %g (rads)\n", ippars[ 3 ] );
         fprintf( fd2, "#\n" );
         fprintf( fd2, "# p = A + B*el_rad + C*el_rad*el_rad\n" );
         fprintf( fd2, "# Qn = p*cos( -2*( el_rad - D ) )\n" );
         fprintf( fd2, "# Un = p*sin( -2*( el_rad - D ) )\n" );
      } else {
         fprintf( fd2, "# A = %g\n", ippars[ 0 ] );
         fprintf( fd2, "# B = %g\n", ippars[ 1 ] );
         fprintf( fd2, "# C = %g (rads)\n", ippars[ 2 ] );
         fprintf( fd2, "# D = %g\n", ippars[ 3 ] );
         fprintf( fd2, "# E = %g (rads)\n", ippars[ 4 ] );
         fprintf( fd2, "# F = %g\n", ippars[ 5 ] );
         fprintf( fd2, "#\n" );
         fprintf( fd2, "# Qn = A + B*cos( -2*( el_rad - C ) ) + D*cos( -2*( el_rad - E ) )\n" );
         fprintf( fd2, "# Un = F + B*sin( -2*( el_rad - C ) ) + D*sin( -2*( el_rad - E ) )\n" );
      }

      fprintf( fd2, "#\n" );
      fprintf( fd2, "# el: Elevation (degrees)\n" );
      fprintf( fd2, "# dQfp: Normalised Q correction referenced to focal plane Y axis\n" );
      fprintf( fd2, "# dUfp: Normalised U correction referenced to focal plane Y axis\n" );
      fprintf( fd2, "# p: Fractional polarisation\n" );
      fprintf( fd2, "#\n" );
      fprintf( fd2, "# el dQfp dUfp p\n" );
      for( k = 20; k <= 80; k++ ) {
         el = k*AST__DD2R;
         if( imodel == SINGLE_COMP ) {
            p = ippars[ 0 ] + ippars[ 1 ]*el + ippars[ 2 ]*el*el;
            q = p*cos( -2.0*( el - ippars[ 3 ] ) );
            u = p*sin( -2.0*( el - ippars[ 3 ] ) );
         } else {
            angle = -2*( el - ippars[ 2 ] );
            q = ippars[ 0 ] + ippars[ 1 ]*cos( angle );
            u = ippars[ 5 ] + ippars[ 1 ]*sin( angle );
            angle = -2*( el - ippars[ 4 ] );
            q += ippars[ 3 ]*cos( angle );
            u += ippars[ 3 ]*sin( angle );
            p = q*q + u*u;
            p = ( p > 0.0 ) ? sqrt( p ) : 0.0;
         }
         fprintf( fd2, "%d %g %g %g\n", k, q, u, p );
      }

   }

/* Free resources. */
   dqlist = astFree( dqlist );
   dulist = astFree( dulist );
   elist = astFree( elist );
   wlist = astFree( wlist );

}



static void smf1_worker( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_worker

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf1_pol2ipcor.

*  Invocation:
*     smf1_worker( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfPol2IpcorData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   AstKeyMap *obskm;
   AstKeyMap *km;
   SmfPol2IpcorData *pdata;
   const char *obsid;
   const int *binsize;
   const int *indx;
   double **pid;
   double **piv;
   double **pqv;
   double **pqd;
   double **pud;
   double **puv;
   double *alist;
   double *beta;
   double *blist;
   double *coslist;
   double *dq;
   double *du;
   double *elist;
   double *ii;
   double *malist;
   double *qc;
   double *qlist;
   double *qn;
   double *qnw;
   double *qtr;
   double *cor;
   double *rms;
   double *qwlist;
   double *rad;
   double *sinlist;
   double *uc;
   double *ulist;
   double *un;
   double *unw;
   double *utr;
   double *uwlist;
   double *wgt;
   double Beta;
   double Beta0;
   double alpha;
   double ang;
   double Cor;
   double diam;
   double i2;
   double i;
   double mq;
   double mu;
   double nrms;
   double q;
   double Qc;
   double Rad;
   double Rms;
   double sigma = 0.0;
   double sq2;
   double sq;
   double su2;
   double su;
   double sx2;
   double sx;
   double sxy;
   double sy2;
   double sy;
   double u;
   double Uc;
   double vi;
   double vq;
   double vqn;
   double vu;
   double vun;
   double w;
   int *obs;
   dim_t *px;
   dim_t *py;
   int ibin;
   dim_t ix;
   dim_t iy;
   int k;
   int nbin;
   int nobs;
   int nuse;
   int nval;
   dim_t xhi;
   dim_t xlo;
   dim_t newsize;
   dim_t ngood;
   dim_t npix_good;
   dim_t offset;
   void *pntrs[6];

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfPol2IpcorData *) job_data_ptr;

/* For convenience, copy stuff into local variables */
   km = pdata->km;
   xlo = pdata->lbnd[ 0 ];
   xhi = pdata->ubnd[ 0 ];
   nobs = pdata->nobs;
   nbin = pdata->nbin;
   binsize = pdata->binsize;
   indx = pdata->indx;

/* Lock the KeyMap for use by the current thread. */
   astLock( km, 0 );

/* Allocate room for the required array pointers. */
   pid = astMalloc( nobs*sizeof(*pid) );
   piv = astMalloc( nobs*sizeof(*piv) );
   pqd = astMalloc( nobs*sizeof(*pqd) );
   pqv = astMalloc( nobs*sizeof(*pqv) );
   pud = astMalloc( nobs*sizeof(*pud) );
   puv = astMalloc( nobs*sizeof(*puv) );

/* Allocate room for the normalised Q and U values in each observation at
   a single pixel. */
   qn = astMalloc( nobs*sizeof(*qn) );
   un = astMalloc( nobs*sizeof(*un) );
   qnw = astMalloc( nobs*sizeof(*qnw) );
   unw = astMalloc( nobs*sizeof(*unw) );

/* Allocate room for the normalised Q and U values in each azimuth bin at
   a single pixel. */
   qlist = astMalloc( nbin*sizeof(*qlist) );
   ulist = astMalloc( nbin*sizeof(*ulist) );
   qwlist = astMalloc( nbin*sizeof(*qwlist) );
   uwlist = astMalloc( nbin*sizeof(*uwlist) );
   malist = astMalloc( nbin*sizeof(*malist) );

/* Allocate other required arrays. */
   coslist = astMalloc( nobs*sizeof(*coslist) );
   sinlist = astMalloc( nobs*sizeof(*sinlist) );
   alist = astMalloc( nobs*sizeof(*alist) );
   elist = astMalloc( nobs*sizeof(*elist) );
   blist = astMalloc( nobs*sizeof(*blist) );

/* Retrieve array pointers, etc, from each observation's KeyMap. */
   for( k = 0; k < nobs; k++ ) {
      obsid = astMapKey( km, k );
      astMapGet0A( km, obsid, &obskm );

      astMapGet1P( obskm, "PNTRS", 6, &nval, pntrs );
      pid[ k ] = (double *) pntrs[ 0 ];
      piv[ k ] = (double *) pntrs[ 1 ];
      pqd[ k ] = (double *) pntrs[ 2 ];
      pqv[ k ] = (double *) pntrs[ 3 ];
      pud[ k ] = (double *) pntrs[ 4 ];
      puv[ k ] = (double *) pntrs[ 5 ];

      astMapGet0D( obskm, "AZ", alist + k );
      astMapGet0D( obskm, "EL", elist + k );
      astMapGet0D( obskm, "ALPHA", &alpha );
      coslist[ k ] = cos( 2*alpha );
      sinlist[ k ] = sin( 2*alpha );
      obskm = astAnnul( obskm );
   }

/* Process the pixels allocated to this thread. */
   if( pdata->operation == 1 && *status == SAI__OK ) {

/* Get pointers, for all observations, to the first pixel to be processed
   by this thread. */
      offset = ( pdata->r1 - pdata->lbnd[ 1 ] )*( xhi - xlo + 1 );
      for( k = 0; k < nobs; k++ ) {
         pid[ k ] += offset;
         piv[ k ] += offset;
         pqd[ k ] += offset;
         pqv[ k ] += offset;
         pud[ k ] += offset;
         puv[ k ] += offset;
      }

/* Initialise pointers to arrays holding returned values. */
      beta = NULL;
      dq = NULL;
      du = NULL;
      ii = NULL;
      obs = NULL;
      px = NULL;
      py = NULL;
      qc = NULL;
      qtr = NULL;
      rad = NULL;
      uc = NULL;
      utr = NULL;
      wgt = NULL;
      cor = NULL;
      rms = NULL;

      ngood = 0;
      npix_good = 0;

/* Process the pixels allocated to this thread. */
      for( iy = pdata->r1; iy <= pdata->r2; iy++ ) {
         for( ix = xlo; ix <= xhi; ix++ ) {

/* For each observation, get the normalised Q and U values at this pixel,
   together with the associated weights (reciprocal of variances). */
            nuse = 0;
            for( k = 0; k < nobs; k++ ) {
               i = *(pid[ k ]);
               q = *(pqd[ k ]);
               u = *(pud[ k ]);
               vi = *(piv[ k ]);
               vq = *(pqv[ k ]);
               vu = *(puv[ k ]);

               if( i != VAL__BADD && i > 0.0 && vi != VAL__BADD && vi > 0.0 &&
                   q != VAL__BADD && vq != VAL__BADD && vq > 0.0 &&
                   u != VAL__BADD && vu != VAL__BADD && vu > 0.0 ){
                  i2 = i*i;
                  qn[ k ] = q/i;
                  vqn = ( i2*vq + q*q*vi )/(i2*i2);
                  qnw[ k ] = 1.0/vqn;

                  un[ k ] = u/i;
                  vun = ( i2*vu + u*u*vi )/(i2*i2);
                  unw[ k ] = 1.0/vun;

                  nuse++;

               } else {
                  qn[ k ] = VAL__BADD;
                  qnw[ k ] = VAL__BADD;
                  un[ k ] = VAL__BADD;
                  unw[ k ] = VAL__BADD;
               }
            }

/* Find any very bad outlier values and exclude them by setting them bad. */
            smf1_reject( nobs, qn, status );
            smf1_reject( nobs, un, status );

/* Get a circle fit to the mean (q,u) value in each azimuth bin. This
   does a form of k-sigma clipping to reject outliers. */
            smf1_circle_fitter( nobs, qn, un, unw, qnw, nbin, binsize, indx,
                                alist, qlist, qwlist, ulist, uwlist, malist,
                                &Rad, &Uc, &Qc, &Rms, &sigma, status );

/* Check a circle was fitted successfully. */
            if( Rad != VAL__BADD ) {

/* Find an estimate of the diameter of the blob encircling most of the
   binned (q,u) values. */
               nval = 0;
               sq = 0.0;
               sq2 = 0.0;
               su = 0.0;
               su2 = 0.0;
               for( ibin = 0; ibin < nbin; ibin++ ) {
                  if( qlist[ ibin ] != VAL__BADD && ulist[ ibin ] != VAL__BADD ) {
                     q = qlist[ ibin ];
                     u = ulist[ ibin ];
                     sq += q;
                     sq2 += q*q;
                     su += u;
                     su2 += u*u;
                     nval++;
                  }
               }

               mq = sq/nval;
               mu = su/nval;

               vq = sq2/nval - mq*mq;
               vu = su2/nval - mu*mu;
               diam = 3*sqrt( vq + vu );

/* Get a measure of the angular extent of the set of mean (Q,U) points as seen
   from the fit centre. */
               ang = AST__DR2D*diam/Rad;

/* Calculate the angle of each (u,q) point around the circle (beta), in
   the range +/- PI. */
               for( k = 0; k < nobs; k++ ) {
                  if( qn[ k ] != VAL__BADD ) {
                     blist[ k ] = atan2( un[k] - Uc, qn[k] - Qc );
                  } else {
                     blist[ k ] = VAL__BADD;
                  }
               }

/* Add or subtract 2.PI from each beta value so that there is no
   wrap-around (from 0 to 2.PI etc) as the beta values are traversed in the
   order of increasing azimuth. Use the "indx" array to access the blist
   array in order of increasing azimuth. The first non-bad beta value is
   forced to be negative. */
               for( k = 0; k < nobs; k++ ) {
                  Beta0 = blist[ indx[ k ] ];
                  if( Beta0 != VAL__BADD ){
                     if( Beta0 > 0.0 )  {
                        Beta0 -= 2*AST__DPI;
                        blist[ indx[ k ] ] = Beta0;
                     }
                     k++;
                     break;
                  }
               }

               for( ; k < nobs; k++ ) {
                  Beta = blist[ indx[ k ] ];
                  if( Beta != VAL__BADD ) {
                     if( Beta > Beta0 + AST__DPI ) {
                        Beta -= 2*AST__DPI;
                     } else if( Beta < Beta0 - AST__DPI ) {
                        Beta += 2*AST__DPI;
                     }
                     blist[ indx[ k ] ] = Beta;
                     Beta0 = Beta;
                  }
               }

/* Calculate the correlation coefficient between azimuth and the angle of each
   (u,q) point around the circle (beta). */
               sx = 0.0;
               sx2 = 0.0;
               sy = 0.0;
               sy2 = 0.0;
               sxy = 0.0;
               nval = 0;
               for( k = 0; k < nobs; k++ ) {
                  Beta = blist[ k ];
                  if( Beta != VAL__BADD ) {
                     sx += Beta;
                     sx2 += Beta*Beta;
                     sy += alist[k];
                     sy2 += alist[k]*alist[k];
                     sxy += Beta*alist[k];
                     nval++;
                  }
               }

               Cor = ( nval*sxy - sx*sy )/( sqrt(nval*sx2-sx*sx)*sqrt(nval*sy2-sy*sy) );

/* Ignore pixels for which the normalised (Q,U) values do not form a good
   circle (rms residual is greater than half the radius), or for which
   the (Q,U) points has a low angular extent, or for which the azimuth
   does increase relatively smoothly round the circle. */
               nrms = Rms/Rad;
               if( nrms < 0.5 && Rad > 2.0*sigma && ang > 120.0 && Cor > 0.7 ) {
                  npix_good++;

/* Extend the arrays holding the returned values. */
                  newsize = ngood + nuse;
                  beta = astGrow( beta, newsize, sizeof(*beta) );
                  dq = astGrow( dq, newsize, sizeof(*dq) );
                  du = astGrow( du, newsize, sizeof(*du) );
                  ii = astGrow( ii, newsize, sizeof(*ii) );
                  obs = astGrow( obs, newsize, sizeof(*obs) );
                  px = astGrow( px, newsize, sizeof(*px) );
                  py = astGrow( py, newsize, sizeof(*py) );
                  qc = astGrow( qc, newsize, sizeof(*qc) );
                  qtr = astGrow( qtr, newsize, sizeof(*qtr) );
                  rad = astGrow( rad, newsize, sizeof(*rad) );
                  uc = astGrow( uc, newsize, sizeof(*uc) );
                  utr = astGrow( utr, newsize, sizeof(*utr) );
                  wgt = astGrow( wgt, newsize, sizeof(*wgt) );
                  cor = astGrow( cor, newsize, sizeof(*cor) );
                  rms = astGrow( rms, newsize, sizeof(*rms) );
                  if( *status != SAI__OK ) break;

/* For each observation, store the returned information. */
                  for( k = 0; k < nobs; k++ ) {
                     if( qn[k] != VAL__BADD ) {
                        beta[ ngood ] = blist[ k ];
                        dq[ ngood ] = qn[ k ] - Qc;
                        du[ ngood ] = un[ k ] - Uc;
                        ii[ ngood ] = *(pid[ k ]);
                        obs[ ngood ] = k;
                        px[ ngood ] = ix;
                        py[ ngood ] = iy;
                        qc[ ngood ] = Qc;
                        qtr[ ngood ] = qn[ k ];
                        rad[ ngood ] = Rad;
                        uc[ ngood ] = Uc;
                        utr[ ngood ] = un[ k ];
                        w = Cor/Rms;
                        wgt[ ngood ] = w*w;
                        rms[ ngood ] = Rms;
                        cor[ ngood ] = Cor;
                        ngood++;
                     }
                  }
               }
            }

/* Move pointers on to the next pixel */
            for( k = 0; k < nobs; k++ ) {
               pid[ k ]++;
               piv[ k ]++;
               pqd[ k ]++;
               pqv[ k ]++;
               pud[ k ]++;
               puv[ k ]++;
            }
         }
      }

/* Return pointers to the arrays of returned values. */
      pdata->beta = beta;
      pdata->dq = dq;
      pdata->du = du;
      pdata->i = ii;
      pdata->ngood = ngood;
      pdata->npix_good = npix_good;
      pdata->obs = obs;
      pdata->px = px;
      pdata->py = py;
      pdata->qc = qc;
      pdata->qtr = qtr;
      pdata->rad = rad;
      pdata->uc = uc;
      pdata->utr = utr;
      pdata->wgt = wgt;
      pdata->cor = cor;
      pdata->rms = rms;

/* Report an error if the requested operation is unknown. */
   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "smf1_worker: Illegal operation %d requested.",
               status, pdata->operation );
   }

/* Unlock the KeyMap so it can be locked by the main thread prior to
   annulling it. */
   astUnlock( km, 0 );

/* Free pointer arrays (do not free the pointers to the arrays that have
   been returned to the master thread). */
   pid = astFree( pid );
   piv = astFree( piv );
   pqd = astFree( pqd );
   pqv = astFree( pqv );
   pud = astFree( pud );
   puv = astFree( puv );
   qlist = astFree( qlist );
   ulist = astFree( ulist );
   blist = astFree( blist );
   qwlist = astFree( qwlist );
   uwlist = astFree( uwlist );
   coslist = astFree( coslist );
   sinlist = astFree( sinlist );
   elist = astFree( elist );
   alist = astFree( alist );
   malist = astFree( malist );
   qn = astFree( qn );
   un = astFree( un );
   qnw = astFree( qnw );
   unw = astFree( unw );
}


#ifdef HAVE_QSORT_R_BSD
static int smf1_qsort_bsd( void *data, const void *a, const void *b ){
   return smf1_qsort( a, b, data );
}
#endif
static int smf1_qsort( const void *a, const void *b, void *data ){
   int ia = *((const int *) a);
   int ib = *((const int *) b);
   double *alist = (double *) data;
   if( alist[ia] < alist[ib] ) {
      return -1;
   } else if( alist[ia] > alist[ib] ) {
      return 1;
   } else {
      return 0;
   }
}

static void smf1_ipfit( int imodel, dim_t n, const double *q,
                        const double *u, const double *w, const double *e,
                        int iwave, double par[6], int *status ){

/* Local variables; */
   Params params;
   double sw;
   gsl_multimin_function_fdf my_func;
   gsl_multimin_fdfminimizer *s;
   gsl_vector *x;
   int gsl_status;
   int ipar;
   int iter;
   int npar;
   dim_t i;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Return immediately if there are very few samples. */
   if( n < 10 ) {
      *status = SAI__ERROR;
      errRepf( " ", "Insufficient usable (Q,U) points (%zu) to do a fit.",
               status, n );
      return;
   }

/* Find the sum of the weights. */
   sw = 0.0;
   for( i = 0; i < n; i++ ) {
      sw += w[ i ];
   }

/* Get the number of free parameters. */
   if( imodel == SINGLE_COMP ) {
      npar = 4;
   } else {
      npar = 6;
   }

/* Store the starting point - zero for all parameters (i.e. no IP at all). */
   x = gsl_vector_alloc( npar );
   for( ipar = 0; ipar < npar; ipar++ ) gsl_vector_set( x, ipar, 0.0 );

/* The 450 IP seems to be perpendicular to the 850 IP. So the starting
   angle for 450 is rotated by 90 degrees. Without this, the best fit
   angle is close to zero (as for 850) but the other best fit parameters
   result in a fractional polarisation that is negative. Starting at D=PI/2
   results in a best fit angle close to 90 degs and positive polarisation. */
   if( iwave == S4 ) {
      if( imodel == SINGLE_COMP ) {
         gsl_vector_set( x, 3, AST__DPIBY2 );
      } else {
         gsl_vector_set( x, 2, AST__DPIBY2 );
         gsl_vector_set( x, 4, AST__DPIBY2 );
      }
   }

/* Store details of the service routines that calculate the function to
   be minimised and its derivatives. */
   my_func.n = npar;
   my_func.f = &smf1_f;
   my_func.df = &smf1_df;
   my_func.fdf = &smf1_fdf;
   my_func.params = (void *) &params;

/* Store information to be passed to the above service routines. */
   params.au = u;
   params.aq = q;
   params.aw = w;
   params.ae = e;
   params.n = n;
   params.sw = sw;
   params.imodel = imodel;

/* Create a 4D or 6D minimiser. */
   s = gsl_multimin_fdfminimizer_alloc( gsl_multimin_fdfminimizer_conjugate_fr,
                                        npar );

/* Store the service routines, step size and tolerance in the minimiser. */
   gsl_multimin_fdfminimizer_set( s, &my_func, x, 0.001, 0.1 );

/* Iterate to a solution. */
   iter = 0;
   gsl_status = GSL_CONTINUE;
   while( gsl_status == GSL_CONTINUE && ++iter < 100 ){
      gsl_status = gsl_multimin_fdfminimizer_iterate( s );
      if( gsl_status ) break;
      gsl_status = gsl_multimin_test_gradient( s->gradient, 1e-6 );
   }

/* Return the fit parameters. */
   for( ipar = 0; ipar < npar; ipar++ ) {
      par[ ipar ] = gsl_vector_get( s->x, ipar );
   }

   if( imodel == SINGLE_COMP ) {
      msgOut( " ", "Best fit parameters (single component model):", status );
      msgOutf( " ", "   A = %g", status, par[0] );
      msgOutf( " ", "   B = %g (rad^-1)", status, par[1] );
      msgOutf( " ", "   C = %g (rad^-2)", status, par[2] );
      msgOutf( " ", "   D = %g (rad)", status, par[3] );
   } else {
      msgOut( " ", "Best fit parameters (double component model):", status );
      msgOutf( " ", "   A = %g", status, par[0] );
      msgOutf( " ", "   B = %g", status, par[1] );
      msgOutf( " ", "   C = %g (rad)", status, par[2] );
      msgOutf( " ", "   D = %g", status, par[3] );
      msgOutf( " ", "   E = %g (rad)", status, par[4] );
      msgOutf( " ", "   F = %g", status, par[5] );
   }
   msgOutf( " ", "RMS = %g", status, sqrt(s->f) );
   msgBlank( status );

/* Clean up. */
   gsl_multimin_fdfminimizer_free( s );
   gsl_vector_free( x );
}

static void smf1_fdf( const gsl_vector *v, void *pars, double *f, gsl_vector *df ){
/*
*   Service routine that returns the value and gradient of the function to
*   be minimised (f, df/dA, df/dB, df/dC, df/dD ), assuming the IP model
*   parameters stored in "v".
*/

   double tmp, A, B, C, D, E, F, dq, du, s1, s2, s3, s4,
          mq, mu, mp, delta1, delta2, cosdel1, sindel1,
          cosdel2, sindel2, s5, s6;
   const double *pe, *pq, *pu, *pw;
   dim_t n, i;
   Params *params = (Params *) pars;

/* Get pointers to the first I, Q, U, weight and elevation value. */
   n = params->n;
   pq = params->aq;
   pu = params->au;
   pw = params->aw;
   pe = params->ae;

/* First do the calculations for a single component model. */
   if( params->imodel == SINGLE_COMP ) {

/* Get the parameters of the IP model. */
      A = gsl_vector_get( v, 0 );
      B = gsl_vector_get( v, 1 );
      C = gsl_vector_get( v, 2 );
      D = gsl_vector_get( v, 3 );

/* Initialise the running sums. */
      *f = 0.0;
      s1 = 0.0;
      s2 = 0.0;
      s3 = 0.0;
      s4 = 0.0;

/* Loop round every point with a positive weight. */
      for( i = 0; i < n; i++,pu++,pq++,pw++,pe++) {
         if( *pw > 0.0 ) {

/* Calculate the model Q and U values, given the current elevation and
   the IP model parameters. */
            delta1 = -2*( *pe - D );
            cosdel1 = cos( delta1 );
            sindel1 = sin( delta1 );
            mp = A + B*(*pe) + C*(*pe)*(*pe);
            mq = mp*cosdel1;
            mu = mp*sindel1;

/* Calculate the Q and U residuals at the current elevation. */
            dq = *pq - mq;
            du = *pu - mu;

/* Increment the sums. */
            *f += (*pw)*( dq*dq + du*du );
            tmp = (*pw)*( dq*cosdel1 + du*sindel1 );
            s1 += tmp;
            tmp *= *pe;
            s2 += tmp;
            tmp *= *pe;
            s3 += tmp;
            s4 += (*pw)*( dq*sindel1 - du*cosdel1 )*mp;
         }
      }

      *f /= params->sw;
      s1 *= -2/params->sw;
      s2 *= -2/params->sw;
      s3 *= -2/params->sw;
      s4 *= 4/params->sw;

      gsl_vector_set( df, 0, s1 );
      gsl_vector_set( df, 1, s2 );
      gsl_vector_set( df, 2, s3 );
      gsl_vector_set( df, 3, s4 );

/* Now do the calculations for a double component model. */
   } else {

/* Get the parameters of the IP model. */
      A = gsl_vector_get( v, 0 );
      B = gsl_vector_get( v, 1 );
      C = gsl_vector_get( v, 2 );
      D = gsl_vector_get( v, 3 );
      E = gsl_vector_get( v, 4 );
      F = gsl_vector_get( v, 5 );

/* Initialise the running sums. */
      *f = 0.0;
      s1 = 0.0;
      s2 = 0.0;
      s3 = 0.0;
      s4 = 0.0;
      s5 = 0.0;
      s6 = 0.0;

/* Loop round every point with a positive weight. */
      for( i = 0; i < n; i++,pu++,pq++,pw++,pe++) {
         if( *pw > 0.0 ) {

/* Calculate the model Q and U values, given the current elevation and
   the IP model parameters. */
            delta1 = -2*( *pe - C );
            cosdel1 = cos( delta1 );
            sindel1 = sin( delta1 );
            delta2 = -2*( *pe - E );
            cosdel2 = cos( delta2 );
            sindel2 = sin( delta2 );
            mq = A + B*cosdel1 + D*cosdel2;
            mu = F + B*sindel1 + D*sindel2;

/* Calculate the Q and U residuals at the current elevation. */
            dq = *pq - mq;
            du = *pu - mu;

/* Increment the sums. */
            *f += (*pw)*( dq*dq + du*du );
            s1 += (*pw)*dq;
            s2 = (*pw)*( dq*cosdel1 + du*sindel1 );
            s3 = (*pw)*( -dq*sindel1 + du*cosdel1 );
            s4 = (*pw)*( dq*cosdel2 + du*sindel2 );
            s5 = (*pw)*( -dq*sindel2 + du*cosdel2 );
            s6 += (*pw)*du;
         }
      }

      *f /= params->sw;
      s1 *= -2/params->sw;
      s2 *= -2/params->sw;
      s3 *= -4*B/params->sw;
      s4 *= -2/params->sw;
      s5 *= -4*D/params->sw;
      s6 *= -2/params->sw;

      gsl_vector_set( df, 0, s1 );
      gsl_vector_set( df, 1, s2 );
      gsl_vector_set( df, 2, s3 );
      gsl_vector_set( df, 3, s4 );
      gsl_vector_set( df, 4, s5 );
      gsl_vector_set( df, 5, s6 );
   }
}

static void smf1_df( const gsl_vector *v, void *pars, gsl_vector *df ){
/*
*   Service routine that returns the gradient of the function to be
*   minimised (df/dA, df/dB, df/dC, df/dD ), assuming the IP model
*   parameters stored in "v".
*/
   double f;
   smf1_fdf( v, pars, &f, df );
}

static double smf1_f( const gsl_vector *v, void *pars ) {
/*
*  Service routine that returns the value of the function to be minimised,
*  assuming the IP model parameters stored in "v".
*/

   double A, B, C, D, E, F, dq, du, mq, mu, mp, delta1, cosdel1, sindel1,
          delta2, cosdel2, sindel2, f;
   const double *pe, *pq, *pu, *pw;
   dim_t n, i;
   Params *params = (Params *) pars;

/* Get pointers to the first Q, U, weight and elevation value. */
   n = params->n;
   pq = params->aq;
   pu = params->au;
   pw = params->aw;
   pe = params->ae;

/* First do the calculations for a single component model. */
   if( params->imodel == SINGLE_COMP ) {

/* Get the parameters of the IP model. */
      A = gsl_vector_get( v, 0 );
      B = gsl_vector_get( v, 1 );
      C = gsl_vector_get( v, 2 );
      D = gsl_vector_get( v, 3 );

/* Initialise the returned value. */
      f = 0.0;

/* Loop round every point with a positive weight. */
      for( i = 0; i < n; i++,pu++,pq++,pw++,pe++) {
         if( *pw > 0.0 ) {

/* Calculate the model Q and U values, given the current elevation and
   the IP model parameters. */
            delta1 = -2*( *pe - D );
            cosdel1 = cos( delta1 );
            sindel1 = sin( delta1 );
            mp = A + B*(*pe) + C*(*pe)*(*pe);
            mq = mp*cosdel1;
            mu = mp*sindel1;

/* Calculate the Q and U residuals at the current elevation. */
            dq = *pq - mq;
            du = *pu - mu;

/* Increment the sum. */
            f += (*pw)*( dq*dq + du*du );
         }
      }

/* Nowdo the calculations for a double component model. */
   } else {

/* Get the parameters of the IP model. */
      A = gsl_vector_get( v, 0 );
      B = gsl_vector_get( v, 1 );
      C = gsl_vector_get( v, 2 );
      D = gsl_vector_get( v, 3 );
      E = gsl_vector_get( v, 4 );
      F = gsl_vector_get( v, 5 );

/* Initialise the returned value. */
      f = 0.0;

/* Loop round every point with a positive weight. */
      for( i = 0; i < n; i++,pu++,pq++,pw++,pe++) {
         if( *pw > 0.0 ) {

/* Calculate the model Q and U values, given the current elevation and
   the IP model parameters. */

/* Calculate the model Q and U values, given the current elevation and
   the IP model parameters. */
            delta1 = -2*( *pe - C );
            cosdel1 = cos( delta1 );
            sindel1 = sin( delta1 );
            delta2 = -2*( *pe - E );
            cosdel2 = cos( delta2 );
            sindel2 = sin( delta2 );
            mq = A + B*cosdel1 + D*cosdel2;
            mu = F + B*sindel1 + D*sindel2;

/* Calculate the Q and U residuals at the current elevation. */
            dq = *pq - mq;
            du = *pu - mu;

/* Increment the sum. */
            f += (*pw)*( dq*dq + du*du );
         }
      }
   }

/* Normalise the sum and return it. */
   return f/params->sw;

}



static void smf1_linfit( dim_t n, const int *xindex, const double *wlist,
                         const double *xlist, const double *ylist, double *slope,
                         double *offset, double *rms, double *cor, int *status ){

/* Local Variables: */
   double limit;
   double res;
   double sw;
   double swx;
   double swxx;
   double swxy;
   double swy;
   double swyy;
   double w;
   double x;
   double y;
   int iter;
   int ok;
   dim_t i;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Avoid compiler warnings about uninitialised variables. */
   limit = 0.0;

/* Three sigma-clipping iterations. */
   for( iter = 0; iter < 3; iter++ ) {

/* Initialise running sums for remaining data points. */
      sw = 0.0;
      swx = 0.0;
      swxx = 0.0;
      swxy = 0.0;
      swyy = 0.0;
      swy = 0.0;

/* Loop round each data point. */
      for( i = 0; i < n; i++ ) {

/* Get the x and y values for this point. */
         x = xlist[ xindex[ i ] ];
         y = ylist[ i ];

/* If we have a previous fit, get the residual of the Y value from the
   fit and check it is smaller than the current limit. */
         if( iter != 0 ) {
            res = fabs( y - ( (*slope)*x + (*offset) ) );
            ok = ( res < limit );
         } else {
            ok = 1;
         }

/* Include the current point in the running sums if it is close enough to
   the previous fit. */
         if( ok ) {
            w = wlist[ i ];
            sw += w;
            swx += w*x;
            swxx += w*x*x;
            swxy += w*x*y;
            swyy += w*y*y;
            swy += w*y;
         }
      }

/* Calculate the slope and offset of the best fitting straight line. */
      *slope = (swxy*sw - swx*swy)/(swxx*sw - swx*swx);
      *offset = (swxx*swy - swxy*swx)/(swxx*sw - swx*swx);

/* Calculate the RMS deviation of the OK points from the line. Multiply
   it by 3.0 to get the three sigma rejection limit for the next iteration. */
      *rms = sqrt( ( swyy - (*slope)*swxy - (*offset)*swy )/sw );
      limit = 3.0*(*rms);
   }

/* Get the correlation between the remaining beta and azimuth values. */
   *cor = fabs( sw*swxy - swx*swy )/( sqrt(sw*swxx-swx*swx)*sqrt(sw*swyy-swy*swy) );

}


static void smf1_reject( int n, double *vals, int *status ){

/* Local Variables: */
   double medval;
   double medres;
   double res[ MXBINSIZE ];
   double thresh;
   int i;
   int indx[ MXBINSIZE ];
   int j;
   int ngood;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Check arguments */
   if( n > MXBINSIZE ) {
      *status = SAI__ERROR;
      errRep( " ", "smf1_reject: input array too large", status );
      return;
   }

/* Count the non-bad values. */
   ngood = 0;
   for( i = 0; i < n; i++ ) {
      if( vals[ i ] != VAL__BADD ) ngood++;
   }
   if( ngood > 2 ) {

/* Get an index that sorts the values into increasing order. Any
   VAL__BADD values are put at the end. */
      for( i = 0; (int) i < n; i++ ) indx[ i ] = i;

#ifdef HAVE_QSORT_R_BSD
      qsort_r( indx, n, sizeof(*indx), vals, smf1_qsort_bsd );
#else
      qsort_r( indx, n, sizeof(*indx), smf1_qsort, vals );
#endif

/* Find the median of the values. */
      if( ngood % 2 == 0 ) {
         medval = 0.5*( vals[ indx[ngood/2] ] + vals[ indx[ngood/2 - 1] ] );
      } else {
         medval = vals[ indx[ngood/2] ];
      }

/* Find the absolute residual from the median at each point. */
      for( i = 0; i < n; i++ ) {
         j = indx[ i ];
         if( vals[ j ] != VAL__BADD ) {
            res[ j ] = fabs( vals[ j ] - medval );
         } else {
            res[ j ] = VAL__BADD;
         }
      }

/* Sort the absolute residuals and find their median. */
      for( i = 0; (int) i < n; i++ ) indx[ i ] = i;
#ifdef HAVE_QSORT_R_BSD
      qsort_r( indx, n, sizeof(*indx), res, smf1_qsort_bsd );
#else
      qsort_r( indx, n, sizeof(*indx), smf1_qsort, res );
#endif

      if( ngood % 2 == 0 ) {
         medres = 0.5*( res[ indx[ngood/2] ] + res[ indx[ngood/2 - 1] ] );
      } else {
         medres = res[ indx[ngood/2] ];
      }

/* Reject points that are more than 15 times the median residual away from
   the median value. */
      thresh = 15*medres;
      for( i = 0; i < n; i++ ) {
         if( vals[ i ] != VAL__BADD ) {
            if( fabs( vals[ i ] - medval ) > thresh ) vals[ i ] = VAL__BADD;
         }
      }
   }
}

static int smf1_madebyskyloop( int indf, int *status ){

/* Local Variables; */
   char app[ 80 ];
   int irec;
   int nrec;
   int result;

/* Initialise */
   result = 0;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* Get the number of History records in the NDF. */
   ndfHnrec( indf, &nrec, status );

/* Loop over them all in reverse order, looking for record that was
   generated by skyloop. */
   for( irec = nrec; irec > 0; irec-- ) {

/* Get the name of the application that created the current history
   record. */
      ndfHinfo( indf, "APPLICATION", irec, app, sizeof(app), status );

/* Convert to upper case. */
      astChrCase( NULL, app, 1, sizeof( app ) );

/* If it contains "SKYLOOP", set the returned flag and leave the loop. */
      if( strstr( app, "SKYLOOP" ) ) {
         result = 1;
         break;
      }
   }

   return result;
}



static void smf1_logread( const char *path, dim_t *n, double **q, double **u,
                          double **w, double **e, int *iwave, int *status ){

/* Local variables; */
   FILE *fd;
   char **words;
   char line[1024];
   int iword;
   int nword;
   int stage;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Open the file. Report an error and return immediately if it cannot be
   opened. */
   fd = fopen( path, "r" );
   if( !fd ) {
      *status = SAI__ERROR;
      errRepf( " ", " Failed to open input text file '%s' specified by "
               "parameter INLOGS", status, path );
      return;
   }

/* Read each line from the log file. */
   stage = 0;
   while( fgets( line, sizeof(line), fd ) ) {

/* Stage 0 - look for the wavelength line. */
      if( stage == 0 ) {
         if( strstr( line, "# wave = S8" ) == line ) {
            stage = 1;
            if( *iwave == NONE ) {
               *iwave = S8;
            } else if( *iwave != S8 ) {
               *status = SAI__ERROR;
               errRepf( " ", "Input text file '%s' holds 850 um data - "
                        "but previous input data was for 450 um.", status,
                        path );
               break;
            }
         } else if( strstr( line, "# wave = S4" ) == line ) {
            stage = 1;
            if( *iwave == NONE ) {
               *iwave = S4;
            } else if( *iwave != S4 ) {
               *status = SAI__ERROR;
               errRepf( " ", "Input text file '%s' holds 450 um data - "
                        "but previous input data was for 850 um.", status,
                        path );
               break;
            }
         } else if( line[0] != '#' ) {
            break;
         }

/* Stage 1 - look for the column headers. */
      } else if( stage == 1 ) {
         if( strstr( line, LOGCOLS ) == line ) {
            stage = 2;
         } else if( line[0] != '#' ) {
            break;
         }

/* Stage 2 - read the numerical values. */
      } else if( stage == 2 ) {
         if( line[0] == '#' ) {
            stage = 3;
            break;
         } else {

/* Split the line into space-separated words. ABort if it does not have
   the expected number of words. */
            words = astChrSplit( line, &nword );
            if( nword != LOGCOLCNT ) {
               stage = 3;
               break;
            }

/* Grow the returned arrays. */
            *q = astGrow( *q, *n + 1, sizeof( *q ) );
            *u = astGrow( *u, *n + 1, sizeof( *u ) );
            *w = astGrow( *w, *n + 1, sizeof( *w ) );
            *e = astGrow( *e, *n + 1, sizeof( *e ) );
            if( *status == SAI__OK ) {

/* Convert the required words to floating point and append to the extended
   arrays. Convert angles from degrees to radians. */
               (*q)[ *n ] = astChr2Double( words[ 6 ] );            /* dQfp */
               (*u)[ *n ] = astChr2Double( words[ 7 ] );            /* dUfp */
               (*w)[ *n ] = astChr2Double( words[ 13 ] );           /* wgt */
               (*e)[ *n ] = astChr2Double( words[ 10 ] )*AST__DD2R; /* el */
               (*n)++;
            }

/* Free all the words. */
            for( iword = 0; iword < nword; iword++ ) {
               words[ iword ] = astFree( words[ iword ] );
            }
            words = astFree( words );
         }
      }
   }

/* Report an error if we didn't get to stage 2. */
   if( stage != 2 ) {
      *status = SAI__ERROR;
      errRepf( " ", "Cannot use input log file '%s' - it has unexpected structure.",
               status, path );
   }

/* Close the input log file. */
   fclose( fd );

}


static void smf1_circle_fitter( int nobs, double *qn, double *un,
                                double *unw, double *qnw, int nbin,
                                const int *binsize, const int *indx,
                                const double *alist, double *qlist,
                                double *qwlist, double *ulist,
                                double *uwlist, double *malist,
                                double *rad, double *uc, double *qc,
                                double *rms, double *sigma, int *status ){

/* Local Variables: */
   double Dq;
   double Du;
   double Qc;
   double Rad;
   double Rms;
   double Uc;
   double maxres;
   double res;
   double sa;
   double saw;
   double sq;
   double sqw;
   double sres2;
   double su;
   double suw;
   double sw;
   double this_rad;
   double vsum;
   double wa;
   int again;
   int iaz;
   int ibin;
   int ik;
   int k;
   int kmax;
   int nbin_good;
   int nuse;

/* Initialise */
   *rad = VAL__BADD;
   *uc = VAL__BADD;
   *qc = VAL__BADD;
   *rad = VAL__BADD;
   *rms = VAL__BADD;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Loop round doing a fit and then rejecting outlier (Q,U) points. */
   again = 1;
   while( again && *status == SAI__OK ) {

/* Get the mean of the standard deviations in the remaining normalised Q
   and U values. */
      nuse = 0;
      vsum = 0.0;
      for( k = 0; k < nobs; k++ ) {
         if( qn[ k ] != VAL__BADD && un[ k ] != VAL__BADD ) {
            vsum += 1.0/qnw[ k ] + 1.0/unw[ k ];
            nuse++;
         } else {
            qn[ k ] = VAL__BADD;
            un[ k ] = VAL__BADD;
         }
      }

/* Check we have sufficient useble values */
      if( nuse < 6 ) break;

/* Get the mean standard deviation in a normalised Q or U value. */
      *sigma = sqrt( vsum/(2*nuse) );

/* Find the mean normalised Q and U in each azimuth bin at this pixel,
   and the associated weights. Also find the weighted mean azimuth in
   each bin. */
      nbin_good = 0;
      ik = 0;
      for( ibin = 0; ibin < nbin; ibin++ ) {
         sq = 0.0;
         sqw = 0.0;
         su = 0.0;
         suw = 0.0;
         sa = 0.0;
         saw = 0.0;

         for( iaz = 0; iaz < binsize[ ibin ]; iaz++,ik++ ) {
            k = indx[ ik ];
            if( qn[ k ] != VAL__BADD ) {
               sq += qnw[ k ]*qn[ k ];
               sqw += qnw[ k ];

               su += unw[ k ]*un[ k ];
               suw += unw[ k ];

               wa = sqrt( qnw[ k ]*unw[ k ] );
               sa += wa*alist[ k ];
               saw += wa;
            }
         }

         if( sqw != 0.0 && suw != 0.0 ) {
            qlist[ ibin ] = sq/sqw;
            qwlist[ ibin ] = sqw;
            ulist[ ibin ] = su/suw;
            uwlist[ ibin ] = suw;
            malist[ ibin ] = sa/saw;
            nbin_good++;
         } else {
            qlist[ ibin ] = VAL__BADD;
            qwlist[ ibin ] = 0.0;
            ulist[ ibin ] = VAL__BADD;
            uwlist[ ibin ] = 0.0;
            malist[ ibin ] = VAL__BADD;
         }
      }

/* Check we have at leats four non-empty bins. */
      if( nbin_good < 4 ) break;

/* Fit a circle to the above mean normalised (Q,U) values. At this point
   the (Q,U) values are with respect to tracking north. */
      smf_fit_circle( nbin, ulist, qlist, uwlist, qwlist, &Rad, &Uc,
                      &Qc, &Rms, status );

/* Check a circle was fitted successfully. */
      if( Rad == VAL__BADD ) break;

/* Find the RMS residual of the individual observation's (Q,U) points
   from the fitted circle. Also find the point with the largest residual. */
      maxres = 0;
      kmax = nobs;
      sres2 = 0.0;
      sw = 0.0;
      for( k = 0; k < nobs; k++ ) {
         if( qn[ k ] != VAL__BADD ) {
            Du = un[ k ] - Uc;
            Dq = qn[ k ] - Qc;
            this_rad = sqrt( Du*Du + Dq*Dq );
            res = Rad - this_rad;
            wa = sqrt( qnw[ k ]*unw[ k ] );
            sres2 += wa*res*res;
            sw += wa;

            res = fabs( res );
            if( res > maxres ) {
               maxres = res;
               kmax = k;
            }

         }
      }
      Rms = sqrt( sres2/sw );

/* Save the results of the current fit. */
      *rad = Rad;
      *uc = Uc;
      *qc = Qc;
      *rad = Rad;
      *rms = Rms;

/* We reject the (Q,U) with the largest residual and do another fit if
   the largest residual is more than 3 sigma. */
      if( maxres > 3*Rms ) {
         un[ kmax ] = VAL__BADD;
         qn[ kmax ] = VAL__BADD;
         unw[ kmax ] = 0.0;
         qnw[ kmax ] = 0.0;
         again = 1;
      } else {
         again = 0;
      }
   }
}

