/* sc2store - routines for storing SCUBA-2 data to disk

   History :
    12Aug2004 : original (bdk)
    17Feb2005 : add sc2_heat (bdk)
    05Dec2005 : include star/hds_types.h (EC)
    05Dec2005 : remove star/hds_types.h, star/hds.h and extra ndf.h (bdk)
    23Jan2006 : replace star/hds.h (bdk)
    25Jul2006 : merge with ACSIS state structure usage (timj)
    29Jan2007 : Added ACS_OFFEXPOSURE (dsb)
    15May2007 : declare global HDSLOCs static (bdk)
    12Jul2007 : change names of static variables by prefixing them all with
                sc2store_ (bdk)
    30Oct2007 : standardise on size_t where possible for variables representing
                number of items (bdk)
    05Nov2007 : add sc2store_bscale and sc2store_compflag (bdk)
    09Nov2007 : have separate sc2store_rdbscale and _wrbscale (bdk)
    11Nov2007 : store compressed data as short, not unsigned short (bdk)
    19Jun2008 : ifdef out kaplibs code not used by DA (timj)
    24Jun2008 : Add tcs_percent_cmp. Some const warnings.
    22Sep2010 : Add sc2_bias and sc2_fputemp (timj)
    09Nov2010 : Add delta compression option (dsb)
    11Jan2011 : Compress JCMTSTATE using simple compression scheme (timj)
    23Feb2012 : Add sc2_1kntdtemp (timj)
    13Dec2012 : Use kpg1Hsect to get a section of an HDS array (dsb)
    06Apr2017 : Add telpar dtai to time series frameset (gsb)
*/

#define _POSIX_C_SOURCE 200112L

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#include "sae_par.h"
#include "prm_par.h"
#include "dat_par.h"
#include "dat_err.h"
#include "star/hds.h"
#include "star/kaplibs.h"
#include "star/util.h"
#include "ast.h"
#include "ndf.h"
#include "star/ndg.h"
#include "star/atl.h"
#include "mers.h"
#include "ems.h"
#include "Dits_Err.h"
#include "Ers.h"

#include "dream_par.h"

#include "jcmt/state.h"
#include "sc2ast.h"
#include "sc2store_par.h"
#include "sc2store_sys.h"
#include "sc2store.h"

/* Private functions */
static double sc2store_tzoffset(void);
static void sc2store_initialise ( int *status );
static void sc2store_fillbounds( size_t colsize, size_t rowsize, size_t dim3,
                                 int lbnd[], int ubnd[], int * status );

/* Private globals */

static HDSLoc *sc2store_bscaleloc = NULL;    /* HDS locator to scale factor */
static sc2store_cmptype sc2store_compflag = SC2STORE__BDK; /* Type of data compression */
static int sc2store_dindf = NDF__NOID;       /* NDF identifier for dark SQUID values */
static int sc2store_htindf = NDF__NOID;      /* NDF identifier for heater track values */
static HDSLoc *sc2store_dreamwtloc = NULL;   /* HDS locator to DREAM weights */
static HDSLoc *sc2store_drmloc = NULL;       /* HDS locator to DREAM parameters */
static char sc2store_errmess[132];           /* error string */
static HDSLoc *sc2store_fdataloc = NULL;     /* HDS locator to FLATDATA structure */
static int sc2store_findf = NDF__NOID;       /* NDF identifier for flat calibration */
static HDSLoc *sc2store_fnameloc = NULL;     /* HDS locator to FLATNAME structure */
static HDSLoc *sc2store_fparloc = NULL;      /* HDS locator to FLATPAR structure */
static HDSLoc *sc2store_frefresloc = NULL;   /* HDS locator to REFRES structure */
static HDSLoc *sc2store_incomploc = NULL;    /* HDS locator to INCOMPS structure */
static int sc2store_indf = NDF__NOID;        /* main NDF identifier */
static int sc2store_indfgridext = NDF__NOID; /* NDF identifier for grid extent */
static int sc2store_indfgridwts = NDF__NOID; /* NDF identifier for weights array */
static int sc2store_indfinvmatx = NDF__NOID; /* NDF identifier for inverse
                                                matrix */
static int sc2store_indfjigext = NDF__NOID;  /* NDF identifier for DREAM extent */
static int sc2store_indfqual = NDF__NOID;    /* NDF identifier for quality */
static int sc2store_indfwindext = NDF__NOID; /* NDF identifier for window extent */
static int sc2store_indfwt = NDF__NOID;      /* main NDF identifier for weights */
static int sc2store_initialised = 0;         /* NDF initialisation flag */
static HDSLoc *sc2store_jcmtstateloc = NULL; /* HDS locator to JCMTSTATE structure */
static int sc2store_jigvndf = NDF__NOID;     /* NDF identifier for jiggle vertices */
static int sc2store_jigpndf = NDF__NOID;     /* NDF identifier for the SMU path */
static double sc2store_rdbscale = 1.0;       /* read data compression divisor */
static int sc2store_sc2open = 0;             /* flag for file open */
static HDSLoc *sc2store_scu2redloc = NULL;   /* HDS locator to SCU2RED structure */
static HDSLoc *sc2store_scuba2loc = NULL;    /* HDS locator to SCUBA2 structure */
static int sc2store_sindf = NDF__NOID;       /* NDF identifier for subtraction frame */
static double sc2store_wrbscale = 1.0;       /* write data compression divisor */
static int sc2store_zindf = NDF__NOID;       /* NDF identifier for compression zero
                                                offsets */

/* Control history writing. Do not want this to happen when SMURF
   is used so we key on a definition that SMURF will have but the
   DA will not. Choose an application name. */
#define APPNAME "SCUBA-2 DA"
#ifndef PACKAGE_UPCASE
#  define SC2STORE_WRITE_HISTORY
#endif

/* Enable AST malloc usage */
#ifdef PACKAGE_UPCASE
#define malloc astMalloc
#define free astFree
#define calloc( count, size )   astCalloc( count, size )
#endif

/*+ sc2store_compress - compress frame of integers to unsigned short */

void sc2store_compress
(
size_t nval,            /* number of values in frame (given) */
const int stackz[],     /* stackzero frame to be subtracted (given) */
int digits[],           /* integer values (given and returned) */
int *bzero,             /* zero offset for compressed values (returned) */
short data[],           /* compressed values (returned) */
size_t *npix,           /* number of incompressible values (returned) */
int pixnum[],           /* indices of incompressible values (returned) */
int pixval[],           /* incompressible values (returned) */
int *status             /* global status (given and returned) */
)
/*  Description :
     Find the typical value of the frame and use it as bzero. Turn each
     integer value into a short by subtracting bzero. If this
     is not possible, store the incompressible value separately and
     replace it by a bad value in the compressed array.

   Authors :
    B.D.Kelly (ROE)

   History :
    24Sep2004:  original (bdk)
    20Mar2007 : Use const in signature (TIMJ)
    01Nov2007 : fix bzero at zero as attempt to avoid bad pixel problems (bdk)
    11Nov2007 : compress to short instead of unsigned short (bdk)
*/
{
   int j;               /* loop counter */
   int temp;            /* intermediate compressed value */
   int bmax;            /* maximum data value */

   if ( !StatusOkP(status) ) return;

/* Subtract the stackzero frame (ie approximate zero points for each
   bolometer) */

   for ( j=0; j<nval; j++ )
   {
      digits[j] -= stackz[j];
   }

   *bzero = digits[0];
   bmax = digits[0];
/*
   for ( j=0; j<nval; j++ )
   {
      if ( *bzero > digits[j] ) (*bzero) = digits[j];
      if ( bmax <digits[j] ) bmax = digits[j];
   }
*/
   *bzero = 0;

   *npix = 0;

   for ( j=0; j<nval; j++ )
   {
      temp = digits[j] - (*bzero);
      if ( ( temp > (int)VAL__MAXW ) || ( temp < (int)VAL__MINW ) )
      {
         data[j] = VAL__BADW;
         pixnum[*npix] = j;
         pixval[*npix] = digits[j];
         (*npix)++;
      }
      else
      {
         data[j] = (short) temp;
      }
   }
}



/*+ sc2store_creimages - create structure to store images */

void sc2store_creimages
(
int *status              /* global status (given and returned) */
)
/*
   Method :
    Create an NDF extension to store processed images.
   History :
    12Aug2004 : original (bdk)
    24Jan2005 : create DA_IMAGE as a scalar instead of an array (bdk)
    15Apr2005 : name DA_IMAGE changed to SCU2RED and da_imloc to
                sc2store_scu2redloc (bdk)
    10Apr2007 : check if extension already exists before attempting to
                create it (agg)
*/
{
   int isthere = 0;        /* Flag to denote whether extension already exists */

   if ( *status != SAI__OK ) return;

   ndfXstat ( sc2store_indf, "SCU2RED", &isthere, status );

   if ( !isthere )
   {
      ndfXnew ( sc2store_indf, "SCU2RED", "SCUBA2_MAP_ARR", 0, 0,
        &sc2store_scu2redloc, status );
   }

   sc2store_errconv ( status );
}

/*+ sc2store_cremapwts - create and write a DREAM weights file */

void sc2store_cremapwts
(
const char *filename,      /* name of HDS container file (given) */
const int *windext,              /* Table of window extents for the DREAM
                              solution (given) */
const int *gridext,              /* Table of grid extents for a single
                              bolometer (given) */
double gridsize,           /* size in arcsec of grid step (given) */
const int *jigext,               /* Table of SMU pattern extents for a single
                              bolometer (given) */
double jigsize,            /* size in arcsec of SMU step (given) */
const int gridwtsdim[],          /* dimensions of grid interpolation weights
                              (given) */
const double *gridwts,           /* grid interpolation weights (given) */
int invmatdim,             /* dimension of inverted matrix (given) */
const double *invmat,            /* inverted matrix (given) */
const int qualdim[],             /* dimensions of quality mask (given) */
const int *qual,                 /* bolometer quality mask (given) */
int *status                /* global status (given and returned) */
)
/*
   Method :
    Create and write the contents of a SCUBA-2 DREAM weights file.
   History :
    15Apr2008 : original (bdk)
    09May2008 : add windext argument (bdk)
*/
{
   int *data;                  /* pointer to top-level NDF data */
   int el;                     /* number of elements */
   HDSLoc *gridszloc = NULL;   /* HDS locator to grid step size */
   HDSLoc *jigszloc = NULL;    /* HDS locator to DREAM step size */
   int lbnd[3];                /* lower dimension bounds */
   int ndim;                   /* number of dimensions */
   int place;                  /* NDF placeholder */
   size_t *tgridext;           /* Table of grid extents for a single
                                  bolometer */
   double *tinterpwt;          /* pointer to interpolation data */
   double *tinvmat;            /* pointer to inverted matrix */
   size_t *tjigext;            /* Table of SMU pattern extents for a single
                                  bolometer */
   int *tqual;                 /* pointer to quality array */
   size_t *twindext;           /* Table of window extents for the DREAM
                                  solution */
   int ubnd[3];                /* upper dimension bounds */


   if ( *status != SAI__OK ) return;

/* Initialise Starlink error reporting NDF and start its context */

   sc2store_initialise( status );
/*
   errMark();
   ndfBegin();
*/
/* Create an HDS container file */

   ndfPlace ( NULL, filename, &place, status );

/* Create an NDF inside the container */

   ndim = 2;
   lbnd[0] = 1;
   lbnd[1] = 1;
   ubnd[0] = 1;
   ubnd[1] = 1;

   ndfNew ( "_INTEGER", ndim, lbnd, ubnd, &place, &sc2store_indfwt, status );
   ndfHcre( sc2store_indfwt, status );
   ndfMap ( sc2store_indfwt, "DATA", "_INTEGER", "WRITE", (void **)&data, &el,
     status );
   data[0] = 0;


/* Create extension for holding DREAM details */

   ndfXnew ( sc2store_indfwt, "DREAM", "DREAM_WEIGHTS", 0, 0,
     &sc2store_dreamwtloc, status );

/* Map interpolation weights array and copy data */

   ndim = 2;
   lbnd[0] = 1;
   lbnd[1] = 1;
   ubnd[0] = gridwtsdim[0];
   ubnd[1] = gridwtsdim[1];
   ndfPlace ( sc2store_dreamwtloc, "GRIDWTS", &place, status );
   ndfNew ( "_DOUBLE", ndim, lbnd, ubnd, &place, &sc2store_indfgridwts,
     status );

   ndfMap ( sc2store_indfgridwts, "DATA", "_DOUBLE", "WRITE",
     (void *)&tinterpwt, &el, status );
   memcpy ( tinterpwt, gridwts, el*sizeof(*gridwts) );

/* Map inverse matrix array and copy data */

   ndim = 1;
   lbnd[0] = 1;
   ubnd[0] = invmatdim;
   ndfPlace ( sc2store_dreamwtloc, "INVMATX", &place, status );
   ndfNew ( "_DOUBLE", ndim, lbnd, ubnd, &place, &sc2store_indfinvmatx,
     status );

   ndfMap ( sc2store_indfinvmatx, "DATA", "_DOUBLE", "WRITE", (void *)&tinvmat,
     &el, status );
   memcpy ( tinvmat, invmat, el*sizeof(*invmat) );

/* Map window extent array */

   ndim = 1;
   lbnd[0] = 1;
   ubnd[0] = 4;
   ndfPlace ( sc2store_dreamwtloc, "WINDEXT", &place, status );
   ndfNew ( "_INTEGER", ndim, lbnd, ubnd, &place, &sc2store_indfwindext,
     status );

   ndfMap ( sc2store_indfwindext, "DATA", "_INTEGER", "WRITE",
     (void *)&twindext, &el, status );
   memcpy ( twindext, windext, el*sizeof(*windext) );

/* Map grid extent array */

   ndim = 1;
   lbnd[0] = 1;
   ubnd[0] = 4;
   ndfPlace ( sc2store_dreamwtloc, "GRIDEXT", &place, status );
   ndfNew ( "_INTEGER", ndim, lbnd, ubnd, &place, &sc2store_indfgridext,
     status );

   ndfMap ( sc2store_indfgridext, "DATA", "_INTEGER", "WRITE",
     (void *)&tgridext, &el, status );
   memcpy ( tgridext, gridext, el*sizeof(*gridext) );

/* Map SMU pattern extent array */

   ndim = 1;
   lbnd[0] = 1;
   ubnd[0] = 4;
   ndfPlace ( sc2store_dreamwtloc, "JIGEXT", &place, status );
   ndfNew ( "_INTEGER", ndim, lbnd, ubnd, &place, &sc2store_indfjigext,
     status );

   ndfMap ( sc2store_indfjigext, "DATA", "_INTEGER", "WRITE", (void *)&tjigext,
     &el, status );
   memcpy ( tjigext, jigext, el*sizeof(*jigext) );

/* Put step intervals */

   datNew ( sc2store_dreamwtloc, "GRID_SIZE", "_DOUBLE", 0, 0, status );
   datFind ( sc2store_dreamwtloc, "GRID_SIZE", &gridszloc, status );
   datPut0D ( gridszloc, gridsize, status );
   datAnnul ( &gridszloc, status );

   datNew ( sc2store_dreamwtloc, "JIG_SIZE", "_DOUBLE", 0, 0, status );
   datFind ( sc2store_dreamwtloc, "JIG_SIZE", &jigszloc, status );
   datPut0D ( jigszloc, jigsize, status );
   datAnnul ( &jigszloc, status );

/* Map bolometer quality array */

   ndim = 2;
   lbnd[0] = SC2STORE__BOL_LBND;
   lbnd[1] = SC2STORE__BOL_LBND;
   ubnd[0] = qualdim[0] + SC2STORE__BOL_LBND - 1;
   ubnd[1] = qualdim[1] + SC2STORE__BOL_LBND - 1;
   ndfPlace ( sc2store_dreamwtloc, "QUAL", &place, status );
   ndfNew ( "_INTEGER", ndim, lbnd, ubnd, &place, &sc2store_indfqual,
     status );

   ndfMap ( sc2store_indfqual, "DATA", "_INTEGER", "WRITE", (void *)&tqual,
     &el, status );
   memcpy ( tqual, qual, el*sizeof(*qual) );


   sc2store_errconv ( status );

}




/*+ sc2store_decompress - decompress frame of unsigned short to integers */

void sc2store_decompress
(
size_t nval,                  /* number of values in frame (given) */
const int stackz[],           /* stackzero frame to be added (given) */
int bzero,                    /* zero offset for compressed values (given) */
const short data[],           /* compressed values (given) */
size_t npix,                  /* number of incompressible values (given) */
const int pixnum[],           /* indices of incompressible values (given) */
const int pixval[],           /* incompressible values (given) */
int digits[],                 /* integer values (returned) */
int *status                   /* global status (given and returned) */
)
/*  Description :
     Turn each unsigned short into an integer value by adding bzero and
     the stackzero frame.
     Patch-in any incompressible values listed.

   Authors :
    B.D.Kelly (ROE)

   History :
    14Oct2004 : original (bdk)
    18Feb2005 : add stackz onto uncompressible values (bdk)
    13May2005 : check for bad values (bdk)
    20Mar2007 : Use const in signature (TIMJ)
    11Nov2007 : make compressed values short instead of unsigned short (bdk)
    13Nov2007 : multiply by sc2store_rdbscale (bdk)
*/
{
   int j;               /* loop counter */

   if ( !StatusOkP(status) ) return;

/* Insert the stackzero frame (ie approximate zero points for each
   bolometer) and bzero */

   for ( j=0; j<nval; j++ )
   {
      if ( data[j] != VAL__BADW )
      {
         digits[j] = stackz[j] + bzero + (int)data[j];
      }
      else
      {
         digits[j] = VAL__BADI;
      }
   }

/* Insert any incompressible values */

   if ( npix > 0 )
   {
      for ( j=0; j<npix; j++ )
      {
         digits[pixnum[j]] = pixval[j] + stackz[pixnum[j]];
      }
   }

/* Multiply by the scale factor */

   for ( j=0; j<nval; j++ )
   {
      if ( digits[j] != VAL__BADI )
      {
         digits[j] = (int)( (double)digits[j] * sc2store_rdbscale );
      }
   }

}


/*+ sc2store_errconv - convert error message from Starlink to DRAMA */

void sc2store_errconv
(
int *status
)
/*
   History :
    12Aug2004 : original (bdk)
    14Dec2006 : Do not call ErsRep with good status (timj)
    18Dec2006 : Do not call errRlse (timj)
*/
{
   char param[17];           /* parameter name */
   int param_length;         /* length of parameter name string */
   int parlen;               /* length of parameter name */
   char opstr[133];          /* error message */
   int opstr_length;         /* length of error message string */
   int oplen;                /* length of error message */
   int tstatus;              /* local status */


   if ( *status == SAI__OK ) return;


   tstatus = *status;

   param_length = 17;
   opstr_length = 133;

   while ( tstatus != SAI__OK )
   {
      errLoad ( param, param_length, &parlen, opstr, opstr_length,
        &oplen, &tstatus );
      if ( tstatus != SAI__OK ) ErsRep ( 0, &tstatus, opstr );
   }
}


/*+ sc2store_free - unmap and close all references to output file */

void sc2store_free
(
int *status          /* global status (given and returned) */
)
/*
   History :
    12Aug2004 : original (bdk)
    04Oct2005 : check sc2store_sc2open flag (bdk)
    23Jan2006 : annul sc2store_scu2redloc (bdk)
    20Jul2006 : annul DREAM extension (agg)
    18Dec2006 : release error context (timj)
    23Oct2007 : rename structure for incompressible pixels to INCOMPS (bdk)
    23Oct2007 : rename structure for per-frame data to SCUBA2 (bdk)
    21Nov2007 : only release INCOMPS if data compressed (bdk)
*/

{
   int tstatus;      /* local status */

   if ( sc2store_sc2open == 0 ) return;

   tstatus = SAI__OK;

/* Free the locator for the SCU2RED structure*/

   if ( sc2store_scu2redloc != NULL )
   {
      datAnnul ( &sc2store_scu2redloc, &tstatus );
   }

/* Release DREAM parameters */

   if ( sc2store_drmloc != NULL )
   {
      ndfUnmap ( sc2store_jigvndf, "DATA", &tstatus );
      ndfAnnul ( &sc2store_jigvndf, &tstatus );
      ndfUnmap ( sc2store_jigpndf, "DATA", &tstatus );
      ndfAnnul ( &sc2store_jigpndf, &tstatus );
      datAnnul ( &sc2store_drmloc, &tstatus );
   }

/* Release Header values for each frame */

   sc2store_headunmap ( &tstatus );

/* Release heater tracking values for each frame */

   if ( sc2store_htindf != NDF__NOID )
   {
      ndfUnmap ( sc2store_htindf, "DATA", &tstatus );
      ndfAnnul ( &sc2store_htindf, &tstatus );
   }

/* Release Dark SQUID values for each frame */

   if ( sc2store_dindf != NDF__NOID )
   {
      ndfUnmap ( sc2store_dindf, "DATA", &tstatus );
      ndfAnnul ( &sc2store_dindf, &tstatus );
   }

   if ( sc2store_incomploc != NULL )
   {
/* Free the locator for the INCOMPS structure*/

      datAnnul ( &sc2store_incomploc, &tstatus );

/* Release compression zero offset for each frame */

      ndfUnmap ( sc2store_zindf, "DATA", &tstatus );
      ndfAnnul ( &sc2store_zindf, &tstatus );

/* Release subtracted (stackzero) frame */

      ndfUnmap ( sc2store_sindf, "DATA", &tstatus );
      ndfAnnul ( &sc2store_sindf, &tstatus );
   }

/* Release flatfield calibration */

   if ( sc2store_findf != NDF__NOID )
   {
      datUnmap ( sc2store_fparloc, &tstatus );
      datAnnul ( &sc2store_fparloc, &tstatus );
      datAnnul ( &sc2store_fnameloc, &tstatus );
      if (sc2store_frefresloc) datAnnul( &sc2store_frefresloc, &tstatus);
      datAnnul ( &sc2store_fdataloc, &tstatus );
      ndfUnmap ( sc2store_findf, "DATA", &tstatus );
      ndfAnnul ( &sc2store_findf, &tstatus );
   }

/* Release SCUBA2 and JCMTSTATE structure */

   datAnnul ( &sc2store_scuba2loc, &tstatus );
   if ( sc2store_jcmtstateloc != NULL )
   {
      datAnnul ( &sc2store_jcmtstateloc, &tstatus );
   }

/* Unmap the main data array */

   ndfAnnul ( &sc2store_indf, &tstatus );

/* Close the NDF context */

   ndfEnd ( &tstatus );
   errRlse();

   sc2store_sc2open = 0;

   sc2store_errconv ( &tstatus );

}



/*+ sc2store_getincomp - get details of incompressible pixels */

void sc2store_getincomp
(
int frame,         /* frame index (given) */
size_t *npix,      /* number of incompressible pixels (returned) */
int pixnum[],      /* indices of incompressible pixels (returned) */
int pixval[],      /* values of incompressible pixels (returned) */
int *status        /* global status (given and returned) */
)
/*
   History :
    12Aug2004 : original (bdk)
    23Nov2005 : Use Official C HDS interface (TIMJ)
    23Oct2007 : rename structure for incompressible pixels to INCOMPS (bdk)
*/
{
   int dim[2];             /* sizes of dimensions */
   int el;                 /* number of elements mapped */
   int *incomp;            /* pointer to incompressible values */
   int j;                  /* loop counter */
   HDSLoc* loc2 = NULL;    /* HDS locator */
   int ndim;               /* number of dimensions */
   int ndimx;              /* max number of dimensions */
   int place;              /* NDF placeholder */
   int strnum;             /* structure element number */
   int there;              /* existence flag */
   int uindf;              /* NDF identifier */


   if ( *status != SAI__OK ) return;

/* Get structure for incompressible pixels for n-th frame */

   strnum = frame + 1;
   datCell ( sc2store_incomploc, 1, &strnum, &loc2, status );
   datThere ( loc2, "INCOMP", &there, status );
   ndimx = 2;
   *npix = 0;

   if ( ( *status == SAI__OK ) && ( there != 0 ) )
   {
      ndfOpen ( loc2, "INCOMP", "READ", "OLD", &uindf, &place, status );
      ndfDim ( uindf, ndimx, dim, &ndim, status );

/* Map the data array */

      ndfMap ( uindf, "DATA", "_INTEGER", "READ", (void *)&incomp, &el,
        status );

/* Copy each pixel index and value */

      if ( *status == SAI__OK )
      {
         *npix = dim[1];

         for ( j=0; j<dim[1]; j++ )
         {
            pixnum[j] = incomp[2*j];
            pixval[j] = incomp[2*j+1];
         }
      }

/* Unmap the data array */

      ndfUnmap ( uindf, "DATA", status );
      ndfAnnul ( &uindf, status );

   }

/* Free the locator for the frame */

   datAnnul ( &loc2, status );

   sc2store_errconv ( status );

}



/*+ sc2store_headget - get values from the header arrays */

void sc2store_headget
(
int frame,                    /* frame index (given) */
JCMTState *head,              /* header data for the frame (returned) */
int *status                   /* global status (given and returned) */
)
/* Method :
    The mapped HDS arrays contain values for all the header elements of
    the frame in the relevant place. These are copied into the given
    structure.
   History :
    19Aug2004 : original (bdk)
    17Feb2005 : add sc2_heat (bdk)
    27Jul2006 : need to check for NULL-ness and handle all JCMTState (timj)
    15Jul2008 : fix types in macro calls for wvm_time and sc2_heat (bdk)
*/
{
   if ( *status != SAI__OK ) return;

/* Note that we have to check for NULL in case we had a missing
   component.  We also check all possible struct members not just
   SCUBA-2 ones. This is done so that SMURF can support the reading
   of the JCMTSTATE extension from other instruments in addition to
   SCUBA-2. This routine is now JCMT-specific rather than SCUBA-2
   specific and could be factored out. All JCMTSTATE access could
   usefully be factored out.
*/

/* use a macro to simplify things */

#define RETRIEVE_STATE(state, index, type, bad )                 \
   if ( sc2store_ptr[index] ) {                                  \
     size_t findex;                                              \
     if (sc2store_array[index]) {                                \
       findex = frame;                                           \
     } else {                                                    \
       findex = 0;                                               \
     }                                                           \
     head->state = ((type *)sc2store_ptr[index])[findex];        \
   } else {                                                      \
     head->state = bad;                                          \
   }

   RETRIEVE_STATE( rts_num, RTS_NUM, unsigned int, (unsigned int)VAL__BADI );
   RETRIEVE_STATE( rts_end, RTS_END, double, VAL__BADD );

   RETRIEVE_STATE( smu_x, SMU_X, double, VAL__BADD );
   RETRIEVE_STATE( smu_y, SMU_Y, double, VAL__BADD );
   RETRIEVE_STATE( smu_z, SMU_Z, double, VAL__BADD );
   RETRIEVE_STATE( smu_jig_index, SMU_JIG_INDEX, short, VAL__BADW );
   RETRIEVE_STATE( smu_az_jig_x, SMU_AZ_JIG_X, double, VAL__BADD );
   RETRIEVE_STATE( smu_az_jig_y, SMU_AZ_JIG_Y, double, VAL__BADD );
   RETRIEVE_STATE( smu_az_chop_x, SMU_AZ_CHOP_X, double, VAL__BADD );
   RETRIEVE_STATE( smu_az_chop_y, SMU_AZ_CHOP_Y, double, VAL__BADD );
   RETRIEVE_STATE( smu_tr_jig_x, SMU_TR_JIG_X, double, VAL__BADD );
   RETRIEVE_STATE( smu_tr_jig_y, SMU_TR_JIG_Y, double, VAL__BADD );
   RETRIEVE_STATE( smu_tr_chop_x, SMU_TR_CHOP_X, double, VAL__BADD );
   RETRIEVE_STATE( smu_tr_chop_y, SMU_TR_CHOP_Y, double, VAL__BADD );

   RETRIEVE_STATE( tcs_tai, TCS_TAI, double, VAL__BADD );
   RETRIEVE_STATE( tcs_airmass, TCS_AIRMASS, double, VAL__BADD );
   RETRIEVE_STATE( tcs_az_ang, TCS_AZ_ANG, double, VAL__BADD );
   RETRIEVE_STATE( tcs_az_ac1, TCS_AZ_AC1, double, VAL__BADD );
   RETRIEVE_STATE( tcs_az_ac2, TCS_AZ_AC2, double, VAL__BADD );
   RETRIEVE_STATE( tcs_az_dc1, TCS_AZ_DC1, double, VAL__BADD );
   RETRIEVE_STATE( tcs_az_dc2, TCS_AZ_DC2, double, VAL__BADD );
   RETRIEVE_STATE( tcs_az_bc1, TCS_AZ_BC1, double, VAL__BADD );
   RETRIEVE_STATE( tcs_az_bc2, TCS_AZ_BC2, double, VAL__BADD );
   RETRIEVE_STATE( tcs_index,  TCS_INDEX, short, VAL__BADW );
   RETRIEVE_STATE( tcs_percent_cmp,  TCS_PERCENT_CMP, short, VAL__BADW );
   RETRIEVE_STATE( tcs_tr_ang, TCS_TR_ANG, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_ac1, TCS_TR_AC1, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_ac2, TCS_TR_AC2, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_dc1, TCS_TR_DC1, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_dc2, TCS_TR_DC2, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_bc1, TCS_TR_BC1, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_bc2, TCS_TR_BC2, double, VAL__BADD );
   RETRIEVE_STATE( tcs_en_dc1, TCS_EN_DC2, double, VAL__BADD );
   RETRIEVE_STATE( tcs_en_dc2, TCS_EN_DC2, double, VAL__BADD );
   RETRIEVE_STATE( tcs_dm_abs, TCS_DM_ABS, double, VAL__BADD );
   RETRIEVE_STATE( tcs_dm_rel, TCS_DM_REL, double, VAL__BADD );

   RETRIEVE_STATE( jos_drcontrol, JOS_DRCONTROL, short, VAL__BADW );

   RETRIEVE_STATE( enviro_rel_hum, ENVIRO_REL_HUM, float, VAL__BADR );
   RETRIEVE_STATE( enviro_pressure, ENVIRO_PRESSURE, float, VAL__BADR );
   RETRIEVE_STATE( enviro_air_temp, ENVIRO_AIR_TEMP, float, VAL__BADR );

   RETRIEVE_STATE( wvm_t12, WVM_T12, float, VAL__BADR );
   RETRIEVE_STATE( wvm_t42, WVM_T42, float, VAL__BADR );
   RETRIEVE_STATE( wvm_t78, WVM_T78, float, VAL__BADR );
   RETRIEVE_STATE( wvm_time, WVM_TIME, double, VAL__BADD );

   RETRIEVE_STATE( sc2_heat, SC2_HEAT, unsigned short, VAL__BADUW );
   RETRIEVE_STATE( sc2_bias, SC2_BIAS, unsigned short, VAL__BADUW );
   RETRIEVE_STATE( sc2_mixtemp, SC2_MIXTEMP, float, VAL__BADR );
   RETRIEVE_STATE( sc2_fputemp, SC2_FPUTEMP, float, VAL__BADR );
   RETRIEVE_STATE( sc2_1kntdtemp, SC2_1KNTDTEMP, float, VAL__BADR );

   RETRIEVE_STATE( acs_exposure, ACS_EXPOSURE, float, VAL__BADR );
   RETRIEVE_STATE( acs_offexposure, ACS_OFFEXPOSURE, float, VAL__BADR );
   RETRIEVE_STATE( acs_no_prev_ref, ACS_NO_PREV_REF, short, VAL__BADW );
   RETRIEVE_STATE( acs_no_next_ref, ACS_NO_NEXT_REF, short, VAL__BADW );
   RETRIEVE_STATE( acs_no_ons, ACS_NO_ONS, short, VAL__BADW );

   RETRIEVE_STATE( fe_lofreq, FE_LOFREQ, double, VAL__BADD );
   RETRIEVE_STATE( fe_doppler, FE_DOPPLER, double, VAL__BADD );

   RETRIEVE_STATE( pol_ang, POL_ANG, double, VAL__BADD );
   RETRIEVE_STATE( fts_pos, FTS_POS, float, VAL__BADR );

/* Now do the character arrays. We use cnfExprt to allow nul termination.
   Note that the struct does allocate a space for a nul in addition to the
   HDS component size.
*/

#define RETRIEVE_CHAR( state, index, len )                              \
   if ( sc2store_ptr[index] ) {                                         \
     size_t findex;                                                     \
     if (sc2store_array[index] ) {                                      \
       findex = frame;                                                  \
     } else {                                                           \
       findex = 0;                                                      \
     }                                                                  \
     cnfImprt( (char*)sc2store_ptr[index]+len*findex, len, head->state ); \
     (head->state)[len] = '\0';                                         \
   } else {                                                             \
     (head->state)[0] = '\0';                                           \
   }

   RETRIEVE_CHAR( smu_chop_phase, SMU_CHOP_PHASE, JCMT__SZSMU_CHOP_PHASE );
   RETRIEVE_CHAR( tcs_beam, TCS_BEAM, JCMT__SZTCS_BEAM );
   RETRIEVE_CHAR( tcs_source, TCS_SOURCE, JCMT__SZTCS_SOURCE );
   RETRIEVE_CHAR( tcs_tr_sys, TCS_TR_SYS, JCMT__SZTCS_TR_SYS );
   RETRIEVE_CHAR( acs_source_ro, ACS_SOURCE_RO, JCMT__SZACS_SOURCE_RO );

   /* tidy name space */
#undef RETRIEVE_STATE
#undef RETRIEVE_CHAR
}



/*+ sc2store_headcremap - create and map the header arrays */

void sc2store_headcremap
(
const HDSLoc *headloc,           /* HDS locator (given) */
size_t nframes,                  /* number of frames to be created (given) */
inst_t instrument,               /* instrument code (given) */
int *status                      /* global status (given and returned) */
)

/* Method :
    Create HDS arrays of size nframes for each header type within the HDS
    component identified by headloc and store the pointers. The instrument
    code is used to determine which components are written. INST__ALL should
    be used if all components are required.

   History :
    18Aug2004 : original (bdk)
    23Nov2005 : Use Official C HDS interface (TIMJ)
    27Jan2006 : Kluge initialising of sc2store_loc[] array
                Risks, overwriting a valid locator (TIMJ)
    27Jul2006 : Use ACSIS/SCUBA-2 merged approach (TIMJ)
    28Jul2006 : Add instrument argument (TIMJ)
    20Mar2007 : use const in signature (timj)
*/
{
   int dim[2];
   int j;
   int ndim;
   int pos;
   size_t len;

/* This routine only creates and maps SCUBA-2 components */

   if ( *status != SAI__OK ) return;

   dim[0] = nframes;
   ndim = 1;

   for ( j=0; j<JCMT_COMP_NUM; j++ )
   {
/* what index should we use? */

      pos = hdsRecords[j].position;

/* Should really initialise outside this routine */

      sc2store_loc[pos] = NULL;
      sc2store_ptr[pos] = NULL;

/* see if component is required for this instrument or not */

      if ( hdsRecords[j].instrument & instrument )
      {
/* create the component */

         datNew ( headloc, hdsRecords[j].name, hdsRecords[j].type,
           ndim, dim, status );

         if ( *status != SAI__OK ) break;

         datFind ( headloc, hdsRecords[j].name, &(sc2store_loc[pos]),
           status );

         if ( *status != SAI__OK ) break;

/* map the data array */

         datMap ( sc2store_loc[pos], hdsRecords[j].type, "WRITE",
	   ndim, dim, &(sc2store_ptr[pos]), status );

/* if this is a string component then we can fill it with
   blanks. Other components will not be initialised but they
   will always be filled later on. */

         if ( strncmp( "_CHAR", hdsRecords[j].type, 5) == 0 )
	 {
	    datLen( sc2store_loc[pos], &len, status );
	    if ( *status == SAI__OK )
	    {
	       memset ( sc2store_ptr[pos], ' ', dim[0] * len );
	    }
         }
      }

      if ( *status != SAI__OK ) break;

   }

}

/*+ sc2store_putjcmtstate - put JCMTState values in the header arrays */

void sc2store_putjcmtstate
(
size_t nframes,             /* number of frames (given) */
const JCMTState head[],     /* header data for each frame (given) */
int *status                 /* global status (given and returned) */
)
/* Method :
    Writes JCMTState information to the relevant chunks of mapped HDS
    arrays. See also sc2store_writejcmtstate.
   History :
    29Dec2010 : original as a wrapper around sc2store_headput (timj)
*/
{

   size_t j;

   if ( *status != SAI__OK ) return;

   for ( j=0; j<nframes; j++ )
   {
      sc2store_headput ( j, head[j], status );
   }

}

/*+ sc2store_headput - put frame state values into header arrays */

void sc2store_headput
(
int frame,                    /* frame index (given) */
JCMTState head,               /* header data for the frame (given) */
int *status                   /* global status (given and returned) */
)
/* Method :
    The given structure contains values for all the header elements of
    the frame. These are copied into the relevant place in the mapped HDS
    arrays. No data compression is applied. The assumption is that the
    full set of JCMTSTATE is written out to the items mapped using
    sc2store_headcremap. Use sc2store_writejcmtstate to do data
    compression.
   History :
    18Aug2004 : original (bdk)
    17Feb2005 : add sc2_heat (bdk)
    27Jul2006 : use JCMTState (timj)
    7Jul2008  : fix type of WVM_TIME and SC2_HEAT. Do not store SMU_XYZ or JOS_DRCONTROL.
                WVM_TH/TW/QUAL not used any more.
*/
{
   if ( *status != SAI__OK ) return;

   /* Use a macro to make the code a bit more readable */
#define STORE_STATE( state, index, type ) \
   if (sc2store_ptr[index]) ((type *)sc2store_ptr[index])[frame] = head.state

#define STORE_CHAR( state, index, len ) \
   if (sc2store_ptr[index]) cnfExprt( head.state, (char *)sc2store_ptr[index]+len*frame, len )

   /* Real Time Sequencer */
   STORE_STATE( rts_num, RTS_NUM, int );
   STORE_STATE( rts_end, RTS_END, double );

   /* Secondary Mirror */
   STORE_CHAR( smu_chop_phase, SMU_CHOP_PHASE, JCMT__SZSMU_CHOP_PHASE );
   STORE_STATE( smu_jig_index, SMU_JIG_INDEX, short );

   STORE_STATE( smu_az_jig_x, SMU_AZ_JIG_X, double );
   STORE_STATE( smu_az_jig_y, SMU_AZ_JIG_Y, double );
   STORE_STATE( smu_az_chop_x, SMU_AZ_CHOP_X, double );
   STORE_STATE( smu_az_chop_y, SMU_AZ_CHOP_Y, double );
   STORE_STATE( smu_tr_jig_x, SMU_TR_JIG_X, double );
   STORE_STATE( smu_tr_jig_y, SMU_TR_JIG_Y, double );
   STORE_STATE( smu_tr_chop_x, SMU_TR_CHOP_X, double );
   STORE_STATE( smu_tr_chop_y, SMU_TR_CHOP_Y, double );

   /* Telescope Control System */
   STORE_STATE( tcs_tai, TCS_TAI, double );
   STORE_STATE( tcs_airmass, TCS_AIRMASS, double );
   STORE_STATE( tcs_az_ang, TCS_AZ_ANG, double );
   STORE_STATE( tcs_az_ac1, TCS_AZ_AC1, double );
   STORE_STATE( tcs_az_ac2, TCS_AZ_AC2, double );
   STORE_STATE( tcs_az_dc1, TCS_AZ_DC1, double );
   STORE_STATE( tcs_az_dc2, TCS_AZ_DC2, double );
   STORE_STATE( tcs_az_bc1, TCS_AZ_BC1, double );
   STORE_STATE( tcs_az_bc2, TCS_AZ_BC2, double );
   STORE_CHAR( tcs_beam, TCS_BEAM, JCMT__SZTCS_BEAM );
   STORE_STATE( tcs_index, TCS_INDEX, short );
   STORE_STATE( tcs_percent_cmp, TCS_PERCENT_CMP, short );
   STORE_CHAR( tcs_source, TCS_SOURCE, JCMT__SZTCS_SOURCE );
   STORE_CHAR( tcs_tr_sys, TCS_TR_SYS, JCMT__SZTCS_TR_SYS );
   STORE_STATE( tcs_tr_ang, TCS_TR_ANG, double );
   STORE_STATE( tcs_tr_ac1, TCS_TR_AC1, double );
   STORE_STATE( tcs_tr_ac2, TCS_TR_AC2, double );
   STORE_STATE( tcs_tr_dc1, TCS_TR_DC1, double );
   STORE_STATE( tcs_tr_dc2, TCS_TR_DC2, double );
   STORE_STATE( tcs_tr_bc1, TCS_TR_BC1, double );
   STORE_STATE( tcs_tr_bc2, TCS_TR_BC2, double );
   STORE_STATE( tcs_en_dc1, TCS_EN_DC1, double );
   STORE_STATE( tcs_en_dc2, TCS_EN_DC2, double );
   STORE_STATE( tcs_dm_abs, TCS_DM_ABS, double );
   STORE_STATE( tcs_dm_rel, TCS_DM_REL, double );

   /* Status flags */
   STORE_STATE( jos_drcontrol, JOS_DRCONTROL, short );

   /* Water Vapour Monitor */
   STORE_STATE( wvm_t12, WVM_T12, float );
   STORE_STATE( wvm_t42, WVM_T42, float );
   STORE_STATE( wvm_t78, WVM_T78, float );
   STORE_STATE( wvm_time, WVM_TIME, double );

   /* SCUBA-2 specific */
   STORE_STATE( sc2_heat, SC2_HEAT, unsigned short );
   STORE_STATE( sc2_bias, SC2_BIAS, unsigned short );
   STORE_STATE( sc2_mixtemp, SC2_MIXTEMP, float );
   STORE_STATE( sc2_fputemp, SC2_FPUTEMP, float );
   STORE_STATE( sc2_1kntdtemp, SC2_1KNTDTEMP, float );

   /* FTS and polarimeter */
   STORE_STATE( fts_pos, FTS_POS, float );
   STORE_STATE( pol_ang, POL_ANG, double );

#undef STORE_STATE
#undef STORE_CHAR
}



/*+ sc2store_headrmap - map the header arrays for read access */

void sc2store_headrmap
(
const HDSLoc *headloc,        /* HDS locator (given) */
size_t nframes,               /* number of frames expected (given) */
inst_t instrument,            /* instrument code (given) */
int *status                   /* global status (given and returned) */
)

/* Method :
    Map HDS arrays of size nframes for each header type within the HDS
    component identified by headloc and store the pointers. All components
    that exist will be mapped. The instrument code is used to determine
    whether a component is mandatory. Passing in INST__NONE will result in no
    mandatory components.

   History :
    18Aug2004 : original (bdk)
    23Nov2005 : Use Official C HDS interface (TIMJ)
    27Jan2006 : Kluge initialising of sc2store_loc[] array
                Risks, overwriting a valid locator (TIMJ)
    27Jul2005 : Use shared ACSIS/SCUBA-2 JCMTState scheme
    28Jul2006 : Add instrument argument (TIMJ)
    20Mar2007 : Use const in signature (TIMJ)
*/
{
   int dim[2];
   int j;
   int ndim;
   int pos;
   int isthere = 0;

   if ( *status != SAI__OK ) return;

   for ( j=0; j<JCMT_COMP_NUM; j++ )
   {
     /* what index should we use? */
     pos = hdsRecords[j].position;

     /* Should really initialise outside this routine */
     sc2store_loc[pos] = NULL;
     sc2store_ptr[pos] = NULL;
     sc2store_array[pos] = 1;

     /* See if the component exists (it may be an ACSIS or SCUBA-2
	file or whatever) */
     datThere( headloc, hdsRecords[j].name, &isthere, status );
     if (isthere) {
       int actdim = 0;
       hdsdim dims[1];

       datFind ( headloc, hdsRecords[j].name, &(sc2store_loc[pos]),
		 status );

       /* see if this is a scalar item - we have to look at the shape
          and not simply count elements. */
       datShape( sc2store_loc[pos], 1, dims, &actdim,status );

       dim[0] = nframes;
       ndim = 1;
       if ( actdim == 0 ) {
         sc2store_array[pos] = 0;
         dim[0] = 1;
         ndim = 0;
       } else if (dims[0] != nframes ) {
         if (*status == SAI__OK) {
           *status = DITS__APP_ERROR;
           sprintf( sc2store_errmess,
                    "sc2store_headrmap: Size mismatch in entry '%s'. Expected %d elements got %" HDS_DIM_FORMAT,
                    hdsRecords[j].name, nframes, dims[0] );
           ErsRep( 0, status, sc2store_errmess );
         }
       }

       if ( *status != SAI__OK ) break;
       datMap ( sc2store_loc[pos], hdsRecords[j].type, "READ",
		ndim, dim, &(sc2store_ptr[pos]), status );

       /* in 2010 November we changed the types of some of the _INTEGER
          entries to _WORD and _UWORD. HDS does not trap bad pixel values
          so any VAL__BADIs in an earlier JCMTSTATE will trigger a conversion
          error in HDS. We simply assume that conversion errors mean
          a bad value and we annul them. */
       if (*status == DAT__CONER) emsAnnul( status );

     } else {
       /* if missing and is a mandatory component then we need
	  to complain somehow. Old files will be a problem if we set
	  status to bad or we can just warn.  It is possible to flag a component
	  as optional in which case we do not warn. This is mainly useful for components
	  that have been added after data release and so we need to work around backwards
	  compatibility. Mandatory is therefore defined as understood by the instrument but
	  not optional. */
       if ( ((hdsRecords[j].instrument & instrument) != 0) &&  /* field is understood by instrument */
	     ((hdsRecords[j].optional & instrument) == 0)) { /* but is not optional */
	 if (*status == SAI__OK) {
	   *status = DITS__APP_ERROR;
	   sprintf( sc2store_errmess, "sc2store_headrmap: Mandatory component not present in file. Can't find '%s'",
		    hdsRecords[j].name);
	   ErsRep( 0, status, sc2store_errmess );
	 }
       }
     }

      if ( *status != SAI__OK ) break;
   }

}



/*+ sc2store_headunmap - unmap the header arrays */

void sc2store_headunmap
(
int *status                   /* global status (given and returned) */
)
/*
   History :
    12Aug2004 : original (bdk)
    27Jul2006 : check for NULL (timj)
*/
{
   int j;

   if ( *status != SAI__OK ) return;

   for ( j=0; j<JCMT_COMP_NUM; j++ )

   {
      if ( sc2store_loc[j] != NULL )
      {
         datUnmap ( sc2store_loc[j], status );
         sc2store_ptr[j] = NULL;
         datAnnul ( &(sc2store_loc[j]), status );
      }
   }
}



/*+ sc2store_mapwts - open and map a DREAM weights file */

void sc2store_mapwts
(
const char *filename,      /* name of HDS container file (given) */
int **windext,             /* Table of window extents for the DREAM
                              solution (returned) */
int **gridext,             /* Table of grid extents for a single
                              bolometer (returned) */
double *gridsize,          /* size in arcsec of grid step (returned) */
int **jigext,               /* Table of SMU pattern extents for a single
                              bolometer (returned) */
double *jigsize,           /* size in arcsec of SMU step (returned) */
int gridwtsdim[],          /* dimensions of grid interpolation weights
                              (returned) */
double **gridwts,          /* grid interpolation weights (returned) */
int *invmatdim,            /* dimension of inverted matrix (returned) */
double **invmat,           /* inverted matrix (returned) */
int qualdim[],             /* dimensions of quality mask (returned) */
int **qual,                /* bolometer quality mask (returned) */
int *status                /* global status (given and returned) */
)
/*
   Method :
    Open and map the contents of a SCUBA-2 DREAM weights file.
   History :
    14Apr2008 : original (bdk)
    22Apr2008 : add arguments for gridwtsdim and invmatdim (bdk)
    02May2008 : add arguments for qual and qualdim (bdk)
    09May2008 : add windext argument (bdk)
    16Sep2009 : handle column/row flip (timj)
*/
{
   HDSLoc *gridszloc = NULL;   /* HDS locator to grid step size */
   HDSLoc *jigszloc = NULL;    /* HDS locator to DREAM step size */
   int el;                     /* number of elements */
   int ndim;                   /* actual number of dimensions queried */
   int ndimx;                  /* maximum number of dimensions queried */
   int place;                  /* NDF placeholder */
   int there;                  /* flag for HDS component existence */

   /* Provide fallback values for array size. Usually read from file */
   const int colsize = 40;
   const int rowsize = 32;

   if ( *status != SAI__OK ) return;

/* Initialise Starlink error reporting NDF and start its context */

   sc2store_initialise( status );
   errMark();
   ndfBegin();

/* Open an HDS container file */

   ndfOpen ( NULL, filename, "READ", "OLD", &sc2store_indfwt, &place, status );


/* Find extension for holding DREAM details */

   ndfXloc ( sc2store_indfwt, "DREAM", "READ", &sc2store_dreamwtloc, status );

/* Map interpolation weights array */

   ndfOpen ( sc2store_dreamwtloc, "GRIDWTS", "READ", "OLD",
     &sc2store_indfgridwts, &place, status );

   ndfMap ( sc2store_indfgridwts, "DATA", "_DOUBLE", "READ", (void **)gridwts,
     &el, status );
   ndimx = 2;
   ndfDim ( sc2store_indfgridwts, ndimx, gridwtsdim, &ndim, status );

/* Map inverse matrix array */

   ndfOpen ( sc2store_dreamwtloc, "INVMATX", "READ", "OLD",
     &sc2store_indfinvmatx, &place, status );

   ndfMap ( sc2store_indfinvmatx, "DATA", "_DOUBLE", "READ", (void **)invmat,
     &el, status );
   ndimx = 1;
   ndfDim ( sc2store_indfinvmatx, ndimx, invmatdim, &ndim, status );

/* Get step intervals */

   datFind ( sc2store_dreamwtloc, "GRID_SIZE", &gridszloc, status );
   datGet0D ( gridszloc, gridsize, status );
   datAnnul ( &gridszloc, status );

/* Get window extents */

   datThere ( sc2store_dreamwtloc, "WINDEXT", &there, status );

   if ( there != 0 )
   {
      ndfOpen ( sc2store_dreamwtloc, "WINDEXT", "READ", "OLD",
        &sc2store_indfwindext, &place, status );

      ndfMap ( sc2store_indfwindext, "DATA", "_INTEGER", "READ",
        (void *)windext, &el, status );
   }
   else
   {
      *windext = calloc ( 4, sizeof(**windext) );
      (*windext)[0] = 0;
      (*windext)[2] = 0;
#if SC2STORE__COL_INDEX
      /* Decide what to do based on SC2STORE__COL_INDEX being
	 non-zero */
      (*windext)[1] = colsize - 1;
      (*windext)[3] = rowsize - 1;
#else
      (*windext)[3] = colsize - 1;
      (*windext)[1] = rowsize - 1;
#endif
   }

/* Get grid extents */

   ndfOpen ( sc2store_dreamwtloc, "GRIDEXT", "READ", "OLD",
     &sc2store_indfgridext, &place, status );

   ndfMap ( sc2store_indfgridext, "DATA", "_INTEGER", "READ", (void *)gridext,
     &el, status );

/* Size of DREAM pattern - not present in early version of file */

   datThere ( sc2store_dreamwtloc, "JIG_SIZE", &there, status );

   if ( there != 0 )
   {
      datFind ( sc2store_dreamwtloc, "JIG_SIZE", &jigszloc, status );
      datGet0D ( jigszloc, jigsize, status );
      datAnnul ( &jigszloc, status );
   }
   else
   {
      *jigsize = 12.56;
   }

/* Get DREAM extents */

   datThere ( sc2store_dreamwtloc, "JIGEXT", &there, status );

   if ( there != 0 )
   {
      ndfOpen ( sc2store_dreamwtloc, "JIGEXT", "READ", "OLD",
        &sc2store_indfjigext, &place, status );

      ndfMap ( sc2store_indfjigext, "DATA", "_INTEGER", "READ", (void *)jigext,
        &el, status );
   }
   else
   {
      *jigext = calloc ( 4, sizeof(**jigext) );
      (*jigext)[0] = -1;
      (*jigext)[1] = 1;
      (*jigext)[2] = -1;
      (*jigext)[3] = 1;
   }

/* Get bolometer quality mask */

   datThere ( sc2store_dreamwtloc, "QUAL", &there, status );

   if ( there != 0 )
   {
      ndfOpen ( sc2store_dreamwtloc, "QUAL", "READ", "OLD",
        &sc2store_indfqual, &place, status );

      ndimx = 2;
      ndfDim ( sc2store_indfqual, ndimx, qualdim, &ndim, status );

      ndfMap ( sc2store_indfqual, "DATA", "_INTEGER", "READ", (void *)qual,
        &el, status );
   }
   else
   {
      qualdim[SC2STORE__COL_INDEX] = rowsize;
      qualdim[SC2STORE__ROW_INDEX] = colsize;
      *qual = calloc ( qualdim[0]*qualdim[1], sizeof(**qual) );
   }

   sc2store_errconv ( status );

}

/*+ sc2store_open - open a SCUBA-2 data file */

void sc2store_open
(
const char *filename,    /* name of HDS container file (given) */
const char *access,      /* "READ" or "UPDATE" access (given) */
size_t *colsize,         /* number of pixels in column (returned) */
size_t *rowsize,         /* number of pixels in row (returned) */
size_t *nframes,         /* number of frames (returned) */
int *status              /* global status (given and returned) */
)
/*
   Method :
    Open a SCUBA-2 data file.
   History :
    16Nov2007 : original (bdk)
*/
{
   int dim[3];                 /* dimensions */
   int ndim;                   /* number of dimensions */
   int ndimx;                  /* max number of dimensions */
   int place;                  /* NDF placeholder */


   if ( *status != SAI__OK ) return;

/* Initialise Starlink error reporting NDF and start its context */

   sc2store_initialise( status );
   errMark();
   ndfBegin();

/* Open an HDS container file */

   ndfOpen ( NULL, filename, access, "OLD", &sc2store_indf, &place, status );

/* Find dimensions of timestream data */

   ndimx = 3;
   ndfDim ( sc2store_indf, ndimx, dim, &ndim, status );
   *colsize = dim[SC2STORE__ROW_INDEX];
   *rowsize = dim[SC2STORE__COL_INDEX];
   *nframes = dim[2];

/* Find extension for holding fixed-size data (subtracted constant and
   dark SQUID measurements) for each frame */

   ndfXloc ( sc2store_indf, "SCUBA2", "READ", &sc2store_scuba2loc, status );
}



/*+ sc2store_putimage - store constructed image */

void sc2store_putimage
(
int frame,               /* frame index (given) */
const AstFrameSet *fset, /* World coordinate transformations (given) */
int ndim,                /* dimensionality of image (given) */
const int dims[],        /* dimensions of image (given) */
size_t colsize,          /* number of pixels in a column (given) */
size_t rowsize,            /* number of pixels in a row (given) */
const double *image,     /* constructed image (given) */
const double *zero,      /* bolometer zero values [can be null pointer] (given) */
const char * obsidss,    /* OBSIDSS string for provenance (given) */
const char * creator,    /* Creator application for provenance (given) */
const char *fitshd,      /* string of concatenated FITS header records to
                            write (given) */
size_t nrec,             /* Number of FITS records */
int *status              /* global status (given and returned) */
)
/* Method :
    Create an image name "I1", "I2" etc from the frame index. Create an
    NDF with this name under SCU2RED. Store the image as the main array
    in the NDF and associated information and the bolometer zero point
    array under .more.
   History :
    12Aug2004 : original (bdk)
    24Jan2005 : create names instead of using structure array (bdk)
    15Apr2005 : add fset argument (bdk)
    18Apr2005 : add FITS header for writing in extension to image (agg)
    21Apr2005 : check nfits > 0 (bdk)
    22Apr2005 : change declaration of fitshd (bdk)
    13Jun2005 : use ndfHcre to create history component on all NDFs (bdk)
    13Jun2005 : allow image to be n-dimensional (BDK)
    23Nov2005 : Use Official C HDS interface (TIMJ)
    20Mar2007 : Use const in signature (TIMJ)
    10Apr2007 : Do not write MAPDATA extension: SEQSTART/END now stored
                as FITS headers; rename BZ_IMAGE -> BOLZERO (AGG)
    15May2007 : handle FITS headers as concatenated string (bdk)
    23Oct2007 : remove calls to ndfHcre (bdk)
    31Oct2007 : change name nfits to nrec (bdk)
    06Dec2007 : restore call to ndfHcre for main NDF (bdk)
    28Mar2008 : call sc2store_creimages to create the containing structure (bdk)
    17Aug2009 : Write explicit history message (timj)
*/
{

   HDSLoc *bz_imloc = NULL;/* HDS locator */
   int bsc2store_zindf;    /* NDF identifier */
   double *bzptr = NULL;   /* pointer to mapped space for zero points */
   int el;                 /* number of elements mapped */
   char imname[DAT__SZNAM];/* name of structure for image */
   double *imptr;          /* pointer to mapped space for image */
   int j;                  /* loop counter */
   int lbnd[7];            /* lower dimension bounds */
   int ntot;               /* total number of elements */
   int place;              /* NDF placeholder */
   AstKeyMap *pkm = NULL;  /* KeyMap holding provenance contents of MORE */
   NdgProvenance *prov = NULL;  /* Pointer to provenance structure */
   int strnum;             /* structure element number */
   int uindf;              /* NDF identifier */
   int ubnd[7];            /* upper dimension bounds */
#ifdef SC2STORE_WRITE_HISTORY
   const char * const history[1] = { "Write reconstructed image." };
#endif

   if ( *status != SAI__OK ) return;

/* create storage structure */

   sc2store_creimages ( status );

   if ( *status == SAI__OK )
   {

/* Pre-fill the provenance keymap */
     pkm = astKeyMap( " " );
     if (obsidss) astMapPut0C( pkm, "OBSIDSS", obsidss, NULL );

/* Get structure for nth constructed image */

      strnum = frame + 1;
      sprintf ( imname, "I%d", strnum );

      ntot = 1;
      for ( j=0; j<ndim; j++ )
      {
         ubnd[j] = dims[j];
         lbnd[j] = 1;
         ntot *= dims[j];
      }
      ndfPlace ( sc2store_scu2redloc, imname, &place, status );
      ndfNew ( "_DOUBLE", ndim, lbnd, ubnd, &place, &uindf, status );
      ndfHcre ( uindf, status );
#ifdef SC2STORE_WRITE_HISTORY
      ndfHput( "NORMAL", APPNAME, 1, 1, (char * const*)history,
               0, 0, 0, uindf, status );
#endif

/* Map the data array */

      ndfMap ( uindf, "DATA", "_DOUBLE", "WRITE", (void *)&imptr, &el,
        status );

/* Copy image array */

      if ( *status == SAI__OK )
      {
        memcpy( imptr, image, ntot*sizeof(*image) );
      }

/* Sort out provenance. This is by definition a root of the provenance tree */
      prov = ndgReadProv( uindf, creator, status );
      ndgPutProv( prov, sc2store_indf, pkm, 1, status );
      ndgWriteProv( prov, uindf, 0, status );
      prov = ndgFreeProv( prov, status );

/* Store world coordinate transformations */

      ndfPtwcs ( fset, uindf, status );

/* Store the bolometer zero points as an NDF in the extension if we have them */

      if ( zero )
      {
	ndfXnew ( uindf, "BOLZERO", "SCUBA2_ZER_ARR", 0, 0, &bz_imloc, status );
	ndfPlace ( bz_imloc, "ZERO", &place, status );

/* Create the array for bolometer zeros */

	sc2store_fillbounds( colsize, rowsize, 0, lbnd, ubnd, status );
	ndfNew ( "_DOUBLE", 2, lbnd, ubnd, &place, &bsc2store_zindf, status );

/* Map the data array */

	ndfMap ( bsc2store_zindf, "DATA", "_DOUBLE", "WRITE", (void *)&bzptr, &el,
		 status );

/* Copy image array */

	if ( *status == SAI__OK )
	{
	  memcpy( bzptr, zero, colsize*rowsize*sizeof(*zero));
        }

/* Unmap the data array */

	ndfUnmap ( bsc2store_zindf, "DATA", status );
	ndfAnnul ( &bsc2store_zindf, status );

/* Free the locators for the frame */

	datAnnul ( &bz_imloc, status );

      }

/* Store the FITS headers */

      if ( nrec > 0 )
      {
         sc2store_writefitshead ( uindf, nrec, fitshd, status );
      }

/* Unmap the data array */

      ndfUnmap ( uindf, "DATA", status );
      ndfAnnul ( &uindf, status );

   }

   if (pkm) pkm = astAnnul( pkm );
   sc2store_errconv ( status );

}




/*+ sc2store_putincomp - store details of incompressible pixels */

void sc2store_putincomp
(
int frame,            /* frame index (given) */
size_t npix,          /* number of incompressible pixels (given) */
const int pixnum[],   /* indices of incompressible pixels (given) */
const int pixval[],   /* values of incompressible pixels (given) */
int *status           /* global status (given and returned) */
)
/*
   History :
    12Aug2004 : original (bdk)
    19Jun2005 : use ndfHcre to create history component on all NDFs (bdk)
    23Nov2005 : Use Official C HDS interface (TIMJ)
    18Dec2006 : Remove some error checks (TIMJ)
    20Mar2007 : Use const in signature (TIMJ)
    23Oct2007 : rename structure for incompressible pixels to INCOMPS (bdk)
    23Oct2007 : remove calls to ndfHcre (bdk)
*/
{

   int el;                 /* number of elements mapped */
   int *incomp;            /* pointer to incompressible values */
   int j;                  /* loop counter */
   int lbnd[3];            /* lower dimension bounds */
   HDSLoc *loc2 = NULL;    /* HDS locator */
   int place;              /* NDF placeholder */
   int strnum;             /* structure element number */
   int uindf;              /* NDF identifier */
   int ubnd[3];            /* upper dimension bounds */


   if ( *status != SAI__OK ) return;

/* Get structure for incompressible pixels for n-th frame */

   strnum = frame + 1;
   datCell ( sc2store_incomploc, 1, &strnum, &loc2, status );

   ubnd[0] = 2;
   lbnd[0] = 1;
   ubnd[1] = npix;
   lbnd[1] = 1;
   ndfPlace ( loc2, "INCOMP", &place, status );
   ndfNew ( "_INTEGER", 2, lbnd, ubnd, &place, &uindf, status );

/* Map the data array */

   ndfMap ( uindf, "DATA", "_INTEGER", "WRITE", (void *)&incomp, &el,
	    status );

/* Copy each pixel index and value */

   if ( *status == SAI__OK )
     {
       for ( j=0; j<npix; j++ )
         {
	   incomp[2*j] = pixnum[j];
	   incomp[2*j+1] = pixval[j];
         }
     }

/* Unmap the data array */

   ndfUnmap ( uindf, "DATA", status );
   ndfAnnul ( &uindf, status );

/* Free the locator for the frame */

   datAnnul ( &loc2, status );

   sc2store_errconv ( status );

}




/*+ sc2store_putscanfit - store scan fit coefficients */

void sc2store_putscanfit
(
size_t colsize,       /* number of bolometers in a column (given) */
size_t rowsize,       /* number of bolometers in a row (given) */
size_t ncoeff,        /* number of coefficients (given) */
const double *coptr,  /* coefficients (given) */
int *status           /* global status (given and returned) */
)
/* Method :
    Create an NDF with image name "SCANFIT", under DA_IMAGE. Store the
    coefficients as the main array in the NDF.
   History :
    25Feb2005 : original (bdk)
    19Jun2005 : use ndfHcre to create history component on all NDFs (bdk)
    26Jan2006 : change component name to SCANFIT (bdk)
    20Mar2007 : Use const in signature (TIMJ)
    23Oct2007 : remove calls to ndfHcre (bdk)
*/
{

   int el;                 /* number of elements mapped */
   double *imptr;          /* pointer to mapped space for image */
   int j;                  /* loop counter */
   int lbnd[3];            /* lower dimension bounds */
   int place;              /* NDF placeholder */
   int uindf;              /* NDF identifier */
   int ubnd[3];            /* upper dimension bounds */


   if ( *status != SAI__OK ) return;


/* create storage structure */

   sc2store_creimages ( status );

   if ( *status == SAI__OK )
   {
      sc2store_fillbounds( colsize, rowsize, ncoeff, lbnd, ubnd, status );
      ndfPlace ( sc2store_scu2redloc, "SCANFIT", &place, status );
      ndfNew ( "_DOUBLE", 3, lbnd, ubnd, &place, &uindf, status );

/* Map the data array */

      ndfMap ( uindf, "DATA", "_DOUBLE", "WRITE", (void *)&imptr, &el,
        status );

/* Copy coefficients array */

      if ( *status == SAI__OK )
      {
        memcpy( imptr, coptr, colsize*rowsize*ncoeff*sizeof(*coptr));
      }

/* Unmap the data array */

      ndfUnmap ( uindf, "DATA", status );
      ndfAnnul ( &uindf, status );

   }

   sc2store_errconv ( status );

}

/*+ sc2store_rdflatcal - read SCUBA-2 flatfield calibration */

void sc2store_rdflatcal
(
const char *filename,    /* name of HDS container file (given) */
size_t flatlen,          /* length of space for flatfield name (given) */
size_t *colsize,         /* number of pixels in column (returned) */
size_t *rowsize,         /* number of pixels in row (returned) */
size_t *nflat,           /* number of flat coeffs per bol (returned) */
double *refres,          /* Reference resistor used to create flatfield (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int *status              /* global status (given and returned) */
)

/* Description :
    Return the flatfield calibration from a SCUBA-2 NDF.
    The file is opened and then closed after the values are copied.
   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    01Oct2005 : original (bdk)
    07Dec2005 : set sc2store_sc2open flag when open is successful (bdk)
    25Jan2006 : add access argument to sc2store_rdmap (bdk)
    08Aug2006 : update call to sc2store_rdmap (agg)
    20Mar2007 : Use const in signature (TIMJ)
    11Nov2007 : make *data short instead of unsigned short (bdk)
    19Nov2007 : use sc2store_open and sc2store_readflatcal (bdk)
    08Sep2011 : Add refres argument (timj)
*/

{
   double *fcal;              /* mapped flatfield calibration */
   double *fpar;              /* mapped flatfield parameters */
   int nbol;                  /* number of bolometers */
   size_t nframes;            /* number of data frames */

   if ( !StatusOkP(status) ) return;

   if ( sc2store_sc2open != 0 )
   {
      *status = DITS__APP_ERROR;
      sprintf ( sc2store_errmess,
        "sc2store_rdflatcal: one SCUBA-2 data file already open, can't open %s", filename );
      ErsRep ( 0, status, sc2store_errmess );
      return;
   }

   sc2store_open ( filename, "READ", colsize, rowsize, &nframes, status );
   sc2store_readflatcal ( "READ", flatlen, nflat, refres, flatname, &fcal, &fpar,
     status );

   if ( !StatusOkP(status) )
   {
      sprintf ( sc2store_errmess, "sc2store_rdflatcal: failed to open %s", filename );
      ErsRep ( 0, status, sc2store_errmess );
   }
   else
   {

/* Create copies of the calibration arrays */

      nbol = (*rowsize) * (*colsize);
      *flatcal = calloc ( nbol*(*nflat), sizeof(**flatcal) );
      memcpy ( *flatcal, fcal, nbol*(*nflat)*sizeof(*fcal) );
      *flatpar = calloc ( *nflat, sizeof(**flatpar) );
      memcpy ( *flatpar, fpar, (*nflat)*sizeof(*fpar) );
   }

   if ( StatusOkP(status) )
   {
      sc2store_sc2open = 1;
   }

   sc2store_free ( status );

}

/*+ sc2store_rdtstream - read SCUBA-2 time stream data from an NDF */

void sc2store_rdtstream
(
const char *filename,    /* name of HDS container file (given) */
const char *access,      /* "READ" or "UPDATE" access (given) */
size_t flatlen,          /* length of space for flatfield name (given) */
size_t maxfits,          /* max number of FITS headers (given) */
size_t *nrec,            /* actual number of FITS records (returned) */
char *fitshead,          /* up to maxfits FITS header records (returned) */
char units[SC2STORE_UNITLEN],/* data units. can be NULL (returned) */
char label[SC2STORE_LABLEN], /* data label. Can be NULL (returned) */
size_t *colsize,         /* number of pixels in column (returned) */
size_t *rowsize,         /* number of pixels in row (returned) */
size_t *nframes,         /* number of frames (returned) */
size_t *nflat,           /* number of flat coeffs per bol (returned) */
double *refres,          /* Reference resistor used to create flatfield (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
JCMTState *frhead[],     /* header data for each frame (returned) */
int **outdata,           /* pointer to data array (returned), or NULL */
int **dksquid,           /* pointer to dark SQUID values (returned), or NULL */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int **jigvert,           /* pointer to DREAM jiggle vertices (returned) */
size_t *nvert,           /* Number of vertices in jiggle pattern (returned) */
double **jigpath,        /* pointer to path of SMU over jiggle pattern (returned) */
size_t *npath,           /* Number of points in SMU path (returned) */
int *status              /* global status (given and returned) */
)

/* Description :
    Return the time stream data, associated headers and flatfield calibration
    from an NDF. The file is left open to allow updating of associated items
    such as the flatfield or other processed data, in which case access
    should be specified as "UPDATE".

    If a NULL value is supplied for "outdata" or "dksquid", then neither the
    data array nor the dark SQUID values are mapped.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)
    D.S.Berry (d.berry@jach.hawaii.edu)

   History :
    01Oct2005 : original (bdk)
    04Oct2005 : check if sc2store already has an open file (bdk)
    08Dec2005 : map space AFTER checking status (bdk)
    25Jan2006 : add access argument (bdk)
    21Aug2006 : update API to return DREAM parameters (agg)
    24Apr2007 : change declaration of fitsrec and remove maxlen to match
                change to rdfitshead (bdk)

    23Oct2007 : make nfits size_t instead of unsigned int (bdk)
    31Oct2007 : change name nfits to nrec (bdk)
    11Nov2007 : make compressed data short instead of unsigned short (bdk)
    16Nov2007 : restructure on top of sc2store_readraw() (bdk)
    20Jun2008 : check for NULL outdata or dksquid pointers (dsb)
    20Oct2009 : Add units and label. _readraw now handles null pointers (timj)
*/

{

   if (units) units[0] = '\0';
   if (label) label[0] = '\0';

   if ( !StatusOkP(status) ) return;

   if ( sc2store_sc2open != 0 )
   {
      *status = DITS__APP_ERROR;
      sprintf ( sc2store_errmess,
        "sc2store_rdtstream: one SCUBA-2 data file already open, can't open %s", filename );
      ErsRep ( 0, status, sc2store_errmess );
      return;
   }

   if ( ( strcmp ( access, "READ" ) != 0 ) &&
	( strcmp ( access, "UPDATE" ) != 0 ) )
   {
      *status = DITS__APP_ERROR;
      sprintf ( sc2store_errmess,
        "sc2store_rdtstream: access mode should be READ or UPDATE, but given as %s", access );
      ErsRep ( 0, status, sc2store_errmess );
      return;
   }


/* Map the required data arrays */

   sc2store_open ( filename, access, colsize, rowsize, nframes, status );

   sc2store_readraw ( access, *colsize, *rowsize, *nframes, units, label, outdata,
                      dksquid, status );

   sc2store_readflatcal ( access, flatlen, nflat, refres, flatname, flatcal, flatpar,
     status );
   sc2store_readjig ( access, jigvert, nvert, jigpath, npath, status );

   if ( !StatusOkP(status) )
   {
      sprintf ( sc2store_errmess, "sc2store_rdtstream: failed to open %s",
        filename );
      ErsRep ( 0, status, sc2store_errmess );
   }

/* Read the per-frame headers */

   sc2store_readframehead ( *nframes, frhead, status );

/* Read the FITS headers */

   sc2store_readfitshead ( maxfits, nrec, fitshead, status );

   sc2store_sc2open = 1;
}



/*+ sc2store_readfitshead - read the FITS headers */

void sc2store_readfitshead
(
size_t maxfits,          /* maximum number of header items (given) */
size_t *nrec,            /* number of header records (returned) */
char *headers,           /* buffer to hold FITS headers (returned) */
int *status              /* global status (given and returned) */
)
/*
   History :
    12Aug2004 : original (bdk)
    23Nov2005 : Use Official C HDS interface (TIMJ)
    24Apr2007 : change headers to char* (bdk)
    23Oct2007 : make nfits size_t instead of unsigned int (bdk)
    31Oct2007 : change name nfits to nrec (bdk)
*/
{
   int dim[1];                /* number of FITS entries */
   HDSLoc *fitsloc = NULL;    /* HDS locator to FITS headers */
   void *fptr = NULL;         /* Pointer to the mapped FITS header */
   int ndim;                  /* number of dimensions in query */
   int ndimx;                 /* maximum number of dimensions in query */

   if ( *status != SAI__OK ) return;

/* Locate the FITS headers */

   ndimx = 1;
   ndfXloc ( sc2store_indf, "FITS", "READ", &fitsloc, status );
   datShape ( fitsloc, ndimx, dim, &ndim, status );
   datMapV ( fitsloc, "_CHAR*80", "READ", &fptr, nrec, status );

   if ( *status == SAI__OK )
   {
      if ( *nrec > maxfits )
      {
         *nrec = maxfits;
      }

      strncpy ( headers, fptr, (*nrec)*80 );
      headers[(*nrec)*80] = 0;
   } else {
     ErsRep(0, status, "sc2store_readfitshead: Error reading FITS header" );
   }

   datAnnul ( &fitsloc, status );

   sc2store_errconv ( status );
}




/*+ sc2store_readflatcal - read flatfield calibration */

void sc2store_readflatcal
(
const char *access,      /* "READ" or "UPDATE" access (given) */
size_t flatlen,          /* length of space for flatfield name (given) */
size_t *nflat,           /* number of flat coeffs per bol (returned) */
double *refres,          /* Reference resistor used to create flatfield (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int *status              /* global status (given and returned) */
)
/*
   Method :
    Read the flatfield calibration from a SCUBA-2 data file.
   History :
    16Nov2006 : original (bdk)
*/
{
   int el;                     /* number of elements mapped */
   int isthere = 0;            /* Is component present? */
   int nfdim;                  /* number of flatpar dimensions */
   int fdims[1];               /* flatpar dimensios */
   int place;                  /* NDF placeholder */


   if ( *status != SAI__OK ) return;


/* FLATCAL flatfield calibration */

   ndfOpen ( sc2store_scuba2loc, "FLATCAL", access, "OLD", &sc2store_findf,
     &place, status );

   ndfMap ( sc2store_findf, "DATA", "_DOUBLE", access, (void *)flatcal, &el,
     status );

   ndfXloc ( sc2store_findf, "FLATDATA", "READ", &sc2store_fdataloc, status );
   datFind ( sc2store_fdataloc, "FLATNAME", &sc2store_fnameloc, status );
   datGet0C ( sc2store_fnameloc, flatname, flatlen, status );

   /* Old flatfields won't have this */
   *refres = VAL__BADD;
   datThere( sc2store_fdataloc, "REFRES", &isthere, status );
   if (isthere) {
     datFind( sc2store_fdataloc, "REFRES", &sc2store_frefresloc, status );
     datGet0D( sc2store_frefresloc, refres, status );
   }

   datFind ( sc2store_fdataloc, "FLATPAR", &sc2store_fparloc, status );
   datShape ( sc2store_fparloc, 1, fdims, &nfdim, status );
   datMap ( sc2store_fparloc, "_DOUBLE", "READ", 1, fdims, (void**)flatpar,
     status );
   *nflat = fdims[0];
   sc2store_errconv ( status );

}



/*+ sc2store_readframehead - read the frame headers */

void sc2store_readframehead
(
size_t nframes,          /* number of data frames (given) */
JCMTState *frhead[],     /* header data for each frame (returned) */
int *status              /* global status (given and returned) */
)
/*
   History :
    16Nov2007 : original (bdk)
*/
{
   HDSLoc *tloc = NULL;       /* Temporary HDS locator */
   int isthere = 0;           /* is an extension present? */
   int j;                     /* loop counter */

   if ( *status != SAI__OK ) return;


/* storage for Header values for each frame - to import old frames we fallback
   to SCUBA2 if JCMT__EXTNAME is not present.
 */

   ndfXstat ( sc2store_indf, JCMT__EXTNAME, &isthere, status );
   if ( isthere )
   {
      ndfXloc( sc2store_indf, JCMT__EXTNAME, "READ", &tloc, status );
/* Re-size the arrays in the JCMTSTATE extension to match the pixel index
   bounds of the NDF. The resized arrays are stored in a new temporary HDS
   object, and the old locator is annull. */
      sc2store_resize_head( sc2store_indf, &tloc, &sc2store_jcmtstateloc,
			    status );
   }
   else
   {

/* needs to be cloned since they are annulled separately */

     datClone ( sc2store_scuba2loc, &sc2store_jcmtstateloc, status );
   }
   sc2store_headrmap ( sc2store_jcmtstateloc, nframes, INST__SCUBA2, status );

/* Read the per-frame headers */

   if ( StatusOkP(status) )
   {
      *frhead = (JCMTState *) calloc ( nframes, sizeof(**frhead) );

      for ( j=0; j<nframes; j++ )
      {
         sc2store_headget ( j, &((*frhead)[j]), status );
      }
   }

   sc2store_errconv ( status );
}




/*+ sc2store_readjig - read details of jiggle pattern */

void sc2store_readjig
(
const char *access,      /* "READ" or "UPDATE" access (given) */
int **jigvert,           /* pointer to DREAM jiggle vertices (returned) */
size_t *nvert,           /* Number of vertices in jiggle pattern (returned) */
double **jigpath,        /* pointer to path of SMU over jiggle pattern (returned) */
size_t *npath,           /* Number of points in SMU path (returned) */
int *status              /* global status (given and returned) */
)
/*
   Method :
    Read the jiggle pattern from a SCUBA-2 file.
   History :
    16Nov2007 : original (bdk)
*/
{
   int el;                     /* number of elements mapped */
   int isthere = 0;            /* is an extension present? */
   int place;                  /* NDF placeholder */


   if ( *status != SAI__OK ) return;


   ndfXstat( sc2store_indf, "DREAM", &isthere, status );
   if ( isthere )
   {
      ndfXloc( sc2store_indf, "DREAM", "READ", &sc2store_drmloc, status );
      ndfOpen ( sc2store_drmloc, "JIGVERT", access, "OLD", &sc2store_jigvndf,
        &place, status );
      ndfMap ( sc2store_jigvndf, "DATA", "_INTEGER", access, (void *)jigvert,
        &el, status );
      *nvert = el/2;
      ndfOpen ( sc2store_drmloc, "JIGPATH", access, "OLD", &sc2store_jigpndf,
        &place, status );
      ndfMap ( sc2store_jigpndf, "DATA", "_DOUBLE", access, (void *)jigpath,
        &el, status );

/* Remember npath = nsampcycle in SMURF = cycle_samples in other places */

      *npath = el/2;
   }
   else
   {

/* Return NULL pointers if we don't have DREAM data */

      jigvert = NULL;
      jigpath = NULL;
      nvert = NULL;
      npath = NULL;
   }
   sc2store_errconv ( status );

}



/*+ sc2store_readraw - read raw SCUBA-2 data */

void sc2store_readraw
(
const char *access,      /* "READ" or "UPDATE" access (given) */
size_t colsize,          /* number of pixels in column (given) */
size_t rowsize,          /* number of pixels in row (given) */
size_t nframes,          /* number of frames (given) */
char units[SC2STORE_UNITLEN],/* data units. can be NULL (returned) */
char label[SC2STORE_LABLEN], /* data label. Can be NULL (returned) */
int **rawdata,           /* raw timestream data. Can be NULL (returned) */
int **dksquid,           /* pointer to dark SQUID values. Can be NULL (returned) */
int *status              /* global status (given and returned) */
)
/*
   Method :
    Return a ponter to existing SCUBA-2 timestream data.
   History :
    16Nov2007 : original (bdk)
    29Nov2007 : calloc returned pointer irrespective of the file data being
                compressed (bdk)
    20Oct2009 : add units and label arguments (timj)
    21Oct2009 : Allow rawdata and dksquid to be null so that we can still
                read the units. Note that we do not trust the label/units
                for compressed data.
*/
{
   int *bzero;                 /* pointer to compression offset values */
   short *data;                /* pointer to compressed data array */
   int el;                     /* number of elements mapped */
   int hasunits = 0;           /* are units defined? */
   int haslabel = 0;           /* is the data label defined? */
   int isthere = 0;            /* is an extension present? */
   int j;                      /* loop counter */
   int nbol;                   /* number of bolometers */
   int place;                  /* NDF placeholder */
   size_t npix;                /* number of incompressible pixels */
   static int pixnum[DREAM__MXBOL];  /* indices of incompressible pixels */
   static int pixval[DREAM__MXBOL];  /* values of incompressible pixels */
   int *stackz;                /* pointer to subtracted frame in compression */
   char type[NDF__SZTYP+1];    /* type of stored raw data */
   int *udata;                 /* pointer to uncompressed data array */

   if (units) units[0] = '\0';
   if (label) label[0] = '\0';

   if ( *status != SAI__OK ) return;


/* Allocate space for the raw data */

    nbol = colsize * rowsize;

    if (rawdata)
      {
        *rawdata = calloc ( nframes*nbol, sizeof(**rawdata) );
        if ( *rawdata == NULL )
          {
            *status = DITS__APP_ERROR;
            ErsRep ( 0, status,
                     "sc2store_readraw: failed to map space for timestream data" );
            return;
          }
      }

/* Check for whether the data are "_SHORT", ie compressed, or "_INTEGER" */

   ndfType ( sc2store_indf, "DATA", type, NDF__SZTYP+1, status );

   if ( strcmp ( type, "_INTEGER" ) == 0 )
   {

/* Units and label - we can trust the values in the file if they
   already exist */
     if (units) {
       ndfState( sc2store_indf, "Units", &hasunits, status );
       if (hasunits) {
         ndfCget( sc2store_indf, "Units", units, SC2STORE_UNITLEN, status );
       } else {
         star_strlcpy( units, "adu", SC2STORE_UNITLEN );
       }
     }
     if (label) {
       ndfState( sc2store_indf, "Label", &haslabel, status );
       if (haslabel) {
         ndfCget( sc2store_indf, "Label", label, SC2STORE_LABLEN, status );
       } else {
         star_strlcpy( label, "Signal", SC2STORE_LABLEN );
       }
     }

/* Map the data array */

     if (rawdata)
       {
         ndfMap ( sc2store_indf, "DATA", "_INTEGER", "READ", (void *)(&udata), &el,
                  status );
         if ( *status == SAI__OK )
           {
             memcpy ( *rawdata, udata, el*sizeof(*udata) );
           }
       }
   }
   else
   {

/* Units and label - we are uncompressing so we ignore the label
   and units that are in the file already. */
     if (units) {
         star_strlcpy( units, "adu", SC2STORE_UNITLEN );
     }
     if (label) {
         star_strlcpy( label, "Signal", SC2STORE_LABLEN );
     }

     if (rawdata)
       {

/* Map the data array */

         ndfMap ( sc2store_indf, "DATA", "_WORD", "READ", (void *)(&data), &el,
                  status );

/* map compression zero offset for each frame */

         ndfOpen ( sc2store_scuba2loc, "BZERO", "READ", "OLD", &sc2store_zindf,
                   &place, status );

         ndfMap ( sc2store_zindf, "DATA", "_INTEGER", "READ", (void *)(&bzero),
                  &el, status );

/* Global data scale factor in compression */

         datThere ( sc2store_scuba2loc, "BSCALE", &isthere, status );

         if ( ( *status == SAI__OK ) && ( isthere != 0 ) )
           {
             datFind ( sc2store_scuba2loc, "BSCALE", &sc2store_bscaleloc, status );
             datGet0D ( sc2store_bscaleloc, &sc2store_rdbscale, status );
             datAnnul ( &sc2store_bscaleloc, status );
           }
         else
           {
             sc2store_rdbscale = 1.0;
           }

/* STACKZERO subtracted frame */

         ndfOpen ( sc2store_scuba2loc, "STACKZERO", "READ", "OLD", &sc2store_sindf,
                   &place, status );

         ndfMap ( sc2store_sindf, "DATA", "_INTEGER", "READ", (void *)(&stackz),
                  &el, status );

/* Map space for the decompressed data */

         if ( StatusOkP(status) )
           {

/* get details of incompressible pixels and decompress frames */

             ndfXloc ( sc2store_indf, "INCOMPS", "READ", &sc2store_incomploc,
                       status );

             for ( j=0; j<nframes; j++ )
               {

                 sc2store_getincomp ( j, &npix, pixnum, pixval, status );
                 sc2store_decompress ( nbol, stackz, bzero[j],
                                       &(data[j*nbol]), npix, pixnum,
                                       pixval, &((*rawdata)[j*nbol]), status );
               }
           }
       }
   }

/* Dark SQUID values for each frame */

   if (dksquid)
     {
       ndfOpen ( sc2store_scuba2loc, "DKSQUID", "READ", "OLD", &sc2store_dindf,
                 &place, status );

       ndfMap ( sc2store_dindf, "DATA", "_INTEGER", "READ", (void *)dksquid, &el,
                status );
     }

   sc2store_errconv ( status );

}

/*+ sc2store_resize_head - modify JCMTSTATE arrays to take account of the
                           NDF pixel origin. */

void sc2store_resize_head
(
int indf,                /* Id. for NDF holding the JCMTSTATE extension */
HDSLoc **xloc,           /* Locator for the JCMTSTATE extension (annuled on
                            exit) */
HDSLoc **yloc,           /* Locator for new HDS object containing resized
                            arrays. */
int *status              /* Global status (given and returned) */
)
/*
   Method :
    Create a new temporary HDS object and copy to it all the information
    from the NDFs JCMTSTATE extension. Each array that has an axis
    corresponding to time slice is modified so that only the section that
    refers to the pixel bounds of the NDF is copied. The supplied locator
    is annulled before returning.
   History :
    18Jan2008 : original (dsb)
    15Jul2008 : use ifdef on SMURF-specific variable declarations (bdk)
*/
{

/* Local Variables: */
   HDSLoc *xloc2 = NULL;
   int isthere;
   int j;
   int lbnd[ 3 ];
   int ndim;
   int ubnd[ 3 ];
   int update;
   size_t nslice;

/* variables only needed in SMURF */

#ifdef PACKAGE_UPCASE
   HDSLoc *xloc3 = NULL;
   char name[ DAT__SZNAM + 1 ];
   hdsdim lower[ 1 ];
   hdsdim upper[ 1 ];
   int ncomp;
#endif

/* Initialise the returned pointer. */
   *yloc = NULL;

/* Check inherited status */
   if ( *status != SAI__OK ) return;

/* Only proceed if we have an NDF. */
   if( indf != NDF__NOID ) {

/* Get the pixel index bounds of the NDF. */
      ndfBound( indf, 3, lbnd, ubnd, &ndim, status );

/* Initially, assume we do not need to change the arrays in the extension. */
      update = 0;

/* We do not change anything unless the NDF is 3-dimensional. */
      if( ndim == 3 ){

/* If the lower pixel bounds on the time slice axis (assumed to be the
   third axis) is not 1, then we need to adjust the arrays. */
         if( lbnd[ 2 ] != 1 ) {
            update = 1;

/* If the lower bound is 1, we check the length of the arrays in the
   extension. If they are not the same length as the third NDF axis,
   then we need to adjust the arrays. We take the array lengths from the
   first array that is present in the extension. */
         } else {

            for( j = 0; j < JCMT_COMP_NUM; j++ ){
               datThere( *xloc, hdsRecords[ j ].name, &isthere, status );
               if( isthere ) {
                  datFind( *xloc, hdsRecords[ j ].name, &xloc2, status );
                  datSize( xloc2, &nslice, status );
                  update = ( nslice != ubnd[ 2 ] );
                  datAnnul( &xloc2, status );
                  break;
               }
            }

         }
      }

/* If required, produce a temporary copy of the supplied object. This includes
   any scalars as well as the arrays. */
      if( update ) {
        /* only do this code in SMURF. The DA does not care and does not want
           to have to worry about kaplibs dependencies. No need to bother with an
           explicit kaplibs existence test since we know the DA will not use config.h */
#ifdef PACKAGE_UPCASE
         datTemp( JCMT__EXTTYPE, 0, NULL, yloc, status );
         datNcomp( *xloc, &ncomp, status );
         for( j = 1; j <= ncomp; j++ ) {
            datIndex( *xloc, j, &xloc2, status );
            datName( xloc2, name, status );
            kpg1Datcp( xloc2, *yloc, name, status );
            datAnnul( &xloc2, status );
         }

/* Store the pixel bounds on the time slice axis. */
         lower[ 0 ] = lbnd[ 2 ];
         upper[ 0 ] = ubnd[ 2 ];

/* Loop round all the arrays that may need to be resized. */
         for( j = 0; j < JCMT_COMP_NUM; j++ ){

/* See if the component exists (it may be an ACSIS or SCUBA-2 or whatever) */
            datThere( *xloc, hdsRecords[ j ].name, &isthere, status );
            if( isthere ) {
               size_t nelem = 0;

/* Get a locator for the component of the NDF extension that holds the
   required values. */
               datFind( *xloc, hdsRecords[ j ].name, &xloc2, status );
               datSize( xloc2, &nelem, status );

/* Only slice the array items */
               if( nelem > 1 ) {

/* Delete the old (full-sized) component from the returned temporary HDS
   object. */
                  datErase( *yloc, hdsRecords[ j ].name, status );

/* Extract a slice of this array that matches the pixel bounds of the NDF
   on the third pixel axis, and copy it to a new component in the returned
   temporary HDS object. */
                  kpg1Hsect( xloc2, 1, lower, upper, *yloc, hdsRecords[ j ].name,
                             status );

               }
               datAnnul( &xloc2, status );
            }
         }
#else
         printf("Oops. Someone other than SMURF is trying to access an NDF section of JCMTSTATE\n");
         abort();
#endif
      }
   }

/* If no new object was created, return a clone of the supplied locator. */
   if( ! *yloc ) datClone( *xloc, yloc, status );

/* Annul the supplied locator. */
   datAnnul( xloc, status );
}



/*+ sc2store_setbscale - Set the scale factor for data compression */

void sc2store_setbscale
(
double bscale,        /* value to be set (given) */
int *status           /* global status (given and returned) */
)
/* Description :
    Store the value of bscale. When raw data is written compressed it is divided
    by this value before the rest of the compression is applied.

   History :
    05Nov2007 : original (bdk)
*/
{


   if ( *status != SAI__OK ) return;

   sc2store_wrbscale = bscale;
}



/*+ sc2store_setcompflag - Set the flag controlling data compression */

void sc2store_setcompflag
(
sc2store_cmptype compflag, /* value to be set (Given),
                              SC2STORE__BDK => original BDK compression scheme
                              SC2STORE__DELTA => delta compression (see SUN/11)
                              SC2STORE__NONE => no compression */
int *status                /* global status (given and returned) */
)
/* Description :
    Store the value of the compression flag which says whether, and how, raw
    data written should be compressed.

   History :
    05Nov2007 : original (bdk)
    11Nov2010 : "compflag" type changed from int to sc2store_cmptype (dsb)
*/
{


   if ( *status != SAI__OK ) return;

   sc2store_compflag = compflag;
}



/*+ sc2store_unmapwts - unmap and close a DREAM weights file */

void sc2store_unmapwts
(
int *status               /* global status (given and returned) */
)
/*
   Method :
    Unmap and close the SCUBA-2 DREAM weights file.
   History :
    15Apr2008 : original (bdk)
    09May2008 : add windext (bdk)
*/
{
   int tstatus;      /* local status */

   tstatus = SAI__OK;

   if ( sc2store_indfwt != NDF__NOID )
   {
      if ( sc2store_indfjigext != NDF__NOID )
      {
         ndfUnmap ( sc2store_indfjigext, "DATA", &tstatus );
         ndfAnnul ( &sc2store_indfjigext, &tstatus );
      }
      if ( sc2store_indfwindext != NDF__NOID )
      {
         ndfUnmap ( sc2store_indfwindext, "DATA", &tstatus );
         ndfAnnul ( &sc2store_indfwindext, &tstatus );
      }
      if ( sc2store_indfgridext != NDF__NOID )
      {
         ndfUnmap ( sc2store_indfgridext, "DATA", &tstatus );
         ndfAnnul ( &sc2store_indfgridext, &tstatus );
      }
      if ( sc2store_indfinvmatx != NDF__NOID )
      {
         ndfUnmap ( sc2store_indfinvmatx, "DATA", &tstatus );
         ndfAnnul ( &sc2store_indfinvmatx, &tstatus );
      }
      if ( sc2store_indfgridwts != NDF__NOID )
      {
         ndfUnmap ( sc2store_indfgridwts, "DATA", &tstatus );
         ndfAnnul ( &sc2store_indfgridwts, &tstatus );
      }
      if ( sc2store_indfqual != NDF__NOID )
      {
         ndfUnmap ( sc2store_indfqual, "DATA", &tstatus );
         ndfAnnul ( &sc2store_indfqual, &tstatus );
      }

      datAnnul ( &sc2store_dreamwtloc, &tstatus );

/* Unmap the main data array */

      ndfAnnul ( &sc2store_indfwt, &tstatus );
   }
}



/*+ sc2store_wrconfigxml - Store the CONFIGURE XML */

void sc2store_wrconfigxml
(
const char *xmlfile,  /* name of CONFIGURE XML file (given) */
int *status           /* global status (given and returned) */
)
/* Description :
    Determine the size of the CONFIGURE XML. Create an NDF extension to hold the
    XML and copy if from the file.

   History :
    25Oct2007 : original, based on fragment from Tim Jenness (bdk)
    26Oct2007 : interpret JITXML_DIR search-path (bdk)
    03Nov2007 : Explicitly allow a NULL filename (timj)
*/
{

   FILE *fd = NULL;                       /* descriptor for XML file */
   HDSLoc *xloc = NULL;                   /* locator to NDF extension */
   HDSLoc *temploc = NULL;                /* HDS locator */
   size_t nlines;                         /* number of lines to be written */
   size_t nchars = 72;                    /* Standard internet line width */
   size_t inlen;                          /* number of bytes in XML file */
   hdsdim dims[1];                        /* number of lines to be written */
   char *tempstr;                         /* padded storage */
   static const char *def = "<OCS_CONFIG></OCS_CONFIG>"; /* default string */

   if ( *status != SAI__OK ) return;

/* Open the XML file */

   if (xmlfile) fd = fopen ( xmlfile, "rb" );


/* Note that HDS does not allow a single scalar character to exceed 65535 bytes
   and so we need to write the string as an array (even though the array size
   does not matter) */

   if ( fd != NULL )
   {

/* Work out the number of lines and round it up to make sure there is enough
   space. */

      fseek ( fd, 0, SEEK_END );
      inlen = ftell ( fd );

      if (inlen > nchars)
      {
         nlines = (size_t)ceil( (double)inlen / (double)nchars );
      }
      else
      {
         nlines = 1;
         nchars = inlen;
      }

/* Since HDS does not pad the buffer we need to pad it here */

      tempstr = malloc ( (nlines * nchars) + 1 );
      fseek ( fd, 0, SEEK_SET );
      fread ( tempstr, sizeof(*tempstr), inlen, fd );
      fclose ( fd );
      memset ( &(tempstr[inlen]), ' ', (nlines * nchars) - inlen );
      tempstr[nlines*nchars] = '\0';

/* create the extension and array component */

      ndfXnew ( sc2store_indf, "JCMTOCS", "OCSINFO", 0, NULL, &xloc, status );
      datNew1C ( xloc, "CONFIG", nchars, nlines, status );

/* and write it - note the use of datPutC because we don't have a real array,
   just a buffer that we are conning HDS with. */

      datFind ( xloc, "CONFIG", &temploc, status );
      dims[0] = nlines;
      datPutC ( temploc, 1, dims, tempstr, nchars, status );
      free ( tempstr );

   }
   else
   {

/* failed to get XML file - create a dummy entry */

      nlines = 1;
      nchars = strlen ( def );
      ndfXnew ( sc2store_indf, "JCMTOCS", "OCSINFO", 0, NULL, &xloc, status );
      datNew1C ( xloc, "CONFIG", nchars, nlines, status );

      datFind ( xloc, "CONFIG", &temploc, status );
      dims[0] = nlines;
      datPutC ( temploc, 1, dims, def, nchars, status );

   }

/* tidy up */

   datAnnul ( &temploc, status );
   datAnnul ( &xloc, status );

   sc2store_errconv ( status );
}



/*+ sc2store_writefitshead - write the FITS headers */

void sc2store_writefitshead
(
int id_ndf,           /* identifier of ndf (given) */
size_t nrec,          /* number of header records (given) */
const char *headers,  /* string of contiguous 80-byte FITS headers (given) */
int *status           /* global status (given and returned) */
)
/* Description :
    Create an NDF extension for FITS headers, map it as a vector, and
    copy the string of headers (contiguous sets of 80 bytes not
    separated by nulls) into the vector. This follows the approach in
    the kaplibs routine kpgPtfts().

   History :
    12Aug2004 : original (bdk)
    23Nov2005 : Use Official C HDS interface (TIMJ)
    20Mar2007 : use const in signature (timj)
    24Apr2004 : pass headers as single string (bdk)
    23Oct2007 : make nfits int instead of unsigned int (bdk)
    31Oct2007 : change nfits to nrec (bdk)
*/
{
   HDSLoc *fitsloc = NULL;    /* HDS locator to FITS headers */
   char *fptr;                /* Pointer to mapped FITS header */
   int nnew;                  /* size of vector requested */
   size_t nvec;               /* size of vector mapped */
   void *vptr;                /* dummy void pointer */


   if ( *status != SAI__OK ) return;
   if ( nrec == 0 ) return;

/* Create and write FITS headers.
   Note Xnew and MapV require different signedness of their count
   argument */

   nnew = nrec;
   ndfXnew ( id_ndf, "FITS", "_CHAR*80", 1, &(nnew), &fitsloc, status );
   datMapV( fitsloc, "_CHAR*80", "WRITE", &vptr, &nvec, status );
   if ( *status == SAI__OK )
   {
      fptr = (char *)vptr;
      strncpy( fptr, headers, nvec*80 );
   }

   datAnnul ( &fitsloc, status );

   sc2store_errconv ( status );
}



/*+ sc2store_writeflatcal - write flatfield calibration */

void sc2store_writeflatcal
(
size_t colsize,             /* number of pixels in a column (given) */
size_t rowsize,             /* number of pixels in a row (given) */
size_t nflat,               /* number of flat coeffs per bol (given) */
double refres,              /* Reference resistor used to create flatfield (given) */
const char *flatname,       /* name of flatfield algorithm (given) */
const double *flatcal,      /* flat-field calibration (given) */
const double *flatpar,      /* flat-field parameters (given) */
int *status              /* global status (given and returned) */
)
/*
   History :
    20Nov2007 : original (bdk)
*/

{
   int el;                     /* number of elements mapped */
   double *fcal;               /* pointer to flatfield calibration */
   double *fpar;               /* pointer to flatfield parameters */
   int j;                      /* loop counter */
   int lbnd[3];                /* lower dimension bounds */
   int place;                  /* NDF placeholder */
   int tdims[1];               /* temporary dimension store */
   int ubnd[3];                /* upper dimension bounds */


   if ( *status != SAI__OK ) return;

   /* if nflat is zero we assume that flatfield writing is disabled */
   if (nflat == 0) return;

/* Create an NDF containing flat-field calibration */

   sc2store_fillbounds( colsize, rowsize, nflat, lbnd, ubnd, status );

   ndfPlace ( sc2store_scuba2loc, "FLATCAL", &place, status );
   ndfNew ( "_DOUBLE", 3, lbnd, ubnd, &place, &sc2store_findf, status );

   ndfMap ( sc2store_findf, "DATA", "_DOUBLE", "WRITE", (void *)(&fcal), &el,
     status );
   ndfXnew ( sc2store_findf, "FLATDATA", "SCUBA2_FD_PAR", 0, 0,
     &sc2store_fdataloc, status );

/* Name of flatfield correction technique */

   datNew ( sc2store_fdataloc, "FLATNAME", "_CHAR*16", 0, 0, status );
   datFind ( sc2store_fdataloc, "FLATNAME", &sc2store_fnameloc, status );
   datPut0C ( sc2store_fnameloc, flatname, status );

/* Reference resistance (if good) */
   if (refres != VAL__BADD) {
     datNew0D( sc2store_fdataloc, "REFRES", status );
     datFind( sc2store_fdataloc, "REFRES", &sc2store_frefresloc, status );
     datPut0D( sc2store_frefresloc, refres, status );
   }

/* Parameter matching each flatfield coefficient */

   tdims[0] = nflat;
   datNew ( sc2store_fdataloc, "FLATPAR", "_DOUBLE", 1, tdims, status );
   datFind ( sc2store_fdataloc, "FLATPAR", &sc2store_fparloc, status );

   datMap ( sc2store_fparloc, "_DOUBLE", "WRITE", 1, tdims, (void *)(&fpar),
     status );

/* Copy the flatfield calibration */

   for ( j=0; j<colsize*rowsize*nflat; j++ )
   {
      fcal[j] = flatcal[j];
   }

   for ( j=0; j<nflat; j++ )
   {
      fpar[j] = flatpar[j];
   }

   sc2store_errconv ( status );
}



/*+ sc2store_writeframehead - store SCUBA-2 per-frame header items */

void sc2store_writeframehead
(
size_t nframes,             /* number of frames (given) */
const JCMTState head[],     /* header data for each frame (given) */
int *status                 /* global status (given and returned) */
)

/*  Description :
     Store the per-frame header items.

    Authors :
     B.D.Kelly (UKATC)

    History :
     21Nov2007:  original (bdk)
*/

{
   int j;                             /* loop counter */

   if ( !StatusOkP(status) ) return;

   if ( !head ) return; /* disable JCMTSTATE writing if null pointer */

/* Create storage for Header values for each frame - store in JCMTSTATE */

   ndfXnew ( sc2store_indf, JCMT__EXTNAME, JCMT__EXTTYPE, 0, 0,
     &sc2store_jcmtstateloc, status );

   sc2store_headcremap ( sc2store_jcmtstateloc, nframes, INST__SCUBA2, status );

/* Insert per-frame headers */

   sc2store_putjcmtstate( nframes, head, status );

   sc2store_errconv ( status );
}

/* sc2store_writejcmtstate - Create JCMTSTATE structure in file */

void sc2store_writejcmtstate
(
int indf,                   /* NDF identifier (can be NDF__NOID) */
size_t nframes,             /* number of frames (given) */
const JCMTState head[],     /* header data for each frame (given) */
int *status                 /* global status (given and returned) */
)

/*  Description :
     Create JCMTSTATE structure and write contents.

     This routine uses the global sc2store NDF identifier
     but on exit there are no open locators or mapped
     HDS components.

     Use sc2store_writeframehead to leave the JCMTSTATE mapped.

     If "indf" is NDF__NOID the routine will use the cached
     global NDF identifier. An external NDF identifier can be
     supplied to write state information to an external file.

    Authors :
     Tim Jenness (JAC)

    History :
     7Jan2011:  original (timj)
*/
{
  HDSLoc * jcmtstateloc = NULL;
  double * dbuff = NULL;
  int * ibuff = NULL;
  short * sbuff = NULL;
  float * fbuff = NULL;
  unsigned short * usbuff = NULL;
  char * strbuff = NULL;
  size_t i = 0;
  int thisndf = NDF__NOID;

  /* Use the external one if defined */
  thisndf = ( indf != NDF__NOID ? indf : sc2store_indf );

  /* Create the JCMTSTATE extension */
  ndfXnew ( thisndf, JCMT__EXTNAME, JCMT__EXTTYPE, 0, 0,
            &jcmtstateloc, status );


  /* We need to go through each state structure and find out whether
     to map it as a full N-element array or a single scalar. We do not
     need to use all buffers at the same time so we just cast pointers
     to the largest buffer. */

  dbuff = astMalloc( nframes * sizeof(*dbuff) );
  fbuff = (float *)dbuff;
  ibuff = (int *)dbuff;
  sbuff = (short *)dbuff;
  usbuff = (unsigned short *)dbuff;

  /* The string buffer is created from the largest string length of an
     item (which we define in jcmt/state.h). Make space for one nul. */
  strbuff = astMalloc( ( nframes * JCMT__SZLARGEST ) + 1 );

  /* Macro to create items in HDS and copy the values. */
#define STORE_STATE( ITEM, ITEMSTR, HDSTYPE, TYPE, BUFFER, DIFFERENT ) \
  if (*status == SAI__OK) {                                            \
    int dim[1];                                                        \
    int different = DIFFERENT;                                         \
    int ndim;                                                          \
    int nwrite;                                                        \
    HDSLoc * tloc = NULL;                                              \
    TYPE previous;                                                     \
    void * mapped = NULL;                                              \
    if (different) {                                                   \
      /* We know beforehand that each entry is different */            \
      for (i=0; i<nframes; i++) {                                      \
        BUFFER[i] = head[i].ITEM;                                      \
      }                                                                \
    } else {                                                           \
      previous = head[0].ITEM;                                         \
      for (i=0; i<nframes; i++) {                                      \
        BUFFER[i] = head[i].ITEM;                                      \
        if (!different) {                                              \
          if (BUFFER[i] != previous) {                                 \
            different = 1;                                             \
          }                                                            \
          previous = BUFFER[i];                                        \
        }                                                              \
      }                                                                \
    }                                                                  \
    if (different) {                                                   \
      ndim = 1;                                                        \
      dim[0] = nframes;                                                \
      nwrite = nframes;                                                \
    } else {                                                           \
      ndim = 0;                                                        \
      dim[0] = 0;                                                      \
      nwrite = 1;                                                      \
    }                                                                  \
    datNew( jcmtstateloc, ITEMSTR, HDSTYPE, ndim, dim, status );       \
    datFind( jcmtstateloc, ITEMSTR, &tloc, status );                   \
    datMap( tloc, HDSTYPE, "WRITE", ndim, dim, &mapped, status );      \
    memcpy( mapped, BUFFER, nwrite * sizeof(*BUFFER) );                \
    datAnnul( &tloc, status );                                         \
  }

#define STORE_CHAR( ITEM, ITEMSTR, ITEMLEN )                            \
  if (*status == SAI__OK) {                                             \
    int dim[1];                                                         \
    int different = 0;                                                  \
    int ndim;                                                           \
    int nwrite;                                                         \
    HDSLoc * tloc = NULL;                                               \
    char previous[ITEMLEN + 1];                                         \
    void * mapped = NULL;                                               \
    char * curpos = NULL;                                               \
    char hdstype[DAT__SZTYP+1];                                         \
    star_strlcpy( previous, head[0].ITEM, sizeof(previous) );           \
    curpos = strbuff;                                                   \
    for (i=0; i<nframes; i++) {                                         \
      cnfExprt( head[i].ITEM, curpos, ITEMLEN );                        \
      if (!different) {                                                 \
        if (strcmp( head[i].ITEM, previous ) != 0 ) {                   \
          different = 1;                                                \
        }                                                               \
        star_strlcpy( previous, head[i].ITEM, sizeof(previous) );       \
      }                                                                 \
      curpos += ITEMLEN;                                                \
    }                                                                   \
    if (different) {                                                    \
      ndim = 1;                                                         \
      dim[0] = nframes;                                                 \
      nwrite = nframes;                                                 \
    } else {                                                            \
      ndim = 0;                                                         \
      dim[0] = 0;                                                       \
      nwrite = 1;                                                       \
    }                                                                   \
    datCctyp( ITEMLEN, hdstype );                                       \
    datNew( jcmtstateloc, ITEMSTR, hdstype, ndim, dim, status );        \
    datFind( jcmtstateloc, ITEMSTR, &tloc, status );                    \
    datMap( tloc, hdstype, "WRITE", ndim, dim, &mapped, status );       \
    memcpy( mapped, strbuff, nwrite * ITEMLEN );                        \
    datAnnul( &tloc, status );                                          \
  }

  /* The first two are always stored in full */
  STORE_STATE( rts_num, "RTS_NUM", "_INTEGER", int, ibuff, 1 );
  STORE_STATE( rts_end, "RTS_END", "_DOUBLE", double, dbuff, 1 );

  STORE_CHAR( smu_chop_phase, "SMU_CHOP_PHASE", JCMT__SZSMU_CHOP_PHASE );
  STORE_STATE( smu_jig_index, "SMU_JIG_INDEX", "_WORD", short, sbuff, 0 );
  STORE_STATE( smu_az_jig_x, "SMU_AZ_JIG_X", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( smu_az_jig_y, "SMU_AZ_JIG_Y", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( smu_az_chop_x, "SMU_AZ_CHOP_X", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( smu_az_chop_y, "SMU_AZ_CHOP_Y", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( smu_tr_jig_x, "SMU_TR_JIG_X", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( smu_tr_jig_y, "SMU_TR_JIG_Y", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( smu_tr_chop_x, "SMU_TR_CHOP_X", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( smu_tr_chop_y, "SMU_TR_CHOP_Y", "_DOUBLE", double, dbuff, 0 );

  STORE_STATE( tcs_tai, "TCS_TAI", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_airmass, "TCS_AIRMASS", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_az_ang, "TCS_AZ_ANG", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_az_ac1, "TCS_AZ_AC1", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_az_ac2, "TCS_AZ_AC2", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_az_dc1, "TCS_AZ_DC1", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_az_dc2, "TCS_AZ_DC2", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_az_bc1, "TCS_AZ_BC1", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_az_bc2, "TCS_AZ_BC2", "_DOUBLE", double, dbuff, 0 );
  STORE_CHAR( tcs_beam, "TCS_BEAM", JCMT__SZTCS_BEAM );
  STORE_STATE( tcs_index, "TCS_INDEX", "_WORD", short, sbuff, 0 );
  STORE_STATE( tcs_percent_cmp, "TCS_PERCENT_CMP", "_WORD", short, sbuff, 0 );
  STORE_CHAR( tcs_source, "TCS_SOURCE", JCMT__SZTCS_SOURCE );
  STORE_CHAR( tcs_tr_sys, "TCS_TR_SYS", JCMT__SZTCS_TR_SYS );
  STORE_STATE( tcs_tr_ang, "TCS_TR_ANG", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_tr_ac1, "TCS_TR_AC1", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_tr_ac2, "TCS_TR_AC2", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_tr_dc1, "TCS_TR_DC1", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_tr_dc2, "TCS_TR_DC2", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_tr_bc1, "TCS_TR_BC1", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_tr_bc2, "TCS_TR_BC2", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_en_dc1, "TCS_EN_DC1", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_en_dc2, "TCS_EN_DC2", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_dm_abs, "TCS_DM_ABS", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( tcs_dm_rel, "TCS_DM_REL", "_DOUBLE", double, dbuff, 0 );

  STORE_STATE( jos_drcontrol, "JOS_DRCONTROL", "_WORD", short, sbuff, 0 );

  STORE_STATE( wvm_t12, "WVM_T12", "_REAL", float, fbuff, 0 );
  STORE_STATE( wvm_t42, "WVM_T42", "_REAL", float, fbuff, 0 );
  STORE_STATE( wvm_t78, "WVM_T78", "_REAL", float, fbuff, 0 );
  STORE_STATE( wvm_time, "WVM_TIME", "_DOUBLE", double, dbuff, 0 );

  STORE_STATE( sc2_heat, "SC2_HEAT", "_UWORD", unsigned short, usbuff, 0 );
  STORE_STATE( sc2_bias, "SC2_BIAS", "_UWORD", unsigned short, usbuff, 0 );
  STORE_STATE( sc2_mixtemp, "SC2_MIXTEMP", "_REAL", float, fbuff, 0 );
  STORE_STATE( sc2_fputemp, "SC2_FPUTEMP", "_REAL", float, fbuff, 0 );
  STORE_STATE( sc2_1kntdtemp, "SC2_1KNTDTEMP", "_REAL", float, fbuff, 0 );

  STORE_STATE( pol_ang, "POL_ANG", "_DOUBLE", double, dbuff, 0 );
  STORE_STATE( fts_pos, "FTS_POS", "_REAL", float, fbuff, 0 );

  /* Free resources */
  dbuff = astFree( dbuff );
  strbuff = astFree( strbuff );
  datAnnul( &jcmtstateloc, status );

  sc2store_errconv ( status );

}

/*+ sc2store_writejig - create and write DREAM extension in output file */

void sc2store_writejig
(
int jigvert[][2],           /* Array of jiggle vertices (given) */
size_t nvert,               /* Number of jiggle vertices (given) */
double jigpath[][2],        /* Path of SMU during jiggle cycle (given) */
size_t npath,               /* Number of positions in jiggle path (given) */
int *status              /* Global status (given and returned) */
)
/*
  History :
   08Aug2006 : original (agg)
   21Nov2007 : write values as well as creating structure (bdk)
*/
{
   int el;               /* number of mapped elements */
   int j;                /* loop counter */
   int *jvert;           /* Pointer to stored jiggle vertices */
   double *jpath;        /* Pointer to stored jiggle path */
   int lbnd[2];          /* lower bounds of mapped array */
   int ubnd[2];          /* upper bounds of mapped array */
   int place;            /* NDF placeholder */


   if ( !StatusOkP(status) ) return;


/* Get new HDS locator for DREAM extension  */

   ndfXnew ( sc2store_indf, "DREAM", "DREAM_PAR", 0, 0, &sc2store_drmloc,
     status );

   if ( sc2store_drmloc == NULL )
   {
      if ( *status == SAI__OK)
      {
         *status = SAI__ERROR;
         ErsRep ( 0, status, "Unable to create NDF extension for DREAM");
      }
   }

/* Create NDF for jigvert array */

   ndfPlace ( sc2store_drmloc, "JIGVERT", &place, status );
   if ( place == NDF__NOPL )
   {
      if ( *status == SAI__OK)
      {
         *status = SAI__ERROR;
         ErsRep ( 0, status, "Unable to create placeholder for DREAM jigvert array");
      }
   }
   lbnd[0] = 1;
   lbnd[1] = 1;
   ubnd[0] = nvert;
   ubnd[1] = 2;
   ndfNew ( "_INTEGER", 2, lbnd, ubnd, &place, &sc2store_jigvndf, status );
   if ( sc2store_jigvndf == NDF__NOID )
   {
      if ( *status == SAI__OK )
      {
         *status = SAI__ERROR;
         ErsRep(0, status, "Unable to obtain an NDF identifier for the jigvert array");
      }
   }

/* Map jigvert array so that it's ready to fill */

   ndfMap ( sc2store_jigvndf, "DATA", "_INTEGER", "WRITE", (void *)(&jvert),
     &el, status );
   if ( jvert == NULL )
   {
      if ( *status == SAI__OK )
      {
         *status = SAI__ERROR;
         ErsRep ( 0, status, "Unable to map DREAM jigvert array");
      }
   }

/* Create NDF for jigpath array */

   ndfPlace ( sc2store_drmloc, "JIGPATH", &place, status );
   if ( place == NDF__NOPL )
   {
      if ( *status == SAI__OK )
      {
         *status = SAI__ERROR;
         ErsRep ( 0, status, "Unable to create placeholder for DREAM jigpath array");
      }
   }
   lbnd[0] = 1;
   lbnd[1] = 1;
   ubnd[0] = npath;
   ubnd[1] = 2;
   ndfNew ( "_DOUBLE", 2, lbnd, ubnd, &place, &sc2store_jigpndf, status );
   if ( sc2store_jigpndf == NDF__NOID )
   {
      if ( *status == SAI__OK )
      {
         *status = SAI__ERROR;
         ErsRep ( 0, status,
	   "Unable to obtain an NDF identifier for the jigpath array" );
      }
   }
   ndfMap ( sc2store_jigpndf, "DATA", "_DOUBLE", "WRITE", (void *)(&jpath),
      &el, status );
   if ( jpath == NULL )
   {
      if ( *status == SAI__OK )
      {
         *status = SAI__ERROR;
         ErsRep ( 0, status, "Unable to map DREAM jigpath array");
      }
   }

/* Copy the DREAM data into the array: Fortran order */

   if ( *status == SAI__OK )
   {

/* First jigvert */

      for ( j=0; j<nvert; j++ )
      {
         jvert[j] = jigvert[j][0];
         jvert[j+nvert] = jigvert[j][1];
      }

/* Then jigpath */

      for ( j=0; j<npath; j++ )
      {
         jpath[j] = jigpath[j][0];
         jpath[j+npath] = jigpath[j][1];
      }
   }
   sc2store_errconv ( status );

}


/*+ sc2store_writeraw - create HDS container file and write raw data */

void sc2store_writeraw
(
const char *filename,    /* name of HDS container file (given) */
size_t colsize,          /* number of pixels in a column (given) */
size_t rowsize,          /* number of pixels in a row (given) */
size_t nframes,          /* number of frames (given) */
size_t ntrack,           /* number of bolometers used for heater tracking (given) */
const int *dbuf,         /* time stream data (given) */
const int *dksquid,      /* dark SQUID time stream data (given) */
const int *trackinfo,    /* 3xntrack int array with (col,row,heat) groups (given) */
int *status              /* global status (given and returned) */
)
/*
   History :
    12Aug2004 : original (bdk)
    23Mar2005 : swap order of nrow and ncol in NDF (bdk)
    20Apr2005 : swap order of nrow and ncol in argument list (bdk)
    20May2005 : add flatcal (bdk)
    19Jun2005 : use ndfHcre to create history component on all NDFs (bdk)
    24Sep2005 : add nflat, flatname and flatpar to generalise flatfield
                storage, and use colsize and rowsize as names (bdk)
    27Jul2006 : Create JCMTSTATE structure (timj)
    23Oct2007 : rename structure for incompressible pixels to INCOMPS (bdk)
    23Oct2007 : only call ndfHcre for top-level NDF (bdk)
    23Oct2007 : rename structure for per-frame data to SCUBA2 (bdk)
    09Nov2007 : store global BSCALE (bdk)
    12Nov2007 : make main data array short rather than unsigned short (bdk)
    17Aug2009 : Write explicit history message (timj)
    13Nov2009 : Add heater track info (timj)
*/

{
   int *bzero;                 /* pointer to subtracted offset values */
   int *darksquid;             /* pointer to dark SQUID values */
   static int digits[2*DREAM__MXBOL]; /* copy of each frame */
   int el;                     /* number of elements mapped */
   size_t framesize;           /* number of values per frame */
   int i;                      /* loop counter */
   int *idata;                 /* pointer to uncompressed data array */
   int j;                      /* loop counter */
   int lbnd[3];                /* lower dimension bounds */
   size_t npix;                       /* number of incompressible pixels */
   static int pixnum[DREAM__MXBOL];   /* indices of incompressible pixels */
   static int pixval[DREAM__MXBOL];   /* values of incompressible pixels */
   int place;                  /* Placeholder for final NDF */
   short *sdata;               /* pointer to compressed data array */
   int *stackz;                /* pointer to subtracted frame */
   int tdims[1];               /* temporary dimension store */
   int tmp_place;              /* Placeholder for temporary NDF */
   int tmp_indf;               /* Temporary NDF identifier */
   int ubnd[3];                /* upper dimension bounds */
   float zratio;               /* Compression ratio achieved */
#ifdef SC2STORE_WRITE_HISTORY
   const char * const history[1] = { "Write raw data." };
#endif



   if ( *status != SAI__OK ) return;


/* Initialise Starlink error reporting NDF and start its context */

   sc2store_initialise( status );
   errMark();
   ndfBegin();

/* Create an HDS container file */

   ndfPlace ( NULL, filename, &place, status );

/* Create an NDF inside the container */

   sc2store_fillbounds( colsize, rowsize, nframes, lbnd, ubnd, status );

   if ( sc2store_compflag == SC2STORE__BDK )
   {

/* Create structures to hold BDK compressed data */

      ndfNew ( "_WORD", 3, lbnd, ubnd, &place, &sc2store_indf, status );
      ndfHcre ( sc2store_indf, status );

/* Labels and units */
      ndfCput( "Compressed Signal", sc2store_indf, "LABEL", status );

/* Map the data array */

      ndfMap ( sc2store_indf, "DATA", "_WORD", "WRITE", (void *)(&sdata), &el,
        status );

/* Create extension for holding fixed-size data (subtracted constant and
   dark SQUID measurements) for each frame */

      ndfXnew ( sc2store_indf, "SCUBA2", "SCUBA2_FM_PAR", 0, 0,
        &sc2store_scuba2loc, status );

      ubnd[0] = nframes;
      lbnd[0] = 1;

/* Create and map compression zero offset for each frame */

      ndfPlace ( sc2store_scuba2loc, "BZERO", &place, status );
      ndfNew ( "_INTEGER", 1, lbnd, ubnd, &place, &sc2store_zindf, status );

      ndfMap ( sc2store_zindf, "DATA", "_INTEGER", "WRITE", (void *)(&bzero),
        &el, status );


/* Global data scale factor in compression */

      datNew ( sc2store_scuba2loc, "BSCALE", "_DOUBLE", 0, 0, status );
      datFind ( sc2store_scuba2loc, "BSCALE", &sc2store_bscaleloc, status );
      datPut0D ( sc2store_bscaleloc, sc2store_wrbscale, status );
      datAnnul ( &sc2store_bscaleloc, status );

/* Frame subtracted from each data frame before compression */

      sc2store_fillbounds( colsize, rowsize, 0, lbnd, ubnd, status );

      ndfPlace ( sc2store_scuba2loc, "STACKZERO", &place, status );
      ndfNew ( "_INTEGER", 2, lbnd, ubnd, &place, &sc2store_sindf, status );

      ndfMap ( sc2store_sindf, "DATA", "_INTEGER", "WRITE", (void *)(&stackz),
        &el, status );

/* Create the store for incompressible values for each frame */

      tdims[0] = nframes;
      ndfXnew ( sc2store_indf, "INCOMPS", "SCUBA2_INC_ARR", 1, tdims,
        &sc2store_incomploc, status );

/* Use the first frame as the stackzero frame */

      framesize = colsize * rowsize;

      if ( StatusOkP(status) )
      {

         for ( j=0; j<framesize; j++ )
         {
            stackz[j] =  (int)( (double)dbuf[j] / sc2store_wrbscale );
         }

/* Compress one frame at a time */

         for ( j=0; j<nframes; j++ )
         {
            for ( i=0; i<framesize; i++ )
            {
               digits[i] =
	         (int)( (double)dbuf[j*framesize+i] / sc2store_wrbscale );
            }

            sc2store_compress ( framesize, stackz, digits, &(bzero[j]),
              &(sdata[j*framesize]), &npix, pixnum, pixval, status );

            if ( npix > 0 )
            {
               sc2store_putincomp ( j, npix, pixnum, pixval, status );
            }
         }

      }

   }
   else
   {

/* Create structures to hold data without compression (delta compression
   may occur later). If the final NDF will be delta compressed, we start off
   by creating a temporary NDF and then later copy it, with compression,
   into the required file. */

      if ( sc2store_compflag == SC2STORE__DELTA ) {
         ndfTemp( &tmp_place, status );
      } else {
         tmp_place = place;
      }

      ndfNew ( "_INTEGER", 3, lbnd, ubnd, &tmp_place, &sc2store_indf, status );

/* Map the data array and copy in the data values. */
      ndfMap ( sc2store_indf, "DATA", "_INTEGER", "WRITE", (void *)(&idata),
        &el, status );
      if (StatusOkP(status) ) memcpy ( idata, dbuf, el*sizeof(*dbuf) );

/* Unmap the data array. */
      ndfUnmap( sc2store_indf, "DATA", status );

/* If required, create a copy of the NDF in which the DATA array is
   stored in delta compressed form. */
      if ( sc2store_compflag == SC2STORE__DELTA ) {
         ndfZdelt( sc2store_indf, "DATA", 0.0, 3, "_WORD", &place, &tmp_indf,
                   &zratio, status );

/* Annul the temporary NDF and use the compressed NDF in its place. */
         ndfAnnul( &sc2store_indf, status );
         sc2store_indf = tmp_indf;
      }

/* Create a history component and a SCUBA2 extension. */
      ndfHcre ( sc2store_indf, status );
      ndfXnew ( sc2store_indf, "SCUBA2", "SCUBA2_FM_PAR", 0, 0,
        &sc2store_scuba2loc, status );

/* Labels and units */
      ndfCput( "adu", sc2store_indf, "UNITS", status );
      ndfCput( "Signal", sc2store_indf, "LABEL", status );
   }

/* Write history entry */
#ifdef SC2STORE_WRITE_HISTORY
   ndfHput( "NORMAL", APPNAME, 1, 1, (char *const*) history,
            0, 0, 0, sc2store_indf, status );
#endif

/* Heater tracking information. We have an array that is 3 x ntrack in size
   where the order is (col1, row1, heat1), (col2, row2, heat2 )...
   We need to determine the maximum extent of col and row and create an 2d
   image accordingly with the correct pixel origin.
*/
   if (ntrack > 0 && trackinfo ) {
     const int * thistrack = trackinfo;
     int * hpntr = NULL;
     int sdims[2];  /* number of dimensions in subset of image */
     int maxbnd[2];
     int minbnd[2];

     minbnd[0] = SC2STORE__BOL_LBND;
     minbnd[1] = SC2STORE__BOL_LBND;
     maxbnd[SC2STORE__ROW_INDEX] = colsize + SC2STORE__BOL_LBND - 1;
     maxbnd[SC2STORE__COL_INDEX] = rowsize + SC2STORE__BOL_LBND - 1;

     /* these are flipped because we are calculating the min/max */
     ubnd[0] = minbnd[0];
     ubnd[1] = minbnd[1];
     lbnd[0] = maxbnd[0];
     lbnd[1] = maxbnd[1];

     for ( j = 0; j < ntrack; j++ ) {
       int colnum = thistrack[0];
       int rownum = thistrack[1];
       if (colnum > ubnd[SC2STORE__COL_INDEX]) ubnd[SC2STORE__COL_INDEX] = colnum;
       if (rownum > ubnd[SC2STORE__ROW_INDEX]) ubnd[SC2STORE__ROW_INDEX] = rownum;
       if (colnum < lbnd[SC2STORE__COL_INDEX]) lbnd[SC2STORE__COL_INDEX] = colnum;
       if (rownum < lbnd[SC2STORE__ROW_INDEX]) lbnd[SC2STORE__ROW_INDEX] = rownum;
       thistrack += 3;
     }

     /* sanity checks */
     if (*status == SAI__OK) {
       if (lbnd[0] < minbnd[0] || lbnd[0] > maxbnd[0] ||
           lbnd[1] < minbnd[1] || lbnd[1] > maxbnd[1] ||
           ubnd[0] < minbnd[0] || ubnd[0] > maxbnd[0] ||
           ubnd[1] < minbnd[1] || ubnd[1] > maxbnd[1] ) {
         *status = SAI__ERROR;
         errRepf( "", "Calculated pixel bounds (%d,%d) -> (%d,%d) are out of "
                  "expected range (%d,%d) -> (%d,%d)", status,
                  lbnd[0],lbnd[1],ubnd[0],ubnd[1],
                  minbnd[0],minbnd[1],maxbnd[0],maxbnd[1]);
       }
     }

     /* calculate dimensions */
     sdims[SC2STORE__COL_INDEX] = ubnd[SC2STORE__COL_INDEX]
       - lbnd[SC2STORE__COL_INDEX] + 1;
     sdims[SC2STORE__ROW_INDEX] = ubnd[SC2STORE__ROW_INDEX]
       - lbnd[SC2STORE__ROW_INDEX] + 1;

     /* now create the extension and map it */
     ndfPlace ( sc2store_scuba2loc, "TRACKINFO", &place, status );
     ndfNew ( "_INTEGER", 2, lbnd, ubnd, &place, &sc2store_htindf, status );

     ndfMap ( sc2store_htindf, "DATA", "_INTEGER", "WRITE/BAD", (void *)(&hpntr),
              &el, status );

     if (*status == SAI__OK) {
       thistrack = trackinfo;
       for ( j = 0; j < ntrack; j++ ) {
         int colnum = thistrack[0];
         int rownum = thistrack[1];
         int offset = 0;

         /* offset into data array must take into account the lbnd */
         colnum -= lbnd[SC2STORE__COL_INDEX];
         rownum -= lbnd[SC2STORE__ROW_INDEX];

         if ( SC2STORE__COL_INDEX == 0 ) {
           offset = colnum + (rownum * sdims[0]);
         } else {
           offset = rownum + (colnum * sdims[0]);
         }

         if (offset < 0 || offset > el ) {
           *status = SAI__ERROR;
           errRepf( "", "Calculated offset for heater tracking information is out of bounds"
                    " ( 0 < %d < %d )", status, offset, el );
           break;
         }

         /* and store the value in the correct place */
         hpntr[offset] = thistrack[2];

         /* skip round to the next group */
         thistrack += 3;
       }
     }
   }


/* Dark SQUID values for each frame */

   if (dksquid) {
     ubnd[0] = rowsize + SC2STORE__BOL_LBND - 1;
     lbnd[0] = SC2STORE__BOL_LBND;
     ubnd[1] = nframes;
     lbnd[1] = 1;

     ndfPlace ( sc2store_scuba2loc, "DKSQUID", &place, status );
     ndfNew ( "_INTEGER", 2, lbnd, ubnd, &place, &sc2store_dindf, status );

     ndfMap ( sc2store_dindf, "DATA", "_INTEGER", "WRITE", (void *)(&darksquid),
              &el, status );

/* Copy the dark SQUID values */

     if (*status == SAI__OK)
       {
         for ( j=0; j<nframes*rowsize; j++ )
           {
             darksquid[j] = dksquid[j];
           }
       }
   }

   if ( StatusOkP(status) )
   {
      sc2store_sc2open = 1;
   }

   sc2store_errconv ( status );
}



/*+ sc2store_wrmcehead - Store the MCE headers for each frame */

void sc2store_wrmcehead
(
AstKeyMap *mcehead,         /* MCE header (given) */
int *status                 /* global status (given and returned) */
)
/* Description :
    Create an NDF extension to hold the headers and insert them.

   History :
    25Oct2007 : original (bdk)
    03Nov2007 : do nothing if mceheadsz = 0 (timj)
    23Sep2010 : change to use AST Keymap (timj)
*/
{
   HDSLoc *xloc = NULL;                   /* locator to NDF extension */

   if ( *status != SAI__OK ) return;
   if ( !mcehead ) return;

/* Create the extension and write the contents of the AST key map */
   datNew( sc2store_scuba2loc, "MCEHEAD", "KEYMAP_ENTRY", 0, 0, status );
   datFind( sc2store_scuba2loc, "MCEHEAD", &xloc,status );
   atlKy2hd( mcehead, xloc, status );

/* tidy up */

   datAnnul ( &xloc, status );

   sc2store_errconv ( status );
}

/*+ sc2store_wrtstream - store SCUBA-2 time stream data as NDF */

void sc2store_wrtstream
(
const char filename[],      /* output file name (given) */
sc2ast_subarray_t subnum,   /* Sub-array number (given) */
size_t nrec,                /* number of FITS header records (given) */
const char *fitsrec,        /* contiguous 80-byte FITS records (given) */
size_t colsize,             /* number of bolometers in column (given) */
size_t rowsize,             /* number of bolometers in row (given) */
size_t nframes,             /* number of frames (given) */
size_t nflat,               /* number of flat coeffs per bol (given) */
double refres,              /* Reference resistor used to create flatfield (given) */
size_t ntrack,              /* number of bolometers used for heater tracking (given) */
const char *flatname,       /* name of flatfield algorithm (given) */
const JCMTState head[],     /* header data for each frame (given) */
const SC2STORETelpar* telpar, /* Additional telescope information (given) */
const int *dbuf,            /* time stream data (given) */
const int *dksquid,         /* dark SQUID time stream data (given) */
const double *flatcal,      /* flat-field calibration (given) */
const double *flatpar,      /* flat-field parameters (given) */
const char *obsmode,        /* Observing mode (given) */
AstKeyMap *mcehead,         /* MCE header (given) */
const int *trackinfo,       /* 3xntrack int array with (col,row,heat) groups (given) */
int jigvert[][2],           /* Array of jiggle vertices (given) */
size_t nvert,               /* Number of jiggle vertices (given) */
double jigpath[][2],        /* Path of SMU during jiggle cycle (given) */
size_t npath,               /* Number of positions in jiggle path (given) */
const char *xmlfile,        /* name of CONFIGURE XML file (given) */
int *status                 /* global status (given and returned) */
)

/*  Description :
     Create and map a SCUBA-2 NDF file. Store a compressed version of the time
     stream, the per-frame header items, the current flatfield calibration
     and the FITS headers.

    Authors :
     B.D.Kelly (UKATC)

    History :
     24Sep2005:  original (bdk)
     04Oct2005 : check if sc2store already has an open file (bdk)
     08Dec2005 : check status after sc2store_cremap (bdk)
     08Aug2006 : add call to credream if we have a DREAM obs (agg)
     19Dec2006 : add WCS to output file (timj)
     20Mar2007 : use const in signature (timj)
     24Apr2007 : change fitsrec to char* (bdk)
     13Jul2007 : remove const from 2-D arrays to avoid warnings when calling
                 this routine (bdk)
     25Oct2007 : write a copy of the CONFIGURE XML (bdk)
     25Oct2007 : use RTS_END time for call to timeWcs (bdk)
     11Nov2007 : make compressed data short instead of unsigned short (bdk)
     13Nov2007 : divide by sc2store_wrbscale (bdk)
     13Nov2009 : Add heater track info (timj)
     04Feb2011 : Add rts_end buffer
*/

{
   size_t i = 0;                      /* Loop counter */
   AstFrameSet * wcs=NULL;            /* World Coordinates frame set */
   int *oldstat=NULL;                 /* Previous status used by AST */
   double * rts_end = NULL;           /* Place to store the times */

   if ( !StatusOkP(status) ) return;

   if ( sc2store_sc2open != 0 )
   {
      *status = DITS__APP_ERROR;
      sprintf ( sc2store_errmess,
        "sc2store_wrtstream: one SCUBA-2 data file already open, can't open %s", filename );
      ErsRep ( 0, status, sc2store_errmess );
      return;
   }

/* Map all the data arrays */

   sc2store_writeraw ( filename, colsize, rowsize, nframes, ntrack, dbuf, dksquid,
                       trackinfo, status );
   sc2store_writeflatcal ( colsize, rowsize, nflat, refres, flatname, flatcal, flatpar,
     status );

   if ( !StatusOkP(status) )
   {
      sprintf ( sc2store_errmess, "sc2store_wrtstream: failed to open %s", filename );
      ErsRep ( 0, status, sc2store_errmess );
   }

/* Write the per-frame headers */
   sc2store_writejcmtstate ( NDF__NOID, nframes, head, status );

/* Create DREAM extension ONLY if we have DREAM data */

   if ( strncmp ( obsmode, "DREAM", 5 ) == 0 )
   {
      sc2store_writejig ( jigvert, nvert, jigpath, npath, status );
   }


/* Store the FITS headers for the main NDF */

   sc2store_writefitshead ( sc2store_indf, nrec, fitsrec, status );

/* And create a convenience frameset for focal plane and time coordinates.
   Need the RTS_END information. */
   rts_end = astMalloc( nframes * sizeof(*rts_end) );
   if( StatusOkP(status) ) {
     for (i=0; i<nframes; i++) {
       rts_end[i] = head[i].rts_end;
     }
   }
   oldstat = astWatch( status );
   wcs = sc2store_timeWcs ( subnum, nframes, 1, telpar,
                            rts_end, status );
   rts_end = astFree( rts_end );
   if (wcs) {
     ndfPtwcs ( wcs, sc2store_indf, status );
     wcs = astAnnul ( wcs );
   }
   astWatch( oldstat );

/* Store the CONFIGURE XML */

   sc2store_wrconfigxml ( xmlfile, status );

/* Store the MCE headers for each frame */

   sc2store_wrmcehead ( mcehead, status );

/* The file is open */

   sc2store_sc2open = 1;
}

/*+ sc2store_updflatcal - update flatfield calibration in SCUBA-2 NDF */

void sc2store_updflatcal
(
const char filename[],      /* name of file to update (given) */
size_t colsize,             /* number of bolometers in column (given) */
size_t rowsize,             /* number of bolometers in row (given) */
size_t nflat,               /* number of flat coeffs per bol (given) */
double refres,              /* Reference resistor used to create flatfield (given) */
const char *flatname,       /* name of flatfield algorithm (given) */
const double *flatcal,      /* flat-field calibration (given) */
const double *flatpar,      /* flat-field parameters (given) */
int *status                 /* global status (given and returned) */
)

/*  Description :
     Map a SCUBA-2 NDF file, delete the pre-existing flatfield calibration
     and store the new values. Does not update the FITS header with the
     FLAT file name or determine whether the subarray is correct.

     File will be closed.

    Authors :
     T. Jenness (JAC, Hawaii)

    History :
     07Oct2009 : original (timj)
*/

{

   size_t refcolsize = 0;
   size_t refrowsize = 0;
   size_t nframes = 0;
   int isthere = 0;

   if ( !StatusOkP(status) ) return;

   if ( sc2store_sc2open != 0 )
   {
      *status = DITS__APP_ERROR;
      sprintf ( sc2store_errmess,
        "sc2store_updflatcal: one SCUBA-2 data file already open, can't open %s", filename );
      ErsRep ( 0, status, sc2store_errmess );
      return;
   }

/* Open the file in UPDATE mode */
   sc2store_open ( filename, "UPDATE", &refcolsize, &refrowsize, &nframes, status );

   if ( ((refcolsize != colsize) || (refrowsize != rowsize)) && StatusOkP(status)) {
     *status = DITS__APP_ERROR;
     sprintf( sc2store_errmess,
              "sc2store_updflatcal: Columns and rows in flatfield do not match the file to be updated" );
     ErsRep( 0, status, sc2store_errmess );
   }

/* Remove existing flatfield solution (consider adding to writeflatcal) */
   datThere( sc2store_scuba2loc, "FLATCAL", &isthere, status );
   if (isthere) {
     datErase( sc2store_scuba2loc, "FLATCAL", status );
   }

   /* write the flatfield information */
   sc2store_writeflatcal ( colsize, rowsize, nflat, refres, flatname, flatcal, flatpar,
     status );

   if ( StatusOkP(status) )
   {
      sc2store_sc2open = 1;
   }

   sc2store_free( status );
   return;
}

/*+ sc2store_force_initialised - indicate that we have already initialised */

void sc2store_force_initialised
(
int *status                 /* global status (given and returned) */
)

/* Description:
    Outside of the DA environment we need to be able to stop
    sc2store initialising ERR and NDF. This routine tells sc2store
    not to bother. This will work so long as sc2store_initialise does
    not start doing essential initialisations that are not related to
    ERR or NDF

   Authors :
    T. Jenness (JAC, Hawaii)

   History:
    11Aug2008 : fix up header (timj)
*/

{
  if (*status != SAI__OK) return;
  sc2store_initialised = 1;
}

/*+ sc2store_timeWcs:  Calculate frameset for time series. */

AstFrameSet *sc2store_timeWcs
(
 sc2ast_subarray_t subnum,
 int ntime,
 int use_tlut,
 const SC2STORETelpar* telpar,
 const double times[],
 int * status
)

/*
*  Name:
*     sc2store_timeWcs

*  Purpose:
*     Calculate frameset for time series.

*  Prototype:
*     AstFrameSet *sc2store_timeWcs( sc2ast_subarray_t subnum, int ntime,
*                const SC2STORETelpar * telpar,const double times[],
*                int * status );

*  Description:
*     Returns a FrameSet in which the base Frame is a 3D GRID Frame, and the
*     current Frame has 3 axes in the order (fplanex,fplaney,time). The time
*     axis is described using a MJD(TAI) TimeFrame, and its relationship to
*     GRID coords is specified by the supplied look-up table of time values.
*     The spatial axis is described by a 2D Frame with Domain "FPLANE" and
*     is connected to the GRID coords via a Mapping created by sc2ast and
*     containing the Mapping from pixel to focal plane arcsecond offsets.

*  Parameters:
*     subnum = sc2ast_subarray_t (Given)
*        Subarray index
*     ntime = int (Given)
*        The number of time values supplied in "times".
*     use_tlut = int (Given)
*        If true use an accurate LutMap for the time axis. If false
*        use an approximation via a WinMap. In most cases the WinMap
*        will be fine assuming the sequence steps are evenly spaced.
*     telpar = const SC2STORETelpar* (Given)
*        Additional telescope information needed to flesh out the frameset.
*     times = const double [] (Given)
*        An array of "ntime" MJD values (in the TAI timescale), one for
*        each pixel along the time axis.
*     status = int * (Given & Returned)
*        Inherited status.

*  Returned Value:
*     timeWcs = AstFrameSet *
*        3-D frameset.

*  Notes:

*  Authors:
*     DSB: David Berry (UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     10-MAR-2006 (DSB):
*        Initial version (untested)
*     01-JUN-2006 (TIMJ):
*        Integrated into specwriter.
*     19-DEC-2006 (TIMJ):
*        Integrated into SCUBA-2 software as variant of ACSIS DA version
*     29-OCT-2009 (DSB):
*        Modified to use atlAddWcsAxis.
*     30-OCT-2009 (DSB):
*        Take a deep copy of the FrameSet to avoid changing the cached
*        FrameSet.
*     2010-06-03 (TIMJ):
*        Add use_tlut parameter to indicate that a LutMap should be used
*        otherwise use a WinMap for speed.

*/

{
/* Local Variables: */
   AstFrameSet *fset;
   AstFrameSet *result;
   double origin = 0.0; /* reference time */
   AstMapping *timemap;
   AstTimeFrame *timefrm;

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if( *status != SAI__OK ) return result;
   if (!times) return result;

/* Start an AST context so we do not need to annul AST pointers explicitly. */
   astBegin;

/* Obtain a pointer to a frameset holding the 2D GRID Frame and a 2D Frame
   describing focal plane coordinates */
   sc2ast_createwcs( subnum, NULL, NULL, NULL, NO_FTS, &fset, status );

/* We are about to change the FrameSet, so take a deep copy. This is
   needed since the FrameSet returned by sc2ast_createwcs is held in a
   cache may be needed elsewhere. */
   result = astCopy( fset );

/* Now create a TimeFrame to describe MJD in the TAI timescale, and a LutMap
   which transforms grid coord into MJD (in days). The default TimeFrame
   attribute values give us what we want. Work out the duration of the
   observation to decide on formatting. To give AST some help with
   formatting axes we use a TimeOrigin. */
   timefrm = astTimeFrame( " " );

   /* If telpar is defined, add additional info */
   if (telpar) {
     astSetD(timefrm, "dut1", telpar->dut1);
     if (telpar->dtai != VAL__BADD) {
       astSetD(timefrm, "dtai", telpar->dtai);
     }
     astSetD(timefrm, "obslat", telpar->latdeg);
     astSetD(timefrm, "obslon", telpar->longdeg);

     /* Local time offset - assume the system running the acquisition
        is in the same time zone as the telescope running it :-) */
     astSetD(timefrm, "LToffset", sc2store_tzoffset() );

   }

   /* Calculate an origin for the time frame */
   origin = floor(times[0]);
   astSetD(timefrm, "TimeOrigin", origin);

   /* And set the formatting */
   /* We would like to use iso.0 for anything that is longer than 10 seconds (say)
      else use iso.3 because can not take spectra faster than 0.005 second. */
   astSet(timefrm, "format=iso.0");
   if ( ntime > 1 && (times[ntime-1] - times[0]) < (10.0 / SC2AST_SPD) ) {
     astSet(timefrm, "format=iso.3");
   }

   /* Now we need to create the mapping. Use a LutMap if we have been requested
      or if we only have one point. */
   if (use_tlut || ntime == 1) {
     double *ltimes = NULL; /* pointer to a time array */
     int malloced = 0;      /* did we malloc a ltimes array */
     double tcopy[2];       /* local copy of time lut for when only 1 number present */

     if (ntime == 1) {
       /* a LutMap needs two numbers in its mapping so double up the
          first time if we only have one value. */
       tcopy[0] = times[0] - origin;
       tcopy[1] = tcopy[0];
       ltimes = tcopy;
       ntime = 2;
     } else {
       int i;
       /* copy values and remove integer part of day */
       ltimes = malloc( sizeof(*ltimes) * ntime );
       if (ltimes) malloced = 1;
       origin = floor( times[0] );
       for (i = 0; i < ntime; i++ ) {
         ltimes[i] = times[i] - origin;
       }
     }
     timemap = (AstMapping*)astLutMap( ntime, ltimes, 1.0, 1.0, " " );

     if (malloced) free( ltimes );
   } else {
     /* Use a winmap for speed. Assume PIXEL coordinates go from 1 -> N
        and assume that we have a single sequence with no gaps. */
     double ina[1];
     double inb[1];
     double outa[1];
     double outb[1];

     ina[0] = 1;
     inb[0] = ntime;
     outa[0] = times[0] - origin;
     outb[0] = times[ntime-1] - origin;

     timemap = (AstMapping*)astWinMap( 1, ina, inb, outa, outb, " " );

   }

/* Now append the time axis to every Frame in the FrameSet, except for the base Frame,
   which receives an extra grid axis instead. */
   atlAddWcsAxis( result, (AstMapping *) timemap, (AstFrame *) timefrm,
                  NULL, NULL, status );

/* If no error has occurred, export the resulting FrameSet pointer
   from the current AST context so that it will not be annulled by the
   following call to astEnd. If an error has occurred, annul it explicitly,
   in order to ensure we are returning a NULL pointer. */
   if( *status == SAI__OK ) {
      astExport( result );
   } else {
      result = astAnnul( result );
   }

/* End the AST context. This annuls all AST Objects pointers created since
   the matching call to astBegin, except for any which have been exempted
   or exported. */
   astEnd;

/* Return the resulting FrameSet. */
   return result;

}


/* Routine to calculate the time zone offset in hours. */
static double sc2store_tzoffset( void ) {
  time_t now;
  double diff;
  struct tm gmt;
  time_t gmt_t;

  /* Portable POSIX compliant code. Convert epoch time to UTC
     then use mktime to convert it back to local time. Calculate
     the difference */
  now = time( NULL );
  (void)gmtime_r(&now, &gmt);
  gmt_t = mktime( &gmt );

  diff = difftime( now, gmt_t );
  /* need answer in hours for AST */
  return (diff/3600.0);
}

/* initialise ERR and NDF */

static void sc2store_initialise ( int * status ) {

  static const char *const argv[] = {
    APPNAME
  };

   if ( sc2store_initialised == 0 )
   {
      errBegin ( status );
      ndfInit ( 1, (char * const *)argv, status );
      sc2store_initialised = 1;
   }

}

/* Fill NDF bounds array with colsize and rowsize dimensionality:
   set dim3 to 0 if only have two dimensions */
static void sc2store_fillbounds( size_t colsize, size_t rowsize, size_t dim3,
                                 int lbnd[], int ubnd[], int * status ) {
  if (*status != SAI__OK) return;
  ubnd[SC2STORE__ROW_INDEX] = colsize + SC2STORE__BOL_LBND - 1;
  lbnd[SC2STORE__ROW_INDEX] = SC2STORE__BOL_LBND;
  ubnd[SC2STORE__COL_INDEX] = rowsize + SC2STORE__BOL_LBND - 1;
  lbnd[SC2STORE__COL_INDEX] = SC2STORE__BOL_LBND;
  if (dim3 > 0) {
    ubnd[2] = dim3;
    lbnd[2] = 1;
  }
}
