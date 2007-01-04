/* sc2store - routines for storing SCUBA-2 data to disk

   History :
    12Aug2004 : original (bdk)
    17Feb2005 : add sc2_heat (bdk)
    05Dec2005 : include star/hds_types.h (EC)
    05Dec2005 : remove star/hds_types.h, star/hds.h and extra ndf.h (bdk)
    23Jan2006 : replace star/hds.h (bdk)
    25Jul2006 : merge with ACSIS state structure usage (timj)
*/

#include <string.h>
#include <stdio.h>
#include <math.h>

#include "sae_par.h"
#include "prm_par.h"
#include "dat_par.h"
#include "star/hds.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "f77.h"
#include "Dits_Err.h"
#include "Ers.h"

#include "fitsio.h"

#include "dream_par.h"

#include "jcmt/state.h"
#include "sc2ast.h"
#include "sc2store_par.h"
#include "sc2store_sys.h"
#include "sc2store.h"

/* Private functions */
static AstFrameSet *timeWcs( int subnum, int ntime, const double times[], int * status );

/* Private globals */

static int sc2open = 0;            /* flag for file open */

static int dindf;                  /* NDF identifier for dark SQUID values */
static char errmess[132];          /* error string */
HDSLoc *fdataloc = NULL;           /* HDS locator to FLATDATA structure */
HDSLoc *fnameloc = NULL;           /* HDS locator to FLATNAME structure */
HDSLoc *fparloc = NULL;            /* HDS locator to FLATPAR structure */
HDSLoc *frameloc = NULL;           /* HDS locator to FRAMEDATA structure */
HDSLoc *jcmtstateloc = NULL;       /* HDS locator to JCMTSTATE structure */
HDSLoc *scu2redloc = NULL;         /* HDS locator to SCU2RED structure */
static int indf;                   /* main NDF identifier */
HDSLoc *sc2loc = NULL;             /* HDS locator to SCUBA2 structure */
static int sindf;                  /* NDF identifier for subtraction frame */
static int findf;                  /* NDF identifier for flat calibration */
static int zindf;                  /* NDF identifier for compression zero 
                                      offsets */
HDSLoc *drmloc = NULL;              /* HDS locator to DREAM parameters */
static int jigvndf;                 /* NDF identifier for jiggle vertices */
static int jigpndf;                 /* NDF identifier for the SMU path */

/*+ sc2store_compress - compress frame of integers to unsigned short */

void sc2store_compress 
( 
int nval,               /* number of values in frame (given) */
int stackz[],           /* stackzero frame to be subtracted (given) */
int digits[],           /* integer values (given and returned) */
int *bzero,             /* zero offset for compressed values (returned) */
unsigned short data[],  /* compressed values (returned) */
int *npix,              /* number of incompressible values (returned) */
int pixnum[],           /* indices of incompressible values (returned) */
int pixval[],           /* incompressible values (returned) */
int *status             /* global status (given and returned) */
)
/*  Description :
     Find the minimum value of the frame and use it as bzero. Turn each
     integer value into an unsigned short by subtracting bzero. If this
     is not possible, store the incompressible value separately and
     replace it by a bad value in the compressed array.

   Authors :
    B.D.Kelly (ROE)

   History :
    24Sep2004:  original (bdk)
*/
{
   int j;               /* loop counter */
   int temp;            /* intermediate compressed value */
   int bmax;

   if ( !StatusOkP(status) ) return;

/* Subtract the stackzero frame (ie approximate zero points for each
   bolometer) */

   for ( j=0; j<nval; j++ )
   {
      digits[j] -= stackz[j];
   }

   *bzero = digits[0];
   bmax = digits[0];
   for ( j=0; j<nval; j++ )
   {
      if ( *bzero > digits[j] ) (*bzero) = digits[j];
      if ( bmax <digits[j] ) bmax = digits[j];
   }

   *npix = 0;

   for ( j=0; j<nval; j++ )
   {
      temp = digits[j] - (*bzero);
      if ( temp >  VAL__MAXUW ) 
      {
         data[j] = VAL__BADUW;
         pixnum[*npix] = j;
         pixval[*npix] = digits[j];
         (*npix)++;
      }
      else
      {
         data[j] = (unsigned short) temp;
      }
   }
}

/*+ sc2store_credream - create DREAM extension in output file */

void sc2store_credream
(
int nvert,               /* Number of vertices in DREAM pattern (given)  */
int **jigvert,           /* Pointer to stored jiggle vertices (returned) */
int npath,               /* Number of points along SMU path in DREAM pattern 
			    (given) */
double **jigpath,        /* Pointer to stored jiggle path (returned) */
int *status              /* Global status (given and returned) */
)
/*
  History :
  08Aug2006 : original (agg)

*/
{
  int el;
  int lbnd[2];
  int ubnd[2];
  int place;

  /* Get new HDS locator for DREAM extension  */
  ndfXnew ( indf, "DREAM", "DREAM_PAR", 0, 0, &drmloc, status );
  if ( drmloc == NULL ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep("", "Unable to create NDF extension for DREAM", status);
    }
  }

  /* Create NDF for jigvert array */
  ndfPlace ( drmloc, "JIGVERT", &place, status );
  if ( place == NDF__NOPL ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep("", "Unable to create placeholder for DREAM jigvert array", status);
    }
  }
  lbnd[0] = 1;
  lbnd[1] = 1;
  ubnd[0] = nvert;
  ubnd[1] = 2;
  ndfNew ( "_INTEGER", 2, lbnd, ubnd, &place, &jigvndf, status );
  if ( jigvndf == NDF__NOID ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep("", "Unable to obtain an NDF identifier for the jigvert array", status);
    }
  }
  /* Map jigvert array so that it's ready to fill */
  ndfMap ( jigvndf, "DATA", "_INTEGER", "WRITE", (void *)jigvert, &el, 
	   status );
  if ( jigvert == NULL ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep("", "Unable to map DREAM jigvert array", status);
    }
  }

  /* Create NDF for jigpath array */
  ndfPlace ( drmloc, "JIGPATH", &place, status );
  if ( place == NDF__NOPL ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep("", "Unable to create placeholder for DREAM jigpath array", status);
    }
  }
  lbnd[0] = 1;
  lbnd[1] = 1;
  ubnd[0] = npath;
  ubnd[1] = 2;
  ndfNew ( "_DOUBLE", 2, lbnd, ubnd, &place, &jigpndf, status );
  if ( jigpndf == NDF__NOID ) {
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep("", "Unable to obtain an NDF identifier for the jigpath array", status);
    }
  }
  ndfMap ( jigpndf, "DATA", "_DOUBLE", "WRITE", (void *)jigpath, &el, 
	   status );
  if ( jigpath == NULL ) {
    if ( *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep("", "Unable to map DREAM jigpath array", status);
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
                scu2redloc (bdk)
*/
{

   if ( *status != SAI__OK ) return;

   ndfXnew ( indf, "SCU2RED", "SCUBA2_MAP_ARR", 0, 0, &scu2redloc, 
     status );

   sc2store_errconv ( status );
}


/*+ sc2store_cremap - create HDS container file and map data arrays */

void sc2store_cremap
(
char *filename,          /* name of HDS container file (given) */
int colsize,             /* number of pixels in a column (given) */
int rowsize,             /* number of pixels in a row (given) */
int nframes,             /* number of frames (given) */
int nflat,               /* number of flat coeffs per bol (given) */
char *flatname,          /* name of flatfield algorithm (given) */
int **bzero,             /* pointer to subtracted offset values (returned) */
unsigned short **data,   /* pointer to data array (returned) */
int **dksquid,           /* pointer to dark SQUID values (returned) */
int **stackz,            /* pointer to subtracted frame (returned) */
double **flatcal,        /* pointer to flat calibration (returned) */
double **flatpar,        /* pointer to flat parameters (returned) */
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
*/

{
   int el;                     /* number of elements mapped */
   static int initialised = 0; /* first-time flag */
   int lbnd[3];                /* lower dimension bounds */
   int place;                  /* NDF placeholder */
   int ubnd[3];                /* upper dimension bounds */

   if ( *status != SAI__OK ) return;


/* Initialise Starlink eror reporting NDF and start its context */

   if ( initialised == 0 )
   {
      errBegin ( status );
      ndfInit ( 0, 0, status );
      initialised = 1;
   }

   errMark();
   ndfBegin();

/* Create an HDS container file */

   ndfPlace ( NULL, filename, &place, status );

/* Create an NDF inside the container */

   ubnd[0] = colsize;
   lbnd[0] = 1;
   ubnd[1] = rowsize;
   lbnd[1] = 1;
   ubnd[2] = nframes;
   lbnd[2] = 1;

   ndfNew ( "_UWORD", 3, lbnd, ubnd, &place, &indf, status );
   ndfHcre ( indf, status );

/* Map the data array */

   ndfMap ( indf, "DATA", "_UWORD", "WRITE", (void *)data, &el, status );

/* Create extension for holding fixed-size data (subtracted constant and
   dark SQUID measurements) for each frame */

   ndfXnew ( indf, "FRAMEDATA", "SCUBA2_FM_PAR", 0, 0, &frameloc, status );

   ubnd[0] = nframes;
   lbnd[0] = 1;

/* Create and map compression zero offset for each frame */

   ndfPlace ( frameloc, "BZERO", &place, status );
   ndfNew ( "_INTEGER", 1, lbnd, ubnd, &place, &zindf, status );
   ndfHcre ( zindf, status );

   ndfMap ( zindf, "DATA", "_INTEGER", "WRITE", (void *)bzero, &el, 
     status );

/* Dark SQUID values for each frame */

   ubnd[0] = rowsize;
   lbnd[0] = 1;
   ubnd[1] = nframes;
   lbnd[1] = 1;

   ndfPlace ( frameloc, "DKSQUID", &place, status );
   ndfNew ( "_INTEGER", 2, lbnd, ubnd, &place, &dindf, status );
   ndfHcre ( dindf, status );

   ndfMap ( dindf, "DATA", "_INTEGER", "WRITE", (void *)dksquid, &el, 
     status );

/* Frame subtracted from each data frame before compression */

   ubnd[0] = colsize;
   lbnd[0] = 1;
   ubnd[1] = rowsize;
   lbnd[1] = 1;

   ndfPlace ( frameloc, "STACKZERO", &place, status );
   ndfNew ( "_INTEGER", 2, lbnd, ubnd, &place, &sindf, status );
   ndfHcre ( sindf, status );

   ndfMap ( sindf, "DATA", "_INTEGER", "WRITE", (void *)stackz, &el, 
     status );

/* NDF containing flat-field calibration */

   ubnd[0] = colsize;
   lbnd[0] = 1;
   ubnd[1] = rowsize;
   lbnd[1] = 1;
   ubnd[2] = nflat;
   lbnd[2] = 1;

   ndfPlace ( frameloc, "FLATCAL", &place, status );
   ndfNew ( "_DOUBLE", 3, lbnd, ubnd, &place, &findf, status );
   ndfHcre ( findf, status );

   ndfMap ( findf, "DATA", "_DOUBLE", "WRITE", (void *)flatcal, &el, 
     status );
   ndfXnew ( findf, "FLATDATA", "SCUBA2_FD_PAR", 0, 0, &fdataloc, status );

/* Name of flatfield correction technique */

   datNew ( fdataloc, "FLATNAME", "_CHAR*16", 0, 0, status );
   datFind ( fdataloc, "FLATNAME", &fnameloc, status );
   datPut0C ( fnameloc, flatname, status );

/* Parameter matching each flatfield coefficient */

   datNew ( fdataloc, "FLATPAR", "_DOUBLE", 1, &nflat, status );
   datFind ( fdataloc, "FLATPAR", &fparloc, status );

   datMap ( fparloc, "_DOUBLE", "WRITE", 1, &nflat, (void **)flatpar, 
     status );

/* Create storage for Header values for each frame - store in JCMTSTATE */

   ndfXnew( indf, JCMT__EXTNAME, JCMT__EXTTYPE, 0, 0, &jcmtstateloc, status );
   sc2store_headcremap ( jcmtstateloc, nframes, INST__SCUBA2, status );

/* Create the structured extension for each frame */

   ndfXnew ( indf, "SCUBA2", "SCUBA2_RTS_ARR", 1, &(nframes), &sc2loc, 
     status );

   if ( StatusOkP(status) )
   {
      sc2open = 1;
   }

   sc2store_errconv ( status );
}


/*+ sc2store_decompress - decompress frame of unsigned short to integers */

void sc2store_decompress 
( 
int nval,               /* number of values in frame (given) */
int stackz[],           /* stackzero frame to be added (given) */
int bzero,              /* zero offset for compressed values (given) */
unsigned short data[],  /* compressed values (given) */
int npix,               /* number of incompressible values (given) */
int pixnum[],           /* indices of incompressible values (given) */
int pixval[],           /* incompressible values (given) */
int digits[],           /* integer values (returned) */
int *status             /* global status (given and returned) */
)
/*  Description :
     Turn each unsigned short into an integer value by adding bzero and 
     the stackzero frame.
     Patch-in any incompressible values listed.

   Authors :
    B.D.Kelly (ROE)

   History :
    14Oct2004:  original (bdk)
    18Feb2005:  add stackz onto uncompressible values (bdk)
    13May2005:  check for bad values (bdk)
*/
{
   int j;               /* loop counter */

   if ( !StatusOkP(status) ) return;

/* Insert the stackzero frame (ie approximate zero points for each
   bolometer) and bzero */

   for ( j=0; j<nval; j++ )
   {
      if ( data[j] != VAL__BADUW )
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
      if (tstatus != SAI__OK) ErsRep ( 0, &tstatus, opstr );
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
    04Oct2005 : check sc2open flag (bdk)
    23Jan2006 : annul scu2redloc (bdk)
    20Jul2006 : annul DREAM extension (agg)
    18Dec2006 : release error context (timj)
*/

{
   int tstatus;      /* local status */

   if ( sc2open == 0 ) return;

   tstatus = SAI__OK;

/* Free the locator for the SCU2RED structure*/

   if ( scu2redloc != NULL )
   {
      datAnnul ( &scu2redloc, &tstatus );
   }

/* Free the locator for the SCUBA2 structure*/

   datAnnul ( &sc2loc, &tstatus );

/* Release DREAM parameters */
   if ( drmloc != NULL ) {
     ndfUnmap ( jigvndf, "DATA", &tstatus );
     ndfAnnul ( &jigvndf, &tstatus );
     ndfUnmap ( jigpndf, "DATA", &tstatus );
     ndfAnnul ( &jigpndf, &tstatus );
     datAnnul ( &drmloc, &tstatus );
   }

/* Release Header values for each frame */

   sc2store_headunmap ( &tstatus );

/* Release Dark SQUID values for each frame */

   ndfUnmap ( dindf, "DATA", &tstatus );
   ndfAnnul ( &dindf, &tstatus );

/* Release compression zero offset for each frame */

   ndfUnmap ( zindf, "DATA", &tstatus );
   ndfAnnul ( &zindf, &tstatus );

/* Release subtracted (stackzero) frame */

   ndfUnmap ( sindf, "DATA", &tstatus );
   ndfAnnul ( &sindf, &tstatus );

/* Release flatfield calibration */

   datUnmap ( fparloc, &tstatus );
   datAnnul ( &fparloc, &tstatus );
   datAnnul ( &fnameloc, &tstatus );
   datAnnul ( &fdataloc, &tstatus );
   ndfUnmap ( findf, "DATA", &tstatus );
   ndfAnnul ( &findf, &tstatus );

/* Release FRAMEDATA and JCMTSTATE structure */

   datAnnul ( &frameloc, &tstatus );
   datAnnul ( &jcmtstateloc, &tstatus );

/* Unmap the main data array */

   ndfUnmap ( indf, "DATA", &tstatus );
   ndfAnnul ( &indf, &tstatus );

/* Close the NDF context */

   ndfEnd ( &tstatus );
   errRlse();

   sc2open = 0;

   sc2store_errconv ( &tstatus );

}



/*+ sc2store_getincomp - get details of incompressible pixels */

void sc2store_getincomp
(
int frame,         /* frame index (given) */
int *npix,         /* number of incompressible pixels (returned) */
int pixnum[],      /* indices of incompressible pixels (returned) */
int pixval[],      /* values of incompressible pixels (returned) */
int *status        /* global status (given and returned) */
)
/*
   History :
    12Aug2004 : original (bdk)
    23Nov2005 : Use Official C HDS interface (TIMJ)
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
   datCell ( sc2loc, 1, &strnum, &loc2, status );
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
JCMTState *head,         /* header data for the frame (returned) */
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
#define RETRIEVE_STATE(state, index, type, bad )	\
   if ( sc2store_ptr[index] ) { \
     head->state = ((type *)sc2store_ptr[index])[frame]; \
   } else { \
     head->state = bad; \
   }

   RETRIEVE_STATE( rts_num, RTS_NUM, unsigned int, (unsigned int)VAL__BADI );
   RETRIEVE_STATE( rts_end, RTS_END, double, VAL__BADD );

   RETRIEVE_STATE( smu_x, SMU_X, double, VAL__BADD );
   RETRIEVE_STATE( smu_y, SMU_Y, double, VAL__BADD );
   RETRIEVE_STATE( smu_z, SMU_Z, double, VAL__BADD );
   RETRIEVE_STATE( smu_jig_index, SMU_JIG_INDEX, int, VAL__BADI );
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
   RETRIEVE_STATE( tcs_index,  TCS_INDEX, int, VAL__BADI );
   RETRIEVE_STATE( tcs_tr_ang, TCS_TR_ANG, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_ac1, TCS_TR_AC1, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_ac2, TCS_TR_AC2, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_dc1, TCS_TR_DC1, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_dc2, TCS_TR_DC2, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_bc1, TCS_TR_BC1, double, VAL__BADD );
   RETRIEVE_STATE( tcs_tr_bc2, TCS_TR_BC2, double, VAL__BADD );

   RETRIEVE_STATE( jos_drcontrol,  JOS_DRCONTROL, int, VAL__BADI );

   RETRIEVE_STATE( enviro_rel_hum, ENVIRO_REL_HUM, float, VAL__BADR );
   RETRIEVE_STATE( enviro_pressure, ENVIRO_PRESSURE, float, VAL__BADR );
   RETRIEVE_STATE( enviro_air_temp, ENVIRO_AIR_TEMP, float, VAL__BADR );

   RETRIEVE_STATE( wvm_th, WVM_TH, float, VAL__BADR );
   RETRIEVE_STATE( wvm_t12, WVM_T12, float, VAL__BADR );
   RETRIEVE_STATE( wvm_t42, WVM_T42, float, VAL__BADR );
   RETRIEVE_STATE( wvm_t78, WVM_T78, float, VAL__BADR );
   RETRIEVE_STATE( wvm_tw, WVM_TW, float, VAL__BADR );
   RETRIEVE_STATE( wvm_qual, WVM_QUAL, int, VAL__BADI );
   RETRIEVE_STATE( wvm_time, WVM_TIME, float, VAL__BADR );

   RETRIEVE_STATE( sc2_heat, SC2_HEAT, double, VAL__BADD );

   RETRIEVE_STATE( acs_exposure, ACS_EXPOSURE, float, VAL__BADR );
   RETRIEVE_STATE( acs_no_prev_ref, ACS_NO_PREV_REF, int, VAL__BADI );
   RETRIEVE_STATE( acs_no_next_ref, ACS_NO_NEXT_REF, int, VAL__BADI );
   RETRIEVE_STATE( acs_no_ons, ACS_NO_ONS, int, VAL__BADI );

   RETRIEVE_STATE( pol_ang, POL_ANG, double, VAL__BADD );
   RETRIEVE_STATE( fts_pos, FTS_POS, float, VAL__BADR );

   /* Now do the character arrays. We use cnfExprt to allow nul termination. Note that
      the struct does allocate a space for a nul in addition to the HDS component size.
    */

#define RETRIEVE_CHAR( state, index, len ) \
   if ( sc2store_ptr[index] ) { \
     cnfImprt( (char*)sc2store_ptr[index]+len*frame, len, head->state ); \
     (head->state)[len] = '\0'; \
   } else { \
     (head->state)[0] = '\0'; \
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
HDSLoc *headloc,              /* HDS locator (given) */
int nframes,                  /* number of frames to be created (given) */
inst_t instrument,            /* instrument code (given) */
int *status                   /* global status (given and returned) */
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
     if ( hdsRecords[j].instrument & instrument ) {
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
       if (strncmp( "_CHAR", hdsRecords[j].type, 5) == 0) {
	 datLen( sc2store_loc[pos], &len, status );
	 if (*status == SAI__OK) memset( sc2store_ptr[pos], ' ', dim[0] * len );
       }
     }

      if ( *status != SAI__OK ) break;

   }

}



/*+ sc2store_headput - put values into the header arrays */

void sc2store_headput
(
int frame,                    /* frame index (given) */
JCMTState head,          /* header data for the frame (given) */
int *status                   /* global status (given and returned) */
)
/* Method :
    The given structure contains values for all the header elements of
    the frame. These are copied into the relevant place in the mapped HDS
    arrays.
   History :
    18Aug2004 : original (bdk)
    17Feb2005 : add sc2_heat (bdk)
    27Jul2006 : use JCMTState (timj)
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
   STORE_STATE( smu_x, SMU_X, double );
   STORE_STATE( smu_y, SMU_Y, double );
   STORE_STATE( smu_z, SMU_Z, double );
   STORE_CHAR( smu_chop_phase, SMU_CHOP_PHASE, JCMT__SZSMU_CHOP_PHASE );
   STORE_STATE( smu_jig_index, SMU_JIG_INDEX, int );

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
   STORE_STATE( tcs_index, TCS_INDEX, int );
   STORE_CHAR( tcs_source, TCS_SOURCE, JCMT__SZTCS_SOURCE );
   STORE_CHAR( tcs_tr_sys, TCS_TR_SYS, JCMT__SZTCS_TR_SYS );
   STORE_STATE( tcs_tr_ang, TCS_TR_ANG, double );
   STORE_STATE( tcs_tr_ac1, TCS_TR_AC1, double );
   STORE_STATE( tcs_tr_ac2, TCS_TR_AC2, double );
   STORE_STATE( tcs_tr_dc1, TCS_TR_DC1, double );
   STORE_STATE( tcs_tr_dc2, TCS_TR_DC2, double );
   STORE_STATE( tcs_tr_bc1, TCS_TR_BC1, double );
   STORE_STATE( tcs_tr_bc2, TCS_TR_BC2, double );

   /* JOS control */
   STORE_STATE( jos_drcontrol, JOS_DRCONTROL, int );

   /* Water Vapour Monitor */
   STORE_STATE( wvm_th, WVM_TH, float );
   STORE_STATE( wvm_t12, WVM_T12, float );
   STORE_STATE( wvm_t42, WVM_T42, float );
   STORE_STATE( wvm_t78, WVM_T78, float );
   STORE_STATE( wvm_tw, WVM_TW, float );
   STORE_STATE( wvm_qual, WVM_QUAL, int );
   STORE_STATE( wvm_time, WVM_TIME, float );

   /* SCUBA-2 specific */
   STORE_STATE( sc2_heat, SC2_HEAT, double );

   /* FTS and polarimeter */
   STORE_STATE( fts_pos, FTS_POS, float );
   STORE_STATE( pol_ang, POL_ANG, double );

#undef STORE_STATE
#undef STORE_CHAR
}



/*+ sc2store_headrmap - map the header arrays for read access */

void sc2store_headrmap
(
HDSLoc *headloc,              /* HDS locator (given) */
int nframes,                  /* number of frames expected (given) */
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
*/
{
   int dim[2];
   int j;
   int ndim;
   int pos;
   int isthere = 0;

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

     /* See if the component exists (it may be an ACSIS or SCUBA-2
	file or whatever) */
     datThere( headloc, hdsRecords[j].name, &isthere, status );
     if (isthere) {
       datFind ( headloc, hdsRecords[j].name, &(sc2store_loc[pos]), 
		 status );

       if ( *status != SAI__OK ) break;
       datMap ( sc2store_loc[pos], hdsRecords[j].type, "READ", 
		ndim, dim, &(sc2store_ptr[pos]), status );
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
	   sprintf( errmess, "Mandatory component not present in file. Can't find '%s'",
		    hdsRecords[j].name);
	   ErsRep( 0, status, errmess );
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
     if (sc2store_loc[j] != NULL) {
       datUnmap ( sc2store_loc[j], status );
       sc2store_ptr[j] = NULL;
       datAnnul ( &(sc2store_loc[j]), status );
     }
   }
}

/*+ sc2store_ndfreadscan - read a single scan from an NDF file */

void sc2store_ndfreadscan
(
char *filename,     /* name of input map file (given) */
char *access,       /* "READ" or "UPDATE" access to data file (given) */
int flatlen,        /* length of string for flatname (given) */
int *nframes,       /* number of frames in scan (returned) */
double **xz,        /* X centre for each frame (returned) */
double **yz,        /* Y centre for each frame (returned) */
double **inptr,     /* measurement values (returned) */
int *nflat,         /* number of flatfield coeffs per pixel (returned) */
char *flatname,     /* name of flatfield algorithm (returned) */
double **flatcal,   /* flatfield calibration (returned) */
double **flatpar,   /* flatfield parameters (returned) */
int **jigvert,      /* pointer to DREAM jiggle vertices (returned) */
int *nvert,         /* Number of vertices in jiggle pattern (returned) */
double **jigpath,   /* pointer to path of SMU over jiggle pattern (returned) */
int *npath,         /* Number of points in SMU path (returned) */
int *status         /* global status (given and returned) */
)

/* Description :
    Open and read an NDF containing scan data. Copy the data to be
    returned, including a double version of the bolometer values.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    15Oct2004 : original (bdk)
    25Feb2005 : keep the file open (bdk)
    20May2005 : add flatcal (bdk)
    12Jul2005 : return COPY of flatfield data (bdk)
    05Oct2005 : use new data interface (bdk)
    25Jan2006 : add access argument to sc2store_rdtstream (bdk)
    26Jan2006 : pass-in access argument to sc2store_rdtstream (bdk)
    13Mar2006 : copied over from map.c (agg)
    14Mar2006 : change FHEAD__MXREC to SC2STORE__MAXFITS (agg)
    21Aug2006 : update API to reflect change to rststream (agg)
*/

{
   int colsize;            /* number of pixels in column */
   int *dksquid;           /* pointer to dark SQUID data */
   double *fcal;           /* pointer to flat field data */
   static char fitsrec[SC2STORE__MAXFITS][81];  /* store for FITS records */
   double *fpar;           /* pointer to flat field parameters */
   JCMTState *frhead; /* structure for headers for a frame */
   int j;                  /* loop counter */
   int nfits;              /* number of FITS records */
   int rowsize;            /* number of pixels in row */
   int *tmptr;             /* pointer to decompressed map data */

   if ( !StatusOkP(status) ) return;


/* Read the TSTREAM data */

   sc2store_rdtstream ( filename, access, flatlen, 81, SC2STORE__MAXFITS, &nfits,
     fitsrec, &colsize, &rowsize, nframes, nflat, flatname, &frhead,
			&tmptr, &dksquid, &fcal, &fpar, jigvert, nvert, jigpath, npath, status );

/* Map space for the data to be copied */

   *xz = (double *) calloc ( *nframes, sizeof(double) );
   *yz = (double *) calloc ( *nframes, sizeof(double) );
   *inptr = (double *) calloc ( (*nframes)*colsize*rowsize, sizeof(double) );
   *flatcal = (double *) calloc ( (*nflat)*colsize*rowsize, sizeof(double) );
   *flatpar = (double *) calloc ( (*nflat), sizeof(double) );

/* Copy flat field data */

   for ( j=0; j<(*nflat)*colsize*rowsize; j++ )
   {
      (*flatcal)[j] = fcal[j];
   }

   for ( j=0; j<(*nflat); j++ )
   {
      (*flatpar)[j] = fpar[j];
   }

/* Extract the per-frame headers */

   for ( j=0; j<*nframes; j++ )
   {
      (*xz)[j] = frhead[j].tcs_tr_ac1;      
      (*yz)[j] = frhead[j].tcs_tr_ac2;      
   }

/* Convert the data to double */

   for ( j=0; j<(*nframes)*colsize*rowsize; j++ )
   {
      (*inptr)[j] = (double)tmptr[j];
   }


}

/*+ sc2store_putimage - store constructed image */

void sc2store_putimage
(
int frame,         /* frame index (given) */
AstFrameSet *fset, /* World coordinate transformations (given) */
int ndim,          /* dimensionality of image (given) */
int dims[],        /* dimensions of image (given) */
int seqstart,      /* first sequence number used in image (given) */
int seqend,        /* last sequence number used in image (given) */
int nbolx,         /* number of bolometers in X (given) */
int nboly,         /* number of bolometers in Y (given) */
double *image,     /* constructed image (given) */
double *zero,      /* bolometer zero values (given) */
char fitshd[41][81], /* string array of FITS header keywords to write (given) */
int nfits,         /* Number of FITS headers */
int *status        /* global status (given and returned) */
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
*/
{

   HDSLoc *bz_imloc = NULL;/* HDS locator */
   int bzindf;             /* NDF identifier */
   double *bzptr;          /* pointer to mapped space for zero points */
   int el;                 /* number of elements mapped */
   char imname[DAT__SZNAM];/* name of structure for image */
   double *imptr;          /* pointer to mapped space for image */
   int j;                  /* loop counter */
   int lbnd[7];            /* lower dimension bounds */
   int ntot;               /* total number of elements */
   int place;              /* NDF placeholder */
   HDSLoc *seq_loc = NULL; /* HDS locator */
   int strnum;             /* structure element number */
   int uindf;              /* NDF identifier */
   int ubnd[7];            /* upper dimension bounds */
   HDSLoc *fitsloc = NULL; /* HDS locator to FITS headers */
   HDSLoc *loc2 = NULL;    /* HDS locator for FITS */

   if ( *status != SAI__OK ) return;

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
   ndfPlace ( scu2redloc, imname, &place, status );
   ndfNew ( "_DOUBLE", ndim, lbnd, ubnd, &place, &uindf, status );
   ndfHcre ( uindf, status );

/* Map the data array */

   ndfMap ( uindf, "DATA", "_DOUBLE", "WRITE", (void *)&imptr, &el, 
     status );

/* Copy image array */

   if ( *status == SAI__OK )
   {
      for ( j=0; j<ntot; j++ )
      {
         imptr[j] = image[j];
      }
   }

/* Store world coordinate transformations */

   ndfPtwcs ( fset, uindf, status );

/* Store start and end sequence numbers in the extension */

   ndfXnew ( uindf, "MAPDATA", "SEQUENCE_RANGE", 0, 0, &seq_loc, status );
   ndfXpt0i ( seqstart, uindf, "MAPDATA", "SEQSTART", status );
   ndfXpt0i ( seqend, uindf, "MAPDATA", "SEQEND", status );

/* Store the bolometer zero points as an NDF in the extension */

   ndfXnew ( uindf, "BZ_IMAGE", "SCUBA2_ZER_ARR", 0, 0, &bz_imloc, 
     status );
   ndfPlace ( bz_imloc, "ZERO", &place, status );

/* Create the array for bolometer zeros */

   ubnd[0] = nbolx;
   lbnd[0] = 1;
   ubnd[1] = nboly;
   lbnd[1] = 1;
   ndfNew ( "_DOUBLE", 2, lbnd, ubnd, &place, &bzindf, status );
   ndfHcre ( bzindf, status );

/* Map the data array */

   ndfMap ( bzindf, "DATA", "_DOUBLE", "WRITE", (void *)&bzptr, &el, 
     status );

/* Copy image array */

   if ( *status == SAI__OK )
   {
      for ( j=0; j<nbolx*nboly; j++ )
      {
         bzptr[j] = zero[j];
      }
   }

/* Store the FITS headers */

   if ( nfits > 0 )
   {
      ndfXnew ( uindf, "FITS", "_CHAR*80", 1, &(nfits), &fitsloc, status );

      for ( j=1; j<=nfits; j++ )
      {
         datCell ( fitsloc, 1, &j, &loc2, status );
         datPut0C ( loc2, fitshd[j-1], status );
         datAnnul ( &loc2, status );
      }
      datAnnul ( &fitsloc, status );
   }

/* Unmap the data array */

   ndfUnmap ( bzindf, "DATA", status );
   ndfAnnul ( &bzindf, status );


/* Unmap the data array */

   ndfUnmap ( uindf, "DATA", status );
   ndfAnnul ( &uindf, status );

/* Free the locators for the frame */

   datAnnul ( &seq_loc, status );
   datAnnul ( &bz_imloc, status );


   sc2store_errconv ( status );

}




/*+ sc2store_putincomp - store details of incompressible pixels */

void sc2store_putincomp
(
int frame,         /* frame index (given) */
int npix,          /* number of incompressible pixels (given) */
int pixnum[],      /* indices of incompressible pixels (given) */
int pixval[],      /* values of incompressible pixels (given) */
int *status        /* global status (given and returned) */
)
/*
   History :
    12Aug2004 : original (bdk)
    19Jun2005 : use ndfHcre to create history component on all NDFs (bdk)
    23Nov2005 : Use Official C HDS interface (TIMJ)
    18Dec2006 : Remove some error checks (TIMJ)
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
   datCell ( sc2loc, 1, &strnum, &loc2, status );

   ubnd[0] = 2;
   lbnd[0] = 1;
   ubnd[1] = npix;
   lbnd[1] = 1;
   ndfPlace ( loc2, "INCOMP", &place, status );
   ndfNew ( "_INTEGER", 2, lbnd, ubnd, &place, &uindf, status );
   ndfHcre ( uindf, status );

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
int nbolx,         /* number of bolometers in X (given) */
int nboly,         /* number of bolometers in Y (given) */
int ncoeff,        /* number of coefficients (given) */
double *coptr,     /* coefficients (given) */
int *status        /* global status (given and returned) */
)
/* Method :
    Create an NDF with image name "SCANFIT", under DA_IMAGE. Store the
    coefficients as the main array in the NDF.
   History :
    25Feb2005 : original (bdk)
    19Jun2005 : use ndfHcre to create history component on all NDFs (bdk)
    26Jan2006 : change component name to SCANFIT (bdk)
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
      ubnd[0] = nbolx;
      lbnd[0] = 1;
      ubnd[1] = nboly;
      lbnd[1] = 1;
      ubnd[2] = ncoeff;
      lbnd[2] = 1;

      ndfPlace ( scu2redloc, "SCANFIT", &place, status );
      ndfNew ( "_DOUBLE", 3, lbnd, ubnd, &place, &uindf, status );
      ndfHcre ( uindf, status );

/* Map the data array */

      ndfMap ( uindf, "DATA", "_DOUBLE", "WRITE", (void *)&imptr, &el, 
        status );

/* Copy coefficients array */

      if ( *status == SAI__OK )
      {
         for ( j=0; j<nbolx*nboly*ncoeff; j++ )
         {
            imptr[j] = coptr[j];
         }
      }

/* Unmap the data array */

      ndfUnmap ( uindf, "DATA", status );
      ndfAnnul ( &uindf, status );

   }

   sc2store_errconv ( status );

}




/*+ sc2store_rdfitshead - read the FITS headers */

void sc2store_rdfitshead
(
int maxlen,           /* maximum length of FITS header (given) */
int maxfits,          /* maximum number of header items (given) */
int *nfits,           /* number of header items (returned) */
char headers[][81],   /* array of FITS headers (returned) */
int *status           /* global status (given and returned) */
)
/*
   History :
    12Aug2004 : original (bdk)
    23Nov2005 : Use Official C HDS interface (TIMJ)
*/
{
   int dim[1];                /* number of FITS entries */
   HDSLoc *fitsloc = NULL;    /* HDS locator to FITS headers */
   int j;                     /* loop counter */
   HDSLoc *loc2 = NULL;       /* HDS locator */
   int ndim;                  /* number of dimensions in query */
   int ndimx;                 /* maximum number of dimensions in query */

   if ( *status != SAI__OK ) return;

/* Locate the FITS headers */

   ndimx = 1;
   ndfXloc ( indf, "FITS", "READ", &fitsloc, status );
   datShape ( fitsloc, ndimx, dim, &ndim, status );

   if ( *status == SAI__OK )
   {
      *nfits = dim[0];
      if ( *nfits > maxfits )
      {
         *nfits = maxfits;
      }

      for ( j=1; j<=*nfits; j++ )
      {
	datCell ( fitsloc, 1, &j, &loc2, status );
	datGet0C ( loc2, headers[j-1], maxlen, status );
	datAnnul ( &loc2, status );
      }
   }

   datAnnul ( &fitsloc, status );

   sc2store_errconv ( status );
}




/*+ sc2store_rdflatcal - read SCUBA-2 flatfield calibration */

void sc2store_rdflatcal
(
char *filename,          /* name of HDS container file (given) */
int flatlen,             /* length of space for flatfield name (given) */
int *colsize,            /* number of pixels in column (returned) */
int *rowsize,            /* number of pixels in row (returned) */
int *nflat,              /* number of flat coeffs per bol (returned) */
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
    07Dec2005 : set sc2open flag when open is successful (bdk)
    25Jan2006 : add access argument to sc2store_rdmap (bdk)
    08Aug2006 : update call to sc2store_rdmap (agg)
*/

{
   int *bzero;                /* pointer to subtracted offset values */
   unsigned short *data;      /* pointer to data array */
   int *dksquid;              /* pointer to dark SQUID values */
   double *fcal;              /* mapped flatfield calibration */
   double *fpar;              /* mapped flatfield parameters */
   int nbol;                  /* number of bolometers */
   int nframes;               /* number of data frames */
   int *stackz;               /* pointer to stackzero frame */
   int *jigvert = NULL;       /* pointer to DREAM jiggle vertices */
   double *jigpath = NULL;    /* pointer to path of SMU over jiggle pattern */
   int nvert;                 /* Number of vertices in jiggle pattern */
   int npath;                 /* Number of samples over jiggle pattern */

   if ( !StatusOkP(status) ) return;

   if ( sc2open != 0 )
   {
      *status = DITS__APP_ERROR;
      sprintf ( errmess, 
        "one SCUBA-2 data file already open, can't open %s", filename );
      ErsRep ( 0, status, errmess );
      return;
   }


/* Map all the data arrays */

   sc2store_rdmap ( filename, "READ", flatlen, colsize, rowsize, &nframes, 
		    nflat, flatname, &bzero, &data, &dksquid, &stackz, &fcal, &fpar,
		    &jigvert, &nvert, &jigpath, &npath, status );

   if ( !StatusOkP(status) ) 
   {
      sprintf ( errmess, "failed to open %s", filename );
      ErsRep ( 0, status, errmess );
   }
   else
   {

/* Create copies of the calibration arrays */

      nbol = (*rowsize) * (*colsize);
      *flatcal = calloc ( nbol*(*nflat), sizeof(double) );
      memcpy ( *flatcal, fcal, nbol*(*nflat)*sizeof(double) );
      *flatpar = calloc ( *nflat, sizeof(double) );
      memcpy ( *flatpar, fpar, (*nflat)*sizeof(double) );
   }

   if ( StatusOkP(status) )
   {
      sc2open = 1;
   }

   sc2store_free ( status );

}



/*+ sc2store_rdmap - open an existing HDS container file and map data arrays */

void sc2store_rdmap
(
char *filename,          /* name of HDS container file (given) */
char *access,            /* "READ" or "UPDATE" access (given) */
int flatlen,             /* length of space for flatfield name (given) */
int *colsize,            /* number of pixels in column (returned) */
int *rowsize,            /* number of pixels in row (returned) */
int *nframes,            /* number of frames (returned) */
int *nflat,              /* number of flat coeffs per bol (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
int **bzero,             /* pointer to subtracted offset values (returned) */
unsigned short **data,   /* pointer to data array (returned) */
int **dksquid,           /* pointer to dark SQUID values (returned) */
int **stackz,            /* pointer to subtracted frame (returned) */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int **jigvert,           /* pointer to DREAM jiggle vertices (returned) */
int *nvert,              /* Number of vertices in jiggle pattern (returned) */
double **jigpath,        /* pointer to path of SMU over jiggle pattern (returned) */
int *npath,              /* Number of points in SMU path (returned) */
int *status              /* global status (given and returned) */
)
/*
   Method :
    Map an existing SCUBA-2 data file for read access to the raw data
   History :
    12Aug2004 : original (bdk)
    23Mar2005 : swap order of nrow and ncol in NDF (bdk)
    20Apr2005 : swap order of nrow and ncol in arguments (bdk)
    20May2005 : add flatcal (bdk)
    30Sep2005 : change names to colsize and rowsize, add nflat, flatname,
                flatpar (bdk)
    25Jan2006 : add access argument (bdk)
    27Jul2006 : use JCMTSTATE extension (timj)
    21Aug2006 : open and read DREAM parameters if present (agg)
*/
{
   int dim[3];             /* dimensions */
   int el;                 /* number of elements mapped */
   static int initialised = 0; /* first-time flag */
   int isthere = 0;        /* is an extension present? */
   int ndim;               /* number of dimensions */
   int ndimx;              /* max number of dimensions */
   int nfdim;              /* number of flatpar dimensions */
   int place;              /* NDF placeholder */


   if ( *status != SAI__OK ) return;

/* Initialise Starlink eror reporting NDF and start its context */

   if ( initialised == 0 )
   {
      errBegin ( status );
      ndfInit ( 0, 0, status );
      initialised = 1;
   }

   errMark();
   ndfBegin();

/* Open an HDS container file */

   ndimx = 3;
   ndfOpen ( NULL, filename, access, "OLD", &indf, &place, status );
   ndfDim ( indf, ndimx, dim, &ndim, status );
   *colsize = dim[0];
   *rowsize = dim[1];
   *nframes = dim[2];

/* Map the data array */

   ndfMap ( indf, "DATA", "_UWORD", "READ", (void *)data, &el, status );

/* Find extension for holding fixed-size data (subtracted constant and
   dark SQUID measurements) for each frame */

   ndfXloc ( indf, "FRAMEDATA", "READ", &frameloc, status );

/* map compression zero offset for each frame */

   ndfOpen ( frameloc, "BZERO", "READ", "OLD", &zindf, &place, status );

   ndfMap ( zindf, "DATA", "_INTEGER", "READ", (void *)bzero, &el, 
     status );

/* Dark SQUID values for each frame */

   ndfOpen ( frameloc, "DKSQUID", "READ", "OLD", &dindf, &place, status );

   ndfMap ( dindf, "DATA", "_INTEGER", "READ", (void *)dksquid, &el, 
     status );

/* STACKZERO subtracted frame */

   ndfOpen ( frameloc, "STACKZERO", "READ", "OLD", &sindf, &place, status );

   ndfMap ( sindf, "DATA", "_INTEGER", "READ", (void *)stackz, &el, 
     status );

/* FLATCAL flatfield calibration */

   ndfOpen ( frameloc, "FLATCAL", access, "OLD", &findf, &place, status );

   ndfMap ( findf, "DATA", "_DOUBLE", access, (void *)flatcal, &el, 
     status );

   ndfXloc ( findf, "FLATDATA", "READ", &fdataloc, status );
   datFind ( fdataloc, "FLATNAME", &fnameloc, status );
   datGet0C ( fnameloc, flatname, flatlen, status );
   datFind ( fdataloc, "FLATPAR", &fparloc, status );
   datShape ( fparloc, 1, nflat, &nfdim, status );
   datMap ( fparloc, "_DOUBLE", "READ", 1, nflat, (void**)flatpar, 
     status );

   /* DREAM jiggle parameters */
   ndfXstat( indf, "DREAM", &isthere, status );
   if ( isthere ) {
     ndfXloc( indf, "DREAM", "READ", &drmloc, status );
     ndfOpen ( drmloc, "JIGVERT", access, "OLD", &jigvndf, &place, status );
     ndfMap ( jigvndf, "DATA", "_INTEGER", access, (void *)jigvert, &el,
	      status );
     *nvert = el/2;
     ndfOpen ( drmloc, "JIGPATH", access, "OLD", &jigpndf, &place, status );
     ndfMap ( jigpndf, "DATA", "_DOUBLE", access, (void *)jigpath, &el,
	      status );
     /* Remember npath = nsampcycle in SMURF = cycle_samples in other places */
     *npath = el/2;
   } else {
     /* Return NULL pointers if we don't have DREAM data */
     jigvert = NULL;
     jigpath = NULL;
     nvert = NULL;
     npath = NULL; 
   }
   /* Reset the isthere flag */
   isthere = 0;

/* storage for Header values for each frame - to import old frames we fallback
   to FRAMEDATA if JCMT__EXTNAME is not present.
 */
   ndfXstat( indf, JCMT__EXTNAME, &isthere, status );
   if (isthere) {
     ndfXloc( indf, JCMT__EXTNAME, "READ", &jcmtstateloc, status );
   } else {
     /* needs to be cloned since they are annulled separately */
     datClone( frameloc, &jcmtstateloc, status );
   }
   sc2store_headrmap ( jcmtstateloc, *nframes, INST__SCUBA2, status );

/* structured extension for each frame */

   ndfXloc ( indf, "SCUBA2", "READ", &sc2loc, status );

   sc2store_errconv ( status );
}



/*+ sc2store_rdtstream - read SCUBA-2 time stream data from an NDF */

void sc2store_rdtstream
(
char *filename,          /* name of HDS container file (given) */
char *access,            /* "READ" or "UPDATE" access (given) */
int flatlen,             /* length of space for flatfield name (given) */
int maxlen,              /* max length of FITS header (given) */
int maxfits,             /* max number of FITS headers (given) */
int *nfits,              /* acual number of FITS headers (returned) */
char fitshead[][81],     /* FITS header records (returned) */
int *colsize,            /* number of pixels in column (returned) */
int *rowsize,            /* number of pixels in row (returned) */
int *nframes,            /* number of frames (returned) */
int *nflat,              /* number of flat coeffs per bol (returned) */
char *flatname,          /* name of flatfield algorithm (returned) */
JCMTState *frhead[],     /* header data for each frame (returned) */
int **outdata,           /* pointer to data array (returned) */
int **dksquid,           /* pointer to dark SQUID values (returned) */
double **flatcal,        /* pointer to flatfield calibration (returned) */
double **flatpar,        /* pointer to flatfield parameters (returned) */
int **jigvert,           /* pointer to DREAM jiggle vertices (returned) */
int *nvert,              /* Number of vertices in jiggle pattern (returned) */
double **jigpath,        /* pointer to path of SMU over jiggle pattern (returned) */
int *npath,              /* Number of points in SMU path (returned) */
int *status              /* global status (given and returned) */
)

/* Description :
    Return the time stream data, associated headers and flatfield calibration
    from an NDF. The file is left open to allow updating of associated items 
    such as the flatfield or other processed data, in which case access 
    should be specified as "UPDATE".
   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    01Oct2005 : original (bdk)
    04Oct2005 : check if sc2store already has an open file (bdk)
    08Dec2005 : map space AFTER checking status (bdk)
    25Jan2006 : add access argument (bdk)
    21Aug2006 : update API to return DREAM parameters (agg)
*/

{
   int *bzero;             /* pointer to subtracted offset values */
   unsigned short *data;   /* pointer to data array */
   int j;                  /* loop counter */
   int nbol;               /* number of bolometers */
   int npix;               /* number of incompressible pixels */
   static int pixnum[DREAM__MXBOL];       /* indices of incompressible pixels */
   static int pixval[DREAM__MXBOL];       /* values of incompressible pixels */
   int *stackz;                     /* pointer to stackzero frame */

   if ( !StatusOkP(status) ) return;

   if ( sc2open != 0 )
   {
      *status = DITS__APP_ERROR;
      sprintf ( errmess,
        "one SCUBA-2 data file already open, can't open %s", filename );
      ErsRep ( 0, status, errmess );
      return;
   }

   if ( ( strcmp ( access, "READ" ) != 0 ) && 
	( strcmp ( access, "UPDATE" ) != 0 ) )
   {
      *status = DITS__APP_ERROR;
      sprintf ( errmess, 
        "access mode should be READ or UPDATE, but give as %s", access );
      ErsRep ( 0, status, errmess );
      return;
   }
   
   
/* Map all the data arrays */

   sc2store_rdmap ( filename, access, flatlen, colsize, rowsize, nframes, 
		    nflat, flatname, &bzero, &data, dksquid, &stackz, 
		    flatcal, flatpar, jigvert, nvert, jigpath, npath, status );

   if ( !StatusOkP(status) ) 
   {
      sprintf ( errmess, "failed to open %s", filename );
      ErsRep ( 0, status, errmess );
   }

/* Map space for the decompressed data */

   if ( StatusOkP(status) ) 
   {
      nbol = (*colsize) * (*rowsize);
      *outdata = (int *) calloc ( (*nframes)*nbol, sizeof(int) );
      *frhead = (JCMTState *) calloc ( (*nframes), 
        sizeof(JCMTState) );

      for ( j=0; j<*nframes; j++ )
      {

/* get details of incompressible pixels and decompress frames */

         sc2store_getincomp ( j, &npix, pixnum, pixval, status );
         sc2store_decompress ( nbol, stackz, bzero[j],
           &(data[j*nbol]), npix, pixnum,
           pixval, &((*outdata)[j*nbol]), status );

/* Read the per-frame headers */

         sc2store_headget ( j, &((*frhead)[j]), status );
      }
   }

/* Read the FITS headers */

   sc2store_rdfitshead ( maxlen, maxfits, nfits, fitshead, status );

   sc2open = 1; 
}

/*+ sc2store_wrfitshead - write the FITS headers */

void sc2store_wrfitshead
(
int nfits,            /* number of header items (given) */
char headers[][81],   /* array of FITS headers (given) */
int *status           /* global status (given and returned) */
)
/*
   History :
    12Aug2004 : original (bdk)
    23Nov2005 : Use Official C HDS interface (TIMJ)
*/
{
   HDSLoc *fitsloc = NULL;    /* HDS locator to FITS headers */
   int j;                     /* loop counter */
   HDSLoc *loc2 = NULL;       /* HDS locator */


   if ( *status != SAI__OK ) return;

/* Create and write FITS headers */

   ndfXnew ( indf, "FITS", "_CHAR*80", 1, &(nfits), &fitsloc, status );

   for ( j=1; j<=nfits; j++ )
   {
      datCell ( fitsloc, 1, &j, &loc2, status );
      datPut0C ( loc2, headers[j-1], status );
      datAnnul ( &loc2, status );
   }
   datAnnul ( &fitsloc, status );

   sc2store_errconv ( status );
}



/*+ sc2store_wrtstream - store SCUBA-2 time stream data as NDF */

void sc2store_wrtstream
(
char file_name[],  /* output file name (given) */
int subnum,        /* Sub-array number (given) */
int nrec,          /* number of FITS header records (given) */
char fitsrec[][81],/* FITS records (given) */
int colsize,       /* number of bolometers in column (given) */
int rowsize,       /* number of bolometers in row (given) */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
JCMTState head[],  /* header data for each frame (given) */
int *dbuf,         /* time stream data (given) */
int *darksquid,    /* dark SQUID time stream data (given) */
double *fcal,      /* flat-field calibration (given) */
double *fpar,      /* flat-field parameters (given) */
char *obsmode,    /* Observing mode (given) */
int jig_vert[][2], /* Array of jiggle vertices (given) */
int nvert,         /* Number of jiggle vertices (given) */
double jig_path[][2], /* Path of SMU during jiggle cycle (given) */
int npath,         /* Number of positions in jiggle path (given) */
int *status        /* global status (given and returned) */
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
*/

{
   int *bzero;                      /* pointer to subtracted offset values */
   unsigned short *data;            /* pointer to data array */
   static int digits[2*DREAM__MXBOL]; /* copy of each frame */
   int *dksquid;                    /* pointer to dark SQUID values */
   double *flatcal;                 /* pointer to flatfield calibration */
   double *flatpar;                 /* pointer to flatfield parameters */
   int framesize;                   /* number of pixels in a frame */
   int i;                           /* loop counter */
   int j;                           /* loop counter */
   double *jigpath;                 /* Pointer to SMU jiggle path */
   int *jigvert;                    /* Pointer to SMU jiggle vertices */
   int npix;                        /* number of incompressible pixels */
   static int pixnum[DREAM__MXBOL]; /* indices of incompressible pixels */
   static int pixval[DREAM__MXBOL]; /* values of incompressible pixels */
   int *stackz;                     /* pointer to stackzero frame */
   AstFrameSet * wcs;               /* World Coordinates frame set */

   if ( !StatusOkP(status) ) return;

   if ( sc2open != 0 )
   {
      *status = DITS__APP_ERROR;
      sprintf ( errmess, 
        "one SCUBA-2 data file already open, can't open %s", file_name );
      ErsRep ( 0, status, errmess );
      return;
   }

   sc2store_cremap ( file_name, colsize, rowsize, numsamples, nflat, flatname,
		     &bzero, &data, &dksquid, &stackz, &flatcal, &flatpar,
		     status );

   framesize = colsize * rowsize;


/* Use the first frame as the stackzero frame */

   if ( StatusOkP(status) )
   {

      for ( j=0; j<framesize; j++ )
      {
         stackz[j] = dbuf[j];
      }

/* Compress one frame at a time */

      for ( j=0; j<numsamples; j++ )
      {
         for ( i=0; i<framesize; i++ )
         {
            digits[i] = dbuf[j*framesize+i];
         }

         sc2store_compress ( framesize, stackz, digits, &(bzero[j]),
           &(data[j*framesize]), &npix, pixnum, pixval, status );

         if ( npix > 0 )
         {
            sc2store_putincomp ( j, npix, pixnum, pixval, status );
         }

/* Insert per-frame headers */

         sc2store_headput ( j, head[j], status );
      }

/* Copy the dark SQUID values */

      for ( j=0; j<numsamples*rowsize; j++ )
      {
         dksquid[j] = darksquid[j];
      }

/* Copy the flatfield calibration */

      for ( j=0; j<framesize*nflat; j++ )
      {
         flatcal[j] = fcal[j];
      }

      for ( j=0; j<nflat; j++ )
      {
         flatpar[j] = fpar[j];
      }

   }

   /* Create DREAM extension ONLY if we have DREAM data */
   if ( strncmp( obsmode, "DREAM", 5 ) == 0 ) {
     sc2store_credream( nvert, &jigvert, npath, &jigpath, status );
     /* Copy the DREAM data into the array: Fortran order */
     if ( *status == SAI__OK ) {
       /* First jigvert */
       for ( j=0; j<nvert; j++ ) {
	 jigvert[j] = jig_vert[j][0];
	 jigvert[j+nvert] = jig_vert[j][1];
       }
       /* Then jigpath */
       for ( j=0; j<npath; j++ ) {
	 jigpath[j] = jig_path[j][0];
	 jigpath[j+npath] = jig_path[j][1];
       }
     }
   }

/* Store the FITS headers */

   sc2store_wrfitshead ( nrec, fitsrec, status );

/* And create a convenience frameset for focal plane and time coordinates */

   wcs = timeWcs( subnum, numsamples, ((double*)sc2store_ptr[TCS_TAI]),status);
   ndfPtwcs(wcs, indf, status);
   wcs = astAnnul( wcs );

/* The file is open */
   sc2open = 1; 
}


AstFrameSet *timeWcs( int subnum, int ntime, const double times[], int * status ){

/*
*+
*  Name:
*     timeWcs

*  Purpose:
*     Calculate frameset for time series.

*  Prototype:
*     AstFrameSet *timeWcs( int subnum, int ntime, const double times[],
*                int * status );

*  Description:
*     Returns a FrameSet in which the base Frame is a 3D GRID Frame, and
*     the current Frame has 3 axes in the order (fplanex,fplaney,time).
*     The time axis is described using a MJD(TAI) TimeFrame, and its relationship
*     to GRID coords is specified by the supplied look-up table of time
*     values. The spatial axis is described by a 2D Frame with
*     Domain "FPLANE" and is connected to the GRID coords via a
*     mapping created by sc2ast and containing the mapping from pixel to focal plane
*     arcsecond offsets.

*  Parameters:
*     subnum = int (Given)
*        Subarray index
*     ntime = int (Given)
*        The number of time values supplied in "times".
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

*-
*/

/* Local Variables: */
   AstCmpFrame *totfrm;
   AstCmpMap *totmap;
   AstFrame *gridfrm;
   AstFrameSet *result, *spacefset;
   AstLutMap *timemap;
   AstTimeFrame *timefrm;
   double tcopy[2];  /* local copy of time lut for when only 1 number present */
   double *ltimes;  /* pointer to a time array */
   int malloced = 0; /* did we malloc a ltimes array */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if( *status != SAI__OK ) return result;

/* Start an AST context so we do not need to annul AST pointers explicitly. */
   astBegin;

   /* Create a frame covering the focal plane coordinates */
   sc2ast_createwcs( subnum, NULL, NULL, NULL, &spacefset, status );

/*
   Now create a TimeFrame to describe MJD in the TAI timescale, and
   a LutMap which transforms grid coord into MJD (in days). The default
   TimeFrame attribute values give us what we want. 
   Work out the duration of the observation to decide on formatting.
   To give AST some help with formatting axes we use a TimeOrigin.
*/
   timefrm = astTimeFrame( "" );
   malloced = 0;
   if (ntime == 1) {
     /* a LutMap needs two numbers in its mapping so double up the
	first time if we only have one value. */
     tcopy[0] = times[0];
     tcopy[1] = times[1];
     ltimes = tcopy;
     ntime = 2;
   } else {
     double origin = 0.0; /* reference time */
     int i;
     /* copy values and remove integer part of day */
     ltimes = malloc( sizeof(*ltimes) * ntime );
     if (ltimes) malloced = 1;
     origin = floor( times[0] );
     for (i = 0; i < ntime; i++ ) {
       ltimes[i] = times[i] - origin;
     }
     astSetD(timefrm, "TimeOrigin", origin);

     /* We would like to use iso.0 for anything that is longer than 10 seconds (say)
       else use iso.3 because can not take spectra faster than 0.005 second. */
     if ( (ltimes[ntime-1] - ltimes[0]) < (10.0 / SC2AST_SPD) ) {
       astSet(timefrm, "format=iso.3");
     } else {
       astSet(timefrm, "format=iso.0");
     }

   }
   timemap = astLutMap( ntime, ltimes, 1.0, 1.0, "" );

   if (malloced) free( ltimes );

/* We now have the Frames and Mappings describing all the individual
   axes. Join all the Frames together into a CmpFrame (in the order spectral,
   spatial, time), and join all the Mappings together into a parallel
   CmpMap. */
   totfrm = astCmpFrame( spacefset, timefrm, "" );
   totmap = astCmpMap( spacefset, timemap, 0, "" );

/* Create a 3D GRID Frame. */
   gridfrm = astFrame( 3, "Domain=GRID,Title=FITS pixel coordinates" );
   astSet( gridfrm, "Unit(1)=pixel,Label(1)=FITS pixel axis 1" );
   astSet( gridfrm, "Unit(2)=pixel,Label(2)=FITS pixel axis 2" );
   astSet( gridfrm, "Unit(3)=pixel,Label(2)=FITS pixel axis 3" );

/* Create the FrameSet to return, initially containing just the above
   GRID Frame. */
   result = astFrameSet( gridfrm, "" );

/* Add the total Frame into the FrameSet using the total Mapping to
   connect it to the base (i.e. GRID) Frame. */
   astAddFrame( result, AST__BASE, totmap, totfrm );

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
