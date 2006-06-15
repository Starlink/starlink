/* dsim - routines for supporting DREAM simulation */

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#include "sae_par.h"
#include "ast.h"
#include "ndf.h"

#include "fitsio.h"

#include "sc2da/Dits_Err.h"
#include "sc2da/Ers.h"

#include "libsim/fhead_par.h"
#include "libsim/fhead.h"
#include "sc2da/sc2ast.h"
#include "sc2da/sc2store_par.h"
#include "sc2da/sc2store_struct.h"
#include "sc2da/sc2store.h"
#include "libsim/dream_par.h"
#include "libsim/dream.h"
#include "libsim/dxml_struct.h"
#include "libsim/dxml.h"
#include "sc2da/sc2math.h"

#include "libsim/dsim_par.h"
#include "libsim/dsim_struct.h"
#include "libsim/dsim.h"

#define C 299792458.0                  /* speed of light in metres/sec */
#define H 6.626e-34                    /* Planck's constant in joule.sec */
#define COUNTTOSEC 6.28                /* arcsec per pixel */
#define RIZERO 40.0                    /* distortion pattern centre */
#define RJZERO -10.0                   /* distortion pattern centre */
#define PIBY2 (AST__DPI/2.0)           /* math constant */
#define DIAMETER 15.0                  /* Diameter JCMT in metres */
#define MM2SEC 5.144                   /* plate scale at Nasmyth */

#define BOLCOL 32                      /* number of columns in a subarray */
#define BOLROW 40                      /* number of rows in a subarray */

/* -----------------------------------------------------------------------*/
/* Extra defines required for slalib routines */

#ifndef dmod
#define dmod(A,B) ((B)!=0.0?((A)*(B)>0.0?(A)-(B)*floor((A)/(B))\
                                        :(A)+(B)*floor(-(A)/(B))):(A))
#endif

#ifndef DS2R
#define DS2R 7.2722052166430399038487115353692196393452995355905e-5
#endif

#ifndef D2PI
#define D2PI 6.2831853071795864769252867665590057683943387987502
#endif


/* -----------------------------------------------------------------------*/

/*+ dsim_addinvf - add 1/f noise to a series of values */

void dsim_addinvf 
( 
int bol,                 /* bolometer number (given) */
double start_time,       /* time at first sample in seconds (given) */
double samptime,         /* time per sample in seconds (given) */
int nframes,             /* number of samples to calculate (given) */
int nterms,              /* number of noise terms per bolometer (given) */
double *noisecoeffs,     /* bolometer noise coefficients (given) */
double *output,          /* bolometer signal (given and returned) */
int *status              /* global status (given and returned) */
)

/*  Description :
     A time stream of nframes for a single bolometer is modified by the
     addition of 1/f noise characterised by the amplitudes of the
     low-order frequencies.

   Authors :
    B.D.Kelly (ROE)

   History :
    23Jan2004:  original (bdk)
*/

{
   int pos;       /* position of coeffs for current bolometer */
   double fnoise; /* calculated noise term */
   int i;         /* loop counter */
   int j;         /* loop counter */
   double phase;  /* phase of a sample relative to a noise frequency */
   double time;   /* time of a sample */

   if ( !StatusOkP(status) ) return;
   
   pos = bol * nterms * 3;
   
   for ( j=0; j<nframes; j++ )
   {
/*   printf ( "dsim_addinvf: frame no = %d\n", j ); */
      fnoise = 0.0;
      time = start_time + j * samptime;
      for ( i=0; i<nterms*3; i+=3 )
      {
         phase = fmod ( time, noisecoeffs[pos+i] ) / noisecoeffs[pos+i];
         fnoise += noisecoeffs[pos+i+1] * cos ( 2.0 * AST__DPI * phase )
	   + noisecoeffs[pos+i+2] * sin ( 2.0 * AST__DPI * phase );
      }
      output[j] += fnoise;
   }

}


/*+ dsim_addpnoise - add photon noise to a flux */

void dsim_addpnoise 
(
double lambda,       /* wavelength in metres (given) */
double bandGHz,      /* bandwidth in GHZ (given) */
double aomega,       /* geometrical optical factor (given) */
double integ_time,   /* Effective integration time in sec (given) */
double *flux,        /* Flux value in pW (given and returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     Calculate the photon equivalent of the given flux allowing the
     corresponding photon noise sigma to be calculated. Use this to
     scale a generated random number.

   Authors :
    B.D.Kelly (ROE)

   History :
    26July2001:  original (bdk@roe.ac.uk)
    19Aug2002 :  C version (bdk)
    22MAy2003 :  multiply photon noise by 1.5 (BDK)
    23Jan2004 :  use dsim_getsigma (bdk)
*/

{

   double err;                  /* error offset */
   double sigma;                /* NEP in pW */



   if ( !StatusOkP(status) ) return;


/* Calculate the dispersion due to photon noise */

   dsim_getsigma ( lambda, bandGHz, aomega, *flux, 
      &sigma, status );


/* Calculate a random number and scale it to the required sigma */

   err = dsim_drand ( sigma );

/* Correct for the integration time - factor 2 because sigma is per root
   Hz */

   *flux = *flux + err / sqrt(2.0*integ_time);

}



/*+ dsim_atmatrans - calculate sky transmission at given airmass */

void dsim_atmatrans
(
double trans0,       /* % Zenith transmission (given) */
double airmass,      /* airmass (given) */
double *atrans,      /* % atmospheric transmission (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     Calculate the atmospheric transmission for a given airmass and 
     zenith optical depth.

   Authors :
     A.G. Gibb (UBC)

   History :
    14Apr2005:  Original (agg@astro.ubc.ca)
*/

{

   if ( !StatusOkP(status) ) return;

   /* Remember transmission is in % !! */
   *atrans = 100.0*pow(trans0/100.0,airmass);

}

/*+ dsim_atmtrans - calculate sky transmission from flux */

void dsim_atmtrans
(
double lambda,       /* wavelength in metres (given) */
double flux,         /* flux per bolometer in pW (given) */
double *trans,       /* % atmospheric transmission (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :

     Calculate the atmospheric transmission equivalent to the given
     flux for the indicated wavelength band using a simple relation
     deduced from modelling. 

   Authors :
    B.D.Kelly (ROE)
    A.G. Gibb (UBC)

   History :
    20Jan2005:  original (bdk@roe.ac.uk)
    04Feb2005:  revised constants (bdk)
    14Apr2005:  alternative method (agg@astro.ubc.ca)
    15Apr2005:  corrected 850 constants (bdk)

*/

{
   double zero;                 /* constant offset */
   double slope;                /* slope of relation */
   double maxflux;              /* Maximum sky flux */

   if ( !StatusOkP(status) ) return;

/* Check whether 450 or 850 microns */
   if ( fabs ( lambda - 0.45e-3 ) < 0.1e-3 )
   {
     zero = 95.818;
     slope = -0.818;
     /*     slope = -8.772e-3;*/
   }
   else
   {
     zero = 102.0;
     slope = -5.0;
     /*     slope = -0.0465;*/
   }
   /*   zero = 1.0;*/
   maxflux = -zero/slope;

   if (flux > maxflux) {
     printf("Error: assumed sky flux, %g pW, is greater than maximum value, %g pW\n",flux,maxflux);
     exit(-1);
   }

   /* agg's version */
   /*   *trans = 100.0*(zero + slope * flux);*/ 
   *trans = zero + slope * flux;
}

/*+ dsim_atmsky - calculate sky flux given sky transmission */

void dsim_atmsky
(
double lambda,       /* wavelength in metres (given) */
double trans,        /* % atmospheric transmission (given) */
double *flux,        /* flux per bolometer in pW (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :

   Calculate the sky noise power given the atmospheric
   transmission. This routine is basically the inverse of
   dsim_atmtrans.

   Authors :
    A.G. Gibb (UBC)

   History :
    20Apr2005:  original (agg@astro.ubc.ca)

*/

{
   double zero;                 /* constant offset */
   double slope;                /* slope of relation */
   double maxflux;              /* Maximum sky flux */

   if ( !StatusOkP(status) ) return;

/* Check whether 450 or 850 microns */
   if ( fabs ( lambda - 0.45e-3 ) < 0.1e-3 )
   {
     zero = 95.818;
     slope = -0.818;
     /*     slope = -8.772e-3;*/
   }
   else
   {
     zero = 102.0;
     slope = -5.0;
     /*     slope = -0.0465;*/
   }
   /*   zero = 1.0;*/
   maxflux = -zero/slope;

   *flux = (trans - zero) / slope;

   if (*flux > maxflux) {
     printf("Error: derived sky flux, %g pW, is greater than maximum value, %g pW\n",*flux,maxflux);
     exit(-1);
   }
}


/*+ dsim_bolnatcoords - get bolometer native coordinates */

void dsim_bolnatcoords
(
double *xbolo,        /* projected X coords of bolometers (returned) */
double *ybolo,        /* projected Y coords of bolometers (returned) */
int *bol,             /* bolometer counter (returned) */
int *status           /* global status (given and returned) */
)
/* Method :
    Calculate the native coordinates of each bolometer in the array
   Authors :
    E.Chapin (UBC)
   History :
    28Feb2006 : original (EC)
*/
{

  int i;           /* row counter */
  int j;           /* column counter */
  
  if ( !StatusOkP(status) ) return;

  /* Set the bolometer indices */
  *bol = 0;
  for ( j=0; j<BOLCOL; j++ ) {
    for ( i=0; i<BOLROW; i++ ) {
      xbolo[*bol] = (double)j;
      ybolo[*bol] = (double)i;
      (*bol)++;
    }
  }
}

/*+ dsim_bolcoords - get bolometer Nasmyth coordinates */

void dsim_bolcoords
(
char *subname,       /* subarray name, s8a-s4d (given) */
double ra,           /* RA of observation in radians (given) */
double dec,          /* Dec of observation in radians (given) */
double elevation,    /* telescope elevation in radians (given) */
double p,            /* parallactic angle in radians (given) */
char *domain,        /* AST domain name to be used (given) */
AstFrameSet **fset,  /* World coordinate transformations (returned) */
int *bol,            /* bolometer counter (returned) */
double xbc[],        /* projected X coords of bolometers (returned) */
double ybc[],        /* projected Y coords of bolometers (returned) */
int *status          /* global status (given and returned) */
)
/* Method :
    Use sc2ast to set up an AST frameset for the world coordinates, then
    use the frameset to get a transformation from pixel numbers to Nasmyth
    coordinates for the given subarray and calculate coordinates for all
    the pixels.
   Authors :
    B.D.Kelly (ROE)
    E.Chapin (UBC)
   History :
    10May2005 : original (bdk)
    13May2005 : pass subname as argument and look-up subnum (bdk)
    05Dec2005 : Fixed new API for sc2ast (EC)
    28Feb2006 : Call dsim_bolnatcoords (EC)
*/
{
   static double bindc[BOLCOL*BOLROW]; /* bolometer indices */
   static double bindr[BOLCOL*BOLROW]; /* bolometer indices */
   int subnum;      /* subarray number */

   int i;

   if ( !StatusOkP(status) ) return;

   /* Set the bolometer indices */

   dsim_bolnatcoords( bindc, bindr, bol, status );

   /*
     printf("NBOLO: %i\n",*bol );

     for( i=0; i<*bol; i++ )
     printf(" %e %e\n", bindc[i], bindr[i] );
   */

   /**bol = 0;
      for ( j=0; j<BOLCOL; j++ ) {
      for ( i=0; i<BOLROW; i++ ) {
      bindc[*bol] = (double)j;
      bindr[*bol] = (double)i;
      (*bol)++; 
      }
      }
   */

   /* Set up the frameset */

   sc2ast_name2num ( subname, &subnum, status );

   /* Currently broken since we need to put in the Azimuth
      and the time */
   
   /*sc2ast_createwcs ( subnum, 0.0, elevation, 0.0, fset,
     status );*/
   
   /* I think this is fixed now EC 05Dec2005 */
   sc2ast_createwcs_compat( subnum, ra, dec, elevation, 0.0, fset,
			    status );
   
   /* Select the required domain */
   
   sc2ast_getdomain ( domain, *fset, status );
   
   /* Find transformations for all the pixels */
   
   astTran2 ( *fset, *bol, bindr, bindc, 1, xbc, ybc );
   
   /* Set the current frame back to the one with domain=SKY */

   sc2ast_getdomain ( "SKY", *fset, status );
}





/*+ dsim_calctau - calculate sky zenith optical depth */

void dsim_calctau
(
double lambda,       /* wavelength in metres (given) */
double trans,        /* % atmospheric transmission (given) */
double airmass,      /* airmass (given) */
double *tauCSO,      /* CSO optical depth (returned) */
double *tau850,      /* 850 optical depth (returned) */
double *tau450,      /* 450 optical depth (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     Calculate the atmospheric optical depths equivalent to the given 
     transmission for the various wavelength bands using the JCMT
     standard relationships.

   Authors :
    B.D.Kelly (ROE)

   History :
    03Mar2005:  original (bdk@roe.ac.uk)
*/

{

   double tau;                  /* tau matching transmission */

   if ( !StatusOkP(status) ) return;

   tau = log ( 100.0 / trans ) ;

/* Check whether 450 or 850 microns and use JCMT linear calibration to
   calculate the other tau values */

   if ( fabs ( lambda - 0.45e-3 ) < 0.1e-3 )
   {
      *tau450 = tau;
      *tauCSO = 0.014 + ( tau / 26.2 );
      *tau850 = 4.02 * ( *tauCSO - 0.001 );
   }
   else
   {
      *tau850 = tau;
      *tauCSO = 0.001 + ( tau / 4.02 );
      *tau450 = 26.2 * ( *tauCSO - 0.014 );
   }

}


/*+ dsim_calctime - calculte UT + LST arrays given a start time */

void dsim_calctime
( 
double mjdaystart,   /* start time as modified juldate */
double samptime,     /* length of a sample in seconds */
int nsamp,           /* number of samples */
double *ut,          /* returned UT at each sample (mod. juldate) */
double *lst,         /* returned LST at each sample (radians) */
int *status          /* global status (given and returned) */
)

/* Description :
   Given a start time and number of samples, calculate the UT and LST at
   each sample.

   Authors :
    Ed Chapin (echapin@phas.ubc.ca)

   History :
    23Feb2006 : Original version (echapin)
*/

{

  double gst;
  int i;
  double lon;
  double tu;
  double sampday;

  if ( !StatusOkP(status) ) return;

  /* JCMT longitude in radians */
  lon = ( 155.0 + (28.0/60.0) + (0.0/3600.0) ) / AST__DR2D;

  /* Length of a single sample in days */
  sampday = samptime/(3600. * 24.);

  /* Loop over each time step, calculate UT and then calculate 
     greenwhich sidereal time using slalib routine slaGmst and convert
     to local sidereal time */
  
  for(i=0; i<nsamp; i++) {
    ut[i] = mjdaystart + ((double) i)*sampday;

    /* Julian centuries from fundamental epoch J2000 to this UT */
    tu = ( ut[i] - 51544.5 ) / 36525.0;

    /* GMST at this UT */
    gst = dmod ( ut[i], 1.0 ) * D2PI +
      ( 24110.54841 +
	( 8640184.812866 +
	  ( 0.093104 - 6.2e-6 * tu ) * tu ) * tu ) * DS2R;
    
    /* Calculate LST from GMST using telescope longitude */
    lst[i] = fmod(gst - lon + D2PI,D2PI);
  }
}

/*+ dsim_crepoints - simulate image of identical gaussian sources */

void dsim_crepoints 
(
double fluxJy,       /* Flux of each point source (given) */
double diam,         /* diameter of telescope aperture in metres (given) */
double fwhm,         /* fwhm of telescope PSF (given) */
double spacingx,     /* spacing in arcsec between sources in X (given) */
double spacingy,     /* spacing in arcsec between sources in Y (given) */
double transmission, /* transmission of optics as a fraction (given) */
double bandGHz,      /* optical bandwidth in GHz (given) */
double pixsize,      /* pixel grid size in arcsec (given) */
int nx,              /* size of image in X (given) */
int ny,              /* size of image in Y (given) */
double *astsim,      /* array to hold returned image (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     Return an array with data representing a "true" astronomical image
     containing a set of identical Gaussian sources on a regular grid.

   Authors :
    B.D.Kelly (ROE)

   History :
    20Oct2003:  original (bdk)
    21Jan2005:  fix pW calculation and scaling of Gaussian (bdk)
*/

{
   double a[149][149];           /* Point source */
   int gsize;                    /* size of point source */
   int i;                        /* loop counter */
   int j;                        /* loop counter */
   int m;                        /* loop counter */
   int n;                        /* loop counter */
   double psigma;                /* sigma of simulated Gaussian */
   double rsq;                   /* square of radial distance */
   double sigsq;                 /* square of sigma */
   double sum;                   /* sum of values in Gaussian */
   double pwatt;                 /* integrated signal in pW */
   double peak;                  /* central peak of Gaussian */
   int xstep;                    /* distance between point sources in pixels */
   int ystep;                    /* distance between point sources in pixels */


   if ( !StatusOkP(status) ) return;

/* Calculate the dispersion of the 2-D Gaussian corresponding to the given 
   FWHM in pixels */

   psigma = fwhm / ( 2.0 * 1.177 * pixsize );

/* Choose an odd-dimensioned size for the array to hold the Gaussian */

   gsize = (int)( 1.0 + 6.0 * psigma );
   gsize = 1 + 2 * ( gsize / 2 );
   if ( gsize > 149 )
   {
      gsize = 149;
   }

/* Create a 2-D Gaussian representing a point source */

   sigsq = psigma * psigma;

/* 1 Jy is defined as 10**(-26) W/sq.m/Hz.
   Convert the flux in Jy arriving at the telescope aperture to pW in the focal
   plane */
   
   pwatt = fluxJy * 1.0e-26 * AST__DPI * 0.25 * diam * diam * bandGHz * 1.0e9 
      * 1.0e12;

/* Correct for transmission of optics */

   pwatt = pwatt * transmission;

/* Create a Gaussian */

   sum = 0.0;
   for ( j=0; j<gsize; j++ )
   {
      for ( i=0; i<gsize; i++ )
      {
         rsq = ( ( (gsize-1)/2 - i ) * ( (gsize-1)/2 - i ) ) + ( (
           (gsize-1)/2 - j ) * ( (gsize-1)/2 - j ) );
         a[j][i] = exp ( - 0.5 * rsq / sigsq );
	 sum += a[j][i];
      }
   }

/* Apply scaling factor which normalises 2-D Gaussian to integrate to pwatt */

   peak = pwatt / sum;
   for ( j=0; j<gsize; j++ )
   {
      for ( i=0; i<gsize; i++ )
      {
         a[j][i] = peak * a[j][i];
      }
   }


/* Coadd the set of point sources into the image */

   xstep = (int) ( 0.5 + spacingx / pixsize );
   ystep = (int) ( 0.5 + spacingy / pixsize );
   for ( j=gsize; j<ny-gsize; j+=ystep )
   {
      for ( i=gsize; i<nx-gsize; i+=xstep )
      {
         for ( n=0; n<gsize; n++ )
         {
            for ( m=0; m<gsize; m++ )
            {
               astsim[i-m+(gsize-1)/2 + nx*(j-n+(gsize-1)/2)] += a[n][m];
            }
         }
      }
   }
}

/*+ dsim_crepoint - simulate image of a single gaussian source */

void dsim_crepoint 
(
double fluxJy,       /* Flux of each point source (given) */
double diam,         /* diameter of telescope aperture in metres (given) */
double fwhm,         /* fwhm of telescope PSF (given) */
double transmission, /* transmission of optics as a fraction (given) */
double bandGHz,      /* optical bandwidth in GHz (given) */
double pixsize,      /* pixel grid size in arcsec (given) */
int nx,              /* size of image in X (given) */
int ny,              /* size of image in Y (given) */
double *astsim,      /* array to hold returned image (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     Return an array with data representing a "true" astronomical image
     containing a set of identical Gaussian sources on a regular grid.

   Authors :
    B.D.Kelly (ROE)

   History :
    20Oct2003:  original (bdk)
    21Jan2005:  fix pW calculation and scaling of Gaussian (bdk)
*/

{
   double a[149][149];           /* Point source */
   int gsize;                    /* size of point source */
   int i;                        /* loop counter */
   int j;                        /* loop counter */
   int m;                        /* loop counter */
   int n;                        /* loop counter */
   double psigma;                /* sigma of simulated Gaussian */
   double rsq;                   /* square of radial distance */
   double sigsq;                 /* square of sigma */
   double sum;                   /* sum of values in Gaussian */
   double pwatt;                 /* integrated signal in pW */
   double peak;                  /* central peak of Gaussian */
   int xstep;                    /* distance between point sources in pixels */
   int ystep;                    /* distance between point sources in pixels */


   if ( !StatusOkP(status) ) return;

/* Calculate the dispersion of the 2-D Gaussian corresponding to the given 
   FWHM in pixels */

   psigma = fwhm / ( 2.0 * 1.177 * pixsize );

/* Choose an odd-dimensioned size for the array to hold the Gaussian */

   gsize = (int)( 1.0 + 6.0 * psigma );
   gsize = 1 + 2 * ( gsize / 2 );
   if ( gsize > 149 )
   {
      gsize = 149;
   }

/* Create a 2-D Gaussian representing a point source */

   sigsq = psigma * psigma;

/* 1 Jy is defined as 10**(-26) W/sq.m/Hz.
   Convert the flux in Jy arriving at the telescope aperture to pW in the focal
   plane */
   
   pwatt = fluxJy * 1.0e-26 * AST__DPI * 0.25 * diam * diam * bandGHz * 1.0e9 
      * 1.0e12;

/* Correct for transmission of optics */

   pwatt = pwatt * transmission;

/* Create a Gaussian */

   sum = 0.0;
   for ( j=0; j<gsize; j++ )
   {
      for ( i=0; i<gsize; i++ )
      {
         rsq = ( ( (gsize-1)/2 - i ) * ( (gsize-1)/2 - i ) ) + ( (
           (gsize-1)/2 - j ) * ( (gsize-1)/2 - j ) );
         a[j][i] = exp ( - 0.5 * rsq / sigsq );
	 sum += a[j][i];
      }
   }

/* Apply scaling factor which normalises 2-D Gaussian to integrate to pwatt */

   peak = pwatt / sum;
   for ( j=0; j<gsize; j++ )
   {
      for ( i=0; i<gsize; i++ )
      {
         a[j][i] = peak * a[j][i];
      }
   }


/* Coadd the set of point sources into the image */

/*   xstep = (int) ( 0.5 + spacingx / pixsize );
     ystep = (int) ( 0.5 + spacingy / pixsize );*/
   xstep = (int) (nx/2);
   ystep = (int) (ny/2);
   for ( j=gsize; j<ny-gsize; j+=ystep )
   {
      for ( i=gsize; i<nx-gsize; i+=xstep )
      {
         for ( n=0; n<gsize; n++ )
         {
            for ( m=0; m<gsize; m++ )
            {
               astsim[i-m+(gsize-1)/2 + nx*(j-n+(gsize-1)/2)] += a[n][m];
            }
         }
      }
   }
}


/*+ dsim_creslope - simulate image of sloping source */

void dsim_creslope
(
double fluxJy,       /* Flux of example point source (given) */
double diam,         /* diameter of telescope aperture in metres (given) */
double fwhm,         /* fwhm of telescope PSF (given) */
double transmission, /* transmission of optics as a fraction (given) */
double bandGHz,      /* optical bandwidth in GHz (given) */
double pixsize,      /* pixel grid size in arcsec (given) */
int nx,              /* size of image in X (given) */
int ny,              /* size of image in Y (given) */
double *astsim,      /* array to hold returned image (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     Return an array with data representing a "true" astronomical image
     containing a slope calculated from the characteristics of a Gaussian
     source.

   Authors :
    B.D.Kelly (ROE)

   History :
    11Aug2005:  original (bdk)
*/

{
   double a[149][149];           /* Point source */
   int gsize;                    /* size of point source */
   int i;                        /* loop counter */
   int j;                        /* loop counter */
   double psigma;                /* sigma of simulated Gaussian */
   double rsq;                   /* square of radial distance */
   double sigsq;                 /* square of sigma */
   double sum;                   /* sum of values in Gaussian */
   double pwatt;                 /* integrated signal in pW */
   double peak;                  /* central peak of Gaussian */


   if ( !StatusOkP(status) ) return;

/* Calculate the dispersion of the 2-D Gaussian corresponding to the given 
   FWHM in pixels */

   psigma = fwhm / ( 2.0 * 1.177 * pixsize );

/* Choose an odd-dimensioned size for the array to hold the Gaussian */

   gsize = (int)( 1.0 + 6.0 * psigma );
   gsize = 1 + 2 * ( gsize / 2 );
   if ( gsize > 149 )
   {
      gsize = 149;
   }

/* Create a 2-D Gaussian representing a point source */

   sigsq = psigma * psigma;

/* 1 Jy is defined as 10**(-26) W/sq.m/Hz.
   Convert the flux in Jy arriving at the telescope aperture to pW in the focal
   plane */
   
   pwatt = fluxJy * 1.0e-26 * AST__DPI * 0.25 * diam * diam * bandGHz * 1.0e9 
      * 1.0e12;

/* Correct for transmission of optics */

   pwatt = pwatt * transmission;

/* Create a Gaussian */

   sum = 0.0;
   for ( j=0; j<gsize; j++ )
   {
      for ( i=0; i<gsize; i++ )
      {
         rsq = ( ( (gsize-1)/2 - i ) * ( (gsize-1)/2 - i ) ) + ( (
           (gsize-1)/2 - j ) * ( (gsize-1)/2 - j ) );
         a[j][i] = exp ( - 0.5 * rsq / sigsq );
	 sum += a[j][i];
      }
   }

/* Determine the scaling factor which normalises 2-D Gaussian to
   integrate to pwatt - this is also the value at the centre */

   peak = pwatt / sum;

/* Coadd the slope into the image */

   for ( j=0; j<ny; j++ )
   {
      for ( i=0; i<nx; i++ )
      {
         astsim[i+nx*j] += peak * (double)j / (double)ny;
      }
   }
}



/*+ dsim_crotate - rotate a 2-D complex array through 90 degrees */

void dsim_crotate
(
int direction,       /* +1 clockwise, -1 anticlockwise (given) */
int size,            /* square dimension of complex image array (given) */
double *array,       /* image array size*2 by size (given and returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     The given array represents a square matrix [size][size] of complex
     numbers arranged so that each real value is followed by its
     imaginary partner.

    History :
     12Nov2003: original (bdk)
*/

{
   int i;                 /* loop counter */
   int j;                 /* loop counter */
   double vali;           /* stored imaginary value */
   double valr;           /* stored real value */
   
   
   if ( !StatusOkP(status) ) return;


   if ( direction == 1 )
   {

/* clockwise */

      for ( i=0; i<size/2; i++ )
      {
         for ( j=i; j<size-1-i; j++ )
	 {
            valr = array[2*(size-1-i) + 2*size * j];
            vali = array[2*(size-1-i)+1 + 2*size * j];

            array[2*(size-1-i) + 2*size * j] = 
	      array[2*(size-1-j) + 2*size * (size-1-i) ];
            array[2*(size-1-i)+1 + 2*size * j] =
	      array[2*(size-1-j)+1 + 2*size * (size-1-i) ];

            array[2*(size-1-j) + 2*size * (size-1-i) ] = 
	      array[2*i + 2*size * (size-1-j) ];
            array[2*(size-1-j)+1 + 2*size * (size-1-i) ] =
	      array[2*i+1 + 2*size * (size-1-j) ];

            array[2*i + 2*size * (size-1-j) ] = 
	      array[2*j + 2*size * i];
            array[2*i+1 + 2*size * (size-1-j) ] =
	      array[2*j+1 + 2*size * i];

            array[2*j + 2*size * i] = valr;
            array[2*j+1 + 2*size * i] = vali;
         }
      }

   }
   else
   {

/* anticlockwise */

      for ( i=0; i<size/2; i++ )
      {
         for ( j=i; j<size-1-i; j++ )
	 {
            valr = array[2*j + 2*size * i];
            vali = array[2*j+1 + 2*size * i];

            array[2*j + 2*size * i] =
               array[2*i + 2*size * (size-1-j) ];
	    array[2*j+1 + 2*size * i] =
              array[2*i+1 + 2*size * (size-1-j) ];

	    array[2*i + 2*size * (size-1-j) ] =
              array[2*(size-1-j) + 2*size * (size-1-i) ]; 
	    array[2*i+1 + 2*size * (size-1-j) ] =
              array[2*(size-1-j)+1 + 2*size * (size-1-i) ];

	    array[2*(size-1-j) + 2*size * (size-1-i) ] =
              array[2*(size-1-i) + 2*size * j];
	    array[2*(size-1-j)+1 + 2*size * (size-1-i) ] =
              array[2*(size-1-i)+1 + 2*size * j];

            array[2*(size-1-i) + 2*size * j] = valr;
            array[2*(size-1-i)+1 + 2*size * j] = vali;
         }
      }
   }
}



/*+ dsim_digitise - Convert array of currents to integers */

void dsim_digitise
(
int nvals,            /* number of values (given) */
double current[],     /* signal values in amps (given) */
double digmean,       /* mean digitised level (given) */
double digscale,      /* digitisation scale factor (given) */
double digcurrent,    /* current in amps at digmean (given) */
int digits[],         /* digitised currents (returned) */
int *status           /* global status (given and returned) */
)

/*  Description :
     The simulated bolometer current is to be "digitised" using the formula

      digval = int ( 0.5 + ( current - digcurrent ) * digscale + digmean )

     where the three digitisation parameters are chosen so that the
     current corresponding to the target power level falls in the middle
     of a 24-bit range, and one digitisation level is one-sixth of the
     photon noise dispersion (this assumes the digitisation can encode
     instrumental noise, and the latter is about 1/3 of photon noise).

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     23Sep2004 :  original (bdk)
     18Feb2005 :  don't allow negative values (bdk)
*/

{
   int j;                     /* loop counter */

   if ( !StatusOkP(status) ) return;


   for ( j=0; j<nvals; j++ )
   {
      digits[j] = (int) ( 0.5 + ( current[j] - digcurrent ) * digscale +
        digmean );
      if ( digits[j] < 0 ) digits[j] = 0;
   }
}



/*+ dsim_doscan - Simulate a single scan from a scanmap */

void dsim_doscan
(
struct dxml_struct inx,         /* structure for values from XML (given) */
struct dxml_sim_struct sinx,    /* structure for sim values from XML (given)*/
long astnaxes[2],               /* dimensions of simulated image (given) */
double astscale,                /* pixel size in simulated image (given) */
double *astsim,                 /* astronomical sky (given) */
long atmnaxes[2],               /* dimensions of simulated atm background
                                   (given) */
double atmscale,                /* pixel size in simulated atm background
                                   (given) */
double *atmsim,                 /* atmospheric emission (given) */
double coeffs[NCOEFFS],         /* bolometer response coeffs (given) */
double heater[DREAM__MXBOL],    /* bolometer heater ratios (given) */
int nboll,                      /* total number of bolometers (given) */
int nframes,                    /* number of frames in scan (given) */
int nterms,                     /* number of 1/f noise coeffs (given) */
double *noisecoeffs,            /* 1/f noise coeffs (given) */
double *pzero,                  /* bolometer power offsets (given) */
double samptime,                /* sample time in sec (given) */
double start_time,              /* time at start of scan in sec  (given) */
double *tau_bol,                /* Bol. time constants (given) */
double telemission,             /* power from telescope emission (given) */
double *weights,                /* impulse response (given) */
double *xbc,                    /* X offsets of bolometers in arcsec */
double *ybc,                    /* Y offsets of bolometers in arcsec */
double xstart,                  /* Xcoord at start of scan (given) */
double xvel,                    /* X velocity along scan (given) */
double ystart,                  /* Ycoord at start of scan (given) */
double yvel,                    /* Y velocity along scan (given) */
double output[DREAM__MXSIM],    /* series of output values (returned) */
double dbuf[DREAM__MXSIM*DREAM__MXBOL], /* Data for whole scan (returned) */
int *status                     /* global status */
)

/*  Method :

    Authors :
     B.D.Kelly (ROE)

    History :
     22Jan2004: original (bdk@roe.ac.uk)
     01Mar2005: apply sky transmission (bdk)
*/

{


   double astvalue;                /* obs. astronomical value in pW */
   double atmvalue;                /* obs. atmospheric emission in pW */
   int bol;                        /* counter for indexing bolometers */
   double current;                 /* bolometer current in amps */
   double flux;                    /* flux at bolometer in pW */
   int frame;                      /* frame counter */
   double skytrans;                /* sky transmission (%) */
   double time;                    /* time from start of observation */
   double xpos;                    /* X measurement position */
   double xsky;                    /* X position on sky screen */
   double ypos;                    /* Y measurement position */
   double ysky;                    /* Y position on sky screen */


   if ( !StatusOkP(status) ) return; 



   for ( bol=0; bol<nboll; bol++ )
   {
      for ( frame=0; frame<nframes; frame++ )
      {
         xpos = xbc[bol] + xstart + frame * samptime * xvel;
         ypos = ybc[bol] + ystart + frame *samptime * yvel;
            
/*  Interpolate bolometer position on astronomical image.
    The scalar ASTVALUE contains the interpolated astronomical map 
    value for the current position (XPOS,YPOS). */

         dsim_getast ( xpos, ypos, inx.bol_distx, astscale, astnaxes[0],
           astsim, &astvalue, status );


         if ( dream_trace ( 4 ) )
         { 
            if ( bol==0 || bol==nboll-1 )
              printf ( " %d  %e  %e  %e\n", bol, xpos, ypos, astvalue );
         }

/*  Lookup atmospheric emission - offset to near centre of the atm frame.
    A typical windspeed moves the sky screen at equivalent to 5000 arcsec
    per sec.
    The scalar ATMVALUE contains the atmosphere map value for the 
    current position (XPOS,YPOS). */

         time = start_time + frame * samptime;
         xsky = xpos + sinx.atmxvel * time + sinx.atmzerox;
         ysky = ypos + sinx.atmyvel * time + sinx.atmzeroy;
         if ( sinx.add_atm == 1 )
         {
            dsim_getbilinear ( xsky, ysky, atmscale, atmnaxes[0], atmsim, 
              &atmvalue, status );
            if ( dream_trace ( 4 ) )
            { 
               printf ( "scansim : atm emission interpolated\n" );
               printf ( "status = %d\n", *status );
            }
         }
         else
         {
            atmvalue = sinx.meanatm;
         }

/* Calculate atmospheric transmission */

         dsim_atmtrans ( inx.lambda, atmvalue, &skytrans, status );

/*  Add atmospheric and telescope emission.
    TELEMISSION is a constant value for all bolometers. */

         flux = 0.01 * skytrans * astvalue + atmvalue + telemission;

/*  Add offset due to photon noise
    This is added to the scalar value FLUX. */

         if ( sinx.add_pns == 1 )
         {
            dsim_addpnoise ( inx.lambda, sinx.bandGHz, sinx.aomega, 
	      samptime, &flux, status );
            if ( dream_trace ( 4 ) )
            { 
               printf ( "scansim : photon noise added\n" );
               printf ( "status = %d\n", *status );
            }
         }

/* Add heater, assuming mean heater level is set to add onto meanatm and
   TELEMISSION to give targetpow */

         if ( sinx.add_hnoise == 1 )
         {
            flux = flux + 
	      ( inx.targetpow - sinx.meanatm - telemission ) * heater[bol];
         }
	 else
	 {
            flux = flux + ( inx.targetpow - sinx.meanatm - telemission );
         }


/*  Convert to current with bolometer power offset.
    The bolometer offset in PZERO(BOL) is added to the FLUX, and then
    the power in FLUX is converted to a current in scalar CURRENT with 
    help of the polynomial expression with coefficients in COEFFS(*) */

         if ( sinx.flux2cur == 1 )
         {
            dsim_ptoi ( flux, NCOEFFS, coeffs, pzero[bol], &current,
              status );
            if ( dream_trace ( 4 ) )
            { 
               printf ( "scansim : converted to current\n" );
               printf ( "status = %d\n", *status );
            }
         }
         else
         {
            current = flux;
         }

/*  Store the value. */

         output[frame] = current;

      }

/*  Now output[] contains the measured currents for a single bolometer during a
    single scan. */

/*  Apply time constant.
    The data is now smoothed with the impulse response function
    with coefficients in WEIGHTS.
    The data at the start of the scan is not entirely good because of the
    length of the smoothing function. */

      dsim_smooth ( DREAM__MXIRF, weights, nframes, output, status );  
      if ( dream_trace ( 3 ) )
      {
         printf ( "scansim : smoothed by impulse response\n" );
         printf ( "status = %d\n", *status );
      }
  

      if ( sinx.add_fnoise == 1 )
      {

/*  Add instrumental 1/f noise to the smoothed data in output */

         dsim_addinvf ( bol, start_time, samptime, nframes, nterms, 
           noisecoeffs, output, status );
      }


/*  NOTE: the order of applying FNOISE and the time constant should
    depend on details of the instrument - the current choice is
    arbitrary (corresponds to thermal bolometer time constant followed
    by 1/f in SQUID amplifiers). */

      for ( frame=0; frame<nframes; frame++ )
      {
         dbuf[frame*nboll+bol] = output[frame];
      }
   }

}



/*+ dsim_drand - return a random number with zero mean */

double dsim_drand
(
double sigma        /* sigma of distribution (given) */
)

/*  Description :
      Generate a double random number from a population with zero mean
      and given sigma and a bell-shaped distribution.
    Authors :
      B.D.Kelly (ROE)

     History :
      19July2001:  original (bdk@roe.ac.uk)
      10Aug2002 :  C version (bdk)
*/

{

   double value;        /* random number generated */
   double tvalue;       /* intermediate result */

/* obtain random number from integer function, adding three results
   together to shape the distribution */

   tvalue = (double)rand() + (double)rand() + (double)rand();

/* tvalue is now in the range 0 to 3*RAND_MAX with sigma 0.5*RAND_MAX */

   tvalue = 2.0 * ( ( tvalue / (double)RAND_MAX ) - 1.5 );

   value = sigma * tvalue;

   return value;
}


/*+ dsim_fft2d - 2-D FFT for double, square arrays */

void dsim_fft2d
(
int direction,       /* transform specification +1 or -1 (given) */
int size,            /* square dimension of complex image array (given) */
double *array,       /* image array size*2 by size (given and returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     The given array represents a square matrix [size][size] of complex
     numbers arranged so that each real value is followed by its
     imaginary partner.

     The algorithm is to transform each row, then rotate through 90
     degrees, then to transform each row.

     When going from data to frequency, the result leaves low frequencies
     at the corners of the 2-D transform.
*/

{
   int j;                 /* loop counter */
   
   
   if ( !StatusOkP(status) ) return;

/* first of all transform all the rows */

   for ( j=0; j<size; j++ )
   {
      dsim_four1 ( direction, size, &(array[j*2*size]) );
   }

   dsim_crotate ( direction, size, array, status );

/* Transform all the rows (formerly columns) */

   for ( j=0; j<size; j++ )
   {
      dsim_four1 ( direction, size, &(array[j*2*size]) );
   }

}



/*+ dsim_fitheat - fit a cubic polynomial to each bolometer */

void dsim_fitheat
(
int nboll,          /* number of bolometers (given) */
int nframes,        /* number of frames in scan (given) */
double *heat,       /* heater values (given) */
double *inptr,      /* measurement values (given) */
double *coptr,      /* coefficients of fit (returned) */
int *status         /* global status (given and returned) */
)

/* Description :
    Fit a cubic to the data set for each bolometer.
   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    19May2005 : original (bdk)
    10Jun2005 : use middle of range as zero point (bdk)
    18Aug2005 : put into dsim library from map library (bdk)
*/

{
   double coeff[4];    /* fit coefficients */
   int ncoeff;         /* number of fit coefficients */
   int i;              /* loop counter */
   int j;              /* loop counter */
   double *ht;         /* copy of heater settings */
   double *scan;       /* copy of scan for single bolometer */
   double var[4];      /* fit variances */
   FILE *fd;
   double t;

   if ( !StatusOkP(status) ) return;

/* provide default values */

   ncoeff = 4;

   for ( j=0; j<nboll; j++ )
   {
      for ( i=0; i<ncoeff; i++ )
      {
         coptr[j+i*nboll] = 0.0;
      }
   }

   if ( nframes > 10 )
   {
      scan = calloc ( nframes, sizeof(double) );
      ht = calloc ( nframes, sizeof(double) );

      for ( i=0; i<nframes; i++ )
      {
         ht[i] = heat[i] - heat[nframes/2];
      }

      for ( j=0; j<nboll; j++ )
      {

/* extract the values for one bolometer */

         for ( i=0; i<nframes; i++ )
         {
            scan[i] = inptr[nboll*i+j] - inptr[nboll*(nframes/2)+j];
         }

/* fit a cubic */

         sc2math_cubfit ( nframes, scan, ht, coeff, var, status );
         for ( i=0; i<4; i++ )
         {
            coptr[(i+2)*nboll+j] = coeff[i];
         }
         coptr[j] = heat[nframes/2];
         coptr[nboll+j] = inptr[nboll*(nframes/2)+j];

         if ( j == 740 )
         {
            fd = fopen ( "fit.txt", "w" );
            for ( i=0; i<nframes; i++ )
            {
               t = coptr[j]
                 + coptr[2*nboll+j]
                 + coptr[3*nboll+j] * scan[i]
                 + coptr[4*nboll+j] * scan[i] * scan[i]
                 + coptr[5*nboll+j] * scan[i] * scan[i] * scan[i];
               fprintf ( fd, "%e %e %e\n", inptr[nboll*i+j], heat[i], t );
            }
            fclose ( fd );
         }
      }

      free ( scan );
   }
}



/*+ dsim_four1 - Cooley-Tukey fft by Brenner */

void dsim_four1 
( 
int isign,         /* direction of transform (given) */
int nn,            /* number of complex values (given) */
double data[]      /* complex signal transformed in-place - even indices 
                      real values, odd imaginary (given and returned) */
)

/*  Description:
     1-D Fourier transform originally published in FORTRAN by Brenner 
     (see Mertz, Applied Optics vol 10 p386 1971).

    History :
     20aug2002 :  C version (bdk)
*/
{
   int m;
   int mmax;
   int n;
   int j;
   int i;
   int istep;
   double tempr;
   double tempi;
   double sinth;
   double wstpr;
   double wstpi;
   double wr;
   double wi;
   double theta;

   n = 2 * nn;
   j = 0;

   for ( i=0; i<n; i+=2 )
   {
      if( i < j )
      {
         tempr = data[j];
         tempi = data[j+1];
         data[j] = data[i];
         data[j+1] = data[i+1];
         data[i] = tempr;
         data[i+1] = tempi;
      }

      m = n / 2;

      while ( m >= 2)
      {
         if ( j < m ) break;
         j = j - m;
         m = m / 2;
      } 
      j = j + m;

   }
   mmax = 2;

   while ( mmax < n ) 
   {
      istep = 2 * mmax;
      theta = 6.28318530717959 / (double)( isign * mmax );
      sinth = sin ( theta / 2.0 );
      wstpr = -2.0 * sinth * sinth;
      wstpi = sin ( theta );
      wr = 1.0;
      wi = 0.0;
      for ( m=0; m<mmax; m+=2 )
      {
         for ( i=m; i<n; i+=istep )
         {
            j = i + mmax;
            tempr = wr * data[j] - wi * data[j+1];
            tempi = wr * data[j+1] + wi * data[j];
            data[j] = data[i] - tempr;
            data[j+1] = data[i+1] - tempi;
            data[i] = data[i] + tempr;
            data[i+1] = data[i+1] + tempi;
         }
         tempr = wr;
         wr = wr * wstpr - wi * wstpi + wr;
         wi = wi * wstpr + tempr * wstpi + wi;
      }
      mmax = istep;
   }
}



/*+ dsim_getast - sample simulated astronomical image */

void dsim_getast 
( 
double xpos,         /* X-Nas coordinate of sample point in arcsec (given) */
double ypos,         /* Y-Nas coordinate of sample point in arcsec (given) */
double bolscale,     /* bolometer scale in arcsec per pixel (given) */
double astscale,     /* scale of image in arcsec per pixel (given) */
int astsize,         /* size of image (given) */
double *astsim,      /* astronomical image (given) (RA/Dec) */
double *astvalue,    /* value sampled from image (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     Interpolate a value at the given position in an image.
     Use bilinear interpolation.
     This is for short-term convenience. Either sinc(x,y) or a J1 Bessel
     function ought to be used.
     Scale the result for the difference between the size of pixels in the
     simulated image and the size of detectors.
    Authors :
     E.Chapin (UBC)

     History :
      19July2001:  original (bdk@roe.ac.uk)
      10Aug2002 :  C version (bdk)
      24Jan2005 :  scale to bolometer pixel sizxe (bdk)
*/

{
  if ( !StatusOkP(status) ) return;
   
  dsim_getbilinear ( xpos, ypos, astscale, astsize, astsim, astvalue, 
		     status );
  *astvalue = (*astvalue) * bolscale * bolscale / ( astscale * astscale );
}

/*+ dsim_getast_wcs - sample simulated astronomical image using wcs info */

void dsim_getast_wcs
( 
int nboll,              /* total number of bolometers (given) */
double *xbolo,          /* x-bolometer coordinates for array (given) */
double *ybolo,          /* y-bolometer coordinates for array (given) */
AstCmpMap *bolo2map,    /* mapping bolo->sky image coordinates (given ) */
double *astsim,         /* astronomical image (given) */
long astnaxes[2],       /* dimensions of simulated image (given) */
double *dbuf,           /* pointer to bolo output (returned) */
int *status             /* global status (given and returned) */
)

/*  Description :
     Interpolate sky values for all bolometers at the current position
     in the astronomical input sky image.
     Use Ast mappings to handle bolometer <--> image pixel coordinates
     transformation.
    Authors :
     E.Chapin (UBC)

     History :
      28Feb2006 : original
      24Mar2006 : use astTranGrid instead of astTran2 
*/

{
  int i,j;                  /* Loop counter */
  int xnear;                /* Nearest-neighbour x-pixel coordinate */
  double *xsky;             /* x-sky image coordinates */
  int ynear;                /* Nearest-neighbour y-pixel coordinate */
  double *ysky;             /* y-sky image coordinates */
  double *skycoord;         /* x- and y- sky map pixel coordinates */
  int lbnd_in[2];           /* Pixel bounds for astRebin */
  int ubnd_in[2];
  int lbnd_out[2];
  int ubnd_out[2];

  if ( !StatusOkP(status) ) return;

  /* Slower astRebinD method ----------------------------------------- */

  /*
  lbnd_in[0] = 1;
  ubnd_in[0] = astnaxes[0];
  lbnd_in[1] = 1;
  ubnd_in[1] = astnaxes[1];

  lbnd_out[0] = 1;
  ubnd_out[0] = BOLROW;
  lbnd_out[1] = 1;
  ubnd_out[1] = BOLCOL;
  */

  /* Use astRebin to dump flux from the map into the bolometer array,
     so we need to invert the bolo->map mapping first */

  /*
  astInvert( bolo2map ); 
  
  astRebinD(bolo2map, 0.0,
	    2, lbnd_in, ubnd_in,
	    astsim,
	    NULL, 
	    AST__NEAREST, NULL, 0, 0.1, 1000000, 0,
	    2,lbnd_out,ubnd_out,
	    lbnd_in, ubnd_in,
	    dbuf, NULL);

  astInvert( bolo2map );

  */

  /* Faster asttran2 method ------------------------------------------ */

  /* Allocate space for arrays */
  
  /*
  xsky = (double *) calloc( nboll, sizeof(double) );
  ysky = (double *) calloc( nboll, sizeof(double) );
  */

  /* Transform bolo offsets into positions on the input sky image 
     NOTE: I'm still a bit confused about the way the 2d arrays
     are stored (x- vs. y- changing more rapidly), and I don't
     understand why the y- coordinate comes first in the call to
     astTran2! I arrived at this through trial and error... */
  
  /*
  astTran2( bolo2map, nboll, ybolo, xbolo, 1, xsky, ysky ); 
  */

  /* Nearest-neighbour sampling of image 
     Notes: -1 to account for FORTRAN array indices starting at 1, and
            +0.5 so that we round to the nearest pixel */
  /*
  for( i=0; i<nboll; i++ ) {
    
    xnear = (int) (xsky[i] - 1. + 0.5);
    ynear = (int) (ysky[i] - 1. + 0.5);
    
    if(	(xnear >= 0) && (xnear < astnaxes[0]) && 
	(ynear >= 0) && (ynear < astnaxes[1]) )
      dbuf[i] = astsim[xnear + astnaxes[0]*ynear];
    
    else dbuf[i] = 0;
  }
  
  free(xsky);
  free(ysky);
  */

  /* astTranGrid method ----------------------------------------- */

  /* Allocate space for arrays */

  skycoord = (double *) calloc( nboll*2, sizeof(double) );

  lbnd_in[0] = 1;
  ubnd_in[0] = BOLROW;
  lbnd_in[1] = 1;
  ubnd_in[1] = BOLCOL;

  /* Transform bolo offsets into positions on the input sky image */

  astTranGrid( bolo2map, 2, lbnd_in, ubnd_in, 0.1, 1000000, 1,
	       2, nboll, skycoord );

  /* Nearest-neighbour sampling of image 
     Notes: -1 to account for FORTRAN array indices starting at 1, and
            +0.5 so that we round to the nearest pixel */

  for( i=0; i<nboll; i++ ) {
    /* Fortran 2d array so stored by column rather than row! */
    xnear = (int) (skycoord[i] - 1. + 0.5);
    ynear = (int) (skycoord[nboll+i] - 1. + 0.5);
    
    if( (xnear >= 0) && (xnear < astnaxes[0]) && 
        (ynear >= 0) && (ynear < astnaxes[1]) )
      dbuf[i] = astsim[xnear + astnaxes[0]*ynear];
    
    else dbuf[i] = 0;
  }

  free(skycoord);
}

/*+ dsim_getbilinear - bilinear interpolation on an image */

void dsim_getbilinear 
( 
double xpos,         /* X-coordinate of sample point in arcsec (given) */
double ypos,         /* Y-coordinate of sample point in arcsec (given) */
double scale,        /* scale of image in arcsec per pixel (given) */
int size,            /* size of image (given) */
double *image,       /* astronomical image (given) */
double *value,       /* value sampled from image (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     Interpolate a value at the given position in an image.
     Use bilinear interpolation.
    Authors :
      B.D.Kelly (ROE)

     History :
      19July2001:  original (bdk@roe.ac.uk)
      10Aug2002 :  C version (bdk)
      11Jul2005 :  trap out-of-range indices (bdk)
*/

{
   double a;             /* image value near point */
   double b;             /* image value near point */
   double c;             /* image value near point */
   double d;             /* image value near point */
   double dx;            /* fractional pixel offset */
   double dy;            /* fractional pixel offset */
   int ixpix;            /* truncated pixel position */
   int iypix;            /* truncated pixel position */
   double xpix;          /* pixel position */
   double ypix;          /* pixel position */


   if ( !StatusOkP(status) ) return;

   xpix = xpos / scale;
   ypix = ypos / scale;
   ixpix = (int)xpix;
   iypix = (int)ypix;

   if ( ( ixpix > 0 ) && ( ixpix+1 < size ) &&
        ( iypix > 0 ) && ( iypix+1 < size ) )
   {
      a = image [ ixpix + size*iypix ];
      b = image [ ixpix+1 + size*iypix ];
      c = image [ ixpix + size*(iypix+1) ];
      d = image [ ixpix+1 + size*(iypix+1) ];
      dx = xpix - (double)ixpix;
      dy = ypix - (double)iypix;

      *value = a * ( 1.0 - dy - dx + dy * dx )
             + b * ( dx - dy * dx )
             + c * ( dy - dy * dx )
             + d * dy * dx;
   }
   else
   {
      *status = DITS__APP_ERROR;
      printf ( "dsim_getbilinear: data point outside image %d %d\n",
        ixpix, iypix );
   }

}


/*+ dsim_getbols - return characteristics of bolometers */

void dsim_getbols 
(
int numbols,             /* Number of bolometers (given) */
struct bolpix list[],    /* Pixel locations of bolometers (given) */
double distfac,          /* Distortion factor (given) */
double x[],              /* Array to hold X-coordinates of bolometers 
                            in arcsec (returned) */
double y[],              /* Array to hold Y-coordinates of bolometers 
                            in arcsec (returned) */
double pzero[],          /* Array to hold response curve offsets 
                            of bolometers in pW (returned) */
double heater[],         /* Array to hold heater factors of bolometers
                            (returned) */
int *status              /* global status (given and returned) */
)

/*   Description :
      Fill the given arrays with lists of the X, Y and calibration curve
      offsets of all the bolometers.
      Simulate a regular X-Y grid distorted by a simple distortion
      function. Use scaled random numbers for the response curve offsets.
      The heater factors are 1.0 with a 5% random spread, simulating a spread in
      the heater resistors of each bolometer.
      The "regular" grid is assumed to contain four subarrays, each
      (40,40), with a 1.5 pixel gap. The subarrays are indexed as follows:
 
      +---+---+
      | 2 | 3 |
      +---+---+
      | 0 | 1 |
      +---+---+

     Authors :
      B.D.Kelly (ROE)

     History :
      19July2001:  original (bdk@roe.ac.uk)
      20Aug2002 :  C version (bdk)
      17Oct2003 :  add heater (bdk)
*/

{

   int bol;                          /* bolometer counter */
   double r;                         /* undistorted distance */
   double rdist;                     /* distorted distance */
   double ri;                        /* grid position */
   double rj;                        /* grid position */
   double xzero[4];                  /* subarray grid zeros */
   double yzero[4];                  /* subarray grid zeros */


   if ( !StatusOkP(status) ) return;

/* Initialise the zero points for the undistorted subarray grids */

      xzero[0] = 0.0;
      yzero[0] = 0.0;

      xzero[1] = 41.5;
      yzero[1] = 0.0;

      xzero[2] = 0.0;
      yzero[2] = 41.5;

      xzero[3] = 41.5;
      yzero[3] = 41.5;

/* Convert the indices into regular grid positions */

   for ( bol=0; bol<numbols; bol++ )
   {
      ri = xzero[list[bol].quad] + (double)list[bol].x;
      rj = yzero[list[bol].quad] + (double)list[bol].y;

/* Calculate undistorted distance from distortion centre */

      r = ( ri - RIZERO ) * ( ri - RIZERO ) +
       ( rj - RJZERO ) * ( rj - RJZERO );
      r = sqrt(r);

/* Apply distortion stretch */

      rdist = r + distfac * r * r;

/* Calculate corresponding coordinates */

      x[bol] = RIZERO + rdist * ( ri - RIZERO ) / r;
      y[bol] = RJZERO + rdist * ( rj - RJZERO ) / r;

/* Apply scale factor to convert to arcsec */

      x[bol] = x[bol] * COUNTTOSEC;
      y[bol] = y[bol] * COUNTTOSEC;
   }

/* Produce some power offsets, zero mean, sigma = 1pW */

   for (bol=0; bol<numbols; bol++ )
   {
      pzero[bol] = dsim_drand ( 1.0 );
   }

/* Produce some heater input ratios, mean=1.0, sigma=0.05 */

   for (bol=0; bol<numbols; bol++ )
   {
      heater[bol] = 1.0 + dsim_drand ( 0.05 );
   }

}


/*+ dsim_getinvf - return a compressed form of 1/f noise */

void dsim_getinvf 
( 
double sigma,        /* dispersion of broad-band noise (given) */ 
double corner,       /* corner frequency, where 1/f dispersion=sigma (given)*/
double samptime,     /* time per data sample (given) */
double nterms,       /* number of frequencies calculated (given) */
double *noisecoeffs, /* 1/f spectrum (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     The lowest few Fourier components of a 1/f noise simulation are calculated
     and returned.
     Each component is described by its period and its cosine and sine
     amplitudes.
    Authors :
     B.D.Kelly (ROE)

    History :
     23Jan2004:  original (bdk)
     29Jun2005:  extend up to twice the corner frequency (bdk)
*/

{
   double deltanu;    /* frequency interval */
   int j;             /* LOOP COUNTER */
   double noise;      /* dispersion at a frequency */
   double nu;         /* frequency */
   

   if ( !StatusOkP(status) ) return;

/* Calculate noise amplitudes at even frequency intervals up to twice the
   corner frequency, ignoring zero frequency */

   deltanu = 2.0 * corner / (double)nterms;
   for ( j=0; j<nterms; j++ )
   {
      nu = deltanu * (double)( 1 + j );
      noise = sigma * corner / nu;
      noisecoeffs[3*j] = 1.0 / nu;
      noisecoeffs[3*j+1] = dsim_drand ( noise );
      noisecoeffs[3*j+2] = dsim_drand ( noise );
   }
}


/*+ dsim_getobsmode - calculate obs enumerated type from string */

obsMode dsim_getobsmode
( 
char *name,         /* string containing name of observing mode */
int *status         /* global status (given and returned) */
)

/* Description :
   Given a string for the observation return the enumerated value
   as defined in dsim_struct.h. Valid modes:
   stare, dstare, dream, pong, polspin, heatrun

   Returns none if the string could not be matched to a valid type

   Authors :
    Ed Chapin (echapin@phas.ubc.ca)

   History :
    28Mar2006 : Original version (echapin)
*/

{

  if ( !StatusOkP(status) ) return;

  if( strcmp( name, "STARE" ) == 0 ) return stare;
  else if( strcmp( name, "DSTARE" ) == 0 ) return dstare;
  else if( strcmp( name, "DREAM" ) == 0 ) return dream;
  else if( strcmp( name, "PONG" ) == 0 ) return pong;
  else if( strcmp( name, "POLSPIN" ) == 0 ) return polspin;
  else if( strcmp( name, "HEATRUN" ) == 0 ) return heatrun;
  else return none;

}


/*+ dsim_getpar - Get parameters from arguments */

void dsim_getpar 
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file (returned) */
int *rseed,              /* seed for random number generator (returned)*/
int *savebols,           /* flag for writing bolometer details (returned) */
int *status              /* global status (given and returned) */
)

/*  Description :
     Get and interpret parameters.

    Authors :
     H.W. van Someren Greve (ASTRON)

    History :
     8November2001:  original (greve@astron.nl)
     21Aug2002    :  C version (bdk)
     26Jun2003    :  introduce XML file specifying simulation (bdk)
     19Aug2003    :  get parameters from arguments (bdk)
     13May2005    :  remove DREAM-specific checks (bdk)
*/

{
   char obs_name[132];            /* XML observation file name */
   char sim_name[132];            /* XML simulation file name */

   int dlength;                   /* length of date string */
   int tlength;                   /* length of time string */
   char cur_day[16];              /* date string */ 
   char cur_time[16];             /* time string */ 
   char tname[132];               /* temporary copy of file name */

   if ( !StatusOkP(status) ) return;

   dlength = 16;
   tlength = 16;

   strcpy ( tname, argv[1] );
   strcpy ( obs_name, getenv ( "SMURF_SIM_XML" ) );
   strcat ( obs_name, "/" );
   strcat ( obs_name, tname );

   strcpy ( tname, argv[2] );
   strcpy ( sim_name, getenv ( "SMURF_SIM_XML" ) );
   strcat ( sim_name, "/" );
   strcat ( sim_name, tname );

   *rseed = atoi ( argv[3] );
   *savebols = 0;

   if ( argc == 6 )
   {
      if ( ( strcmp ( "t", argv[5] ) == 0 ) ||
           ( strcmp ( "T", argv[5] ) == 0 ) )
      {
         *savebols = 1;
      }
   }


/*  Read all parameters from the scuba_definition file */

   dxml_readXML ( obs_name, status );
   dxml_returnXML ( inx, status );
   dxml_readsimXML ( sim_name, status );
   dxml_returnsimXML ( sinx, status );

/* Expand file names */

   strcpy ( tname, sinx->astname );
   strcpy ( sinx->astname, getenv ( "SMURF_SIM_IN" ) );
   strcat ( sinx->astname, "/" );
   strcat ( sinx->astname, tname );

   strcpy ( tname, sinx->atmname );
   strcpy ( sinx->atmname, getenv ( "SMURF_SIM_IN" ) );
   strcat ( sinx->atmname, "/" );
   strcat ( sinx->atmname, tname );

   strcpy ( tname, inx->bolfile );
   strcpy ( inx->bolfile, getenv ( "SMURF_SIM_IN" ) );
   strcat ( inx->bolfile, "/" );
   strcat ( inx->bolfile, tname );

   dream_timenow( dlength, tlength, 0, cur_day, cur_time, NULL, NULL, status );

}



/*+ dsim_getpat - return jiggle pattern */

void dsim_getpat 
(
int nvert,            /* Number of vertices per pattern (given) */
int smu_samples,      /* number of samples between vertices (given) */
double sample_t,      /* time between data samples in msec (given) */
double smu_offset,    /* smu timing offset in msec (given) */
int conv_shape,       /* choice of convolution function (given) */
double conv_sig,      /* convolution parameter (given) */
int move_code,        /* SMU waveform choice (given) */
double jig_stepx,     /* X interval in arcsec (given) */
double jig_stepy,     /* Y interval in arcsec (given) */
int jig_vert[][2],    /* Array with relative jiggle coordinates in units of
                         jiggle steps in case jiggle positions are 
                         visited (given) */

int *cycle_samples,   /* The number of samples per cycle (returned) */

double pattern[][2],  /* The array to hold the coordinates of the jiggle 
                         offsets in arcsec. There are cycle_samples entries 
                         filled. (returned) */

int *status           /* global status (given and returned) */
)

/*  Description :
     Fill the given arrays with lists of the X, Y offsets for a jiggle 
     pattern.

    Authors :
     H.W. van Someren Greve (ASTRON)


    History :
     8November2001:  original (greve@astron.nl)
     20Aug2002 :  C version (bdk)
*/

{
   int j;
   double frac;
   double vertex_t;    /* time interval between jiggle vertices in msec */

   if ( !StatusOkP(status) ) return;

/*  The implemented codes for the Jiggle Pattern are :
    nvert =1 : Circular pattern with 256 sampls.
          =8 : DREAM 8-pixel pattern. */

   if ( nvert == 0 ) 
   {

/* Generate a 256 point circular jiggle pattern, 2.5 pixels in diameter */

      frac = 2.0 * AST__DPI / 256.0;
      *cycle_samples = 256;
      if ( *cycle_samples > DREAM__MXSIM )
      {
         *status = DITS__APP_ERROR;
         printf ( "GETPAT: cycle_samples too large, increase DREAM__MXSIM\n" );
         return;
      }
      
      for ( j=0; j<*cycle_samples; j++ )
      {
         pattern[j][0] = COUNTTOSEC * 1.25 * cos ( frac * (double)(j-1) );
	 pattern[j][1] = COUNTTOSEC * 1.25 * sin ( frac * (double)(j-1) );
      }
   }
   else
   {

/* Calculate positions corresponding to data samples */

      *cycle_samples = nvert * smu_samples;
      if ( *cycle_samples > DREAM__MXSIM )
      {
         *status = DITS__APP_ERROR;
         printf ( "GETPAT: cycle_samples too large, increase DREAM__MXSIM\n" );
         return;
      }

      vertex_t = sample_t * smu_samples;
      dream_smupath ( nvert, vertex_t, jig_vert, jig_stepx, 
        jig_stepy, move_code, smu_samples, sample_t, smu_offset,
        *cycle_samples, pattern, status );

   }

   if ( dream_trace(1) )
   {
      printf ( "DSIM_GETPAT : %d Sample positions established\n",
        *cycle_samples );
   }
}     



/*+ dsim_getpatpar - Get parameters for pattern calculation */

void dsim_getpatpar 
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
char *outfile,           /* name of output text file (returned) */
int *status              /* global status (given and returned) */
)

/*  Description :
     Get and interpret parameters.

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     10Feb2004:  original (bdk)
*/

{
   char obs_name[132];            /* XML observation file name */

   char tname[132];               /* temporary copy of file name */


   if ( !StatusOkP(status) ) return;


/* XML observation file" */

   strcpy ( tname, argv[1] );
   strcpy ( obs_name, getenv ( "SMURF_SIM_XML" ) );
   strcat ( obs_name, "/" );
   strcat ( obs_name, tname );

/* Output text file file */

   strcpy ( tname, argv[2] );
   strcpy ( outfile, getenv ( "SMURF_SIM_OUT" ) );
   strcat ( outfile, "/" );
   strcat ( outfile, tname );


/*  Read all parameters from the scuba_definition file */

   dxml_readXML ( obs_name, status );

   dxml_returnXML ( inx, status );


/* Expand file names */


   strcpy ( tname, inx->bolfile );
   strcpy ( inx->bolfile, getenv ( "SMURF_SIM_IN" ) );
   strcat ( inx->bolfile, "/" );
   strcat ( inx->bolfile, tname );

}



/*+ dsim_getpointpar - Get parameters for point simulator */

void dsim_getpointpar 
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file (returned) */
int *rseed,              /* seed for random number generator (returned)*/
double *fluxJy,          /* source flux in Jansky (returned) */
int *mapwidth,           /* width of created map in pixels (returned) */
double *pixsize,         /* pixel size in arcsec (returned) */
int *spacing,            /* point source spacing in arcsec (returned) */
int *status              /* global status (given and returned) */
)

/*  Description :
     Get and interpret parameters from command line.

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     11Nov2003:  original (bdk)
*/

{
   int i4;
   int nspc;
   char obs_name[132];            /* XML observation file name */
   char sim_name[132];            /* XML simulation file name */

   int dlength;                   /* length of date string */
   int tlength;                   /* length of time string */
   char tname[132];               /* temporary copy of file name */


   if ( !StatusOkP(status) ) return;

   if ( dream_trace ( 3 ) )
   {
       printf ( "dsim_getpointpar : entered routine\n" );
   }

   dlength = 16;
   tlength = 16;

   strcpy ( tname, argv[1] );
   strcpy ( obs_name, getenv ( "SMURF_SIM_XML" ) );
   strcat ( obs_name, "/" );
   strcat ( obs_name, tname );

   strcpy ( tname, argv[2] );
   strcpy ( sim_name, getenv ( "SMURF_SIM_XML" ) );
   strcat ( sim_name, "/" );
   strcat ( sim_name, tname );

   *rseed = atoi ( argv[3] );
   *fluxJy = atof ( argv[4] );
   *mapwidth = atoi ( argv[5] );
   *pixsize = atoi ( argv[6] );
   *spacing = atof ( argv[7] );

   if ( dream_trace ( 3 ) )
   {
       printf ( "dsim_getpointpar : Got command-line parameters\n" );
   }


/*  Read all parameters from the scuba_definition file */

   dxml_readXML ( obs_name, status );

   dxml_returnXML ( inx, status );
   
   if ( dream_trace ( 3 ) )
   {
       printf ( "dsim_getpointpar : Got observation parameters\n" );
   }
   
   dxml_readsimXML ( sim_name, status );

   dxml_returnsimXML ( sinx, status );

   if ( dream_trace ( 3 ) )
   {
       printf ( "dsim_getpointpar : Got simulation parameters\n" );
   }
   
   
/* Expand file names */

   strcpy ( tname, sinx->astname );
   strcpy ( sinx->astname, getenv ( "SMURF_SIM_IN" ) );
   strcat ( sinx->astname, "/" );
   strcat ( sinx->astname, tname );

   strcpy ( tname, sinx->atmname );
   strcpy ( sinx->atmname, getenv ( "SMURF_SIM_IN" ) );
   strcat ( sinx->atmname, "/" );
   strcat ( sinx->atmname, tname );

   strcpy ( tname, inx->bolfile );
   strcpy ( inx->bolfile, getenv ( "SMURF_SIM_IN" ) );
   strcat ( inx->bolfile, "/" );
   strcat ( inx->bolfile, tname );

/* Overwrite dataname with output filename */

   strcpy ( inx->dataname, sinx->astname );


   if ( inx->nvert == 0 )
   {
      nspc = 256;
   }
   else
   {
      nspc = inx->nvert * inx->smu_samples;
   }
   i4 = nspc * ( sinx->ncycle + 1 );

}



/*+ dsim_getpong - Get coordinates of full PONG pattern */

void dsim_getpong
(
double angle,        /* angle of pattern relative to telescope
                        axes in radians anticlockwise (given) */
int gridcount,       /* number of grid lines (odd) (given) */
double spacing,      /* grid spacing in arcsec (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
double samptime,     /* sample interval in sec (given) */
double grid[][2],    /* pong grid coordinates (returned) */
int *pongcount,      /* number of positions in pattern (returned) */
double **posptr,     /* list of positions (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     The grid count is an odd number. eg 7 implies the generating grid is
     7x7. The total number of vertices is 2*gridcount-1. The pattern
     generated is as follows (starting middle right, ending bottom centre):

                +--+--0->0--+--+--+
                |  |  |  |  |  |  |
                +--0->+--+--0--+--+
                |  |  |  |  |  |  |
                0->+--+--+--+--0--+
                |  |  |  |  |  |  |
                0--+--+--+--+--+<-0
                |  |  |  |  |  |  |
                +--0--+--+--+<-0--+
                |  |  |  |  |  |  |
                +--+--0--+<-0--+--+
                |  |  |  |  |  |  |
                +--+--+--0--+--+--+

     The grid coordinates generated have (0,0) at the pattern centre.

     The pattern is rotated through the given angle to get coordinates
     related to the axes of telescope motion. The telescope motion along
     the pattern is then computed and the positions at samptime intervals
     recorded. 

     It is assumed that a sample instant coincides with the start of each
     straight line section, which is a reasonable approximation as the
     telescope has to be "stationary" there.

     Effects of "jerk" - ie time for change in acceleration, or other
     telescope inertia effects are ignored.

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     07Jul2005:  original (bdk)
*/

{
   double cend[2];          /* ending coordinates in arcsec */
   double cstart[2];        /* starting coordinates in arcsec */
   int curroff;             /* index of next free slot in position list */
   int j;                   /* loop counter */
   double tx;               /* temporary X coordinate */
   double ty;               /* temporary Y coordinate */


   if ( !StatusOkP(status) ) return;

   dsim_getpongends ( gridcount, spacing, grid, status );

/* Rotate the grid coordinates */

   for ( j=0; j<gridcount*2-1; j++ )
   {
      tx = grid[j][0] * cos(angle) - grid[j][1] * sin(angle);
      ty = grid[j][0] * sin(angle) + grid[j][1] * cos(angle);
      grid[j][0] = tx;
      grid[j][1] = ty;
   }


   (*pongcount) = 0;

   for ( j=0; j<gridcount*2-2; j++ )
   {
      cstart[0] = grid[j][0];
      cstart[1] = grid[j][1];
      cend[0] = grid[j+1][0];
      cend[1] = grid[j+1][1];
      dsim_getscansegsize ( samptime, cstart, cend, accel, vmax, &curroff,
        status );
      (*pongcount) += curroff;
   }

   *posptr = (double *) calloc ( (*pongcount)*2, sizeof(double) );

   curroff = 0;

   for ( j=0; j<gridcount*2-2; j++ )
   {
      cstart[0] = grid[j][0];
      cstart[1] = grid[j][1];
      cend[0] = grid[j+1][0];
      cend[1] = grid[j+1][1];
      dsim_getscanseg ( samptime, cstart, cend, accel, vmax, *pongcount,
        &curroff, *posptr, status );
   }

}



/*+ dsim_getpongends - Get coordinates of PONG vertices */

void dsim_getpongends
(
int gridcount,           /* number of grid lines (odd) (given) */
double spacing,          /* grid spacing in arcsec (given) */
double grid[][2],        /* coordinates of pong vertices (returned) */
int *status              /* global status (given and returned) */
)

/*  Description :
     The grid count is an odd number. eg 7 implies the generating grid is
     7x7. The total number of vertices is 2*gridcount-1. The pattern
     generated is as follows (starting middle right, ending bottom centre):

                +--+--0->0--+--+--+
                |  |  |  |  |  |  |
                +--0->+--+--0--+--+
                |  |  |  |  |  |  |
                0->+--+--+--+--0--+
                |  |  |  |  |  |  |
                0--+--+--+--+--+<-0
                |  |  |  |  |  |  |
                +--0--+--+--+<-0--+
                |  |  |  |  |  |  |
                +--+--0--+<-0--+--+
                |  |  |  |  |  |  |
                +--+--+--0--+--+--+

    The grid coordinates generated have (0,0) at the pattern centre.

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     05Jul2005:  original (bdk)
     07Jul2005:  change name to getpongends (bdk)
*/

{
   int g;               /* integer grid distance */
   double gridpos;      /* scaled grid position */
   int j;               /* loop counter */
   int k;               /* alternating sign */


   if ( !StatusOkP(status) ) return;

   k = -1;

   for ( j=0; j<gridcount-1; j++ )
   {
      g = k * ( ( j + 1 ) / 2 );
      gridpos = spacing * (double)g ;
      grid[2*j][1] =  gridpos;
      grid[2*j+1][1] = gridpos;
      grid[(gridcount-1)*2-(2*j+1)][0] = -gridpos;
      grid[(gridcount-1)*2-2*j][0] = -gridpos;
      k *= -1;
   }
   grid[gridcount*2-2][1] = -grid[gridcount*2-3][1];
   grid[0][0] = -grid[1][0];
}



/*+ dsim_getscaling - Get parameters for scaling data to integers */

void dsim_getscaling 
(
int ncoeffs,          /* number of coefficients describing response curve
                         (given) */
double coeffs[],      /* array to hold response curve coefficients (given) */
double targetpow,     /* target power level in pW (given) */
double photonsigma,   /* photon noise level (given) */
double *digmean,      /* mean digitised level (returned) */
double *digscale,     /* digitisation scale factor (returned) */
double *digcurrent,   /* current in amps at digmean (returned) */
int *status           /* global status (given and returned) */
)

/*  Description :
     The simulated bolometer current is to be "digitised" using the formula

      digval = int ( 0.5 + ( current - digcurrent ) * digscale + digmean )

     where the three digitisation parameters are chosen so that the
     current corresponding to the target power level falls in the middle
     of a 24-bit range, and one digitisation level is one-sixth of the
     photon noise dispersion (this assumes the digitisation can encode
     instrumental noise, and the latter is about 1/3 of photon noise).

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     23Sep2004 :  original (bdk)
     10Jun2005 :  return fixed values as experiment to guarantee
                  consistent flat-fielding (bdk)
*/

{
   double currentsigma;       /* photon noise current equivalent */
   double meanplus;           /* current matching targetpow plus noise */


   if ( !StatusOkP(status) ) return;


   dsim_ptoi ( targetpow, ncoeffs, coeffs, 0.0, digcurrent, status );
   dsim_ptoi ( targetpow+photonsigma, ncoeffs, coeffs, 0.0, &meanplus,
     status );
   currentsigma = fabs ( meanplus - (*digcurrent) );

   *digscale = 6.0 / currentsigma;
   *digmean = (double) 8388608;

/* replace by fixed values */

   *digcurrent = 1.333301e-05;
/* experiment
   *digscale = 4.655870e+10;
*/
   *digscale = 4.655870e+11;
}



/*+ dsim_getscanpat - return scan pattern */

void dsim_getscanpat 
(
long astnaxes[2],    /* dimensions of simulated image (given) */
double astscale,     /* pixel size in simulated image (given) */
int nboll,           /* total number of bolometers (given) */
double *xbc,         /* projected X offsets of bolometers in arcsec (given) */
double *ybc,         /* projected Y offsets of bolometers in arcsec (given) */
int *ncoords,        /* The number of coordinates in pattern (returned) */
double pattern[][2], /* The array to hold the coordinates of the ends of all
                        the scans relative to the bottom-left of the mapped
			area in arcsec (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :

     +---+---+---+---+---+---+---+
     | Y |   |   |   |   |   |   |
     +---+---+---+---+---+---+---+
     |   |                   |   |
     +---+                   +---+
     |   |                   |   |
     +---+                   +---+
     |   |                   | X |
     +---+                   +---+
     |   |                   |   |
     +---+                   +---+
     |   |                   |   |
     +---+---+---+---+---+---+---+
     | X |   |   | Y |   |   |   |
     +---+---+---+---+---+---+---+

     The diagram shows the footprint of the array, neglecting distortion,
     mapping a square boundary close to (but inside) the edge of the simulated
     astronomical field.
     
     An odd-integral number of footprints are calculated in X and Y
     representing the ends of scans wholly contained within the map. The
     squares marked with X and Y show examples of the ends of "X" and "Y"
     scans respectively.

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)


    History :
     27Jan2004 :  original (bdk)
*/

{
   double footprint;  /* smallest array extent in X or Y */
   int j;
   double sizemap;    /* extent of astronomical map in arcsec */
   double xmax;       /* maximum coordinate offset in array */
   double xmin;       /* minimum coordinate offset in array */
   double ymax;       /* maximum coordinate offset in array */
   double ymin;       /* minimum coordinate offset in array */

   if ( !StatusOkP(status) ) return;

/* Choose characteristic dimension of the array footprint */

   xmin = xbc[0];
   ymin = ybc[0];
   xmax = xmin;
   ymax = ymin;
   for ( j=0; j<nboll; j++ )
   {
      if ( xbc[j] < xmin ) xmin = xbc[j];
      if ( xbc[j] > xmax ) xmax = xbc[j];
      if ( ybc[j] < ymin ) ymin = ybc[j];
      if ( ybc[j] > ymax ) ymax = ybc[j];
   }
   footprint = xmax - xmin;
   if ( footprint > (ymax-ymin) ) footprint = ymax-ymin;
   
/* The area to be mapped is square, find an odd-integral multiple of the
   footprint size to fit inside the map - ensure a couple of map pixels
   spare at both ends of a scan */

   sizemap = ( astnaxes[0] - 4 ) * astscale;
   *ncoords = (int) ( sizemap / footprint );
   if ( ( *ncoords & 1 ) == 0 )
   {
      *ncoords -= 1;
   }

/* Generate the list of coordinates for the pattern which place the bolometer
   with lowest coordinates 2 sky pixels inside the simulation */

   for ( j=0; j<*ncoords; j++ )
   {
      pattern[j][0] = 2.0 * astscale - xmin + footprint * (double)j;
      pattern[j][1] = 2.0 * astscale - ymin + footprint * (double)j;
   }
}



/*+ dsim_getscanseg - return scan segment */

void dsim_getscanseg
(
double samptime,     /* sample time in sec (given) */
double cstart[2],    /* starting coordinates in arcsec (given) */
double cend[2],      /* ending coordinates in arcsec (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
int maxoff,          /* maximum total of offsets in pattern (given) */
int *curroff,        /* current offset in pattern (given and returned) */
double *pattern,     /* pointing coordinates in arcsec (given and returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     Add pointing positions to the end of the list of positions being
     built up. The positions added belong on a straight-line segment. The
     telescope is accelerated and decelerated at the ends of the segment,
     and travels at the maximum velocity (if reached) in the centre.
    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     29Jun2005 :  original (bdk)
*/

{
   double c0;          /* distance in coordinate 0 */
   double c1;          /* distance in coordinate 1 */
   double dtime;       /* time at start of deceleration */
   double eps;         /* small number to trap angles */
   int j;              /* loop counter */
   int jdec;           /* count at start of deceleration */
   int jend;           /* count at end of scan */
   int jmax;           /* count at maximum velocity */
   int jmid;           /* count near midway */
   double midway;      /* midway position along scan */
   double theta;       /* angle of path relative to coordinate [0] */
   double raccel;      /* acceleration along path */
   double rmax;        /* distance along path when max velocity reached */
   double rmaxvel;     /* max velocity along path */
   double tdec;        /* time at start of deceleration */
   double tmaxvel;     /* max velocity along path */
   double tmidway;     /* time at mid way */
   double tsq;         /* time squared */
   double vmidway;     /* velocity at mid way */
   
   
   if ( !StatusOkP(status) ) return;


   eps = 0.001;

/* Determine angle of motion */

   theta = atan2 ( ( cend[1] - cstart[1] ), ( cend[0] - cstart[0] ) );

/* Check for motion parallel to one of the axes */

   if ( fabs ( theta ) < eps ) 
   {
       raccel = accel[0];
       rmaxvel = vmax[0];
   }
   else if ( fabs ( fabs(theta)-AST__DPI ) < eps )
   {
       raccel = accel[0];
       rmaxvel = vmax[0];
   }
   else if ( fabs ( theta - PIBY2 ) < eps )
   {
       raccel = accel[1];
       rmaxvel = vmax[1];
   }
   else if ( fabs ( theta + PIBY2 ) < eps )
   {
       raccel = accel[1];
       rmaxvel = vmax[1];
   }
   else
   {
      raccel = fabs ( accel[0] / cos(theta) );
      if ( raccel > fabs( accel[1] / sin(theta) ) )
      { 
         raccel = fabs ( accel[1] / sin(theta) );
      }
      rmaxvel = fabs ( vmax[0] / cos(theta) );
      if ( rmaxvel > fabs ( vmax[1] / sin(theta) ) ) 
      {
         rmaxvel = fabs ( vmax[1] / sin(theta) );
      }
   }

/* Determine whether the maximum velocity is reached before mid-way */

   c0 = cend[0] - cstart[0];
   c1 = cend[1] - cstart[1];
   midway = 0.5 * sqrt ( c0 * c0 + c1 * c1 );

   tmidway = sqrt ( fabs ( 2.0 * midway / raccel ) );
   vmidway = fabs(raccel) * tmidway;
   jmid = (int) ( tmidway / samptime );
   
   if ( vmidway > fabs(rmaxvel) )
   {

/* Need to accelerate, coast, then decelerate */

      tmaxvel = fabs ( rmaxvel / raccel );
      jmax = (int) ( tmaxvel / samptime );
      rmax  = 0.5 * raccel * tmaxvel * tmaxvel;
      tmidway = tmaxvel + fabs ( ( midway - rmax ) / rmaxvel );
      tdec = 2.0 * tmidway - tmaxvel;
      jdec = (int) ( tdec / samptime );
      jend = (int) ( 2.0 * tmidway / samptime );

      if ( ( (*curroff) + jend ) < maxoff )
      {

/* Accelerate to the maximum velocity */

         for ( j=0; j<=jmax; j++ )
         {
            tsq = (double)j * samptime;
            tsq = tsq * tsq;
            pattern[((*curroff)+j)*2] = cstart[0] + 
	      0.5 * raccel * tsq * cos(theta);
            pattern[((*curroff)+j)*2+1] = cstart[1] + 
	      0.5 * raccel * tsq * sin(theta);
         }

/* Constant velocity past mid way to deceleration zone */

         for ( j=jmax+1; j<=jdec; j++ )
         {
            dtime = (double) ( j-jmax-1) * samptime;
            pattern[((*curroff)+j)*2] = cstart[0] + 
              ( rmax + rmaxvel * dtime ) * cos(theta);
            pattern[((*curroff)+j)*2+1] = cstart[1] + 
              ( rmax + rmaxvel * dtime ) * sin(theta);
         }

/* Deceleration */

         for ( j=jdec+1; j<=jend; j++ )
         {
            tsq = 2.0 * tmidway - (double)j * samptime;
	    tsq = tsq * tsq;
            pattern[((*curroff)+j)*2] = cend[0] - 
	      0.5 * raccel * tsq * cos(theta);
            pattern[((*curroff)+j)*2+1] = cend[1] - 
	      0.5 * raccel * tsq * sin(theta);
         }
         (*curroff) += jend + 1;
      }
      else
      {
         *status = DITS__APP_ERROR;
      }
   }
   else
   {

/* Accelerate all the way to the midway point */

      jend = (int) ( 2.0 * tmidway / samptime );

      if ( ( (*curroff) + jend ) < maxoff )
      {
         for ( j=0; j<=jmid; j++ )
         {
            tsq = (double)j * samptime;
            tsq = tsq * tsq;
            pattern[((*curroff)+j)*2] = cstart[0] + 
	      0.5 * raccel * tsq * cos(theta);
            pattern[((*curroff)+j)*2+1] = cstart[1] + 
	      0.5 * raccel * tsq * sin(theta);
         }
         for ( j=jmid+1; j<=jend; j++ )
         {
            tsq = 2.0 * tmidway - (double)j * samptime;
	    tsq = tsq * tsq;
	    pattern[((*curroff)+j)*2] = cend[0] -
	      0.5 * raccel * tsq * cos(theta);
	    pattern[((*curroff)+j)*2+1] = cend[1] -
	      0.5 * raccel * tsq * sin(theta);
         }
         (*curroff) += jend + 1;
      }
      else
      {
         *status = DITS__APP_ERROR;
      }
   }
}



/*+ dsim_getscansegsize - return size of scan segment */

void dsim_getscansegsize
(
double samptime,     /* sample time in sec (given) */
double cstart[2],    /* starting coordinates in arcsec (given) */
double cend[2],      /* ending coordinates in arcsec (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
int *size,           /* number of samples in pattern (returned) */
int *status          /* global status (given and returned) */
)

/*  Description :
     Calculate the number of positions belonging on a straight-line scan
     segment. The telescope is accelerated and decelerated at the ends of
     the segment, and travels at the maximum velocity (if reached) in the
     centre.
    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     07Jul2005 :  original (bdk)
*/

{
   double c0;          /* distance in coordinate 0 */
   double c1;          /* distance in coordinate 1 */
   double eps;         /* small number to trap angles */
   int jend;           /* count at end of scan */
   int jmax;           /* count at maximum velocity */
   double midway;      /* midway position along scan */
   double theta;       /* angle of path relative to coordinate [0] */
   double raccel;      /* acceleration along path */
   double rmax;        /* distance along path when max velocity reached */
   double rmaxvel;     /* max velocity along path */
   double tmaxvel;     /* max velocity along path */
   double tmidway;     /* time at mid way */
   double vmidway;     /* velocity at mid way */
   
   
   if ( !StatusOkP(status) ) return;


   eps = 0.001;

/* Determine angle of motion */

   theta = atan2 ( ( cend[1] - cstart[1] ), ( cend[0] - cstart[0] ) );

/* Check for motion parallel to one of the axes */

   if ( fabs ( theta ) < eps ) 
   {
       raccel = accel[0];
       rmaxvel = vmax[0];
   }
   else if ( fabs ( fabs(theta)-AST__DPI ) < eps )
   {
       raccel = accel[0];
       rmaxvel = vmax[0];
   }
   else if ( fabs ( theta - PIBY2 ) < eps )
   {
       raccel = accel[1];
       rmaxvel = vmax[1];
   }
   else if ( fabs ( theta + PIBY2 ) < eps )
   {
       raccel = accel[1];
       rmaxvel = vmax[1];
   }
   else
   {
      raccel = fabs ( accel[0] / cos(theta) );
      if ( raccel > fabs( accel[1] / sin(theta) ) )
      { 
         raccel = fabs ( accel[1] / sin(theta) );
      }
      rmaxvel = fabs ( vmax[0] / cos(theta) );
      if ( rmaxvel > fabs ( vmax[1] / sin(theta) ) ) 
      {
         rmaxvel = fabs ( vmax[1] / sin(theta) );
      }
   }

/* Determine whether the maximum velocity is reached before mid-way */

   c0 = cend[0] - cstart[0];
   c1 = cend[1] - cstart[1];
   midway = 0.5 * sqrt ( c0 * c0 + c1 * c1 );

   tmidway = sqrt ( fabs ( 2.0 * midway / raccel ) );
   vmidway = fabs(raccel) * tmidway;
   
   if ( vmidway > fabs(rmaxvel) )
   {

/* Need to accelerate, coast, then decelerate */

      tmaxvel = fabs ( rmaxvel / raccel );
      jmax = (int) ( tmaxvel / samptime );
      rmax  = 0.5 * raccel * tmaxvel * tmaxvel;
      tmidway = tmaxvel + fabs ( ( midway - rmax ) / rmaxvel );
      jend = (int) ( 2.0 * tmidway / samptime );
   }
   else
   {

/* Accelerate all the way to the midway point then decelerate to the end */

      jend = (int) ( 2.0 * tmidway / samptime );
   }

   *size = jend + 1;

}



/*+ dsim_getsigma - Calculate photon noise */

void dsim_getsigma
(
double lambda,         /* wavelength in metres (given) */
double bandGHz,        /* bandwidth in GHz (given) */
double aomega,         /* geometrical factor (given) */
double flux,           /* sky power per pixel in pW (given) */
double *sigma,         /* photon noise in pW (returned) */
int *status            /* global status (given and returned) */
)

{

   double coherence;   /* spatial coherence of the beam */

   if ( !StatusOkP(status) ) return;

   coherence = ( 1.0 + exp ( -aomega ) ) / ( 2.0 + aomega );
   *sigma = 1.0e12 * sqrt ( 2.0 * flux * 1.0e-12 * H * C / lambda 
   + flux * flux * 1.0e-24 * coherence / ( bandGHz * 1.0e9 ) );
}



/*+ dsim_getskypar - Get parameters for point simulator */

void dsim_getskypar 
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file (returned) */
int *rseed,              /* seed for random number generator (returned)*/
double *exponent,        /* exponent of power spectrum (returned) */
int *status              /* global status (given and returned) */
)

/*  Description :
     Get and interpret parameters from command line.

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     09Dec2003:  original (bdk)
*/

{
   int i4;
   int nspc;
   char obs_name[132];            /* XML observation file name */
   char sim_name[132];            /* XML simulation file name */

   int dlength;                   /* length of date string */
   int tlength;                   /* length of time string */
   char tname[132];               /* temporary copy of file name */

   if ( !StatusOkP(status) ) return;

   if ( dream_trace ( 3 ) )
   {
       printf ( "dsim_getskypar : entered routine\n" );
   }
   
   dlength = 16;
   tlength = 16;

   strcpy ( tname, argv[1] );
   strcpy ( obs_name, getenv ( "SMURF_SIM_XML" ) );
   strcat ( obs_name, "/" );
   strcat ( obs_name, tname );

   strcpy ( tname, argv[2] );
   strcpy ( sim_name, getenv ( "SMURF_SIM_XML" ) );
   strcat ( sim_name, "/" );
   strcat ( sim_name, tname );

   *rseed = atoi ( argv[3] );
   *exponent = atof ( argv[4] );

   if ( dream_trace ( 3 ) )
   {
       printf ( "dsim_getskypar : Got command-line parameters\n" );
   }

/*  Read all parameters from the scuba_definition file */

   dxml_readXML ( obs_name, status );

   dxml_returnXML ( inx, status );

   if ( dream_trace ( 3 ) )
   {
       printf ( "dsim_getskypar : Got observation parameters\n" );
   }

   dxml_readsimXML ( sim_name, status );

   dxml_returnsimXML ( sinx, status );

   if ( dream_trace ( 3 ) )
   {
       printf ( "dsim_getskypar : Got simulation parameters\n" );
   }

/* Expand file names */

   strcpy ( tname, sinx->astname );
   strcpy ( sinx->astname, getenv ( "SMURF_SIM_IN" ) );
   strcat ( sinx->astname, "/" );
   strcat ( sinx->astname, tname );

   strcpy ( tname, sinx->atmname );
   strcpy ( sinx->atmname, getenv ( "SMURF_SIM_IN" ) );
   strcat ( sinx->atmname, "/" );
   strcat ( sinx->atmname, tname );

   strcpy ( tname, inx->bolfile );
   strcpy ( inx->bolfile, getenv ( "SMURF_SIM_IN" ) );
   strcat ( inx->bolfile, "/" );
   strcat ( inx->bolfile, tname );

/* Overwrite dataname with output filename */

   strcpy ( inx->dataname, sinx->atmname );

   if ( inx->nvert == 0 )
   {
      nspc = 256;
   }
   else
   {
      nspc = inx->nvert * inx->smu_samples;
   }
   i4 = nspc * ( sinx->ncycle + 1 );

}



/*+ dsim_getslopepar - Get parameters for slope simulator */

void dsim_getslopepar 
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file (returned) */
int *rseed,              /* seed for random number generator (returned)*/
double *fluxJy,          /* source flux in Jansky (returned) */
int *mapwidth,           /* width of created map in pixels (returned) */
double *pixsize,         /* pixel size in arcsec (returned) */
char *file_name,         /* name of output file (returned) */
int *status              /* global status (given and returned) */
)

/*  Description :
     Get and interpret parameters from command line.

    Authors :
     B.D.Kelly (bdk@roe.ac.uk)

    History :
     11Aug2005:  adapted from getpointpar (bdk)
*/

{
   int i4;
   int nspc;
   char obs_name[132];            /* XML observation file name */
   char sim_name[132];            /* XML simulation file name */

   int dlength;                   /* length of date string */
   int tlength;                   /* length of time string */
   char tname[132];               /* temporary copy of file name */


   if ( !StatusOkP(status) ) return;

   dlength = 16;
   tlength = 16;

   strcpy ( tname, argv[1] );
   strcpy ( obs_name, getenv ( "SMURF_SIM_XML" ) );
   strcat ( obs_name, "/" );
   strcat ( obs_name, tname );

   strcpy ( tname, argv[2] );
   strcpy ( sim_name, getenv ( "SMURF_SIM_XML" ) );
   strcat ( sim_name, "/" );
   strcat ( sim_name, tname );

   *rseed = atoi ( argv[3] );
   *fluxJy = atof ( argv[4] );
   *mapwidth = atoi ( argv[5] );
   *pixsize = atoi ( argv[6] );
   
   strcpy ( file_name, getenv ( "SMURF_SIM_IN" ) );
   strcat ( file_name, "/" );
   strcat ( file_name, argv[7] );



/*  Read all parameters from the scuba_definition file */

   dxml_readXML ( obs_name, status );

   dxml_returnXML ( inx, status );

   dxml_readsimXML ( sim_name, status );

   dxml_returnsimXML ( sinx, status );

/* Expand file names */

   strcpy ( tname, sinx->astname );
   strcpy ( sinx->astname, getenv ( "SMURF_SIM_IN" ) );
   strcat ( sinx->astname, "/" );
   strcat ( sinx->astname, tname );

   strcpy ( tname, sinx->atmname );
   strcpy ( sinx->atmname, getenv ( "SMURF_SIM_IN" ) );
   strcat ( sinx->atmname, "/" );
   strcat ( sinx->atmname, tname );

   strcpy ( tname, inx->bolfile );
   strcpy ( inx->bolfile, getenv ( "SMURF_SIM_IN" ) );
   strcat ( inx->bolfile, "/" );
   strcat ( inx->bolfile, tname );

/* Overwrite dataname with parameter */

   strcpy ( tname, argv[5] );
   strcpy ( inx->dataname, getenv ( "SMURF_SIM_IN" ) );
   strcat ( inx->dataname, "/" );
   strcat ( inx->dataname, tname );


   if ( inx->nvert == 0 )
   {
      nspc = 256;
   }
   else
   {
      nspc = inx->nvert * inx->smu_samples;
   }
   i4 = nspc * ( sinx->ncycle + 1 );

}



/*+ dsim_getspread - return scattered characteristics of bolometers */

void dsim_getspread
(
int numbols,             /* Number of bolometers (given) */
double pzero[],          /* Array to hold response curve offsets 
                            of bolometers in pW (returned) */
double heater[],         /* Array to hold heater factors of bolometers
                            (returned) */
int *status              /* global status (given and returned) */
)

/*   Description :
      Use scaled random numbers for the response curve offsets.
      The heater factors are 1.0 with a 5% random spread, simulating a
      spread in the heater resistors of each bolometer.

     Authors :
      B.D.Kelly (ROE)

     History :
      11May2005:  original (bdk@roe.ac.uk)
*/

{

   int bol;                          /* bolometer counter */


   if ( !StatusOkP(status) ) return;


/* Produce some power offsets, zero mean, sigma = 1pW */

   for (bol=0; bol<numbols; bol++ )
   {
      pzero[bol] = dsim_drand ( 1.0 );
   }

/* Produce some heater input ratios, mean=1.0, sigma=0.05 */

   for (bol=0; bol<numbols; bol++ )
   {
      heater[bol] = 1.0 + dsim_drand ( 0.05 );
   }

}


/*+ dsim_getweights - return weights for smoothing by impulse response */

void dsim_getweights 
( 
double decay,      /* time constant in millisec (given) */
double samptime,   /* sampling time in millisec (given) */
int nweights,      /* number of values to be returned (given) */
double weights[],  /* array to hold returned values (returned) */
int *status        /* global status (given and returned) */
)

/*    Description :
       A set of values suitable to act as a convolving kernel to match the
       impulse response is generated.

      Authors :
       B.D.Kelly (ROE)
      History :
       21Aug2001:  original (bdk@roe.ac.uk)
       20Aug2002:  C version (bdk)
*/

{
   int j;
   double pos;
   double sum;


   if ( !StatusOkP(status) ) return;

   sum = 0.0;
   for ( j=0; j<nweights; j++ )
   {
      pos = ( samptime * (double) ( nweights - j - 1 ) ) / decay;
      weights[j] = 1.0 / exp(pos);
      sum = sum + weights[j];
   }

   for ( j=0; j<nweights; j++ )
   {
      weights[j] = weights[j] / sum;
   }

}


/*+ dsim_hor2eq - get telescope position and orientation */

void dsim_hor2eq
( 
double az,          /* Azimuth in radians (given) */
double el,          /* Elevation in radians (given) */
double lst,         /* local sidereal time in radians (given) */
double *ra,         /* Right Ascension in radians (returned) */
double *dec,        /* Declination in radians (returned) */
int *status         /* global status (given and returned) */
)

/* Description :
    Use slalib algorithms to get from horizontal to equatorial coordinates.

   Authors :
    Ed Chapin (echapin@phas.ubc.ca)

   History :
    10Jan2006 : Original version (ec)
*/

{
  double phi, ha, x, y, z, r;

   if ( !StatusOkP(status) ) return;

/* JCMT is 19:49:33 N */

   phi = ( 19.0 + (49.0/60.0) + (33.0/3600.0) ) / AST__DR2D;

/*
   Equivalent to slaDh2e ( az, el, phi, ha, dec );
*/


/* HA,Dec as x,y,z */
   x = - cos(az) * cos(el) * sin(phi) + sin(el) * cos(phi);
   y = - sin(az) * cos(el);
   z = cos(az) * cos(el) * cos(phi) + sin(el) * sin(phi);

/* To spherical */
   r = sqrt ( x * x + y * y );
   ha = ( r == 0.0 ) ? 0.0 : atan2 ( y, x ) ;
   *dec = atan2 ( z, r );

   *ra = lst - ha;
}


/*+ dsim_initast - return simulated astronomical image */

void dsim_initast 
(
char *filename,        /* name of input file (given) */
double *pixsize,       /* pixel size in arcsec (returned) */
long naxes[2],         /* dimensions of image (returned) */ 
double **astsim,       /* array to hold returned image (returned) */
AstFrameSet **fitswcs, /* frameset containing WCS of FITS image */
int *status            /* global status (given and returned) */
)

/*  Description :
     Read and return an array with data representing an astronomical image.

   Authors :
    B.D.Kelly (ROE)
    E.Chapin (UBC)

   History :
    19July2001:  original (bdk@roe.ac.uk)
    20Aug2002 : C version (bdk)
    14Nov2003 : read the image from a FITS file (bdk)
    28Feb2005 : Extract WCS info from FITS header (ec)
*/

{
   int anynull;            /* null element reporter */
   int bitpix;             /* FITS storage type */
   char card[132];         /* Single FITS card */
   char comment[132];      /* FITS header comment */
   int exists;             /* whether or not the filename exists */
   AstFitsChan *fc=NULL;   /* FITS channels for retrieving WCS */
   fitsfile *fptr;         /* pointer to FITS data */
   int i;                  /* loop counter */
   long inc[2];            /* FITS sampling parameter */
   int naxis;              /* array dimensionality */
   int nchan;              /* Number of channels in FITS header */
   long pend[2];           /* subarray end coordinates */
   long pstart[2];         /* subarray start coordinates */

   if ( !StatusOkP(status) ) return;

 /* Check to make sure the input file exists, then open it, 
   and read the header information */

   exists = 0;
   fits_file_exists( filename, &exists, status );

   if ( !exists ) {
      *status = DITS__APP_ERROR;
      printf ( "dsim_initast: file %s could not be found.\n", filename );
      printf ( "\n" );
      return;
   }//if

   fits_open_image ( &fptr, filename, 0, status );
   fits_read_key ( fptr, TDOUBLE, "PIXSIZE", pixsize, comment, status );
   
   /* Get the WCS information from the FITS header */

   fits_get_hdrspace( fptr, &nchan, NULL, status );
   fc = astFitsChan ( NULL, NULL, "" );
 
   for( i=1; i<=nchan; i++ ) {
     fits_read_record( fptr, i, card, status );
     astPutFits( fc, card, 0 );
   }

   astClear( fc, "Card" );
   *fitswcs = astRead(fc);

   fc = astAnnul(fc);

   /* Find the array size */
   
   fits_get_img_param ( fptr, 2, &bitpix, &naxis, naxes, status );
   pstart[0] = 1;
   pstart[1] = 1;
   pend[0] = naxes[0];
   pend[1] = naxes[1];
   inc[0] = 1;
   inc[1] = 1;

   /* Create the storage space */
   
   *astsim = (double *) calloc ( naxes[0]*naxes[1], sizeof(double)  );
   
   if ( *astsim != 0 ) {
     fits_read_subset ( fptr, TDOUBLE, pstart, pend, inc, 0, *astsim, &anynull,
			status );
     
     fits_close_file ( fptr, status );
     
     fits_report_error ( stderr, *status );
     
   } else {
     *status = DITS__APP_ERROR;
     printf ( "dsim_initast: failed to allocate memory\n" );
     printf ( strerror(errno) );
     printf ( "\n" );
   }
}


/*+ dsim_initatm - return simulated atmospheric emission */

void dsim_initatm
(
char *filename,    /* name of input file (given) */
double *pixsize,   /* pixel size in arcsec (returned) */
long naxes[2],     /* dimensions of image (returned) */ 
double **atmsim,   /* array to hold returned image (returned) */
int *status        /* global status (given and returned) */
)

/*  Description :
     Read and return an array with data representing the background emission
     from the Earth's atmosphere.

   Authors :
    B.D.Kelly (ROE)

   History :
    19July2001:  original (bdk@roe.ac.uk)
    20Aug2002 : C version (bdk)
    14Nov2003 : read the image from a FITS file (bdk)
*/

{
   int anynull;            /* null element reporter */
   int bitpix;             /* FITS storage type */
   char comment[132];      /* FITS header comment */
   int exists;             /* whether or not the filename exists */
   fitsfile *fptr;         /* pointer to FITS data */
   long inc[2];            /* FITS sampling parameter */
   int naxis;              /* array dimensionality */
   long pend[2];           /* subarray end coordinates */
   long pstart[2];         /* subarray start coordinates */

   if ( !StatusOkP(status) ) return;

/* Check to make sure the input file exists, then open it, 
   and read the header information */

   exists = 0;
   fits_file_exists ( filename, &exists, status );

   if ( !exists ) {
      *status = DITS__APP_ERROR;
      printf ( "dsim_initatm: file %s could not be found.\n", filename );
      printf ( "\n" );
      return;
   }//if

   fits_open_image ( &fptr, filename, 0, status );
   fits_read_key ( fptr, TDOUBLE, "PIXSIZE", pixsize, comment, status );

/* Find the array size */

   fits_get_img_param ( fptr, 2, &bitpix, &naxis, naxes, status );
   pstart[0] = 1;
   pstart[1] = 1;
   pend[0] = naxes[0];
   pend[1] = naxes[1];
   inc[0] = 1;
   inc[1] = 1;

/* Create the storage space */

   *atmsim = (double *) calloc ( naxes[0]*naxes[1], sizeof(double)  );

   if ( *atmsim != 0 )
   {

      fits_read_subset ( fptr, TDOUBLE, pstart, pend, inc, 0, *atmsim, &anynull,
        status );

      fits_close_file ( fptr, status );

      fits_report_error ( stderr, *status );

   }
   else
   {
      *status = DITS__APP_ERROR;
      printf ( "dsim_initatm: failed to allocate memory\n" );
      printf ( strerror(errno) );
      printf ( "\n" );
   }
}




/*+ dsim_instrinit - initialise instrument parameters */

void dsim_instrinit
(
int argc,                /* argument count (given) */
char **argv,             /* argument list (given) */
struct dxml_struct *inx, /* structure for values from XML file (returned) */
struct dxml_sim_struct *sinx, /* structure for values from XML file(returned) */
int *rseed,              /* seed for random number generator (returned)*/
double coeffs[NCOEFFS],  /* bolometer response coeffs (returned) */
double *digcurrent,      /* digitisation mean current (returned) */
double *digmean,         /* digitisation mean value (returned) */
double *digscale,        /* digitisation scale factore (returned) */
double *elevation,       /* telescope elevation (radians) (returned) */
AstFrameSet **fset,      /* World coordinate transformations (returned) */
double weights[],        /* impulse response (returned) */
double **heater,         /* bolometer heater ratios (returned) */
double **pzero,          /* bolometer power offsets (returned) */
double **xbc,            /* X offsets of bolometers in arcsec (returned) */
double **ybc,            /* Y offsets of bolometers in arcsec (returned) */
double **xbolo,          /* Native bolo x-offsets */
double **ybolo,          /* Native bolo x-offsets */
int *status              /* global status (given and returned) */
)
/*  Method :

    Authors :
     B.D.Kelly (ROE)
     E.Chapin (UBC)

    History :
     13May2005 : original (bdk)
     18May2005 : returned xbc,ybc (bdk)
     17Jun2005 : allocate space for bolometer parameter arrays (bdk)
     28Feb2006 : added xbolo/ybolo (EC)
*/


{
   double azimuth;                /* Azimuth in radians */
   double decay;                  /* bolometer time constant (msec) */
   int j;                         /* loop counter */
   double lst;                    /* local sidereal time in radians */
   int nboll;                     /* total number of bolometers */
   double p;                      /* parallactic angle (radians) */
   double photonsigma;            /* typical photon noise level in pW */
   double samptime;               /* sample time in sec */
   int savebols;                  /* flag for bol details (unused here) */


   if ( !StatusOkP(status) ) return;

   /* Initialise tracing */
   
   dream_traceinit();
   
   /* Get control parameters */
   
   if ( dream_trace ( 1 ) ) {
     printf ( "staresim : Start of the program\n" );
   }

   dsim_getpar ( argc, argv, inx, sinx, rseed, &savebols, status );

   samptime = inx->sample_t / 1000.0;
   
   /* Initialise bolometer characteristics */
   
   dream_bolinit ( 1, inx->nbolx, inx->nboly, status );
   
   /* Get the bolometer information */
   
   nboll = inx->nbolx * inx->nboly;
   
   decay = 5.0;
   *heater = (double *)calloc ( nboll, sizeof(double) );
   *pzero = (double *)calloc ( nboll, sizeof(double) );
   *xbc = (double *)calloc ( nboll, sizeof(double) );
   *ybc = (double *)calloc ( nboll, sizeof(double) );
   *xbolo = (double *)calloc ( nboll, sizeof(double) );
   *ybolo = (double *)calloc ( nboll, sizeof(double) );
   
   /*  Initialise the standard bolometer response function.
       The routine sets the values for 6 polynomial coefficients which
       translate input power in pico Watts to output current in Amp. */
   
   dsim_response ( inx->lambda, NCOEFFS, coeffs, status );

   /*  Calculate the parameters for simulating digitisation */
   
   dsim_getsigma ( inx->lambda, sinx->bandGHz, sinx->aomega,
		   (sinx->telemission+sinx->meanatm), &photonsigma, status );
   
   dsim_getscaling ( NCOEFFS, coeffs, inx->targetpow, photonsigma,
		     digmean, digscale, digcurrent, status );
  
   
   /*  Initialise the bolometer impulse response
       WEIGHTS contain 16 coefficients in reversed order given by
       exp(-ti/DECAY) divided by the sum of all 16 values.
       This is practically identical to the fb.exp(-t1.fb) formula
       used in the DREAM software with fb=1/DECAY. */
   
   dsim_getweights ( decay, inx->sample_t, DREAM__MXIRF, weights, status );
 
   /* Get the subsystem number */ 
   
   /*sc2ast_name2num( sinx->subname, &(inx->subsysnr), status );*/
   
   /* Setup world coordinate information and get the bolometer positions in
      Nasmyth and native coordinates */
   
   lst = inx->ra;

   dsim_telpos ( inx->ra, inx->dec, lst, &azimuth, elevation, &p, status );

   dsim_bolcoords ( sinx->subname, inx->ra, inx->dec, *elevation, p,
		    "NASMYTH", fset, &nboll, *xbc, *ybc, status );

   dsim_bolnatcoords( *xbolo, *ybolo, &nboll, status );
   
   /* Convert Nasmyth coordinates from mm to arcsec */
		     
   for ( j=0; j<nboll; j++ ) {
     (*xbc)[j] *= MM2SEC;
     (*ybc)[j] *= MM2SEC;
   }
   
   dsim_getspread ( nboll, *pzero, *heater, status );

}



/*+ dsim_invf - generate a 1/f plus white noise sequence */

void dsim_invf 
( 
double sigma,     /* white noise level (given) */
double corner,    /* corner frequency (given) */
double samptime,  /* time in sec between samples (given) */
int nsamples,     /* number of positions in sequence (given) */
double *fnoise,   /* array to hold noise sequence (returned) */
int *status       /* global status (given and returned) */
)

/*    Description :
       Return a sequence of values with combined 1/f and white noise 
       characteristics.
       Simulate a 1/f noise sequence by generating white noise sequence, 
       Fourier transforming it, applying a 1/f law, then transforming back
       again. Generate and add a new white noise sequence.

     Authors :
       B.D.Kelly (ROE)

     History :
      24July2001:  original (bdk@roe.ac.uk)
      20Aug2002 :  C version (bdk)
      13Jun2003 :  change argument name to nsamples (bdk)
*/

{
   int j;
   static double spectrum[4096];
   double counttonu;
   double fourfact;
   double fscale;
   double nu;
   double power;
   double y;


   if ( !StatusOkP(status) ) return;

/* Generate a set of random numbers with a bell-shaped distribution,
   dispersion 1.0 and zero mean */

   for ( j=0; j<2048; j++ )
   {
      spectrum[2*j] = dsim_drand ( 1.0 );
      spectrum[1+2*j] = 0.0;
   }

/* Fourier transform */

   dsim_four1 ( +1, 2048, spectrum );
      
/* Modify to give 1/f characteristics */

   counttonu = 1.0 / ( samptime * 1024.0 );
   power = 1.0;
   fscale = sigma * pow ( corner, power );

   for ( j=0; j<2048; j++ )
   {
      if ( j > 1024 )
      {
         y = (double) ( 2048 - j );
      }
      else
      {
         y = (double) j;
      }

      nu = counttonu * y;

      if ( nu > 1.0e-10 )
      {
         spectrum[j*2] = spectrum[j*2] * fscale / pow ( nu, power );
         spectrum[j*2+1] = spectrum[j*2+1] * fscale / pow ( nu, power );
      }
      else
      {
         spectrum[j*2] = spectrum[j*2] * fscale;
         spectrum[j*2+1] = spectrum[j*2+1] * fscale;
      }

   }

/* Reverse Fourier transform */

   dsim_four1 ( -1, 2048, spectrum );

/* Extract a noise sequence */

   fourfact = 1.0 / 2048.0;
   for ( j=0; j<nsamples; j++ )
   {
      fnoise[j] = fourfact * spectrum[2*j];
   }

/* Generate a set of random numbers with a bell-shaped distribution
   and add onto the 1/f noise */

   for ( j=0; j<nsamples; j++ )
   {
      fnoise[j] = fnoise[j] + dsim_drand ( sigma );
   }

}


/*+ dsim_invf2d - generate a 2-D image of 1/f noise  */

void dsim_invf2d 
( 
double sigma,     /* white noise level (given) */
double corner,    /* corner frequency in per arcsec (given) */
double p,         /* power law to be used (given) */
double pixsize,   /* pixel size in arcsec (given) */
int size,         /* size of square image array (given) */
double *fnoise,   /* array to hold noise image (returned) */
double *spectrum, /* array to hold complex 2-D spectrum (returned) */
int *status       /* global status (given and returned) */
)

/*    Description :
       Return a 2-D image containing spatial noise following a power law with
       exponent p which would give a specified noise corner if combined with 
       white spatial noise of given sigma.

       Simulate an f**(-p) noise field by generating a white noise image, 
       Fourier transforming it, applying an f**(-p) law, then transforming back
       again.

     Authors :
       B.D.Kelly (ROE)

     History :
      06Nov2003:  original (bdk@roe.ac.uk)
*/

{
   int i;
   int j;
   double counttonu;
   double fourfact;
   double fscale;
   double nu;
   double x;
   double y;


   if ( !StatusOkP(status) ) return;

/* Generate a set of random numbers with a bell-shaped distribution,
   dispersion 1.0 and zero mean */

   for ( j=0; j<size*size; j++ )
   {
      spectrum[2*j] = dsim_drand ( 1.0 );
      spectrum[1+2*j] = 0.0;
   }


/* Fourier transform  */

   dsim_fft2d ( -1, size, spectrum, status );

/* Modify to give 1/f characteristics */

   counttonu = 1.0 / ( pixsize * (double)size * 0.5 );
   fscale = sigma * pow ( corner, p );

   for ( j=0; j<size; j++ )
   {
      if ( j > size/2 )
      {
         y = (double) ( size - j );
      }
      else
      {
         y = (double) j;
      }
      for ( i=0; i<size; i++ )
      {
         if ( i > size/2 )
         {
            x = (double) ( size - i );
         }
         else
         {
            x = (double) i;
         }

         nu = counttonu * sqrt ( x*x + y*y );

         if ( nu > 1.0e-10 )
         {
            spectrum[j*size*2+i*2] = 
	      spectrum[j*size*2+i*2] * fscale / pow ( nu, p );
            spectrum[j*size*2+i*2+1] = 
	      spectrum[j*size*2+i*2+1] * fscale / pow ( nu, p );

         }
         else
         {
            spectrum[j*size*2+i*2] = 
	      spectrum[j*size*2+i*2] * fscale;
            spectrum[j*size*2+i*2+1] = 
	      spectrum[j*size*2+i*2+1] * fscale;
         }
      }
   }

/* Reverse Fourier transform */

   dsim_fft2d ( 1, size, spectrum, status );

/* Extract a noise sequence */

   fourfact = 1.0 / (size*size);
   for ( j=0; j<size*size; j++ )
   {
      fnoise[j] = fourfact * spectrum[2*j];
   }

/* Generate a set of random numbers with a bell-shaped distribution
   and add onto the 1/f noise */
/*
   for ( j=0; j<size*size; j++ )
   {
      fnoise[j] = fnoise[j] + dsim_drand ( sigma );
   }
*/
}


/*+ dsim_kludgemodjuldate - kludge for the modified julian date */

double dsim_kludgemodjuldate
( 
double ra,           /* Right Ascension in radians (given) */
int *status          /* global status (given and returned) */
)

/* Description :
    Until the simulator runs on a single clock, this kludge will calculate
    a modified julian date that is consistent with the astronomical source
    being on the meridian (i.e. lst=ra as it is assumed everywhere in the
    simulator). This routine is needed to calculate the time fields
    for TAI/RTS.

   Authors :
    Ed Chapin (echapin@phas.ubc.ca)

   History :
    27Oct2005 : Original version (echapin)
*/

{
  double date;          /* ref. modified julian date */
  double delta_st;      /* How much does reflst have to change? radians */
  double lon;           /* JCMT west longitude in radians */
  double obsdate;       /* The modified julian date of the observation */
  double refgst;        /* ref. gst in radians */
  double reflst;        /* ref. lst in radians */
  double t;             /* Julian centuries since J2000 */
  double ut;            /* ref. universal time as day fraction */


  /*if ( !StatusOkP(status) ) return;*/

  /* JCMT coordinates in radians */

  /*lat = ( 19.0 + (49.0/60.0) + (33.0/3600.0) ) / AST__DR2D;*/
  lon = ( 155.0 + (28.0/60.0) + (0.0/3600.0) ) / AST__DR2D;

  /* Define an arbitrary modified julian date + UT time */

  date = 51544;        /* Jan 1, 2000 */
  ut = 0.5;            /* noon */

  /* Use slalib slaGmsta algorithm to calculate greenwich LST at this date */

  t = ( ut + ( date - 51544.5 ) ) / 36525.0;

  refgst = DS2R * ( 24110.54841
                           + ( 8640184.812866
                           + ( 0.093104
                             - 6.2e-6 * t ) * t ) * t
                             + 86400.0 * ( dmod ( ut, 1.0 ) +
                                           dmod ( date, 1.0 ) ) ); 

  /* Calculate the reference LST from the GST using the telescope longitude */

  reflst = refgst - lon;

  /* If we want ra = lst, how far do we have to move in ST?
     i.e. what is the hour angle for the reference lst */
 
  delta_st = ra - reflst;

  /* Calculate the obsdate. Note that delta_st is multiplied by
     the ratio of a civil time interval to a sidereal time interval */

  obsdate = date + ut + delta_st/(2.*AST__DPI*1.002737909);

  return obsdate;
}


/*+ dsim_ndfwrdata - generic digitise/compress and store SC2 data as NDF */

void dsim_ndfwrdata
(
double ra,        /* RA of observation in radians (given) */
double dec,       /* Dec of observation in radians (given) */
int add_atm,      /* flag for adding atmospheric emission (given) */
int add_fnoise,   /* flag for adding 1/f noise (given) */
int add_pns,      /* flag for adding photon noise (given) */
int flux2cur,     /* flag for converting flux to current (given) */
double amstart,   /* Airmass at beginning (given) */
double amend,     /* Airmass at end (given) */
double meanwvm,   /* 225 GHz tau */
double obslam,    /* Wavelength */
char file_name[], /* output file name (given) */
int ncol,         /* number of bolometers in column (given) */
int nrow,         /* number of bolometers in row (given) */
double sample_t,  /* sample interval in msec (given) */
char subarray[],  /* name of the subarray */
int numsamples,   /* number of samples (given) */
int nflat,        /* number of flat coeffs per bol (given) */
char *flatname,   /* name of flatfield algorithm (given) */
struct sc2head *head, /* header data for each frame (given) */
int *dbuf,        /* simulated data (given) */
int *dksquid,     /* dark SQUID time stream data (given) */
double *fcal,     /* flatfield calibration (given) */
double *fpar,     /* flat-field parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
double *posptr,   /* Pointing offsets from map centre */
char *obsmode,    /* Observing mode */
int *status       /* global status (given and returned) */
)

/*  Description :
     Create and map a SCUBA-2 NDF file. Scale the data to integers one
     frame at a time and add a compressed version to the mapped file.
     Store the per-frame header items and the FITS headers.

    Authors :
     E. Chapin (UBC)

    History :
     29Mar2006:  dsim_ndfwrdata adapted from dsim_ndfwrpong (ec)
     11May2006:  add obsmode (agg)
*/

{
  double decd;                     /* Dec of observation in degrees */
  static char fitsrec[FHEAD__MXREC][81];  /* store for FITS records */
  int i;                           /* Loop counter */
  int nrec;                        /* number of FITS header records */
  double rad;                      /* RA of observation in degrees */
  double map_hght;   /* Map height in arcsec */
  double map_wdth;   /* Map width in arcsec  */
  double map_pa;     /* Map PA in degrees  */
  double map_x = 0;  /* Map X offset in arcsec */
  double map_y = 0;  /* Map Y offset in arcsec */
  double x_min;      /* Maximum extend of pointing centre offsets */
  double x_max;
  double y_min;
  double y_max;
  
  if ( !StatusOkP(status) ) return;
  
  /* Format the FITS headers */
  
  fhead_init ( status );
  
  fhead_putfits ( TSTRING,
		  "DATE-OBS", "YYYY-MM-DDThh:mm:ss",
		  "observation date", status );
  
  rad = ra * AST__DR2D;
  
  fhead_putfits ( TDOUBLE,
		  "RA", &rad,
		  "Right Ascension of observation", status );
  
  decd = dec * AST__DR2D;
  
  fhead_putfits ( TDOUBLE,
		  "DEC", &decd,
		  "Declination of observation", status );
  
  fhead_putfits ( TINT,
		  "ADD_ATM", &add_atm,
		  "flag for adding atmospheric emission", status );
  
  fhead_putfits ( TINT,
		  "ADDFNOIS", &add_fnoise,
		  "flag for adding 1/f noise", status );
  
  fhead_putfits ( TINT,
		  "ADD_PNS", &add_pns,
		  "flag for adding photon noise", status );
  
  fhead_putfits ( TINT,
		  "FLUX2CUR", &flux2cur,
		  "flag for converting flux to current", status );
  
  fhead_putfits ( TINT,
		  "NBOLX", &ncol,
		  "number of bolometers in X direction", status );
  
  fhead_putfits ( TINT,
		  "NBOLY", &nrow,
		  "number of bolometers in Y direction", status );
  
  fhead_putfits ( TDOUBLE,
		  "SAMPLE_T", &sample_t,
		  "The sample interval in msec", status );
  
  fhead_putfits ( TSTRING,
		  "SUBARRAY", subarray,
		  "subarray name", status );
  
  fhead_putfits ( TINT,
		  "NUMSAMP", &numsamples,
		  "number of samples", status );
  
  fhead_putfits ( TDOUBLE,
		  "AMSTART", &amstart,
		  "Air mass at start", status );
  
  fhead_putfits ( TDOUBLE,
		  "AMEND", &amend,
		  "Air mass at end", status );
  
  fhead_putfits ( TDOUBLE,
		  "MEANWVM", &meanwvm,
		  "Mean zenith tau at 225 GHz from WVM", status );
  
  fhead_putfits ( TSTRING,
		  "FILTER", filter,
		  "filter used", status );
  
  fhead_putfits ( TDOUBLE,
		  "ATSTART", &atstart,
		  "Ambient temperature at start (C)", status );
  
  fhead_putfits ( TDOUBLE,
		  "ATEND", &atend,
		  "Ambient temperature at end (C)", status );

  fhead_putfits ( TSTRING,
		  "OBSMODE", obsmode,
		  "Observing mode", status );

  
  /* Determine extent of the map from posptr + known size of the arrays */
  
  for( i=0; i<numsamples; i++ ) {
    
    if( i == 0 ) {
      x_min = posptr[0];
      x_max = posptr[0];
      y_min = posptr[1];
      y_max = posptr[1];
    }
    
    if( posptr[i*2] < x_min ) x_min = posptr[i*2];
    if( posptr[i*2] > x_max ) x_max = posptr[i*2];
    if( posptr[i*2+1] < y_min ) y_min = posptr[i*2+1];
    if( posptr[i*2+1] > y_max ) y_max = posptr[i*2+1];
  }
  
  map_wdth = (x_max - x_min) + 1000; /* 1000 arcsec for array FOV */
  map_hght = (y_max - y_min) + 1000; /* 1000 arcsec for array FOV */
  map_pa = 0; /* kludge */
  map_x = (x_max + x_min)/2.;
  map_y = (y_max + y_min)/2.;
  
  fhead_putfits ( TDOUBLE,
		  "MAP_HGHT", &map_hght,
		  "Map height (arcsec)", status );
  fhead_putfits ( TDOUBLE,
		  "MAP_WDTH", &map_wdth,
		  "Map height (arcsec)", status );
  fhead_putfits ( TDOUBLE,
		  "MAP_PA", &map_pa,
		  "Map PA (degrees)", status );
  fhead_putfits ( TDOUBLE,
		  "MAP_X", &map_x,
		  "Map X offset (arcsec)", status );
  fhead_putfits ( TDOUBLE,
		  "MAP_Y", &map_y,
		  "Map Y offset (arcsec)", status );
  
  /* Get the accumulated FITS headers */ 
  fhead_getfits ( &nrec, fitsrec, status );
  
  /* Store the timestream data */

  sc2store_wrtstream ( file_name, nrec, fitsrec, ncol, nrow, numsamples, 
		       nflat, flatname, head, dbuf, dksquid, fcal, fpar, 
		       status );
  
  /* Close the file */
  sc2store_free ( status );
}


/*+ dsim_ndfwrdream - digitise and compress the DREAM simulation and store
    as NDF */

void dsim_ndfwrdream
(
double ra,         /* RA of observation in radians (given) */
double dec,        /* Dec of observation in radians (given) */
int jig_vert[][2], /* Array with relative jiggle coordinates in units 
                      of pixel distance in case jiggle positions are 
                      visited (given) */
int add_atm,       /* flag for adding atmospheric emission (given) */
int add_fnoise,    /* flag for adding 1/f noise (given) */
int add_pns,       /* flag for adding photon noise (given) */
int flux2cur,      /* flag for converting flux to current (given) */
double amstart,    /* Airmass at beginning (given) */
double amend,      /* Airmass at end (given) */
double meanwvm,    /* 225 GHz tau */
double obslam,     /* Wavelength */
double astflux,    /* Point source flux as specified when ast.fits generated */
int smu_samples,   /* number of samples between jiggle vertices (given) */
double distfac,    /* distortion factor (0=no distortion) (given) */
int conv_shape,    /* convolution function (Gaussian=0) (given) */
double conv_sig,   /* convolution function parameter (given) */
char file_name[],  /* output file name (given) */
int ncol,          /* number of bolometers in column (given) */
int nrow,          /* number of bolometers in row (given) */
double sample_t,   /* sample interval in msec (given) */
char subarray[],   /* Subarray name (given) */ 
int nvert,         /* Nr of vertices in the Jiggle pattern (given) */
int move_code,     /* Code for the SMU move algorithm (given) */
double jig_stepx,  /* The Jiggle step value in -X-direction on the sky 
                      in arcsec (given)*/
double jig_stepy,  /* The Jiggle step value in -Y-direction on the sky 
                      in arcsec (given) */
int ncycle,        /* number of cycles (given) */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
struct sc2head *head, /* header data for each frame (given) */
int *dbuf,         /* simulated data (given) */
int *dksquid,      /* dark SQUID time stream data (given) */
double *fcal,      /* flat-field calibration (given) */
double *fpar,      /* flat-field parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
char *obsmode,     /* Observing mode */
int *status        /* global status (given and returned) */
)

/*  Description :
     Create and map a SCUBA-2 NDF file. Scale the data to integers one
     frame at a time and add a compressed version to the mapped file.
     Store the per-frame header items and the FITS headers.

    Authors :
     B.D.Kelly (UKATC)
     A.G. Gibb (UBC)

    History :
     24Sep2004:  original (bdk)
     04Mar2005:  pass amstart, amend and meanwvm as arguments (bdk)
     16Mar2005:  includes subarray information in ICD-compliant format,
                 which means the wavelength must be passed to the subroutine (agg)
                 writes flux of point source in ast.fits into header (agg)
     17May2005:  pass-in subname, ra and dec (bdk)
     20May2005:  pass-in flatcal (bdk)
     08Jul2005:  change use of fhead library (bdk)
*/

{
   double decd;                     /* Dec of observation in degrees */
   static char fitsrec[FHEAD__MXREC][81];  /* store for FITS records */
   int nrec;                        /* number of FITS header records */
   double rad;                      /* RA of observation in degrees */
   double inttime = 1;              /* Will be able to calculate this from no of DREAM steps */
   char object[10] = "TEST";        /* Name of object */
   char isodate[25];                /* Date-time string in ISO format */
   int scannum = 1;                 /* Placeholder for scan number */
   int obsnum = 1;                  /* Placeholder for observation number */
   char obsend[1] = "F";               /* Placeholder for OBSEND */

   if ( !StatusOkP(status) ) return;


/* Format the FITS headers */

/* Call a date-obs fn to return DATE-OBS string */

   dream_timenow(0, 0, 24, NULL, NULL, isodate, NULL, status);

   fhead_init ( status );

   fhead_putfits ( TSTRING,
     "DATE-OBS", isodate,
     "observation date", status );

   rad = ra * AST__DR2D;
   decd = dec * AST__DR2D;
   fhead_putfits ( TDOUBLE,
     "RA", &rad,
     "Right Ascension of observation", status );
   fhead_putfits ( TDOUBLE,
     "DEC", &decd,
     "Declination of observation", status );

   fhead_putfits ( TINT,
     "ADD_ATM", &add_atm,
     "flag for adding atmospheric emission", status );

   fhead_putfits ( TINT,
     "ADDFNOIS", &add_fnoise,
     "flag for adding 1/f noise", status );

   fhead_putfits ( TINT,
     "ADD_PNS", &add_pns,
     "flag for adding photon noise", status );

   fhead_putfits ( TINT,
     "FLUX2CUR", &flux2cur,
     "flag for converting flux to current", status );

   fhead_putfits ( TINT,
     "SMU_SAMP", &smu_samples,
     "number of samples between jiggle vertices", status );

   fhead_putfits ( TDOUBLE,
     "DISTFAC", &distfac,
     "distortion factor (0=no distortion)", status );

   fhead_putfits ( TINT,
     "CONVSHAP", &conv_shape,
     "convolution function (Gaussian=0)", status );

   fhead_putfits ( TDOUBLE,
     "CONV_SIG", &conv_sig,
     "convolution function parameter", status );

   fhead_putfits ( TINT,
     "NBOLX", &ncol,
     "number of bolometers in X direction", status );

   fhead_putfits ( TINT,
     "NBOLY", &nrow,
     "number of bolometers in Y direction", status );

   fhead_putfits ( TDOUBLE,
     "SAMPLE_T", &sample_t,
     "The sample interval in msec", status );

   fhead_putfits ( TSTRING,
     "SUBARRAY", subarray,
     "subarray name", status );

   fhead_putfits ( TSTRING,
     "OBSMODE", obsmode,
     "Observation mode", status );

   fhead_putfits ( TINT,
     "NVERT", &nvert,
     "Nr of vertices in the Jiggle pattern", status );

   fhead_putfits ( TINT,
     "MOVECODE", &move_code,
     "Code for the SMU move algorithm", status );

   fhead_putfits ( TDOUBLE,
     "JIG_STEPX", &jig_stepx,
     "The Jiggle step value in -X-direction on the sky in arcsec", 
     status );

   fhead_putfits ( TDOUBLE,
     "JIG_STEPY", &jig_stepy,
     "The Jiggle step value in -Y-direction on the sky in arcsec", 
     status );

   fhead_putfits ( TINT,
     "NCYCLE", &ncycle,
     "number of cycles", status );

   fhead_putfits ( TINT,
     "NUMSAMP", &numsamples,
     "number of samples", status );

   /* Insert static FITS header keywords */
   /* Telescope-specific keywords*/
   fhead_putfits ( TSTRING,
     "TELESCOP", "JCMT",
     "Name of telescope", status );
   fhead_putfits ( TSTRING,
     "OBSGEO-X", "-1.601185365E+06",
     "X cpt of posn rel to centre of Earth [m]", status );
   fhead_putfits ( TSTRING,
     "OBSGEO-Y", "-5.041977547E+06",
     "Y cpt [m]", status );
   fhead_putfits ( TSTRING,
     "OBSGEO-Z", "3.554875870E+06",
     "Z cpt [m]", status );
   fhead_putfits ( TSTRING,
     "ALT-OBS", "4092",
     "Height above sea level [m]", status );
   fhead_putfits ( TSTRING,
     "LAT-OBS", "19.8258323669",
     "Latitude [deg]", status );
   fhead_putfits ( TSTRING,
     "LONG-OBS", "204.520278931",
     "Longitude east [deg]", status );

   /* Observation, date specific*/
   fhead_putfits ( TSTRING,
     "OBJECT", &object,
     "Object name", status );
   fhead_putfits ( TINT,
     "OBSNUM", &obsnum,
     "Observation number", status );
   fhead_putfits ( TINT,
     "SCANNUM", &scannum,
     "Scan number", status );
   fhead_putfits ( TSTRING,
     "OBSEND", obsend,
     "True if frame is last in current observation", status );
   fhead_putfits ( TSTRING,
     "OBSMODE", "DREAM",
     "Observation mode", status );
   fhead_putfits ( TSTRING,
     "OBSTYPE", "IMAGE",
     "Observation type", status );
   fhead_putfits ( TDOUBLE,
     "AMSTART", &amstart,
     "Air mass at start", status );
   fhead_putfits ( TDOUBLE,
     "AMEND", &amend,
     "Air mass at end", status );
   /* Other */
   fhead_putfits ( TDOUBLE,
     "MEANWVM", &meanwvm,
     "Mean zenith tau at 225 GHz from WVM", status );
   fhead_putfits ( TSTRING,
     "INSTRUME", "SCUBA-2",
     "Instrument", status );
   fhead_putfits ( TSTRING,
     "SUBARRAY", subarray,
     "Subarray; s8(a-d) for 850um, s4(a-d) for 450um", status );
   fhead_putfits ( TDOUBLE,
     "INT-TIME", &inttime,
     "Integration time [s]", status );
   /* Store input flux from ast.fits */
   fhead_putfits ( TDOUBLE,
     "ASTFLUX", &astflux,
     "Flux of point sources in input image [Jy]", status );

   fhead_putfits ( TSTRING,
     "FILTER", filter,
     "filter used", status );

   fhead_putfits ( TDOUBLE,
     "ATSTART", &atstart,
     "Ambient temperature at start (C)", status );

   fhead_putfits ( TDOUBLE,
     "ATEND", &atend,
     "Ambient temperature at end (C)", status );

/* Get the accumulated FITS headers */

   fhead_getfits ( &nrec, fitsrec, status );

/* Store the timestream data */

   sc2store_wrtstream ( file_name, nrec, fitsrec, ncol, nrow, numsamples, 
     nflat, flatname, head, dbuf, dksquid, fcal, fpar, status );

/* Close the file */

   sc2store_free ( status );
}


/*+ dsim_ndfwrheat - digitise and compress the heater simulation and store
    as NDF */

void dsim_ndfwrheat
(
int add_atm,       /* flag for adding atmospheric emission (given) */
int add_fnoise,    /* flag for adding 1/f noise (given) */
int add_pns,       /* flag for adding photon noise (given) */
double heatstart,  /* initial heater setting in pW (given) */
double heatstep,   /* increment of heater setting in pW (given) */
char file_name[],  /* output file name (given) */
int ncol,          /* number of bolometers in column (given) */
int nrow,          /* number of bolometers in row (given) */
double sample_t,   /* sample interval in msec (given) */
char subarray[],   /* name of the subarray */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
struct sc2head *head, /* header data for each frame (given) */
int *dbuf,         /* time stream data (given) */
int *dksquid,      /* dark SQUID time stream data (given) */
double *fcal,      /* flat-field calibration (given) */
double *fpar,      /* flat-field parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
int *status        /* global status (given and returned) */
)

/*  Description :
     Create and map a SCUBA-2 NDF file. Scale the data to integers one
     frame at a time and add a compressed version to the mapped file.
     Store the per-frame header items and the FITS headers.

    Authors :
     B.D.Kelly (UKATC)

    History :
     10Feb2005:  original (bdk)
     11Feb2005:  add heatval (bdk)
     20May2005:  add flatcal (bdk)
     08Jul2005:  change use of fhead library (bdk)
     19Aug2005:  pass-in digitised data, remove digitisation parameters
                 and unused flags (bdk)
*/

{
   static char fitsrec[FHEAD__MXREC][81];  /* store for FITS records */
   double fpos;                     /* RA or Dec in degrees */
   int nrec;                        /* number of FITS header records */


   if ( !StatusOkP(status) ) return;


/* Format the FITS headers */

   fhead_init ( status );

   fhead_putfits ( TSTRING,
     "DATE-OBS", "YYYY-MM-DDThh:mm:ss",
     "observation date", status );
   fhead_putfits ( TDOUBLE,
     "RA", &fpos,
     "Right Ascension of observation", status );
   fhead_putfits ( TDOUBLE,
     "DEC", &fpos,
     "Declination of observation", status );


   fhead_putfits ( TINT,
     "ADD_ATM", &add_atm,
     "flag for adding atmospheric emission", status );

   fhead_putfits ( TINT,
     "ADDFNOIS", &add_fnoise,
     "flag for adding 1/f noise", status );

   fhead_putfits ( TINT,
     "ADD_PNS", &add_pns,
     "flag for adding photon noise", status );

   fhead_putfits ( TDOUBLE,
     "HEATVAL", &heatstart,
     "heater setting in pW", status );

   fhead_putfits ( TINT,
     "NBOLX", &ncol,
     "number of bolometers in X direction", status );

   fhead_putfits ( TINT,
     "NBOLY", &nrow,
     "number of bolometers in Y direction", status );

   fhead_putfits ( TDOUBLE,
     "SAMPLE_T", &sample_t,
     "The sample interval in msec", status );

   fhead_putfits ( TSTRING,
     "SUBARRAY", subarray,
     "subarray name", status );
   
   /*
   fhead_putfits ( TINT,
     "SUBSYSNR", &subsysnr,
     "subsystem number", status );
   */

   fhead_putfits ( TINT,
     "NUMSAMP", &numsamples,
     "number of samples", status );

   fhead_putfits ( TSTRING,
     "FILTER", filter,
     "filter used", status );

   fhead_putfits ( TDOUBLE,
     "ATSTART", &atstart,
     "Ambient temperature at start (C)", status );

   fhead_putfits ( TDOUBLE,
     "ATEND", &atend,
     "Ambient temperature at end (C)", status );

/* Get the accumulated FITS headers */

   fhead_getfits ( &nrec, fitsrec, status );

/* Store the timestream data */

   sc2store_wrtstream ( file_name, nrec, fitsrec, ncol, nrow, numsamples, 
     nflat, flatname, head, dbuf, dksquid, fcal, fpar, status );

/* Close the file */

   sc2store_free ( status );
}


/*+ dsim_ndfwrpol - digitise and compress the polarimeter simulation and store
    as NDF */

void dsim_ndfwrpol
(
int add_atm,       /* flag for adding atmospheric emission (given) */
int add_fnoise,    /* flag for adding 1/f noise (given) */
int add_pns,       /* flag for adding photon noise (given) */
int flux2cur,      /* flag for converting flux to current (given) */
double distfac,    /* distortion factor (0=no distortion) (given) */
double heatval,    /* heater setting in pW (given) */
char file_name[],  /* output file name (given) */
int ncol,          /* number of bolometers in column (given) */
int nrow,          /* number of bolometers in row (given) */
double sample_t,   /* sample interval in msec (given) */
char subarray[],   /* name of the subarray */
int ncycle,        /* number of cycles (given) */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
struct sc2head *head, /* header data for each frame (given) */
int *dbuf,         /* simulated data (given) */
int *dksquid,      /* dark SQUID time stream data (given) */
double *fcal,      /* flatfield calibration (given) */
double *fpar,      /* flat-field parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
int *status        /* global status (given and returned) */
)

/*  Description :
     Create and map a SCUBA-2 NDF file. Scale the data to integers one
     frame at a time and add a compressed version to the mapped file.
     Store the per-frame header items and the FITS headers.

    Authors :
     B.D.Kelly (UKATC)

    History :
     10Feb2005:  original (bdk)
     11Feb2005:  add heatval (bdk)
     08Jul2005:  change use of fhead library (bdk)
*/

{
   static char fitsrec[FHEAD__MXREC][81];  /* store for FITS records */
   double fpos;                     /* RA or Dec in degrees */
   int nrec;                        /* number of FITS header records */


   if ( !StatusOkP(status) ) return;


/* Format the FITS headers */

   fpos = 0.0;
   fhead_init ( status );

   fhead_putfits ( TSTRING,
     "DATE-OBS", "YYYY-MM-DDThh:mm:ss",
     "observation date", status );
   fhead_putfits ( TDOUBLE,
     "RA", &fpos,
     "Right Ascension of observation", status );
   fhead_putfits ( TDOUBLE,
     "DEC", &fpos,
     "Declination of observation", status );


   fhead_putfits ( TINT,
     "ADD_ATM", &add_atm,
     "flag for adding atmospheric emission", status );

   fhead_putfits ( TINT,
     "ADDFNOIS", &add_fnoise,
     "flag for adding 1/f noise", status );

   fhead_putfits ( TINT,
     "ADD_PNS", &add_pns,
     "flag for adding photon noise", status );

   fhead_putfits ( TINT,
     "FLUX2CUR", &flux2cur,
     "flag for converting flux to current", status );

   fhead_putfits ( TDOUBLE,
     "DISTFAC", &distfac,
     "distortion factor (0=no distortion)", status );

   fhead_putfits ( TDOUBLE,
     "HEATVAL", &heatval,
     "heater setting in pW", status );

   fhead_putfits ( TINT,
     "NBOLX", &ncol,
     "number of bolometers in X direction", status );

   fhead_putfits ( TINT,
     "NBOLY", &nrow,
     "number of bolometers in Y direction", status );

   fhead_putfits ( TDOUBLE,
     "SAMPLE_T", &sample_t,
     "The sample interval in msec", status );

   fhead_putfits ( TSTRING,
     "SUBARRAY", subarray,
     "subarray name", status );

   fhead_putfits ( TINT,
     "NCYCLE", &ncycle,
     "number of cycles", status );

   fhead_putfits ( TINT,
     "NUMSAMP", &numsamples,
     "number of samples", status );

   fhead_putfits ( TSTRING,
     "FILTER", filter,
     "filter used", status );

   fhead_putfits ( TDOUBLE,
     "ATSTART", &atstart,
     "Ambient temperature at start (C)", status );

   fhead_putfits ( TDOUBLE,
     "ATEND", &atend,
     "Ambient temperature at end (C)", status );

/* Get the accumulated FITS headers */

   fhead_getfits ( &nrec, fitsrec, status );

/* Store the timestream data */

   sc2store_wrtstream ( file_name, nrec, fitsrec, ncol, nrow, numsamples, 
     nflat, flatname, head, dbuf, dksquid, fcal, fpar, status );

/* Close the file */

   sc2store_free ( status );
}


/*+ dsim_ndfwrpong - digitise and compress a PONG simulation and store
    as NDF */

void dsim_ndfwrpong
(
double ra,        /* RA of observation in radians (given) */
double dec,       /* Dec of observation in radians (given) */
int add_atm,      /* flag for adding atmospheric emission (given) */
int add_fnoise,   /* flag for adding 1/f noise (given) */
int add_pns,      /* flag for adding photon noise (given) */
int flux2cur,     /* flag for converting flux to current (given) */
double amstart,   /* Airmass at beginning (given) */
double amend,     /* Airmass at end (given) */
double meanwvm,   /* 225 GHz tau */
double obslam,    /* Wavelength */
char file_name[], /* output file name (given) */
int ncol,         /* number of bolometers in column (given) */
int nrow,         /* number of bolometers in row (given) */
double sample_t,  /* sample interval in msec (given) */
char subarray[],  /* name of the subarray */
int numsamples,   /* number of samples (given) */
int nflat,        /* number of flat coeffs per bol (given) */
char *flatname,   /* name of flatfield algorithm (given) */
struct sc2head *head, /* header data for each frame (given) */
int *dbuf,        /* simulated data (given) */
int *dksquid,     /* dark SQUID time stream data (given) */
double *fcal,     /* flatfield calibration (given) */
double *fpar,     /* flat-field parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
double gridcount, /* Number of grid points (given) */
double spacing,   /* Spacing in arcsec (given) */
double angle,     /* Rotation angle of PONG pattern in radians (given) */
char *obsmode,    /* Observing mode */
int *status       /* global status (given and returned) */
)

/*  Description :
     Create and map a SCUBA-2 NDF file. Scale the data to integers one
     frame at a time and add a compressed version to the mapped file.
     Store the per-frame header items and the FITS headers.

    Authors :
     B.D.Kelly (UKATC)
     A.G. Gibb (UBC)

    History :
     08Jul2005:  dsim_ndfwrscan adapted for dsim_ndfwrpong (bdk)
     24Mar2006:  First quick method for calculating & writing out map 
                 width/height, PA, offsets. Needs more work. (agg)
     09May2005:  Add obsmode (agg)
*/

{
   double decd;                     /* Dec of observation in degrees */
   static char fitsrec[FHEAD__MXREC][81];  /* store for FITS records */
   int nrec;                        /* number of FITS header records */
   double rad;                      /* RA of observation in degrees */
   double pong_hght;  /* Height in arcsec */
   double pong_wdth;  /* Width of PONG pattern in arcsec  */
   double map_hght;   /* Map height in arcsec */
   double map_wdth;   /* Map width in arcsec  */
   double map_pa;     /* Map PA in degrees  */
   double map_x = 0;  /* Map X offset in arcsec */
   double map_y = 0;  /* Map Y offset in arcsec */

   if ( !StatusOkP(status) ) return;

/* Format the FITS headers */

   fhead_init ( status );

   fhead_putfits ( TSTRING,
     "DATE-OBS", "YYYY-MM-DDThh:mm:ss",
     "observation date", status );

   rad = ra * AST__DR2D;

   fhead_putfits ( TDOUBLE,
     "RA", &rad,
     "Right Ascension of observation", status );

   decd = dec * AST__DR2D;

   fhead_putfits ( TDOUBLE,
     "DEC", &decd,
     "Declination of observation", status );

   fhead_putfits ( TINT,
     "ADD_ATM", &add_atm,
     "flag for adding atmospheric emission", status );

   fhead_putfits ( TINT,
     "ADDFNOIS", &add_fnoise,
     "flag for adding 1/f noise", status );

   fhead_putfits ( TINT,
     "ADD_PNS", &add_pns,
     "flag for adding photon noise", status );

   fhead_putfits ( TINT,
     "FLUX2CUR", &flux2cur,
     "flag for converting flux to current", status );

   fhead_putfits ( TINT,
     "NBOLX", &ncol,
     "number of bolometers in X direction", status );

   fhead_putfits ( TINT,
     "NBOLY", &nrow,
     "number of bolometers in Y direction", status );

   fhead_putfits ( TDOUBLE,
     "SAMPLE_T", &sample_t,
     "The sample interval in msec", status );

   fhead_putfits ( TSTRING,
     "SUBARRAY", subarray,
     "subarray name", status );

   fhead_putfits ( TSTRING,
     "OBSMODE", obsmode,
     "Observation mode", status );
   /*
   fhead_putfits ( TINT,
     "SUBSYSNR", &subsysnr,
     "subsystem number", status );
   */

   fhead_putfits ( TINT,
     "NUMSAMP", &numsamples,
     "number of samples", status );

   fhead_putfits ( TDOUBLE,
     "AMSTART", &amstart,
     "Air mass at start", status );

   fhead_putfits ( TDOUBLE,
     "AMEND", &amend,
     "Air mass at end", status );

   /* Other */
   fhead_putfits ( TDOUBLE,
     "MEANWVM", &meanwvm,
     "Mean zenith tau at 225 GHz from WVM", status );

   fhead_putfits ( TSTRING,
     "FILTER", filter,
     "filter used", status );

   fhead_putfits ( TDOUBLE,
     "ATSTART", &atstart,
     "Ambient temperature at start (C)", status );

   fhead_putfits ( TDOUBLE,
     "ATEND", &atend,
     "Ambient temperature at end (C)", status );

   /* PONG pattern is currently square so W and H are equal */
   pong_wdth = (double)(gridcount - 1) * spacing;
   pong_hght = pong_wdth;
   map_pa = 0; /* Unknown at the moment */
   
   map_wdth = pong_wdth*cos(angle) + pong_hght*sin(angle);
   map_hght = pong_wdth*sin(angle) + pong_hght*cos(angle);

   /*   printf("pong_wdth = %g, angle = %g, map_wdth = %g, map_hght = %g\n",pong_wdth,angle,map_wdth,map_hght);*/

   fhead_putfits ( TDOUBLE,
                 "MAP_HGHT", &map_hght,
                 "Map height (arcsec)", status );
   fhead_putfits ( TDOUBLE,
                 "MAP_WDTH", &map_wdth,
                 "Map height (arcsec)", status );
   fhead_putfits ( TDOUBLE,
                 "MAP_PA", &map_pa,
                 "Map PA (degrees)", status );

   /* Simulator CURRENTLY has RA, Dec as the starting position of the
      scan, NOT the map centre. Therefore, need to calculate the
      effective offsets in the X and Y directions */

   map_x = map_wdth/2.0;
   map_y = 0;

   fhead_putfits ( TDOUBLE,
                 "MAP_X", &map_x,
                 "Map X offset (arcsec)", status );
   fhead_putfits ( TDOUBLE,
                 "MAP_Y", &map_y,
                 "Map Y offset (arcsec)", status );

/* Get the accumulated FITS headers */

   fhead_getfits ( &nrec, fitsrec, status );

/* Store the timestream data */

   sc2store_wrtstream ( file_name, nrec, fitsrec, ncol, nrow, numsamples, 
     nflat, flatname, head, dbuf, dksquid, fcal, fpar, status );

/* Close the file */

   sc2store_free ( status );
}


/*+ dsim_ndfwrscan - digitise and compress the SCAN simulation and store
    as NDF */

void dsim_ndfwrscan
(
int add_atm,      /* flag for adding atmospheric emission (given) */
int add_fnoise,   /* flag for adding 1/f noise (given) */
int add_pns,      /* flag for adding photon noise (given) */
int flux2cur,     /* flag for converting flux to current (given) */
double distfac,   /* distortion factor (0=no distortion) (given) */
char file_name[], /* output file name (given) */
double xstart,    /* X position at start of scan (given) */
double ystart,    /* Y position at start of scan (given) */
double xvel,      /* scan velocity in X (given) */
double yvel,      /* scan velocity in Y (given) */
int ncol,         /* number of bolometers in column (given) */
int nrow,         /* number of bolometers in row (given) */
double sample_t,  /* sample interval in msec (given) */
char subarray[],  /* name of the subarray */
int numsamples,   /* number of samples (given) */
int nflat,        /* number of flat coeffs per bol (given) */
char *flatname,   /* name of flatfield algorithm (given) */
struct sc2head *head, /* header data for each frame (given) */
int *dbuf,        /* simulated data (given) */
int *dksquid,     /* dark SQUID time stream data (given) */
double *fcal,     /* flatfield calibration (given) */
double *fpar,     /* flat-field parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
int *status       /* global status (given and returned) */
)

/*  Description :
     Create and map a SCUBA-2 NDF file. Scale the data to integers one
     frame at a time and add a compressed version to the mapped file.
     Store the per-frame header items and the FITS headers.

    Authors :
     B.D.Kelly (UKATC)

    History :
     30Sep2004:  original (bdk)
     08Jul2005:  change use of fhead library (bdk)
*/

{
   static char fitsrec[FHEAD__MXREC][81];  /* store for FITS records */
   double fpos;                     /* RA or Dec in degrees */
   int nrec;                        /* number of FITS headers */


   if ( !StatusOkP(status) ) return;


/* Format the FITS headers */

   fhead_init ( status );

   fhead_putfits ( TSTRING,
     "DATE-OBS", "YYYY-MM-DDThh:mm:ss",
     "observation date", status );
   fhead_putfits ( TDOUBLE,
     "RA", &fpos,
     "Right Ascension of observation", status );
   fhead_putfits ( TDOUBLE,
     "DEC", &fpos,
     "Declination of observation", status );


   fhead_putfits ( TINT,
     "ADD_ATM", &add_atm,
     "flag for adding atmospheric emission", status );

   fhead_putfits ( TINT,
     "ADDFNOIS", &add_fnoise,
     "flag for adding 1/f noise", status );

   fhead_putfits ( TINT,
     "ADD_PNS", &add_pns,
     "flag for adding photon noise", status );

   fhead_putfits ( TINT,
     "FLUX2CUR", &flux2cur,
     "flag for converting flux to current", status );

   fhead_putfits ( TDOUBLE,
     "DISTFAC", &distfac,
     "distortion factor (0=no distortion)", status );

   fhead_putfits ( TDOUBLE,
     "XSTART", &xstart,
     "X start of scan", status );

   fhead_putfits ( TDOUBLE,
     "YSTART", &ystart,
     "Y start of scan", status );

   fhead_putfits ( TDOUBLE,
     "XVEL", &xvel,
     "X scan speed in arcsec/sec", status );

   fhead_putfits ( TDOUBLE,
     "YVEL", &yvel,
     "Y scan speed in arcsec/sec", status );

   fhead_putfits ( TINT,
     "NBOLX", &ncol,
     "number of bolometers in X direction", status );

   fhead_putfits ( TINT,
     "NBOLY", &nrow,
     "number of bolometers in Y direction", status );

   fhead_putfits ( TDOUBLE,
     "SAMPLE_T", &sample_t,
     "The sample interval in msec", status );

   fhead_putfits ( TSTRING,
     "SUBARRAY", subarray,
     "subarray name", status );

   /*
   fhead_putfits ( TINT,
     "SUBSYSNR", &subsysnr,
     "subsystem number", status );
   */

   fhead_putfits ( TINT,
     "NUMSAMP", &numsamples,
     "number of samples", status );

   fhead_putfits ( TSTRING,
     "FILTER", filter,
     "filter used", status );

   fhead_putfits ( TDOUBLE,
     "ATSTART", &atstart,
     "Ambient temperature at start (C)", status );

   fhead_putfits ( TDOUBLE,
     "ATEND", &atend,
     "Ambient temperature at end (C)", status );

/* Get the accumulated FITS headers */

   fhead_getfits ( &nrec, fitsrec, status );

/* Store the timestream data */

   sc2store_wrtstream ( file_name, nrec, fitsrec, ncol, nrow, numsamples, 
     nflat, flatname, head, dbuf, dksquid, fcal, fpar, status );

/* Close the file */

   sc2store_free ( status );
}


/*+ dsim_ndfwrstare - digitise and compress a STARE simulation and store
    as NDF */

void dsim_ndfwrstare
(
AstFrameSet *fset, /* World coordinate transformations (given) */
double ra,         /* RA of observation in radians (given) */
double dec,        /* Dec of observation in radians (given) */
int add_atm,       /* flag for adding atmospheric emission (given) */
int add_fnoise,    /* flag for adding 1/f noise (given) */
int add_pns,       /* flag for adding photon noise (given) */
int flux2cur,      /* flag for converting flux to current (given) */
double distfac,    /* distortion factor (0=no distortion) (given) */
double heatval,    /* heater setting in pW (given) */
char file_name[],  /* output file name (given) */
int ncol,          /* number of bolometers in column (given) */
int nrow,          /* number of bolometers in row (given) */
double sample_t,   /* sample interval in msec (given) */
char subarray[],   /* name of the subarray */
int numsamples,    /* number of samples (given) */
int nflat,         /* number of flat coeffs per bol (given) */
char *flatname,    /* name of flatfield algorithm (given) */
struct sc2head *head, /* header data for each frame (given) */
int *dbuf,         /* simulated data (given) */
int *dksquid,      /* dark SQUID values (given) */
double *fcal,      /* flatfield calibration (given) */
double *fpar,      /* flatfield parameters (given) */
char filter[],    /* String representing filter (e.g. "850") (given) */
double atstart,   /* Ambient temperature at start (Celsius) (given) */
double atend,     /* Ambient temperature at end (Celsius) (given) */
char *obsmode,     /* Observing mode */
int *status        /* global status (given and returned) */
)

/*  Description :
     Create and map a SCUBA-2 NDF file. Scale the data to integers one
     frame at a time and add a compressed version to the mapped file.
     Create and average frame and store it.
     Store the per-frame header items and the FITS headers.

    Authors :
     B.D.Kelly (UKATC)

    History :
     14Nov2004:  original (bdk)
     01Feb2005:  convert ndfwrheat to ndfwrstare and add heatval argument
                 (bdk)
     19Apr2005:  pass in FrameSet, fix range of sequence numbers stored,
                 pass-in ra and dec (bdk)
     20May2005:  add flatcal (bdk)
     08Jun2005:  change order of averaging and flatfielding (bdk)
     13Jun2005:  change arguments to sc2store_putimage (bdk)
     08Jul2005:  change use of fhead library (bdk)
     06Oct2005:  use new data interface (bdk)
*/

{
   static double coadd[2*DREAM__MXBOL];  /* coadded values */
   double decd;                     /* Dec of observation in degrees */
   int dims[2];                     /* image dimensions */
   static char fitsrec[FHEAD__MXREC][81];  /* store for FITS records */
   int framesize;                   /* number of pixels in a frame */
   int i;                           /* loop counter */
   int j;                           /* loop counter */
   int ndim;                        /* number of image dimensions */
   int nrec;                        /* number of FITS header records */
   double rad;                      /* RA of observation in degrees */
   double *rdata;                   /* double copy of data */
   static double zero[2*DREAM__MXBOL];   /* bolometer zero offsets */


   if ( !StatusOkP(status) ) return;

/* Format the FITS headers */

   rad = ra * AST__DR2D;
   decd = dec * AST__DR2D;

   fhead_init ( status );

   fhead_putfits ( TSTRING,
     "DATE-OBS", "YYYY-MM-DDThh:mm:ss",
     "observation date", status );
   fhead_putfits ( TDOUBLE,
     "RA", &rad,
     "Right Ascension of observation", status );
   fhead_putfits ( TDOUBLE,
     "DEC", &decd,
     "Declination of observation", status );


   fhead_putfits ( TINT,
     "ADD_ATM", &add_atm,
     "flag for adding atmospheric emission", status );

   fhead_putfits ( TINT,
     "ADDFNOIS", &add_fnoise,
     "flag for adding 1/f noise", status );

   fhead_putfits ( TINT,
     "ADD_PNS", &add_pns,
     "flag for adding photon noise", status );

   fhead_putfits ( TINT,
     "FLUX2CUR", &flux2cur,
     "flag for converting flux to current", status );


   fhead_putfits ( TDOUBLE,
     "DISTFAC", &distfac,
     "distortion factor (0=no distortion)", status );

   fhead_putfits ( TDOUBLE,
     "HEATVAL", &heatval,
     "heater setting in pW", status );

   fhead_putfits ( TINT,
     "NBOLX", &ncol,
     "number of bolometers in X direction", status );

   fhead_putfits ( TINT,
     "NBOLY", &nrow,
     "number of bolometers in Y direction", status );

   fhead_putfits ( TDOUBLE,
     "SAMPLE_T", &sample_t,
     "The sample interval in msec", status );

   fhead_putfits ( TSTRING,
     "SUBARRAY", subarray,
     "subarray name", status );

   fhead_putfits ( TSTRING,
     "OBSMODE", obsmode,
     "Observation mode", status );

   /*
   fhead_putfits ( TINT,
     "SUBSYSNR", &subsysnr,
     "subsystem number", status );
   */

   fhead_putfits ( TINT,
     "NUMSAMP", &numsamples,
     "number of samples", status );

   fhead_putfits ( TSTRING,
     "FILTER", filter,
     "filter used", status );

   fhead_putfits ( TDOUBLE,
     "ATSTART", &atstart,
     "Ambient temperature at start (C)", status );

   fhead_putfits ( TDOUBLE,
     "ATEND", &atend,
     "Ambient temperature at end (C)", status );

/* Get the accumulated FITS headers */

   fhead_getfits ( &nrec, fitsrec, status );

/* Store the timestream data */

   sc2store_wrtstream ( file_name, nrec, fitsrec, ncol, nrow, numsamples, 
     nflat, flatname, head, dbuf, dksquid, fcal, fpar, status );

/* Initialise the coadd frame */

   framesize = ncol * nrow;
   rdata = (double *)calloc ( framesize*numsamples, sizeof(double) );

   for ( j=0; j<framesize; j++ )
   {
      coadd[j] = 0.0;
      zero[j] = 0.0;
   }

   for ( j=0; j<framesize*numsamples; j++ )
   {
      rdata[j] = (double)dbuf[j];
   }

/* apply flatfield */

   sc2math_flatten ( framesize, numsamples, flatname, nflat, fcal, fpar,
     rdata, status );

   for ( j=0; j<numsamples; j++ )
   {

/* coadd frame */

      for ( i=0; i<framesize; i++ )
      {
         coadd[i] += rdata[framesize*j+i];
      }
	
   }


/* Average the coadd frame */

   for ( j=0; j<framesize; j++ )
   {
      coadd[j] /= (double)numsamples;
   }

/* Store the averaged frame */

   ndim = 2;
   dims[0] = ncol;
   dims[1] = nrow;
   sc2store_creimages ( status );
   sc2store_putimage ( 0, fset, ndim, dims, 1, numsamples, ncol, nrow, coadd, 
     zero, fitsrec, 0, status );

/* Close the file */

   sc2store_free ( status );
   free ( rdata );
}


/*+ dsim_pongframe - Simulate a single frame from a PONG map */

void dsim_pongframe
(
struct dxml_struct inx,      /* structure for values from XML (given) */
struct dxml_sim_struct sinx, /* structure for sim values from XML (given)*/
long astnaxes[2],            /* dimensions of simulated image (given) */
double astscale,             /* pixel size in simulated image (given) */
double *astsim,              /* astronomical sky (given) */
long atmnaxes[2],            /* dimensions of simulated atm background
                                (given) */
double atmscale,             /* pixel size in simulated atm background
                                (given) */
double *atmsim,              /* atmospheric emission (given) */
double coeffs[],             /* bolometer response coeffs (given) */
double heater[],             /* bolometer heater ratios (given) */
int nboll,                   /* total number of bolometers (given) */
int frame,                   /* number of current frame (given) */
int nterms,                  /* number of 1/f noise coeffs (given) */
double *noisecoeffs,         /* 1/f noise coeffs (given) */
double *pzero,               /* bolometer power offsets (given) */
double samptime,             /* sample time in sec (given) */
double start_time,           /* time at start of scan in sec  (given) */
double telemission,          /* power from telescope emission (given) */
double *weights,             /* impulse response (given) */
AstCmpMap *bolo2map,         /* mapping bolo->sky image coordinates */
double *xbolo,               /* native X offsets of bolometers */
double *ybolo,               /* native Y offsets of bolometers */
double *xbc,                 /* nasmyth X offsets of bolometers */
double *ybc,                 /* nasmyth Y offsets of bolometers */
double *position,            /* nasmyth positions of bolometers */
double *dbuf,                /* generated frame (returned) */
int *status                  /* global status (given and returned) */
)

/*  Method :
     Interpolate values of atmospheric background and astronomical image
     for each bolometer in the current frame. Simulate photon and
     instrumental noise and bolometer responses.
    Authors :
     B.D.Kelly (ROE)
     E.Chapin (UBC)

    History :
     08Jul2005: original (bdk@roe.ac.uk)
     28Feb2006: modified to use framesets to connect bolos with actual
                WCS info for input sky image (ec)
*/

{


  double astvalue;                /* obs. astronomical value in pW */
  double atmvalue;                /* obs. atmospheric emission in pW */
  int bol;                        /* counter for indexing bolometers */
  double current;                 /* bolometer current in amps */
  double flux;                    /* flux at bolometer in pW */
  double fnoise;                  /* 1/f noise value */
  int i;                          /* loop counter */
  double phase;                   /* 1/f phase calculation */
  int pos;                        /* lookup in noise coefficients */
  double skytrans;                /* sky transmission (%) */
  double time;                    /* time from start of observation */
  double xpos;                    /* X measurement position */
  double xsky;                    /* X position on sky screen */
  double ypos;                    /* Y measurement position */
  double ysky;                    /* Y position on sky screen */
  
  
  if ( !StatusOkP(status) ) return; 
    
  /* Sample astronomical sky image */
  dsim_getast_wcs( nboll, xbolo, ybolo, bolo2map, astsim, astnaxes, dbuf,
		   status);

  /* Get time when frame taken in seconds */
  time = start_time + frame * samptime;
  
  for ( bol=0; bol<nboll; bol++ ) {
    xpos = position[0] + xbc[bol] + 0.5 * (double)astnaxes[0] * astscale;
    ypos = position[1] + ybc[bol] + 0.5 * (double)astnaxes[1] * astscale;
    
    /*  Interpolate bolometer position on astronomical image.
	The scalar ASTVALUE contains the interpolated astronomical map 
	value for the current position (XPOS,YPOS). */
    
    /*
      dsim_getast ( xpos, ypos, inx.bol_distx, astscale, astnaxes[0],
      astsim, &astvalue, status );
      if ( !StatusOkP(status) ) {
      printf ( 
      "dsim_pongframe: failed to interpolate image bol=%d x=%e y=%e\n",
      bol, xpos, ypos );
      break;
    }
    */

    astvalue = dbuf[bol];

    /*  Lookup atmospheric emission - offset to near centre of the atm frame.
	A typical windspeed moves the sky screen at equivalent to 5000 arcsec
	per sec.
	The scalar ATMVALUE contains the atmosphere map value for the 
	current position (XPOS,YPOS). */
    
    xsky = xpos + sinx.atmxvel * time + sinx.atmzerox;
    ysky = ypos + sinx.atmyvel * time + sinx.atmzeroy;
    if ( sinx.add_atm == 1 ) {
      dsim_getbilinear ( xsky, ysky, atmscale, atmnaxes[0], atmsim, 
			 &atmvalue, status );
      if ( !StatusOkP(status) ) {
	printf ( 
		"dsim_pongframe: failed to interpolate sky bol=%d x=%e y=%e\n",
		bol, xpos, ypos );
	break;
      }
      if ( dream_trace ( 4 ) ) { 
	printf ( "scansim : atm emission interpolated\n" );
	printf ( "status = %d\n", *status );
      }
    }
    else {
      atmvalue = sinx.meanatm;
    }
    
    /* Calculate atmospheric transmission */
    
    dsim_atmtrans ( inx.lambda, atmvalue, &skytrans, status );
    
    /*  Add atmospheric and telescope emission.
	TELEMISSION is a constant value for all bolometers. */
    
    flux = 0.01 * skytrans * astvalue + atmvalue + telemission;
    
    /*  Add offset due to photon noise */
    
    if ( sinx.add_pns == 1 ) {
      dsim_addpnoise ( inx.lambda, sinx.bandGHz, sinx.aomega, 
		       samptime, &flux, status );
      if ( dream_trace ( 4 ) ) { 
	printf ( "scansim : photon noise added\n" );
	printf ( "status = %d\n", *status );
      }
    }
    
    /* Add heater, assuming mean heater level is set to add onto meanatm and
       TELEMISSION to give targetpow */
    
    if ( sinx.add_hnoise == 1 ) {
      flux = flux + 
	( inx.targetpow - sinx.meanatm - telemission ) * heater[bol];
    }
    else {
      flux = flux + ( inx.targetpow - sinx.meanatm - telemission );
    }
    
    /*  Convert to current with bolometer power offset.
	The bolometer offset in PZERO(BOL) is added to the FLUX, and then
	the power in FLUX is converted to a current in scalar CURRENT with 
	help of the polynomial expression with coefficients in COEFFS(*) */
    
    if ( sinx.flux2cur == 1 ) {
      dsim_ptoi ( flux, NCOEFFS, coeffs, pzero[bol], &current,
		  status );
      if ( dream_trace ( 4 ) ) { 
	printf ( "scansim : converted to current\n" );
	printf ( "status = %d\n", *status );
      }
    }
    else {
      current = flux;
    }
    
    if ( sinx.add_fnoise == 1 ) {
      
      /*  Add instrumental 1/f noise to the smoothed data in output */
      
      pos = bol * nterms * 3;
      fnoise = 0.0;
      time = start_time + frame * samptime;
      for ( i=0; i<nterms*3; i+=3 ) {
	phase = fmod ( time, noisecoeffs[pos+i] ) / noisecoeffs[pos+i];
	fnoise += noisecoeffs[pos+i+1] * cos ( 2.0 * AST__DPI * phase )
	  + noisecoeffs[pos+i+2] * sin ( 2.0 * AST__DPI * phase );
      }
      current += fnoise;
      
    }

    dbuf[bol] = current;
    
  }
  
}


/*+ dsim_ptoi - convert input pW to bolometer current */

void dsim_ptoi 
( 
double flux,       /* input flux in pW (given) */
int ncoeffs,       /* number of coefficients describing response curve
                      (given) */
double coeffs[],   /* array to hold response curve coefficients (given) */
double pzero,      /* calibration offset in pW (given) */
double *current,   /* signal from bolometer in amps (returned) */
int *status        /* global status (given and returned) */
)

/*   Description :
      Given a flux in pW, apply the bolometer response curve with the
      bolometer-specific shift.
      The coefficients represent a numerical model of the bolometer
      response by Damian Audley which he fitted to a 5th order polynomial 
      so that
      I = a0 + a1*P +a2*P^2 + a3*P^3 + a4*P^4 + a5*P^5
      where I is the current in amps and P is the input power in picowatts.
 
      This is valid for powers between 0 and 50 pW at 850.

     Authors :
      B.D.Kelly (ROE)
     History :
      26July2001:  original (bdk@roe.ac.uk)
      21Aug2002 ;  C version (bdk)
      11Aug2005 :  trap obviously out-of-range values (bdk)
*/

{
   double p;


   if ( !StatusOkP(status) ) return;

   p = flux + pzero;

   if ( ( p > 0.0 ) && ( p < 200.0 ) )
   {
      *current = coeffs[0] + coeffs[1] * p + coeffs[2] * p*p + 
        coeffs[3] * p*p*p + coeffs[4] * p*p*p*p + coeffs[5] * p*p*p*p*p;
   }
   else
   {
      *status = DITS__APP_ERROR;
      printf ( "dsim_ptoi: flux value outside bolometer range\n" );
   }
}



/*+ dsim_response - return coefficients of average bolometer response */

void dsim_response 
( 
double lambda,     /* wavelength in metres (given) */
int ncoeffs,       /* number of coefficients to be returned (given) */
double coeffs[],   /* array to hold returned coefficients (returned) */
int *status        /* global status (given and returned) */
)

/*   Description :
      Fill the given array with a list of polynomial coefficients 
      representing the bolometer response curve.
      The coefficients represent a numerical model of the bolometer
      response by Damian Audley which he fitted to a 5th order polynomial 
      so that
      I = a0 + a1*P +a2*P^2 + a3*P^3 + a4*P^4 + a5*P^5
      where I is the current in amps and P is the input power in picowatts.
 
      This is valid for powers between 0 and 50 pW.

     Authors :
      B.D.Kelly (ROE)
     History :
      19July2001:  original (bdk@roe.ac.uk)
      21Aug2002 :  C version (bdk)
      16Feb2005 :  allow choice of wavelength (bdk)
      15Apr2005 :  put in coefficients for 450 (bdk)
*/

{


   if ( !StatusOkP(status) ) return;

/* Original 850 values used for DREAM simulations */

   coeffs[0] =  5.88628682e-05;
   coeffs[1] = -2.01609623e-06;
   coeffs[2] = 9.42948262e-08;
   coeffs[3] = -6.2155352e-09;
   coeffs[4] = 1.60394822e-10;
   coeffs[5] = -1.32057874e-12;

/* 850 Values provided by Damian in Nov 2001 but not added until Feb 2005 */

   if ( lambda > 0.8e-3 )
   {
      coeffs[0] =   7.12511028E-05;
      coeffs[1] =  -2.15770388E-06;
      coeffs[2] =  -8.91768082E-08;
      coeffs[3] =   1.04025402E-08;
      coeffs[4] =  -4.69516093E-10;
      coeffs[5] =   7.43680464E-12;
   }
   else
   {

/* 450 values provided by Damian in April 2005 */

      coeffs[0] =   1.19403481E-04;
      coeffs[1] =  -6.28742218E-07;
      coeffs[2] =   2.92078918E-09;
      coeffs[3] =  -2.06985387E-11;
      coeffs[4] =   7.09562826E-14;
      coeffs[5] =  -7.57326701E-17;

   }
}


/*+ dsim_saveast - store a simulated astronomical image in a FITS file */

void dsim_saveast 
(
char file_name[], /* output file name (given) */
double fluxJy,       /* Flux of each point source (given) */
double diam,         /* diameter of telescope aperture in metres (given) */
double fwhm,         /* fwhm of telescope PSF (given) */
double spacingx,     /* spacing in arcsec between sources in X (given) */
double spacingy,     /* spacing in arcsec between sources in Y (given) */
double transmission, /* transmission of optics as a fraction (given) */
double bandGHz,      /* optical bandwidth in GHz (given) */
double pixsize,   /* distance between samples in arcsec (given) */
int nbolx,        /* number of bolometers in X (given) */
int nboly,        /* number of bolometers in Y (given) */
double *dbuf,     /* simulated data (given) */
int *status       /* global status (given and returned) */
)

/*  Description :
     Store the simulated image plus all information to a FITS
     file.

    Authors :
     B.D.Kelly (UKATC)
     A.G. Gibb (UBC)

    History :
     20Oct2003:  original (bdk)
     31Jan2005:  First hack at an astronomical header (agg)
*/

{
   fitsfile *fptr;         /* fitsio file pointer */
   long naxis;             /* array dimensionality */
   long naxes[2];          /* array dimensions */
   long pstart[2];         /* subarray start coordinates */
   long pend[2];           /* subarray end coordinates */

   double crpix1; /* Reference pixel in X */
   double crpix2; /* Reference pixel in Y */
   double dra;    /* Increment in RA  */
   double ddec;   /* Increment in Dec */

   double ra = 0.0;
   double dec = 0.0; /* HACKS */

   if ( !StatusOkP(status) ) return;
   fits_create_file ( &fptr, file_name, status );

   naxis = 2;
   naxes[0] = nbolx;
   naxes[1] = nboly;
   pstart[0] = 1;
   pstart[1] = 1;
   pend[0] = naxes[0];
   pend[1] = naxes[1];

   fits_create_img ( fptr, DOUBLE_IMG, naxis, naxes, status );

   /* Quick and dirty way to add RA/Dec header info to o/p FITS file */
   /* First find centre of array */
   crpix1 = (double)(pend[0])/2.0;
   crpix2 = (double)(pend[1])/2.0;
   dra = pixsize/3600.;
   ddec = dra;
   /* Write to FITS file */
   fits_write_key( fptr, TDOUBLE, "CRPIX1", &crpix1, "Reference pixel in X ", status );
   fits_write_key( fptr, TDOUBLE, "CRPIX2", &crpix2, "Reference pixel in Y ", status );
   fits_write_key( fptr, TDOUBLE, "CRVAL1", &ra, "Right Ascension", status );
   fits_write_key( fptr, TDOUBLE, "CRVAL2", &dec, "Declination", status );
   fits_write_key( fptr, TDOUBLE, "CDELT1", &dra, "Increment in RA ", status );
   fits_write_key( fptr, TDOUBLE, "CDELT2", &ddec, "Increment in Dec ", status );
   fits_write_key( fptr, TSTRING, "CTYPE1", "RA---TAN", " ", status );
   fits_write_key( fptr, TSTRING, "CTYPE2", "DEC--TAN", " ", status );
   fits_write_key( fptr, TSTRING, "EPOCH", "2000.0", " ", status);
   /* Normal service will now be resumed... */

   fits_write_key ( fptr, TDOUBLE,
     "FLUXJY", &fluxJy,
     "flux in Jansky", status );

   fits_write_key ( fptr, TDOUBLE,
     "DIAM", &diam,
     "telescope aperture in metres", status );

   fits_write_key ( fptr, TDOUBLE,
     "FWHM", &fwhm,
     "full-width half maximum of PSF in arcsec", status );

   fits_write_key ( fptr, TDOUBLE,
     "SPACINGX", &spacingx,
     "source separation in X in arcsec", status );

   fits_write_key ( fptr, TDOUBLE,
     "SPACINGY", &spacingy,
     "source separation in Y in arcsec", status );

   fits_write_key ( fptr, TDOUBLE,
     "TRANSMIS", &transmission,
     "transmission factor", status );

   fits_write_key ( fptr, TDOUBLE,
     "BANDGHZ", &bandGHz,
     "bandwidth in GHz", status );

   fits_write_key ( fptr, TDOUBLE,
     "PIXSIZE", &pixsize,
     "pixel separation in arcsec", status );


/* Output the whole image */

   fits_write_subset ( fptr, TDOUBLE, pstart, pend, dbuf, status );
  
   fits_close_file ( fptr, status );
  
   fits_report_error ( stderr, *status );
}



/*+ dsim_savesky - store a simulated sky image in a FITS file */

void dsim_savesky 
(
char file_name[], /* output file name (given) */
double sigma,     /* dispersion at corner frequency  (given) */
double corner,    /* corner frequency of the noise spectrum (given) */
double p,         /* power-law exponent (given) */
double pixsize,   /* distance between samples in arcsec (given) */
int nx,           /* size in X (given) */
int ny,           /* size in Y (given) */
double *dbuf,     /* simulated data (given) */
int *status       /* global status (given and returned) */
)

/*  Description :
     Store the simulated image plus all information to a FITS
     file.

    Authors :
     B.D.Kelly (UKATC)

    History :
     12Nov2003:  original (bdk)
*/

{
   fitsfile *fptr;         /* fitsio file pointer */
   long naxis;             /* array dimensionality */
   long naxes[2];          /* array dimensions */
   long pstart[2];         /* subarray start coordinates */
   long pend[2];           /* subarray end coordinates */

   if ( !StatusOkP(status) ) return;

   fits_create_file ( &fptr, file_name, status );

   naxis = 2;
   naxes[0] = nx;
   naxes[1] = ny;
   pstart[0] = 1;
   pstart[1] = 1;
   pend[0] = naxes[0];
   pend[1] = naxes[1];

   fits_create_img ( fptr, DOUBLE_IMG, naxis, naxes, status );



   fits_write_key ( fptr, TDOUBLE,
     "SIGMA", &sigma,
     "dispersion at corner frequency", status );

   fits_write_key ( fptr, TDOUBLE,
     "CORNER", &corner,
     "corner frequency in 1/arcsec", status );

   fits_write_key ( fptr, TDOUBLE,
     "EXPONENT", &p,
     "frequency exponent of inverse power law", status );

   fits_write_key ( fptr, TDOUBLE,
     "PIXSIZE", &pixsize,
     "pixel separation in arcsec", status );


/* Output the whole image */

   fits_write_subset ( fptr, TDOUBLE, pstart, pend, dbuf, status );

   fits_close_file ( fptr, status );

   fits_report_error ( stderr, *status );

}


/*+ dsim_simframe - Simulate a single frame of bolometer data */

void dsim_simframe
(
struct dxml_struct inx,      /* structure for values from XML (given) */
struct dxml_sim_struct sinx, /* structure for sim values from XML (given)*/
long astnaxes[2],            /* dimensions of simulated image (given) */
double astscale,             /* pixel size in simulated image (given) */
double *astsim,              /* astronomical sky (given) */
long atmnaxes[2],            /* dimensions of simulated atm background
                                (given) */
double atmscale,             /* pixel size in simulated atm background
                                (given) */
double *atmsim,              /* atmospheric emission (given) */
double coeffs[],             /* bolometer response coeffs (given) */
double heater[],             /* bolometer heater ratios (given) */
int nboll,                   /* total number of bolometers (given) */
int frame,                   /* number of current frame (given) */
int nterms,                  /* number of 1/f noise coeffs (given) */
double *noisecoeffs,         /* 1/f noise coeffs (given) */
double *pzero,               /* bolometer power offsets (given) */
double samptime,             /* sample time in sec (given) */
double start_time,           /* time at start of scan in sec  (given) */
double telemission,          /* power from telescope emission (given) */
double *weights,             /* impulse response (given) */
AstCmpMap *bolo2map,         /* mapping bolo->sky image coordinates */
double *xbolo,               /* native X offsets of bolometers */
double *ybolo,               /* native Y offsets of bolometers */
double *xbc,                 /* nasmyth X offsets of bolometers */
double *ybc,                 /* nasmyth Y offsets of bolometers */
double *position,            /* nasmyth positions of bolometers */
double *dbuf,                /* generated frame (returned) */
int *status                  /* global status (given and returned) */
)

/*  Method :
     Interpolate values of atmospheric background and astronomical image
     for each bolometer in the current frame. Simulate photon and
     instrumental noise and bolometer responses.
    Authors :
     B.D.Kelly (ROE)
     E.Chapin (UBC)

    History :
     08Jul2005: original (bdk@roe.ac.uk)
     28Feb2006: modified to use framesets to connect bolos with actual
                WCS info for input sky image (ec)
     29Mar2006: renamed from dsim_pongframe to be used generically (ec)
*/

{

  double astvalue;                /* obs. astronomical value in pW */
  double atmvalue;                /* obs. atmospheric emission in pW */
  int bol;                        /* counter for indexing bolometers */
  double current;                 /* bolometer current in amps */
  double flux;                    /* flux at bolometer in pW */
  double fnoise;                  /* 1/f noise value */
  int i;                          /* loop counter */
  double phase;                   /* 1/f phase calculation */
  int pos;                        /* lookup in noise coefficients */
  double skytrans;                /* sky transmission (%) */
  double time;                    /* time from start of observation */
  double xpos;                    /* X measurement position */
  double xsky;                    /* X position on sky screen */
  double ypos;                    /* Y measurement position */
  double ysky;                    /* Y position on sky screen */
  
  
  if ( !StatusOkP(status) ) return; 
    
  /* Sample astronomical sky image */
  dsim_getast_wcs( nboll, xbolo, ybolo, bolo2map, astsim, astnaxes, dbuf,
		   status);

  /* Get time when frame taken in seconds */
  time = start_time + frame * samptime;
  
  for ( bol=0; bol<nboll; bol++ ) {
    xpos = position[0] + xbc[bol] + 0.5 * (double)astnaxes[0] * astscale;
    ypos = position[1] + ybc[bol] + 0.5 * (double)astnaxes[1] * astscale;
    
    /*  Interpolate bolometer position on astronomical image.
	The scalar ASTVALUE contains the interpolated astronomical map 
	value for the current position (XPOS,YPOS). */
    
    /*
      dsim_getast ( xpos, ypos, inx.bol_distx, astscale, astnaxes[0],
      astsim, &astvalue, status );
      if ( !StatusOkP(status) ) {
      printf ( 
      "dsim_pongframe: failed to interpolate image bol=%d x=%e y=%e\n",
      bol, xpos, ypos );
      break;
    }
    */

    astvalue = dbuf[bol];

    /*  Lookup atmospheric emission - offset to near centre of the atm frame.
	A typical windspeed moves the sky screen at equivalent to 5000 arcsec
	per sec.
	The scalar ATMVALUE contains the atmosphere map value for the 
	current position (XPOS,YPOS). */
    
    xsky = xpos + sinx.atmxvel * time + sinx.atmzerox;
    ysky = ypos + sinx.atmyvel * time + sinx.atmzeroy;
    if ( sinx.add_atm == 1 ) {
      dsim_getbilinear ( xsky, ysky, atmscale, atmnaxes[0], atmsim, 
			 &atmvalue, status );
      if ( !StatusOkP(status) ) {
	printf ( 
		"dsim_simframe: failed to interpolate sky bol=%d x=%e y=%e\n",
		bol, xpos, ypos );
	break;
      }
      if ( dream_trace ( 4 ) ) { 
	printf ( "sc2sim : atm emission interpolated\n" );
	printf ( "status = %d\n", *status );
      }
    }
    else {
      atmvalue = sinx.meanatm;
    }
    
    /* Calculate atmospheric transmission */
    
    dsim_atmtrans ( inx.lambda, atmvalue, &skytrans, status );
    
    /*  Add atmospheric and telescope emission.
	TELEMISSION is a constant value for all bolometers. */
    
    flux = 0.01 * skytrans * astvalue + atmvalue + telemission;
    
    /*  Add offset due to photon noise */
    
    if ( sinx.add_pns == 1 ) {
      dsim_addpnoise ( inx.lambda, sinx.bandGHz, sinx.aomega, 
		       samptime, &flux, status );
      if ( dream_trace ( 4 ) ) { 
	printf ( "sc2sim : photon noise added\n" );
	printf ( "status = %d\n", *status );
      }
    }
    
    /* Add heater, assuming mean heater level is set to add onto meanatm and
       TELEMISSION to give targetpow */
    
    if ( sinx.add_hnoise == 1 ) {
      flux = flux + 
	( inx.targetpow - sinx.meanatm - telemission ) * heater[bol];
    }
    else {
      flux = flux + ( inx.targetpow - sinx.meanatm - telemission );
    }
    
    /*  Convert to current with bolometer power offset.
	The bolometer offset in PZERO(BOL) is added to the FLUX, and then
	the power in FLUX is converted to a current in scalar CURRENT with 
	help of the polynomial expression with coefficients in COEFFS(*) */
    
    if ( sinx.flux2cur == 1 ) {
      dsim_ptoi ( flux, NCOEFFS, coeffs, pzero[bol], &current,
		  status );
      if ( dream_trace ( 4 ) ) { 
	printf ( "sc2sim : converted to current\n" );
	printf ( "status = %d\n", *status );
      }
    }
    else {
      current = flux;
    }
    
    if ( sinx.add_fnoise == 1 ) {
      
      /*  Add instrumental 1/f noise to the smoothed data in output */
      
      pos = bol * nterms * 3;
      fnoise = 0.0;
      time = start_time + frame * samptime;
      for ( i=0; i<nterms*3; i+=3 ) {
	phase = fmod ( time, noisecoeffs[pos+i] ) / noisecoeffs[pos+i];
	fnoise += noisecoeffs[pos+i+1] * cos ( 2.0 * AST__DPI * phase )
	  + noisecoeffs[pos+i+2] * sin ( 2.0 * AST__DPI * phase );
      }
      current += fnoise;
      
    }

    dbuf[bol] = current;
    
  }
  
}


/*+ dsim_smooth - apply smoothing kernel */

void dsim_smooth 
( 
int nweights,      /* number of values in kernel (given) */
double weights[],  /* smoothing kernel (given) */
int numvals,       /* number of values in dataset (given) */
double output[],   /* dataset to be smoothed (given and returned) */
int *status        /* global status (given and returned) */
)

/*  Description :
     A dataset is convolved with a kernel representing an impulse
     response function. This means the kernel is not time symmetrical
     (the smoothed value doesn't depend on raw values which haven't
     happened yet), and so the smooth can be performed without a work
     array.

    Authors :
     B.D.Kelly (ROE)
    History :
     21Aug2001:  original (bdk@roe.ac.uk)
     21Aug2002:  C version (bdk)
*/

{
   int j;
   int k;
   double temp;

   if ( !StatusOkP(status) ) return;


/* Perform the main part of the smooth starting at the end of the data */

   for ( k=numvals-1; k>=nweights-1; k-- )
   {
      temp = 0.0;
      for ( j=0; j<nweights; j++ )
      {
         temp = temp + output[k-j] * weights[nweights-1-j];
      }
      output[k] = temp;
   }

/* Handle the start of the data where replication of the first value is
   required */

   for ( k=nweights-2; k>0; k-- )
   {
      temp = 0.0;
      for ( j=0; j<k; j++ )
      {
         temp = temp + output[k-j] * weights[nweights-1-j];
      }
      for ( j=k; j<nweights; j++ )
      {
         temp = temp + output[0] * weights[nweights-1-j];
      }
      output[k] = temp;
   }

}


/*+ dsim_starerange - Determine size of stare array to cover DREAM pattern */

void dsim_starerange 
( 
int nbolx,        /* Number of DREAM bolometers in X (given) */
int nboly,        /* Number of DREAM bolometers in Y (given) */
int ngrid,        /* number of grid coordinates within jiggle area (given) */
int gridpts[][2], /* relative grid coordinates within jiggle area (given) */
int *starebolx,   /* Number of stare bolometers needed in X (returned) */ 
int *stareboly,   /* Number of stare bolometers needed in X (returned) */
int *nextxpl,     /* Number of extra bolometers at the left (returned) */
int *nextypb,     /* Number of extra bolometers at the bottom (returned) */
int *status       /* global status (given and returned) */
)

/*  Description :
     The DREAM pattern means that an actual array of bolometers of size
     (nbolx,nboly) maps a larger area of sky than if the same array were
     used in a STARE mode. This routine calculates the size of the array
     (starebolx,stareboly) which would duplicate the DREAM map by staring.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
     26May2003 : Original version (bdk)
*/

{

   int j;                   /* loop counter */
   int xmin;                /* minimum X offset */
   int xmax;                /* maximum X offset */
   int ymin;                /* minimum Y offset */
   int ymax;                /* maximum Y offset */


   if ( !StatusOkP(status) ) return;

/* Find the extreme excursions in the grid patter */

   xmin = gridpts[0][0];
   xmax = xmin;
   ymin = gridpts[0][1];
   ymax = ymin;
   for ( j=0; j<ngrid; j++ )
   {
      if ( gridpts[j][0] < xmin ) xmin = gridpts[j][0];
      if ( gridpts[j][0] > xmax ) xmax = gridpts[j][0];
      if ( gridpts[j][1] < ymin ) ymin = gridpts[j][1];
      if ( gridpts[j][1] > ymax ) ymax = gridpts[j][1];
   }

/* Calculate the extension needed to the array */

   *starebolx = nbolx - xmin + xmax;
   *stareboly = nboly - ymin + ymax;
   *nextxpl = -xmin;
   *nextypb = -ymin;
}


/*+ dsim_telpos - get telescope position and orientation */

void dsim_telpos
( 
double ra,           /* Right Ascension in radians (given) */
double dec,          /* Declination in radians (given) */
double lst,          /* local sidereal time in radians (given) */
double *az,          /* Azimuth in radians (returned) */
double *el,          /* Elevation in radians (returned) */
double *p,           /* Parallactic angle in radians (returned) */
int *status          /* global status (given and returned) */
)

/* Description :
    Use slalib algorithms to get from equatorial to horizontal coordinates.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    12May2005 : Original version (bdk)
*/

{
   double cosp;      /* intermediate result */
   double phi;       /* latitude of telescope in radians */
   double ha;        /* hour angle in radians */
   double r;         /* intermediate result */
   double sinp;      /* intermediate result */
   double x;         /* cartesian coordinates */
   double y;         /* cartesian coordinates */
   double z;         /* cartesian coordinates */

   if ( !StatusOkP(status) ) return;

/* JCMT is 19:49:33 N */

   phi = ( 19.0 + (49.0/60.0) + (33.0/3600.0) ) / AST__DR2D;
   ha = lst - ra;

/*
   Equivalent to slaDe2h ( ha, dec, phi, az, el );
*/

/* Az,El as x,y,z */

   x = - cos(ha) * cos(dec) * sin(phi) + sin(dec) * cos(phi);
   y = - sin(ha) * cos(dec);
   z = cos(ha) * cos(dec) * cos(phi) + sin(dec) * sin(phi);

/* To spherical */

   r = sqrt ( x*x + y*y );

   if ( r < 1.0e-20 )
   {
      *az = 0.0;
   }
   else
   {
      *az = atan2 ( y, x );
   }

   if ( *az < 0.0 )
   {
      *az += 2.0 * AST__DPI;
   }

   *el = atan2 ( z, r );

/*
   *p = slaPa ( ha, dec, phi ); 
*/
   sinp = cos ( phi ) * sin ( ha );
   cosp = sin ( phi ) * cos ( dec) - cos ( phi ) * sin ( dec) * cos ( ha );

   if ( sinp != 0.0 || cosp != 0.0 )
   {
      *p = atan2 ( sinp, cosp );
   }
   else
   {
      *p = 0.0;
   }

}
