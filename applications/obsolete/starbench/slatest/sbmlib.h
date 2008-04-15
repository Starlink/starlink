#ifndef SLALIBHDEF
#define SLALIBHDEF

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32
#pragma warning(disable: 4996) /* suppress MS nagging about strcpy etc. */
#endif
#include <math.h>

/*
**  - - - - - - - - -
**   s l a l i b . h
**  - - - - - - - - -
**
**  Prototype function declarations for slalib library.
**
**  Last revision:   12 May 2007
**
**  Copyright P.T.Wallace.  All rights reserved.
*/

/* Star-independent ICRS-to-CIRS parameters */
typedef struct {
   double pmt;        /* time interval for proper motion (Julian years) */
   double eb[3];      /* SSB to Earth (AU) */
   double ehn[3];     /* Sun to Earth unit vector */
   double gr2e;       /* (grav rad Sun)*2/(Sun-Earth distance) */
   double abv[3];     /* barycentric Earth velocity in units of c */
   double ab1;        /* sqrt(1-v**2) where v=modulus(abv) */
   double bpn[3][3];  /* bias-precession-nutation matrix */
} CIpars;

/* Star-independent intermediate-to-observed parameters */
typedef struct {
   double along;      /* longitude + s' + dERA(DUT) (radians) */
   double phi;        /* geodetic latitude (radians) */
   double hm;         /* height above sea level (metres) */
   double xpl;        /* polar motion xp wrt local meridian (radians) */
   double ypl;        /* polar motion yp wrt local meridian (radians) */
   double sphi;       /* sine of geodetic latitude */
   double cphi;       /* cosine of geodetic latitude */
   double diurab;     /* magnitude of diurnal aberration vector */
   double p;          /* pressure (mb,hPa) */
   double tk;         /* ambient temperature (K) */
   double rh;         /* relative humidity (0-1) */
   double tlr;        /* tropospheric lapse rate (K per metre) */
   double wl;         /* wavelength (micrometers) */
   double refa;       /* refraction constant A (radians) */
   double refb;       /* refraction constant B (radians) */
   double eral;       /* "Local" Earth Rotation Angle (radians) */
} IOpars;

void sbmaddet ( double, double, double, double*, double* );

void sbmafin ( char*, int*, float*, int* );

double sbmairmas ( double zd );

void sbmaltaz ( double, double, double, double*, double*, double*,
                double*, double*, double*, double*, double*, double* );

void sbmamp ( double, double, double, double, double*, double* );

void sbmampqk ( double, double, double[21], double*, double* );

void sbmaop ( double, double, double, double, double, double, double,
              double, double, double, double, double, double, double,
              double*, double*, double*, double*, double* );

void sbmaoppa ( double, double, double, double,
                double, double, double, double, double,
                double, double, double, double[14] );

void sbmaoppat ( double, double[14] );

void sbmaopqk ( double, double, double[14], double*, double*, double*,
                double*, double* );

void sbmatmdsp ( double, double, double, double,
                 double, double, double, double*, double* );

void sbmav2m ( float[3], float[3][3] );

float sbmbear ( float, float, float, float );

void sbmc2i ( double, double, double, double,
              double, double, double, double, double,
              double*, double* );

void sbmc2ipas ( double, double[2][3], double, double, CIpars*, double* );

void sbmc2ipa ( double, double, double, CIpars*, double* );

void sbmc2ipad ( CIpars* );

void sbmc2iqk ( double, double, double, double, double, double, CIpars*,
                double*, double* );

void sbmc2iqkz  ( double, double, CIpars*, double*, double* );

void sbmcaf2r ( int, int, float, float*, int* );

void sbmcaldj ( int, int, int, double*, int* );

void sbmcalyd ( int, int, int, int*, int*, int* );

void sbmcc2s ( float[3], float*, float* );

void sbmcc62s ( float[6], float*, float*, float*, float*,
                float*, float* );

void sbmcd2tf ( int, float, char*, int[4] );

void sbmcldj ( int, int, int, double*, int* );

void sbmclyd ( int, int, int, int*, int*, int* );

void sbmcombn ( int, int, int[], int* );

void sbmcr2af ( int, float, char*, int[4] );

void sbmcr2tf ( int, float, char*, int[4] );

void sbmcs2c ( float, float, float[3] );

void sbmcs2c6 ( float, float, float, float, float, float, float[6] );

void sbmctf2d ( int, int, float, float*, int* );

void sbmctf2r ( int, int, float, float*, int* );

void sbmdaf2r ( int, int, double, double*, int* );

void sbmdafin ( char*, int*, double*, int* );

double sbmdat ( double );

void sbmdav2m ( double[3], double[3][3] );

double sbmdbear ( double, double, double, double );

void sbmdbjin ( char*, int*, double*, int*, int* );

void sbmdc62s ( double[6], double*, double*, double*,
                double*, double*, double* );

void sbmdcc2s ( double[3], double*, double* );

void sbmdcmpf ( double[6], double*, double*, double*,
                double*, double*, double* );

void sbmdcs2c ( double, double, double[3] );

void sbmdd2tf ( int, double, char*, int[4] );

void sbmde2h ( double, double, double, double*, double* );

void sbmdeuler ( char*, double, double, double, double[3][3] );

void sbmdfltin ( char*, int*, double*, int* );

void sbmdh2e ( double, double, double, double*, double* );

void sbmdimxv ( double[3][3], double[3], double[3] );

void sbmdjcal ( int, double, int[4], int* );

void sbmdjcl ( double, int*, int*, int*, double*, int* );

void sbmdm2av ( double[3][3], double[3] );

void sbmdmat ( int, double*, double*, double*, int*, int* );

void sbmdmoon ( double, double[6] );

void sbmdmxm ( double[3][3], double[3][3], double[3][3] );

void sbmdmxv ( double[3][3], double[3], double[3] );

double sbmdpav ( double[3], double[3] );

void sbmdr2af ( int, double, char*, int[4] );

void sbmdr2tf ( int, double, char*, int[4] );

double sbmdrange ( double );

double sbmdranrm ( double );

void sbmds2c6 ( double, double, double, double, double, double,
                double[6] );

void sbmds2tp ( double, double, double, double, double*, double*, int* );

double sbmdsep ( double, double, double, double );

double sbmdsepv ( double[3], double[3] );

double sbmdt ( double );

void sbmdtf2d ( int, int, double, double*, int* );

void sbmdtf2r ( int, int, double, double*, int* );

void sbmdtp2s ( double, double, double, double, double*, double* );

void sbmdtp2v ( double, double, double[3], double[3] );

void sbmdtps2c ( double, double, double, double, double*, double*,
                 double*, double*, int* );

void sbmdtpv2c ( double, double, double[3], double[3], double[3], int* );

double sbmdtt ( double );

void sbmdv2tp ( double[3], double[3], double*, double*, int* );

double sbmdvdv ( double[3], double[3] );

void sbmdvn ( double[3], double[3], double* );

void sbmdvxv ( double[3], double[3], double[3] );

void sbme2h ( float, float, float, float*, float* );

void sbmearth ( int, int, float, float[6] );

void sbmecleq ( double, double, double, double*, double* );

void sbmecmat ( double, double[3][3] );

void sbmecor ( float, float, int, int, float, float*, float* );

void sbmeg50 ( double, double, double*, double* );

void sbmel2ue ( double, int, double, double, double, double, double,
                double, double, double, double[], int* );
double sbmeo ( double );

double sbmeors ( double[3][3], double );

double sbmepb ( double );

double sbmepb2d ( double );

double sbmepco ( char, char, double );

double sbmepj ( double );

double sbmepj2d ( double );

void sbmepv ( double, double[3], double[3], double[3], double[3] );

void sbmeqecl ( double, double, double, double*, double* );

double sbmeqeqx ( double );

void sbmeqgal ( double, double, double*, double* );

double sbmera ( double, double );

void sbmetrms ( double, double[3] );

void sbmeuler ( char*, float, float, float, float[3][3] );

void sbmevp ( double, double, double[3], double[3], double[3], double[3] );

void sbmfitxy ( int, int, double[][2], double[][2], double[6], int* );

void sbmfk425 ( double, double, double, double, double, double,
                double*, double*, double*, double*, double*, double* );

void sbmfk45z ( double, double, double, double*, double* );

void sbmfk524 ( double, double, double, double, double, double,
                double*, double*, double*, double*, double*, double* );

void sbmfk52h ( double, double, double, double,
                double*, double*, double*, double* );

void sbmfk54z ( double, double, double, double*, double*,
                double*, double* );

void sbmfk5hz ( double, double, double, double*, double* );

void sbmflotin ( char*, int*, float*, int* );

void sbmfw2m ( double, double, double, double, double[3][3] );

void sbmfw2xy ( double, double, double, double, double*, double* );

void sbmg2ixys ( double, double, double, double[3][3] );

void sbmgaleq ( double, double, double*, double* );

void sbmgalsup ( double, double, double*, double* );

void sbmge50 ( double, double, double*, double* );

void sbmgeoc ( double, double, double*, double* );

double sbmgmst ( double );

double sbmgmsta ( double, double );

double sbmgst ( double, double, double );

void sbmh2e ( float, float, float, float*, float* );

void sbmh2fk5 ( double, double, double, double,
                double*, double*, double*, double* );

void sbmhfk5z ( double, double, double,
                double*, double*, double*, double* );

void sbmi2c ( double, double, double, double, double, double*, double* );

void sbmi2cqk ( double, double, CIpars*, double*, double* );

void sbmi2opa ( double, double, double, double, double, double, double,
                double, double, double, double, double, IOpars* );

void sbmi2opad ( IOpars* );

void sbmi2opat ( double, IOpars* );

void sbmi2o ( double, double, double, double, double, double, double,
              double, double, double, double, double, double, double,
              double*, double*, double*, double*, double* );

void sbmi2oqk ( double, double, IOpars*, double*, double*, double*,
                double*, double* );

void sbmimxv ( float[3][3], float[3], float[3] );

void sbmint2in ( char*, int*, int*, int* );

void sbmintin ( char*, int*, long*, int* );

void sbminvf ( double[6], double[6], int* );

void sbmkbj ( int, double, char*, int* );

void sbmm2av ( float[3][3], float[3] );

void sbmmap ( double, double, double, double, double, double,
              double, double, double*, double* );

void sbmmappa ( double, double, double[21] );

void sbmmapqk ( double, double, double, double, double, double,
                double[21], double*, double* );

void sbmmapqkz ( double, double, double[21], double*, double* );

void sbmmoon ( int, int, float, float[6] );

void sbmmxm ( float[3][3], float[3][3], float[3][3] );

void sbmmxv ( float[3][3], float[3], float[3] );

void sbmnu ( double, double*, double* );

void sbmnu00a ( double, double*, double* );

void sbmnut ( double, double[3][3] );

void sbmnutc ( double, double*, double*, double* );

void sbmnutc80 ( double, double*, double*, double* );

void sbmo2i ( char*, double, double, double, double, double, double,
              double, double, double, double, double, double,
              double, double, double*, double* );

void sbmo2iqk ( char*, double, double, IOpars*, double*, double* );

void sbmoap ( char*, double, double, double, double, double, double,
              double, double, double, double, double, double, double,
              double, double*, double* );

void sbmoapqk ( char*, double, double, double[14], double*, double* );

void sbmobs ( int, char*, char*, double*, double*, double* );

double sbmpa ( double, double, double );

double sbmpav ( float[3], float[3] );

void sbmpcd ( double, double*, double* );

void sbmpda2h ( double, double, double, double*, int*, double*, int* );

void sbmpdq2h ( double, double, double, double*, int*, double*, int* );

void sbmpermut ( int, int[], int[], int* );

void sbmpertel (int, double, double, double, double, double, double,
                double, double, double, double*, double*, double*,
                double*, double*, double*, double*, int* );

void sbmpertue ( double, double[], int* );

void sbmpfw ( double, double*, double*, double*, double* );

void sbmplanel ( double, int, double, double, double, double, double,
                 double, double, double, double[6], int* );

void sbmplanet ( double, int, double[6], int* );

void sbmplante ( double, double, double, int, double, double, double,
                 double, double, double, double, double,
                 double*, double*, double*, int* );

void sbmplantu ( double, double, double, double[],
                 double*, double*, double*, int* );

void sbmpm ( double, double, double, double, double, double, double,
             double, double*, double* );

void sbmpncio ( double, double[3][3] );

void sbmpneqx ( double, double[3][3] );

void sbmpolmo ( double, double, double, double,
                double*, double*, double* );

void sbmpomom ( double, double, double, double[3][3] );

void sbmprebn ( double, double, double[3][3] );

void sbmprec ( double, double, double[3][3] );

void sbmprecl ( double, double, double[3][3] );

void sbmpreces ( char[3], double, double, double*, double* );

void sbmprenut ( double, double, double[3][3] );

void sbmpv2el ( double[], double, double, int,
                int*, double*, double*, double*, double*,
                double*, double*, double*, double*, int* );

void sbmpv2ue ( double[], double, double, double[], int* );

void sbmpvobs ( double, double, double, double[6] );

void sbmpxy ( int, double[][2], double[][2], double[6],
              double[][2], double*, double*, double* );

float sbmrange ( float );

float sbmranorm ( float );

double sbmrcc ( double, double, double, double, double );

void sbmrdplan ( double, int, double, double, double*, double*, double* );

void sbmrefco ( double, double, double, double, double, double, double,
                double, double*, double* );

void sbmrefcoq ( double, double, double, double, double*, double* );

void sbmrefro ( double, double, double, double, double, double, double,
                double, double, double* );

void sbmrefv ( double[3], double, double, double[3] );

void sbmrefz ( double, double, double, double* );

float sbmrverot ( float, float, float, float );

float sbmrvgalc ( float, float );

float sbmrvlg ( float, float );

float sbmrvlsrd ( float, float );

float sbmrvlsrk ( float, float );

double sbms ( double, double, double );

void sbms2tp ( float, float, float, float, float*, float*, int* );

float sbmsep ( float, float, float, float );

float sbmsepv ( float[3], float[3] );

void sbmsmat ( int, float*, float*, float*, int*, int* );

double sbmsp ( double );

void sbmsubet ( double, double, double, double*, double* );

void sbmsupgal ( double, double, double*, double* );

void sbmsvd ( int, int, int, int, double*, double*, double*, double*, int* );

void sbmsvdcov ( int, int, int, double*, double*, double*, double* );

void sbmsvdsol ( int, int, int, int, double*, double*, double*, double*,
                 double*, double* );

void sbmtp2s ( float, float, float, float, float*, float* );

void sbmtp2v ( float, float, float[3], float[3] );

void sbmtps2c ( float, float, float, float, float*, float*,
                float*, float*, int* );

void sbmtpv2c ( float, float, float[3], float[3], float[3], int* );

void sbmue2el ( double[], int, int*, double*, double*, double*, double*,
                double*, double*, double*, double*, int* );

void sbmue2pv ( double, double[], double[], int* );

void sbmunpcd ( double, double*, double* );

void sbmv2tp ( float[3], float[3], float*, float*, int* );

float sbmvdv ( float[3], float[3] );

void sbmvn ( float[3], float[3], float* );

void sbmvxv ( float[3], float[3], float[3] );

void sbmxy2xy ( double, double, double[6], double*, double* );

double sbmzd ( double, double, double );

#ifdef __cplusplus
}
#endif

#endif
