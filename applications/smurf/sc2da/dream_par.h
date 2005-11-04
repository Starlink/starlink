/* dream_par.h - DREAM constants */

/* 
   Authors :
    B.D.Kelly (bdk@roe.ac.uk)
   History:
    16Aug2002 : original (bdk)
    13Jun2003 : change MAXJIGGLES to MAXSAMPSET (bdk)
    16Jun2003 : show interdependence of NWEIGHTS, MAXNOPS and MAXIRF (bdk)
    20Jun2003 : revise all names and definitions (bdk)
    26Jun2003 : add DREAM__FLEN (bdk)
*/

#define DREAM__FLEN 132    /* length of file names */
#define DREAM__MXBOL 1600  /* maximum no. of bolometers */


#define DREAM__MXOUT 1764  /* maximum number of output points.(42x42) */


#define DREAM__MXIRF 8     /* length of impulse response function in
                              samples */

#define DREAM__MXGRID 81   /* The maximum number of sky grid points
                              enclosed by the DREAM pattern */
#define DREAM__MXVERT 32   /* maximum number of vertices in a pattern */
#define DREAM__MXSIM 1024  /* maximum number of values simulated */
#define DREAM__MXSAM 512   /* maximum number of samples in a path */
#define DREAM__OVER 50     /* computational oversampling factor */
