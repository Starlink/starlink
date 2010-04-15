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
    02Aug2006 : protect against multiple includes (agg)
    02Sep2006 : add enum types for the conv_shape parameter (agg)
*/

#ifndef DREAM_PAR_DEFINED
#define DREAM_PAR_DEFINED

#define DREAM__FLEN 132    /* Length of file names */
#define DREAM__MXBOL 1600  /* Maximum no. of bolometers */
#define DREAM__MXOUT 1764  /* Maximum number of output points (42x42) */
#define DREAM__MXIRF 8     /* Length of impulse response function in
                              samples */
#define DREAM__MXGRID 81   /* The maximum number of sky grid points
                              enclosed by the DREAM pattern */
#define DREAM__MXVERT 32   /* Maximum number of vertices in a pattern */
#define DREAM__MXSIM 1024  /* Maximum number of values simulated */
#define DREAM__MXSAM 512   /* Maximum number of samples in a path */
#define DREAM__OVER 50     /* Computational oversampling factor */

/* Flags specifying the convolution pattern to use for the DREAM
   reconstruction grid */
typedef enum {
  CONV__GAUS = 0,     /* Gaussian */
  CONV__SINC = 1,     /* sinc(x) * sinc(y) */
  CONV__SINCTAP = 2,  /* sinc(x) * sinc(y) tapered */
  CONV__SINCDEL = 3   /* sinc(x) * sinc(y) delay tapered */
} conv_shape;

#endif /* DREAM_PAR_DEFINED */
