/*  sc2sim_par.h - constants for DREAM simulator */

/*  History :
     16Aug2002: original (bdk)
     16Jun2003: move NWEIGHTS to dream_par.h (bdk)
     04Feb2005: remove TELEMISSION, ASTSCALE, ASTSIZE, ATMSCALE, ATMSIZE (bdk)
     22SEP2006: add DREAM parameters, change to SC2SIM paramters (jb)
     15Aug2007: add SC2SIM_MXMS - maximum number of microsteps (cv)
*/

#define SC2SIM__NCOEFFS 6         /* no. of coeffs for bolometer response */
#define SC2SIM__FLEN 132    /* length of file names */
#define SC2SIM__MXBOL 1600  /* maximum no. of bolometers */
#define SC2SIM__SUBLEN 4    /* Number of characters in subarray name including nul */
#define SC2SIM__MAXSUBS 8   /* Maximum number of subarrays */

#define SC2SIM__MXOUT 1764  /* maximum number of output points.(42x42) */


#define SC2SIM__MXIRF 8     /* length of impulse response function in
                              samples */

#define SC2SIM__MXGRID 81   /* The maximum number of sky grid points
                              enclosed by the DREAM pattern */
#define SC2SIM__MXVERT 32   /* maximum number of vertices in a pattern */
#define SC2SIM__MXSIM 1024  /* maximum number of values simulated */
#define SC2SIM__MXSAM 512   /* maximum number of samples in a path */
#define SC2SIM__OVER 50     /* computational oversampling factor */

#define SC2SIM__MXMSTP 10     /* maximum number of microsteps */
