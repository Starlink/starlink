
/* Macros */
#define YES            1          /* C versions of .TRUE. and .FALSE.     */
#define NO             0
#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define BETWEEN(a,b,c) ( (a) < (b) ? (b) : ((a) > (c) ? (c) : (a) ) )
/* #define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )     */

#define DISP2FWHM(a)   ( a * (2.0*sqrt(2.0*log(2.0))) )
#define FWHM2DISP(a)   ( a / (2.0*sqrt(2.0*log(2.0))) )

/* LSQ FIT */
#define MAXITERS       200
#define TOLERANCE      0.002
#define LAMBDA         0.01

/* The exp(arg) will be set to 0.0 for arg smaller than this              */
#define MINARG        -16


/* Number of planes in the FIT1D parameter files; use a fixed rather
   than dynamic number so that fits of different types can be combined
   Since one of the planes is used for the function id, that leaves NPAR-1
   for the max number of function parameters    */
#define NPAR           7

/* Max array sizes */
#define MAXCOMPS       7     /* Max. nr of components in single profile   */
#define MAXPAR        53     /* Maximum number of parameters to be fitted */

/* Switch off fitting a zerolevel globally since the option has not been
   tested extensively and MFITTREND is better suited. Smurf_fit1d is not
   set up to include fitting a zerolevel. Smf_fit_profile should be 
   reasonably ok (but virtually untested. The main complication is how 
   to deal wit zerolevels with wide profiles such as Voigts               */
#define FITZEROLEVEL   0

/* Fit control struct */
typedef struct {
  smf_math_function fid;          /* Function identifier                  */
  int    axis;                    /* Axis to fit 1,2...; 0=highest        */
  int    ncomp;                   /* Number of components to fit          */
  double rms;                     /* RMS in data                          */
  double range[2];                /* Coordinate range to fit              */
  double fixval[MAXPAR];          /* User defined fixed values            */
  int    fixmask[MAXPAR];         /* Fit mask: 1: fixed; 0: fit           */
  double clip[2];                 /* Clip levels for data                 */
  double lolimit[NPAR];           /* Lower limits for parameters          */
  double hilimit[NPAR];           /* Upper limits for parameters          */
  int    estimate_only;           /* Initial estimates only, do not fit   */
  int    model_only;              /* Generate model from suppplied params */
} fitStruct;                      /* fit control struct                   */

