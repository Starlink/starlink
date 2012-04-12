
/* Macros */
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0
#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define BETWEEN(a,b,c) ( (a) < (b) ? (b) : ((a) > (c) ? (c) : (a) ) )
/* #define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) ) */

#define DISP2FWHM(a)   ( a * 2.0*sqrt(2.0*log(2.0)) )
#define FWHM2DISP(a)   ( a / 2.0*sqrt(2.0*log(2.0)) )

/* Max array sizes */
#define MAXGAUSS      10     /* Max. gaussians in estimates routine */
#define MAXPAR        53     /* Maximum number of parameters to be fitted */

/* LSQ FIT */
#define MAXITERS       200
#define TOLERANCE      0.002
#define LAMBDA         0.01

/* The exp(arg) will be set to 0.0 for arg smaller than this */
#define MINARG        -16

/* FUNCTIONS */
#define LEN__FUNC      20     /* Max strlen for function name */

/* Number of planes in the FIT1D parameter files; use a fixed rather
   than dynamic number so that fits of different types can be combined */
#define NPAR           7

/* Fit control struct */
typedef struct {
  smf_math_function fid;          /* Function identifier */
  char   function[LEN__FUNC];     /* Function name */
  int    fitmask[MAXPAR];         /* Fit mask: 1: fit; 0: fixed */
  double clip[2];                 /* Clip levels for data */
  double rms;                     /* RMS in data */
  double critamp;                 /* Minimal Amplitude */
  double critdisp;                /* Mimimal Dispersion */
  int    estimate_only;           /* Initial estimates only, do not fit */
  int    model_only;              /* Generate model from suppplied params */
} fitStruct;                      /* fit control struct */
