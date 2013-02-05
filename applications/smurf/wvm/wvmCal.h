#ifndef _WVMCAL_INCLUDE_
#define _WVMCAL_INCLUDE_ 1

/*
 *				wvmCal.h
 *	This include file is used for both wvmCal.c and wvmOpt.c
 * 
 */


#define DEBUG_OPT          0    /* 1 = turn on printing of final data 0=no print*/
#define DEBUG_OPT_FUNC     0    /* 1 = turn on printing in optimization function 0=no */

double pwv2tau(double mmH2O_z);
double pwv2tau_bydate(double mjdate, double mmH2O_z);
double tau2pwv(double tau);
void wvmCal(int cycleCnt,float * data,float eta,
	      float tAmb, float * tSky,float * tSys, FILE *rawFP);
void wvmOpt(float airMass,float tAmb, const float tSky[],
             float * wa,float * tOff, float * tWat, float * rms);
void wvmOptMulti(size_t n, const float aMass[], const float tAmb[], const float tSky[],
             float * waterDens, float * tau0, float * tWater,
             float * waterDensErr, float * tau0Err, float *tWaterErr, float *rms );
void wvmEst(double airMass, double WA, double TWAT, double TAUO,
              double *TBRI, double *TTAU, double *TEFF, double *AEFF);
void wvmReadConstants(int *);


/* These variables must be declared extern in code other than
   wcmCal.c. These constants should probably not be in the same
   include file as the general WVM prototypes defined above. */

#if WVMCAL_INTERNAL
# define EXTERN
#else
# define EXTERN extern
#endif

/* The offsets into VFC RF Channels themselves */

EXTERN int skyOff1Rf;
EXTERN int skyOff2Rf;
EXTERN int hotOffRf;
EXTERN int warmOffRf;

/* The ofsets into the temperature VFC channel only */ 

EXTERN int skyOff1T;
EXTERN int skyOff2T;
EXTERN int hotOffT;
EXTERN int warmOffT;

/* Given the following wiring:

    VFC1 => 1.2 GHz IF
    VFC2 => 4.2 GHz IF
    VFC3 => 7.8 GHZ IF
    VFC4 => Temperature monitor

The data were supposed to be in this format comming from the PC

cycle_count => integer cycle counter reset to 0 on "SYNC"
state_count => integer internal state counter
VFC1_sky_RF => 1.2 GHz IF channel while mirror is looking at the sky and
               the temperature channel is reading the RF plate temperature
VFC1_sky_IF => 1.2 GHz IF channel while mirror is looking at the sky and
               the temperature channel is reading the IF plate temperature
VFC1_hot    => 1.2 GHz IF channel while mirror is looking at the hot load
VFC1_warm   => 1.2 GHz IF channel while mirror is looking at the warm load
VFC2_sky_RF => 4.2 GHz IF channel while mirror is looking at the sky and
               the temperature channel is reading the RF plate temperature
VFC2_sky_IF => 4.2 GHz IF channel while mirror is looking at the sky and
               the temperature channel is reading the IF plate temperature
VFC2_hot    => 4.2 GHz IF channel while mirror is looking at the hot load
VFC2_warm   => 4.2 GHz IF channel while mirror is looking at the warm load
VFC3_sky_RF => 7.8 GHz IF channel while mirror is looking at the sky and
               the temperature channel is reading the RF plate temperature
VFC3_sky_IF => 7.8 GHz IF channel while mirror is looking at the sky and
               the temperature channel is reading the IF plate temperature
VFC3_hot    => 7.8 GHz IF channel while mirror is looking at the hot load
VFC3_warm   => 7.8 GHz IF channel while mirror is looking at the warm load
VFC1_sky_RF => The temperature of the RF plate
VFC1_sky_IF => The temperature of the IF plate
VFC1_hot    => The temperature of the hot load
VFC1_warm   => The temperature of the warm load

But, VFC3 went flaky, so its inputs were wired to VFC4. Now the temperatures
that were flaky anyway themselves are on the flaky VFC3.

I am going to define the starting index into the
data where each of the IF channels start.  If the VFC3 is ever fixed,
this should make things straight forward to change back, and a little
easier to change back.  So they would be, if VFC3 was working:

#define VFC_1200_MHZ  0
#define VFC_4200_MHZ  4
#define VFC_7800_MHZ  8
#define VFC_TEMP     12

But now For the original WVM head they should be:

#define VFC_1200_MHZ  0
#define VFC_4200_MHZ  4
#define VFC_7800_MHZ 12
#define VFC_TEMP      8

But the SMA head is wired like the original was delivered to us:

#define VFC_1200_MHZ  0
#define VFC_4200_MHZ  4
#define VFC_7800_MHZ  8
#define VFC_TEMP     12

*/

EXTERN int vfc_1200_mhz;
EXTERN int vfc_4200_mhz;
EXTERN int vfc_7800_mhz;
EXTERN int vfc_temp;

/* The following needs to contain on of these two strings: ORIGINAL or SMA */
EXTERN char headName[10];

/* These are the cals from count to degrees C for the hot and warm loads 
   unused if the load temperatures are fixed */

EXTERN float hotBias;
EXTERN float warmBias;
EXTERN float hotSlope;
EXTERN float warmSlope;


  /* brightAdjTHot and brightAdjTWarm are the adjustments to get the effective 
     temperatures (brightness temperature) of the calibration loads from their
     measured temperatures. This effect is about -4.4 degrees.  Thus a hot 
     load at 100 C is about 368.75 K effective 

     These will likely vary between the two heads */

EXTERN float brightAdjTHot[3];
EXTERN float brightAdjTWarm[3];

EXTERN int fixedLoadTemperatures;   /* True if we are using fixed load temperatures */
EXTERN float fixedHotLoadTemp;      /* If using fixed load temperatures, the hot load temperature in C */
EXTERN float fixedWarmLoadTemp;     /* If using fixed load temperatures, the warm load temperature in C */



#endif
