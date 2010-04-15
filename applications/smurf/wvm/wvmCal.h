/*
 *				wvmCal.h
 *	This include file is used for both wvmCal.c and wvmOpt.c
 *
 */


#define DEBUG_OPT          0    /* 1 = turn on printing of final data 0=no print*/
#define DEBUG_OPT_FUNC     0    /* 1 = turn on printing in optimization function 0=no */

double pwv2tau(double airMass, double mmH2O_a);
double tau2pwv(double tau);
void wvmCal(int cycleCnt,float * data,float eta,
	      float tAmb, float * tSky,float * tSys, FILE *rawFP);
void wvmOpt(float airMass,float tAmb,float * tSky,
	      float * wa,float * tOff, float * tWat);
float aFunction(float *p, float airMass, float *tSky);
void wvmEst(float airMass, float WA, float TWAT, float TAUO,
	      float *TBRI, float *TTAU, float *TEFF, float *AEFF);

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

But, VFC3 went bad, so its inputs were wired to VFC4, so we no longer
receive temperatures.  I am going to define the starting index into the
data where each of the IF channels start.  If the VFC3 is ever fixed,
this should make things straight forward to change back, and a little
easier to change back.  So they would be, if VFC3 was working:

#define VFC_1200_MHZ  0
#define VFC_4200_MHZ  4
#define VFC_7800_MHZ  8
#define VFC_TEMP     12

And now they are: */

#define VFC_1200_MHZ  0
#define VFC_4200_MHZ  4
#define VFC_7800_MHZ 12
#define VFC_TEMP      8

#define SKY_OFF1      0
#define SKY_OFF2      1
#define HOT_OFF       2
#define WARM_OFF      3
