/*
                                                                  
  File name: 
    wvmCal.c

  Description:  

  Functions:

 History: 
   $Log: wvmCal.c,v $
   Revision 1.9  2011/12/02 21:52:36  timj
   The variables in wvmCal.h should be marked extern in routines other than wvmCal.c

   On some operating systems you can not declare a global variable in two
   files. One of them must be marked extern.

   Revision 1.8  2010/11/23 01:53:19  cwalther
   These now have the ability for different ordering in the temperature and the RF data

   Revision 1.7  2010/01/26 01:51:38  cwalther
   Changes to make the WVM code have multiple personalities

   Revision 1.6  2010/01/25 19:58:40  cwalther
   These changes were made to exclusively handle the new head we got from SMA in 2009

   Revision 1.5  2008/04/10 21:04:56  cwalther
   Make sure the averaging is done in floating point

   Revision 1.4  2008/04/10 20:06:17  cwalther
   Fixed sizes of the airmass and temperature prints

   Revision 1.3  2003/06/02 21:21:49  mrippa
   Writes glitch occurances to raw file now (passed in).

   Revision 1.2  2003/04/09 20:21:41  mrippa
   #include wvmCal.h

   Revision 1.1.1.1  2002/07/26 01:41:33  cwalther
   Original Version


   19-July-2002 - Craig Walther - Created (translated from FORTRAN)
*-
*/

/* C Includes */

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

/* WVM includes */
#define WVMCAL_INTERNAL 1
#include "wvmCal.h"

/********************************************************************/
/*+		       w v m C a l

 *  Function name:
      wvmCal

 *  Function:
      Calculate effective sky temperatures and system temperatures for each
      of the three receivers

 *  Description:
      

 *  Language:
      C

 *  Call:
      wvmCal(int cycleCnt,float * data,float eta,float tAmb,
	     float * tSky,float * tSys);

      

 *  Parameters:
      ("<" input, "!" modified, "W" workspace, ">" output)
      (<) cycleCnt   The integer cycle counter
      (<) data       The floating point data array (16 points)
      (<) eta        The coupling efficiency to sky
                     If == 0.0 and internal value will get used
      (<) tAmb       The ambient temperature at the telescope
      (>) tSky       The sky temperature measured by each 
                     receiver (3 data points)
      (>) tSys       The system temperature of each receiver (3 data points)

 *  Support: Craig Walther, {JAC}


 *- 

 */

void wvmCal(int cycleCnt,float * data,float eta,float tAmb,
	    float * tSky, float * tSys, FILE *rawFP)
{
  /* The arrays fSky, fSum and fDif will hold the average of the two 
     readings of sky frequency, the sum of the frequency readings 
     looking at the load and the difference of the frequency readings
     while looking at the loads */
  float fSky[3], fSum[3], fDif[3];

  /* These arrays hold the brightness temperature of 
     each load at each frequency */
  float brightTHot[3], brightTWarm[3];

  float tHot;                                          
  float tWarm;

  /* The arrays avgSky, avgSum and avgDif will hold a running average over
     the past few cycles */
  static float avgSky[] = {0.0, 0.0, 0.0};
  static float avgSum[] = {0.0, 0.0, 0.0};
  static float avgDif[] = {0.0, 0.0, 0.0};
  static float avgTHot[] = {0.0, 0.0, 0.0};
  static float avgTWarm[] = {0.0, 0.0, 0.0};

  /* etaInternal is the Nominal value of coupling efficiency */
  float etaInternal = 0.958;

  /* General purpose parameters */
  float test, scaleFac;
  int i;

  /* avgFacSky, avgFacSum and avgFacDif are Smoothing factors - the 
     fraction carried to the next value If avgFac = 0, no averaging */
  float avgFacSky = 0.0;
  float avgFacSum = 0.6;
  float avgFacDif = 0.9;
  float avgFacTHot = 0.9;
  float avgFacTWarm = 0.9;

  /* Correction for Rayleigh-Jeans  0.5 * h * nu / k  for nu = 183 GHz */
  float tCorr = 4.4;

  /* errCnt is an error counter */
  static int errCnt[] = {0, 0, 0};
  
  /* The vfcIndex array holds the offsets to:
     1.2 GHz channel in position 0
     4.2 GHz channel in position 1
     7.8 GHz channel in position 2
     The temperature data in position 3 */

  int vfcIndex[4];

  /* The index into the raw data might be different for the different heads */

  vfcIndex[0] = vfc_1200_mhz;
  vfcIndex[1] = vfc_4200_mhz;
  vfcIndex[2] = vfc_7800_mhz;
  vfcIndex[3] = vfc_temp;

  

  /* We will either use the counts of the VtoF to calculate a load temperature
     or we will have fixed load temperatures */

  if(fixedLoadTemperatures)
    {
      tHot = fixedHotLoadTemp + 273.15;
      tWarm = fixedWarmLoadTemp + 273.15;
    }
  else
    {
      /* Use the counts from the hot and warm loads to adjust the brightness 
	 temperatures of the loads */

      tHot = hotBias + hotSlope * data[vfcIndex[3] + hotOffT] + 273.15;
      tWarm = warmBias + warmSlope * data[vfcIndex[3] + warmOffT] + 273.15;
    }
 
  /* Convert from actual load temperature in kelvin to brightness 
     temperature, also in kelvin, and then average */

  for(i=0; i<3; i++)
    {
      /* Now adjust for the reflectivity of the loads */

      brightTHot[i] = tHot + brightAdjTHot[i];
      brightTWarm[i] = tWarm + brightAdjTWarm[i];

      /* Average (filter) the results */

      if(cycleCnt < 4)
	{
	  avgTHot[i] = brightTHot[i];
	  avgTWarm[i] = brightTWarm[i];
	}
      else
	{

	  /* Average the results */

	  avgTHot[i] = avgTHot[i] * avgFacTHot + brightTHot[i] * (1.0 - avgFacTHot);
	  avgTWarm[i] = avgTWarm[i] * avgFacTWarm + brightTWarm[i] * (1.0 - avgFacTWarm);
	}
    }

  /* Convert the data into temperatures  (just assume Rayleigh-Jeans 
     for now) */

  for(i=0; i<3; i++)
    {

      /* Average of two sky readings */
      fSky[i] = (data[vfcIndex[i] + skyOff1Rf] + 
		 data[vfcIndex[i] + skyOff2Rf]) / 2.0; 

      /* Sum and difference of the cal readings */
      fSum[i] = data[vfcIndex[i] + hotOffRf] + data[vfcIndex[i] + warmOffRf];
      fDif[i] = data[vfcIndex[i] + hotOffRf] - data[vfcIndex[i] + warmOffRf];



      /* Do not average, if less than four cycles into run */
      if(cycleCnt < 4)
	{
	  avgSky[i] = fSky[i];
	  avgSum[i] = fSum[i];
	  avgDif[i] = fDif[i];
	}
      /* Begin smoothing */
      else
	{
	  /* Smooth these:  this is roughly equivalent to an RC 
	     filter.  If avsk = 0.9 it gives an effective time constant 
	     of about 10 sample times. */

	  avgSky[i] = avgSky[i] * avgFacSky + fSky[i] * (1.0 - avgFacSky);

	  /* Do simple error spike checking here */
	  test = fSum[i] - avgSum[i];
	  if(test < 0.0) test *= -1.0;
	  if(test < 1.0)
	    {
	      avgSum[i] = avgSum[i] * avgFacSum + fSum[i] * (1.0 - avgFacSum);
	      avgDif[i] = avgDif[i] * avgFacDif + fDif[i] * (1.0 - avgFacDif);
	      errCnt[i] = 0;
	    }
	  else
	    {
	      if(errCnt[i] < 3)
		{
		  errCnt[i] = errCnt[i] + 1;
		  if (rawFP != NULL)
		      fprintf(rawFP, "Glitch channel: %d\n",vfcIndex[i]);
		}
	      else
		{
		    if (rawFP != NULL)
			fprintf(rawFP, "Too many errors - restarting averages\n");
		    errCnt[i] = 0;
		    avgSky[i] = fSky[i];
		    avgSum[i] = fSum[i];
		    avgDif[i] = fDif[i];
		}
	    }
	}

      /* Calculate the scale factor (degrees per kHz) for tSys and tSky */


      scaleFac = (avgTHot[i] - avgTWarm[i]) / avgDif[i];

      /*printf("params: %8.1f %8.1f %8.1f\n", avgTHot[i], avgTWarm[i], avgDif[i]);*/
      
      tSys[i] = (avgSum[i] * scaleFac -avgTHot[i] - avgTWarm[i]) / 2.0;
      tSky[i] = avgSky[i] * scaleFac - tSys[i];
    }

  /* Convert from measured temperatures to actual sky brightness */
  for(i=0; i<3; i++)
    {
      tSky[i] = (tSky[i] - (1.0 - etaInternal)*(tAmb - tCorr)) /
	etaInternal;
    }
}




