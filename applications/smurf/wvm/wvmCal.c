/*

  File name:
    wvmCal.c

  Description:

  Functions:

 History:
   $Log: wvmCal.c,v $
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

  /* The arrays avgSky, avgSum and avgDif will hold a running average over
     the past few cycles */
  static float avgSky[] = {0.0, 0.0, 0.0};
  static float avgSum[] = {0.0, 0.0, 0.0};
  static float avgDif[] = {0.0, 0.0, 0.0};


  /* tHot and tWarm are the effective temperatures of the calibration
     load.  Note that these must truely be the effective R-J brightness
     tempertures and NOT the actual temperatures the effect of this
     is to subtract 4.4 K from all actual temperatures. Thus a hot
     load at 100 C is at 368.75 effective */
  static float tHot[] = {364.7, 363.8, 364.6};
  static float tWarm[] = {303.2 , 303.2, 303.2};

  /* etaInternal is the Nominal value of coupling efficiency */
  float etaInternal = 0.958;

  /* General purpose parameters */
  float test, scaleFac;
  int i;
  char inputStr[100];
  char channel_names[3][10] = {"1.2 GHz", "4.2 GHz", "7.8 GHz"};

  /* avgFacSky, avgFacSum and avgFacDif are Smoothing factors - the
     fraction carried to the next value If avgFac = 0, no averaging */
  float avgFacSky = 0.0;
  float avgFacSum = 0.6;
  float avgFacDif = 0.9;

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
  vfcIndex[0] = VFC_1200_MHZ;
  vfcIndex[1] = VFC_4200_MHZ;
  vfcIndex[2] = VFC_7800_MHZ;
  vfcIndex[3] = VFC_TEMP;

/* If this is the first time in this function and eta is greater than zero
   then prompt for tHot and tWarm */

  if((cycleCnt == 1) && (eta > 0.0))
    {
      etaInternal = eta;

      for(i=0; i<3; i++)
	{
	  printf("\nEnter hot load effective temperature for the %s channel: (%7.3f) ",
		 channel_names[i], tHot[i]);
	  fgets(inputStr,sizeof(inputStr),stdin);
	  if (strlen(inputStr) > 1)
	    tHot[i] = atof(inputStr);
	}
      for(i=0; i<3; i++)
	{
	  printf("\nEnter warm load effective temperature for the %s channel: (%7.3f) ",
		 channel_names[i], tWarm[i]);
	  fgets(inputStr,sizeof(inputStr),stdin);
	  if (strlen(inputStr) > 1)
	    tWarm[i] = atof(inputStr);
	}

      printf("\n");
      for(i=0; i<3; i++)
	  printf("%s channel hot temp: %f  warm temp: %f\n",
		 channel_names[i], tHot[i], tWarm[i]);

    }

  /* Convert the data into temperatures  (just assume Rayleigh-Jeans
     for now) */

  for(i=0; i<3; i++)
    {

      /* Average of two sky readings */
      fSky[i] = (data[vfcIndex[i] + SKY_OFF1] +
		 data[vfcIndex[i] + SKY_OFF2]) / 2.0;

      /* Sum and difference of the cal readings */
      fSum[i] = data[vfcIndex[i] + HOT_OFF] + data[vfcIndex[i] + WARM_OFF];
      fDif[i] = data[vfcIndex[i] + HOT_OFF] - data[vfcIndex[i] + WARM_OFF];

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

      scaleFac = (tHot[i] - tWarm[i]) / avgDif[i];
      tSys[i] = (avgSum[i] * scaleFac -tHot[i] - tWarm[i]) / 2.0;
      tSky[i] = avgSky[i] * scaleFac - tSys[i];
    }

  /* Convert from measured temperatures to actual sky brightness */
  for(i=0; i<3; i++)
    {
      tSky[i] = (tSky[i] - (1.0 - etaInternal)*(tAmb - tCorr)) /
	etaInternal;
    }
}




