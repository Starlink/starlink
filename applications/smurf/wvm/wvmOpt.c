/*
                                                                  
  File name: 
    wvmOpt.c

  Description:  

  Functions:

  aFunction
  wvmEst  

 History: 
   $Log: wvmOpt.c,v $
   Revision 1.3  2006/07/14 18:50:32  rkackley
   Corrected format specifier and added val to list of quantities printed because it looked like val was intended to be printed bu was not in the argument list

   Revision 1.2  2003/04/09 20:22:10  mrippa
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
#include <math.h>

/* WVM includes */
#include "wvmCal.h"


/********************************************************************/
/*+		       w v m O p t

 *  Function name:
      wvmOpt

 *  Function:
      Calculate the best fitting water vapour content, broadband opacity
      and effective temperature from the effective sky temperatures.

 *  Description:
   This routine performs a simple optimisation, 1 parameter at a time, for 
   the best fitting water, broadband opacity and effective temperature.   
   The latter are only fit if the data seem to require it.
   This version using function call aFunction for compatability with Num Rec.
      
 *  Language:
      C

 *  Call:
      wvmOpt(float aMass, float tAmb, float * tSky, float * waterDens, 
             float * tau0, float * tWat);

 *  Parameters:
      ("<" input, "!" modified, "W" workspace, ">" output)
      (<) aMass      The air Mass 
      (<) tAmb       The ambient temperature
      (<) tSky       The sky termperature of each receiver (3 data points)
      (>) waterDens  The line-of sight water density in mm
      (>) tau0       The line of site opacity
      (>) tWat       The effective temperature

 *  Support: Craig Walther, {JAC}


 *- 

 */
void wvmOpt(float aMass, float tAmb, float * tSky, float * waterDens, 
            float * tau0, float * tWater)
{

  int forever;                     /* Keeps do loop going forever */
  int i, j, K;                     /* Counters */
  float tp[] = {19.9, 275., 0.2};  /* upper limits */
  float bp[] = { 0.0, 250., -.1};  /* lower limits */
  float p[3], q[3];                /* current parameters and increments */
  float val, VALP;                 /* value at start of round */
  float Y[3], X[3];                /* stacks */
  float SL, CU, DEL;               /* Fitted parabola */
  float test;                      /* used for ABS of floating point number */

  forever = 1;

  /* Make simple initial estimates */
  *tWater = tAmb - 10.0;
  *tau0 = 0.0;

  /* If we do not have reasonable values, return with the defaults only */
  if((tSky[2] > bp[1]) || (tSky[1] < tSky[2]))
    {
      *waterDens = 99.0;
      return;
    }

  /* make a first guess at the water density (assumes channel 3 is 
     optically thin) */

  *waterDens = (tSky[2] - 3.0)/29.0;

  /* Set parameters */
 
  p[0] = *waterDens;
  p[1] = *tWater;
  p[2] = *tau0;

  /* Initial increments - water density only the first time */

  q[0] = 0.1 * *waterDens;
  q[1] = 0.0;
  q[2] = 0.0;

  /* Get intial value and initialise stack, etc. */

  val = aFunction(p, aMass, tSky);
  Y[0] = val;
  VALP = val;

  /* The following was line 5 in the FORTRAN */
  do        
    {

      if(DEBUG_OPT_FUNC)
	    printf("In wvmOpt p= %8.4f %8.2f %8.4f q= %9.5f %9.5f %9.5f val = %9.4f\n",
		   p[0], p[1], p[2], q[0], q[1], q[2], val);

      X[1] = 0.0;
      Y[1] = 0.0;

      /* No point in messing around for ever - just go through maximum 
	 of 20 times */
      for(j=0; j<20; j++)
	{
	  for(K = 0; K < 3; K++)
	    {
	      if(q[K] != 0.0)
		{
		  X[0] = p[K];

		  /* only 10 steps max at this point */
		  for(i=0; i<10; i++)
		    {
		      p[K] = p[K] + q[K];

		      /* apply limits - not very efficient but it 
			 should work */

		      if(p[K] > tp[K]) p[K] = tp[K];
		      if(p[K] < bp[K]) p[K] = bp[K];
		      val = aFunction(p, aMass, tSky);

		      if(val > Y[0])
			{

			  /* doing OK  -  push onto stack and try again */
			  X[2] = X[1];
			  Y[2] = Y[1];
			  X[1] = X[0];
			  Y[1] = Y[0];
			  X[0] = p[K];
			  Y[0] = val;
			}
		      else if(val == Y[0])
			{

			  /* not likely but could happen  - take 
			     midpoint and exit */

			  p[K] = p[K] - q[K]/2.0;
			  break; /* GO TO 10 in FORTRAN */
			}
		      else if(i == 0)
			{

			  /* going wrong direction put these values one 
			     down in the stack and leave initial ones 
			     on top */
			  X[1] = p[K];
			  Y[1] = val;

			  /* reverse increment */
			  q[K] = -q[K];

			  /* go back to where we were and try again */
			  p[K] = p[K] + q[K];
			}
		      else
			{

			  /* we have gone over the peak put on stack again */
			  X[2] = X[1];
			  Y[2] = Y[1];
			  X[1] = X[0];
			  Y[1] = Y[0];
			  X[0] = p[K];
			  Y[0] = val;

			  /* calculate slope and curvature */
			  SL = (Y[2] - Y[0])/2.0;
			  CU = 2*Y[1] - Y[0] - Y[2];

			  /* fractional increment required */
			  DEL  = -SL/CU;
			  p[K] = X[1] + DEL*q[K];
			  break; /*  GO TO 10 in FORTRAN */
			}
		    }

		  /* This parameter OK.  Get final value (also serves as 
		     start of next iteration */
		  val = aFunction(p, aMass, tSky); /* This was line 10 in FORTRAN */
		  Y[0] = val;

		  /* reduce increment but not too much */
		  q[K] = q[K]/1.2;
		}
	    }

	  /* Are we done?   val greater than 10 means rms error is 
	     less than a tenth of a degree */

	  /* This line was commented out in FORTRAN
	     IF (val .GT. 10.) GO TO 99  */

	  /* val not changed by more than 1% on this iteration */
	  test = val - VALP;
	  if(test < 0.0) test *=-1.0;
	  if(test/val < 0.01) break; /* GO TO 19 */
	  VALP = val;
	}

      if(*waterDens > 1.4) /* This was FORTRAN line 19 */
	{
          if(q[1] == 0.0)
	    {

	      /* turn on fitting for temperature */
	      q[0] = 0.1 * *waterDens;
	      q[1] = 1.0;
	      q[2] = 0.0;
	      continue; /* GO TO 5 in FORTRAN */
	    }
	  else if(q[2] == 0.0)
	    {

	      /* All three on */
	      q[0] = 0.1 * *waterDens;
	      q[1] = 1.0;
	      q[2] = 0.003;
	      continue; /* GO TO 5 in FORTRAN */
	    }
	}
      else if(q[2] == 0.0)
	{
	  
	  /* Too dry to get good estimate of temperature but 
	     try offset */
	  q[0] = 0.1 * *waterDens;
	  q[1] = 0.0;
	  q[2] = 0.003;
	  continue;   /* GO TO 5 in FORTRAN */
	}
  
	  *waterDens = p[0];   /* This was line 99 in FORTRAN */ 
	  *tWater = p[1];
	  *tau0 = p[2];
	  return;
    }while(forever == 1);

}

float aFunction(float *p, float aMass, float *tSky)
{

  int j;
  float TBRI[3], TTAU[3], TEFF[3], AEFF[3];
  float SSQ, ERR, val;
  double temporary;

  wvmEst(aMass, p[0], p[1], p[2], TBRI, TTAU, TEFF, AEFF);

  /* Floor on error */
        
  SSQ = 0.01;
  for(j=0; j<3; j++)
    {
      ERR = tSky[j] - TBRI[j];
      SSQ = SSQ + ERR * ERR;
    }
  
  temporary = SSQ/3.0;
  val = 1./sqrt(temporary);

  if(DEBUG_OPT_FUNC)
    {
      printf("In aFunction p= %6.3f %6.3f %6.3f tSky = %7.3f %7.3f %7.3f",
	     p[0], p[1], p[2], tSky[0], tSky[1], tSky[2]);
      printf(" TBRI = %7.3f %7.3f %7.3f val = %9.3f\n",
	     TBRI[0], TBRI[1], TBRI[2], val); 
    }

  return(val);
}

void wvmEst(float aMass, float WA, float TWAT, float TAUO,
	    float *TBRI, float *TTAU, float *TEFF, float *AEFF)
{
  /* Given the water vapour in the line of sight, the effective temperature 
     of the water and the excess broadband opacity (e.g. due to clouds), 
     calculates the expected (Rayleigh Jeans) brightness temperatures for 
     the three channels of the radiometer plus the total opacity and the 
     effective temperature of the water.
     Uses a simple 2-slab model but with coefficients adjusted to match ATM
  */


  int   j;                                 /* counter */
  /* float AR[] = {1.212,0.2634,0.09928 }; nominal tau per mm of water */
  float AR[] = {1.08,0.2634,0.1125 };	   /* empirical values */
  float AO[] = {-0.0022,0.00038,0.00017};  /* slope (due to pressure change) */
  float AT[] = {  0.0,    0.0,    0.0 };   /* temperature coefficient */
  float TDRY = 245.;                       /* physical temp of dry component */
  float B[] = {0.0200,0.0090,0.0110 };     /* dry opacity (ozone, etc.) */
  float TOFF[] = { 3.5,   0.6,  0.1  };    /* offset of temp at zero water */
  float DELT[] = { 14.2,  7.0,  7.0 };     /* effect of water on eff temp */
  float RJC =  4.4;   		           /* R-J correction for 183 GHz (T large) */
  float CMB = 0.367;		           /* R-J temp for 2.7 K         */
  float TOUT[3];                           /* Brightness temp above water */
  float TAUW[3];			   /* Water opacity */
  float ABSP;				   /* Absorption */
  double temporary;

  /* Loop over 3 channels with increasing offset from line centre.*/
  for(j=0; j<3; j++)
    {

      /* Dry component - assumed to be optically thin */
      TOUT[j] = CMB + TDRY * B[j] * aMass;

      /* Opacity of water (quadratic term is mostly effect of altitude) */
      AEFF[j] = AR[j] + AO[j] * WA + AT[j] * (TWAT - 269.);
      TAUW[j] = WA * AEFF[j];

      /* Effective temperature (includes effect of opacity, crudely) */
      temporary = -1 * TAUW[j]/3.5;
      TEFF[j] = TWAT - RJC - TOFF[j] 
	+ DELT[j]*(1. - exp(temporary));

      /* Radiative transfer */
      temporary = -1 * TAUW[j]-TAUO; 
      ABSP = exp(temporary);
      TBRI[j] = TOUT[j]*ABSP + TEFF[j]*(1.-ABSP);

      /* Total opacity */ 
      TTAU[j] = TAUW[j] + TAUO + B[j] * aMass;
      
    }
}


