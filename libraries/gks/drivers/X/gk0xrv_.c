/*
 * Valuator.c --- Module for handling a valuator tool on the SUN workstation
 * for RAL GKS.
 *
 * Written by: A C Arnold, University of Manchester Computer Graphics Unit,
 * Oxford Road, Manchester M13 9PL Tel: 061-273 7121 x 5405
 *
 * Maintenance Log:
 *
 *  26/02/87  PJWR  Corrected #includes for integration.
 *  18/03/87  TAW   Changed name to gk9srv_.
 *  06/05/87  PJWR  Corrected to use GKS drawing area of bitmap rather than
 *                  the entire bitmap.
 *  26/10/87  ACA   Added code to cope with multiple PETs
 *  10/12/87  PJWR  Changed BMCLEAR to BMCLEARALL in dialerase to get rid of
 *                  set pixels left around edge of dial.
 *  20/09/88  TAW   Modified to be used in X driver.
 *  21/09/88  TAW   Changed valuatorecb to xvaluatorecb.
 *  30/06/89  TAW   Changed nint to knint to please os4.
 */

#include <wwinfo.h>
#include <math.h>
#include "gk0xwd.h"
#include "../../system/include/f77_type.h"
#include "../../system/include/gks.h"
#include "../../system/include/gkinp.h"
#include "../../system/include/gkdt.h"
#include "../../system/include/gkwca.h"
#include "../../system/include/gkwdt.h"

#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))

extern char *sprintf();

static int slideopen(),slideecho(),slideerase(),slideclose(),
  dialopen(),dialecho(),dialerase(),dialclose(),
  digitalopen(),digitalecho(),digitalerase(),digitalclose();

struct ecb xvaluatorecb[]={			/* Echo control block decl */
	slideopen,slideecho,slideerase,slideclose,
	dialopen,dialecho,dialerase,dialclose,
	digitalopen,digitalecho,digitalerase,digitalclose
  };
static int ival;
static f77_integer inta[10],			/* Integer data for i/p */
  knint = 4,					/* Number of ints to get */
  nreal = 7,					/* Number of reals to get */
  ipcls = GVALUA;				/* Input class */
static f77_real reala[10],			/* Array of reals */
  currentvalue;					/* Current value */
static box echoarea;				/* Box for echo area */
static float vw, xw,				/* width of valuator and box */
  vs, vt,					/* Scale & shift value -> x */
  xs, xt;					/* Scale & shift x -> value */
static char label[12];				/* Label text goes here */

gk0xrv_(value)
  f77_real *value;
{
  gkrqip_(&ipcls, &gkywca_.kwi1,		/* Get i/p data */
	  &knint, &nreal, &inta[1], &reala[1]);
  echoarea = gk0xboxbuild((int) reala[KIPEXL],	/* Construct echo box */
		      (int)(gkywdt_.kdsry[gkywca_.kwkix - 1] - 1) - (int) reala[KIPEYT],
		      (int) reala[KIPEXR],
		      (int)(gkywdt_.kdsry[gkywca_.kwkix - 1] - 1) - (int) reala[KIPEYB]);
  (*xvaluatorecb[inta[KIPPRT]-1].open)();	/* Open up the echo area */
  gk0xipset(IPON);					/* Enable input */
  do {
    (*xvaluatorecb[inta[KIPPRT]-1].echo)();	/* Echo current value */
    gk0xipwait();					/* Wait for input */
    (*xvaluatorecb[inta[KIPPRT]-1].erase)();	/* Erase the echo */
  } while ((dd->d_event != IPOTHER || dd->d_buttons == 0) &&
	   (dd->d_event != IPKEY || dd->d_char != break_char));
  gk0xbu();					/* Wait for button up */
  gk0xipset(IPOFF);					/* Disable input */
  (*xvaluatorecb[inta[KIPPRT]-1].close)();	/* Close the echo area */
  *value = currentvalue;			/* Return current value */
  return (dd->d_event == IPKEY) ? GNONE : GOK;
}

/* Draw bar - routine to draw the pointer bar on our valuator tool */
static
drawbar(echoarea, ival)
  box echoarea;
  int ival;
{
  gk0xline(ival, echoarea.b_bottom, LNMOVEABS);
  gk0xline(ival, echoarea.b_top, LNDRAWABS);
}

/*************************************************************************************/

static int slideopen()
{
  gk0xoe(echoarea,"Valuator device");		/* Open up the echo area */
						/* Compute conversion factors */
  vw = reala[KVLMXV] - reala[KVLMNV];		/* Range of valuator */
  xw = ddbm->bm_box.b_right - ddbm->bm_box.b_left - 2;	/* Width of display */
  vs = xw / vw;					/* Scale factor */
  vt = ddbm->bm_box.b_left + 1 - reala[KVLMNV] * vs;
  xs = vw / xw;
  xt = reala[KVLMNV] - (ddbm->bm_box.b_left + 1) * xs;
  (void)sprintf(label, "%.2g", reala[KVLMNV]);
  gk0xftprint(ddbm->bm_box, label, FTVERT | FTOVER);/* Put in the minimum value */
  (void)sprintf(label, "%.2g", reala[KVLMXV]);
  gk0xftprint(ddbm->bm_box, label,
	  FTVERT | FTRIGHT | FTOVER);		/* And the maximum value */
  currentvalue = reala[KVLINV];			/* Get initial value */
  ival = currentvalue * vs + vt;		/* Start here */
}

static int slideecho()
{
	drawbar(ddbm->bm_box, ival);		/* draw the bar */
}

static int slideerase()
{
    drawbar(ddbm->bm_box, ival);		/* draw the bar */
    currentvalue = (ival = dd->d_x) * xs + xt;	/* Convert to a value */
    ival = max(min(ival, ddbm->bm_box.b_right-1),/* Constrain position */
               ddbm->bm_box.b_left+1);
    currentvalue = max(min(currentvalue,
			   reala[KVLMXV]),	/* Constrain the value, this */
		       reala[KVLMNV]);		/* avoids rounding errors */
}

static int slideclose()
{
  gk0xce();					/* Close the echo area */
}

/*****************************************************************************/
static int xc,yc,r;
static int dialopen()
{
	int yh;
	gk0xoe(echoarea,"Valuator device");	/* Open up the echo area */
/* Compute range of values and store initial value */
	vw = reala[KVLMXV] - reala[KVLMNV];	/* Range of valuator */
	currentvalue = reala[KVLINV];		/* Get initial value */
/* Compute centre of echo area */
	xc = (ddbm->bm_box.b_right+ddbm->bm_box.b_left)/2;
	yc = (ddbm->bm_box.b_top+ddbm->bm_box.b_bottom)/2;
/* Compute width and height of echo area */
	xw = ddbm->bm_box.b_right - ddbm->bm_box.b_left - 2;
	yh = ddbm->bm_box.b_bottom - ddbm->bm_box.b_top;
	r = min(yh/2,xw/2);			/* Compute radius */
	gk0xbmcircle(ddbm,xc,yc,r,BMEDGES);		/* Draw a circle */
	gk0xbmcircle(ddbm,xc,yc,r-3,BMEDGES);
	r = r-5;				/* Adjust to avoid erasure */
}

static int dialecho()
{
	double a;
	int xp,yp;
	a=M_PI/2-(currentvalue-reala[KVLMNV])/vw*2*M_PI;
	xp=xc+r*cos(a);
	yp=yc-r*sin(a);
	gk0xline(xc,yc,LNMOVEABS);			/* Draw the line */
	gk0xline(xp,yp,LNDRAWABS);
}

static int dialerase()
{
	double a,dx,dy;
	gk0xbmcircle(ddbm,xc,yc,r,BMCLEARALL);	/* Clear the circle */
	dx=dd->d_x-xc; dy=dd->d_y-yc;		/* Compute deltas */
/*
   Here we compute a value based on the angle of the dial pointer. The minimum
   value is at 12 o'clock (and so is the maximum!) and values increase
   positively when moving clockwise (this is the cause of the strange 'drem'
   statement below). The -dy in the call to atan2 below is due to WW's upside
   down coordinate system!
*/
	a=atan2(-dy,dx);			/* Compute angle */
	a=M_PI/2-a;				/* Convert to req range */
	if (a < 0) a += M_PI*2;			/* Adjust if -ve */
	currentvalue=reala[KVLMNV]+(vw*a/(2*M_PI)); /* Convert to a value */
}

static int dialclose()
{
	gk0xce();				/* Close the window */
}

/*****************************************************************************/

static int digitalopen()
{
  gk0xoe(echoarea,"Valuator device");		/* Open up the echo area */
						/* Compute conversion factors */
  vw = reala[KVLMXV] - reala[KVLMNV];		/* Range of valuator */
  xw = ddbm->bm_box.b_right - ddbm->bm_box.b_left - 2;	/* Width of display */
  vs = xw / vw;					/* Scale factor */
  vt = ddbm->bm_box.b_left + 1 - reala[KVLMNV] * vs;
  xs = vw / xw;
  xt = reala[KVLMNV] - (ddbm->bm_box.b_left + 1) * xs;
  (void)sprintf(label, "%.2g", reala[KVLMNV]);
  gk0xftprint(ddbm->bm_box, label, FTVERT | FTOVER);/* Put in the minimum value */
  (void)sprintf(label, "%.2g", reala[KVLMXV]);
  gk0xftprint(ddbm->bm_box, label,
	  FTVERT | FTRIGHT | FTOVER);		/* And the maximum value */
  currentvalue = reala[KVLINV];			/* Get initial value */
  ival = currentvalue * vs + vt;		/* Start here */
}

static int digitalecho()
{
	(void)sprintf(label, "%.2g", currentvalue);
	gk0xftprint(ddbm->bm_box, label,
		FTCENTRE | FTOVER);		/* Display the value */
}

static int digitalerase()
{
    gk0xftprint(ddbm->bm_box, label,
            FTCENTRE | FTOVER);			/* Erase the value */
    currentvalue = (ival = dd->d_x) * xs + xt;	/* Convert to a value */
    ival = max(min(ival, ddbm->bm_box.b_right-1),/* Constrain position */
               ddbm->bm_box.b_left+1);
    currentvalue = max(min(currentvalue,
			   reala[KVLMXV]),	/* Constrain the value, this */
		       reala[KVLMNV]);		/* avoids rounding errors */
}

static int digitalclose()
{
	gk0xce();				/* Close down the window */
}

/*****************************************************************************/











