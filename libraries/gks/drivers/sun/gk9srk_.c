/*
 * gk9srk_.c --- Module for handling a stroke tool on the SUN workstation for
 * 		 RAL GKS.
 *
 * Written by: A C Arnold, University of Manchester Computer Graphics Unit,
 * Oxford Road, Manchester M13 9PL Tel: 061-273 7121 x 5405
 *
 * Maintenance log:
 *
 * 12/03/87  ???   Modified to use buffer size correctly.
 * 18/03/87  TAW   Changed name to gk9srk_
 * 06/05/87  PJWR  Corrected to use GKS drawing area of bitmap rather than
 *                 the entire bitmap.
 * 14/07/87  PJWR  IS: Changed error 901 to error 2001.  Documented error.
 * 04/12/87  ACA   Restructured to allow multiple PETs
 * 04/12/87  ACA   Added PET 2, digital echo.
 * 10/12/87  PJWR  Modified to use new colour management scheme and to
 *                 open echo areas before starting input.
 *                 Digital echo modified to use a fixed length string and
 *                 filter -ve coordinates. Digital erase is now a no-op.
 *                 WW <-> NDC transformation routines now invert the DC
 *                 rather than the NDC to avoid the transformation number
 *                 problem found with locator.
 * 18/11/88  ACA   Made structecb static.
 * 03/05/90  RMK   Added check on kerror after gkrqip call.
 *
 * Errors:
 *
 *   2001  Output parameter size insufficient.
 */

#include "./varinc/wwinfo.h"
#include <string.h>
#include "gk9swd.h"
#include "../../system/include/f77_type.h"
#include "../../system/include/gks.h"
#include "../../system/include/gkinp.h"
#include "../../system/include/gkdt.h"
#include "../../system/include/gkwca.h"
#include "../../system/include/gkwdt.h"
#include "../../system/include/gkerr.h"

#define TRIGGER ITEMBUTTON
#define SUBTRIGGER MENUBUTTON
#define DELETE SHOWBUTTON

/*
 * Here we declare any external or forward functions needed.
 */

char* calloc();					/* Used to allocate buffer */
void cfree();					/* Used to free buffer */

static int
  rubberopen(),rubber(),rubberclose(),	/* Declare routine */
  digitalopen(),digitalecho(),digitalerase(),digitalclose(),
  markeropen(),marker(),markerclose(),
  lineopen(),doline(),lineclose(),
  noaction();


/*
 * Here are some variables that are local to this module but accessed by
 * a number of routines. Hence they are declared static.
 */

static f77_integer one = 1;

/*
 * The following is derived from stroke.cursor (generated from SUN's
 * iconeditor run through conv and then hacked into here. I'm sure there's
 * a better way of doing it.
 */

static char skcupat[] = { WWXOR, 0, 15, 0, -15,
/* lines= */ 0,16, /* width= */ 0,16,
0,010,
0,034,
0,076,
0,0136,
0,0214,
01,030,
02,060,
04,0140,
010,0302,
021,0206,
043,014,
0106,0170,
0154,0100,
0370,0140,
0360,020,
0337,0360}; /* ww style */

static struct skelem {f77_real xpt, ypt;} *skbuff;	/* Pointer to stroke */

static struct ecb strokeecb [] = {				/* Table of routines */
	rubberopen,rubber,rubber,rubberclose,
	digitalopen,digitalecho,noaction,digitalclose,
	markeropen,noaction,marker,markerclose,
	lineopen,noaction,doline,lineclose
  };

static f77_integer
    inta[11],					/* Integer data from gkrqip */
    lp;						/* Index to last point */

static f77_real
    reala[10],					/* Array of reals */
    cxdc, cydc, cx, cy;				/* Current point */

static int
    line_mode,					/* Saved line mode */
    colour_index;				/* Saved foreground colour */

static cursor *skcp;				/* Pointer to cursor */

static box echoarea;				/* Box for echo area */
static window *echowin;				/* Window for digital echo */

f77_integer
gk9srk_(npts, maxpts, rx, ry)
  f77_integer *npts, *maxpts;
  f77_real rx[], ry[];

{
  f77_integer
    knint = 10,					/* Number of ints to get */
    nreal = 4,					/* Number of reals to get */
    ipcls = GSTROK;				/* Input class */
  int i, done = FALSE;	       			/* Termination flag */

  gkrqip_(&ipcls, &gkywca_.kwi1,		/* Get i/p data */
	  &knint, &nreal, &inta[1], &reala[1]);

	if(gkyerr_.kerror !=0) return(GNONE);
  echoarea = boxbuild((int) reala[KIPEXL],	/* Construct echo box */
		      (int)(gkywdt_.kdsry[gkywca_.kwkix - 1] - 1) - (int) reala[KIPEYT],
		      (int) reala[KIPEXR],
		      (int)(gkywdt_.kdsry[gkywca_.kwkix - 1] - 1) - (int) reala[KIPEYB]);

  skbuff = (struct skelem *)calloc((unsigned) inta[KSKINB],
                  sizeof(struct skelem));	/* Allocate a buffer */

  lp = -1;					/* Flag no last point */
  if (*npts > 0) {
    for (i = 0; i < *npts; i++){		/* Copy initial stroke */
      skbuff[i].xpt = rx[i];
      skbuff[i].ypt = ry[i];
    }
    lp = *npts - 1;				/* Note last pt */
    cx = rx[lp];
    cy = ry[lp];				/* Note current point */
  };

  (*strokeecb[inta[KIPPRT]-1].open)();		/* Open the echo area */

  ipset(IPON);					/* Enable input */

  do {						/* Main loop */
    gk9sbu();					/* Wait for button up */
    (*strokeecb[inta[KIPPRT]-1].echo)();	/* Echo current point */
    ipwait();					/* Wait for input */
    (*strokeecb[inta[KIPPRT]-1].erase)();	/* Erase current point */
    if (dd->d_event == IPOTHER) {		/* Button or mouse */
      if (dd->d_buttons == SUBTRIGGER) {	/* Sub trigger ? */
	cxdc = dd->d_x;
	cydc = dd->d_y;				/* convert to reals */
	gk9twwn(1, &cxdc, &cydc, &cx, &cy);	/* convert to NDC */
	if (gkyerr_.kerror == 0){
	  if (lp < inta[KSKINB] - 1){		/* Store if room */
            (*strokeecb[inta[KIPPRT]-1].echo)();/* Echo current point */
	    skbuff[++lp].xpt = cx;
	    skbuff[lp].ypt = cy;
	  }				/* note as last point */
	} else gkyerr_.kerror = 0;
      } else if (dd->d_buttons == TRIGGER) {	/* Main trigger ? */
	cxdc = dd->d_x;
	cydc = dd->d_y;				/* convert to reals */
	gk9twwn(1, &cxdc, &cydc, &cx, &cy);	/* convert to NDC */
	if (gkyerr_.kerror == 0){
	  if (lp < inta[KSKINB] - 1){		/* Ensure room in buffer */
            (*strokeecb[inta[KIPPRT]-1].echo)();/* Echo current point */
	    skbuff[++lp].xpt = cx;
	    skbuff[lp].ypt = cy;		/* note as last point */
	    *npts = lp + 1;			/* Return number of points */
	    done = TRUE;			/* Note we've finished */
	  }
	} else gkyerr_.kerror = 0;
      } else if (dd->d_buttons == DELETE) {	/* Delete line ? */
	if (lp > 0) {				/* At lease 1 point ? */
	  cx = skbuff[lp].xpt;
	  cy = skbuff[lp].ypt;			/* Make last point current */
	  lp -= 1;				/* Restore last point */
          (*strokeecb[inta[KIPPRT]-1].erase)();	/* Erase current point */
	} else
	  lp = -1;				/* Set no last point */
      } else {					/* Must be mouse movement */
	cxdc = dd->d_x;
	cydc = dd->d_y;				/* convert to reals */
	gk9twwn(1, &cxdc, &cydc, &cx, &cy);	/* convert to NDC */
	gkyerr_.kerror = 0;			/* Ignore errors */
      }
    } else if (dd->d_event == IPKEY && dd->d_char == break_char) {
      done = TRUE;
    }
  } while (!done);
  gk9sbu();					/* Wait for button up */
  ipset(IPOFF);					/* Disable i/p */

  (*strokeecb[inta[KIPPRT]-1].close)();		/* Close echo area */

  if (*npts > *maxpts) {
    gkyerr_.kerror = 2001;			/* Language binding error */
    *npts = *maxpts;				/* Return as much as we can */
  }
  for (i=0; i<*npts; i++){			/* Copy back */
    rx[i] = skbuff[i].xpt;
    ry[i] = skbuff[i].ypt;
  }
  cfree((char *)skbuff);			/* Free the buffer */
  return (dd->d_char == break_char ? GNONE : GOK);
}


/*
 * draw_line draws a line from (xx1,yy1) to (xx2,yy2) where the coords are in
 * NDC space.
 */

static
  draw_line(xx1, yy1, xx2, yy2)
double xx1, yy1, xx2, yy2;
{
	f77_real x1_dc, x2_dc, y1_dc, y2_dc,
	x1 = xx1, y1 = yy1, x2 = xx2, y2 = yy2;	/* This is 'orrible! */

	gk9tnww(1, &x1, &y1, &x1_dc, &y1_dc);
	gk9tnww(1, &x2, &y2, &x2_dc, &y2_dc);
	line((int) x1_dc, (int) y1_dc, LNMOVEABS);
	line((int) x2_dc, (int) y2_dc, LNDRAWABS);
}

/* draw_stroke draws a polyline with the coords in NDC */

static
  draw_stroke(npts, skpts)
f77_integer npts;
struct skelem skpts[];
{
	int i;
	f77_real cx, cy;

	gk9tnww(1, &skpts[0].xpt, &skpts[0].ypt,
		&cx, &cy);			/* Move to first point */
	line((int) cx, (int) cy, LNMOVEABS);
	for (i = 1; i < npts; i++) {		/* Line for rest of points */
		gk9tnww(1, &skpts[i].xpt, &skpts[i].ypt,
			&cx, &cy);
		line((int) cx, (int) cy, LNDRAWABS);
	}
}

/*
 * gk9twwn is a C callable routine for converting from WW space to NDC space.
 * It also inverts the Y coordinate to correct for WW's silly coordinate
 * space.
 */

static
  gk9twwn(n, xdc, ydc, xndc, yndc)		/* Convert DC to NDC */
int n;
f77_real xdc[], ydc[], xndc[], yndc[];
{
	int i,
	    t = n,
	    ymax = (int)gkywdt_.kdsry[gkywca_.kwkix - 1] - 1;

	for (i = 0; i < n; i++) {		/* Invert to "real" DC */
		ydc[i] = ymax - ydc[i];
	}
	gktdn_(&t, xdc, ydc, xndc, yndc);	/* Convert using utility */
}

/*
 * gk9tnww is a C callable routine for converting from NDC space to WW space.
 * It inverts the Y coordinate to cope with WW's silly coordinate space.
 */

static
  gk9tnww(n, xndc, yndc, xdc, ydc)
int n;
f77_real xndc[], yndc[], xdc[], ydc[];
{
	int i,
	    t = n,
	    ymax = (int)gkywdt_.kdsry[gkywca_.kwkix - 1] - 1;

	gktnd_(&t, xndc, yndc, xdc, ydc);		/* Convert it */
	for (i = 0; i < n; i++) {
		ydc[i] = ymax - ydc[i];			/* Invert Y */
	}
}

/****************************************************************************/

static int rubberopen()
{
  f77_integer gk9scc_();

  /* gk9soe(echoarea, 0); */			/* open the echo area */
  lnstack(WWPUSH);				/* Save pen position */
  line_mode=dd->d_line;				/* Save line mode */
  dd->d_line=WWXOR;				/* Set XOR mode */
  colour_index = dd->d_fore;			/* Save foreground colour */
  dd->d_fore = (int)gk9scc_(&one);		/* Set to "default" */
  skcp = cudecode(skcupat,ENWWSTYLE);		/* Generate cursor */
  custack(skcp,WWPUSH);				/* Establish it */
  draw_stroke(lp+1, skbuff);			/* Draw initial stroke */
}

static int rubber()
{
    if (lp != -1)
      draw_line(skbuff[lp].xpt, skbuff[lp].ypt,
                cx, cy);			/* Draw the current line */
}

static int rubberclose()
{
  custack(skcp,WWPOP);				/* Restore default cursor */
/*  gk9sbu();*/					/* Wait for button up */
  draw_stroke(lp+1,skbuff);			/* Erase the stroke */
  /* gk9sce(echoarea); */			/* Close the echo area */
  dd->d_line=line_mode;				/* Restore line mode */
  dd->d_fore=colour_index;			/* Restore foreground colour */
  lnstack(WWPOP);				/* Restore position */
}

/****************************************************************************/

static int digitalopen()
{
	gk9soe(echoarea,"Stroke device");
	echoarea=ddbm->bm_box;
	echowin = ddwin;			/* Save echo area to delete */
	winstack(WWPOP);			/* Pop display surface so that
						 * events are from there */
	skcp = cudecode(skcupat,ENWWSTYLE);	/* Generate cursor */
	custack(skcp,WWPUSH);			/* Save cursor */
}

static int digitalecho()
{
   char *sprintf();		/* BSD Only! ANSI C will make this int */
   char text[22];

   int				/* DC limits in X and Y */
     xmax = gkywdt_.kdsrx[gkywca_.kwkix - 1] - 1,
     ymax = gkywdt_.kdsry[gkywca_.kwkix - 1] - 1,
     x = (int)cxdc,
     y = (int)cydc;		/* Already inverted by gktwwn()! */
	 if(x < 0) x = 0;
	 else if(x > xmax) x = xmax;
	 if(y < 0) y = 0;
	 else if(y > ymax) y = ymax;
   (void)sprintf(text,"#%4d: X=%4d, Y=%4d",
	   lp+1,x,y);
   ftprint(echoarea,text,FTCENTRE);
}

static int digitalerase()
{
   ftprint(echoarea,"                     ",FTCENTRE);
}

static int digitalclose()
{
	winstack(WWPUSH);	/* Push ddwin for gk9sce() to pop again! */
	ddwin = echowin;	/* So the echo window is deleted by gk9sce() */
	gk9sce();
	custack(skcp,WWPOP);			/* Restore default cursor */
}

/****************************************************************************/

static int markeropen()
{
}

static int marker()
{
}

static int markerclose()
{
}

/****************************************************************************/

static int lineopen()
{
}

static int doline()
{
}

static int lineclose()
{
}

/****************************************************************************/

static int noaction()
{
}

/***************************************************************************/
