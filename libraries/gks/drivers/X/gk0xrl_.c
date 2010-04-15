/*
 * gk0xrl_.c --- module containing routines for handling locator input for
 * RAL GKS on the Xlib workstations.
 *
 * Written by A C Arnold, University of Manchester Computer Graphics Unit,
 * Computer Building, Oxford Road, Manchester M13 9PL Tel: 061-273 7121 x
 * 5405.
 *
 * The routines should really map to GKS's FORTRAN common blocks to pick up
 * everything, but until I know how to do that I'll pass things as
 * parameters. It's also a single WS version.
 *
 * Maintenance log:
 *
 * ??? - created by ACA 26-Jan-1987 : Modified to be callable from GKS Replaced
 * su_initlocator by call to gkilc in su_reqlocator Added conversion to NDC
 * at end of su_reqlocator Various to support the above. Changed name to
 * gk9rlc.
 *
 *  26/02/87  PJWR  Corrected #includes for integration.
 *  18/03/87  TAW   Changed name to gk9srl_.
 *  02/04/87  PJWR  Corrected #include for gk9sri.h.
 *  24/07/87  ACA   Added extra prompt & echo types as per standard.
 *  28/07/87  ACA   Fixed rubberrect to draw all 4 sides.
 *                  Adjusted index to locator by 1 (C arrays start at 0)
 *                  Modified to use dc version of initial position.
 *  29/07/87  ACA   Added digital echo (PET 6)
 *  26/11/87  PJWR  [1] Digital PET modified to give GKS DC rather then raster
 *                  coordinates.  Negative coordinates generated when cursor
 *                  passes through frame are filtered.  Fixed width fields used
 *                  in an effort to cut flicker.  [2] PETs 1-3 now blank the ww
 *                  cursor during input and restore it afterward.  [3] Locator
 *                  value is now only accepted if it's within the workstation
 *                  window (code taken from gk9srk_()).  [4] Echoes now drawn
 *                  in GKS colour index 1.
 *  27/11/87  PJWR  Inverted Y coordinates so all calls to GKS utilities used
 *                  GKS device coordinates rather than ww.  This cured a fault
 *                  reported by Bryan Colyer where incorrect values for norm.
 *                  trans. were being returned by REQUEST LOCATOR.
 *  10/12/87  PJWR  Updated to use new colour management scheme.  Input is now
 *                  activated after opening the echo area,  so that the echo
 *                  area will track the workstation properly if it's been
 *                  moved.  Digital open and close modified to suit.
 * 20/09/88   TAW   Changed to use with Xlib workstation.
 * 21/09/88   TAW   Changed locator to xlocator.
 */
#include <wwinfo.h>
#include "../../system/include/f77_type.h"
#include "../../system/include/gks.h"
#include "../../system/include/gkinp.h"
#include "../../system/include/gkdt.h"
#include "../../system/include/gkwdt.h"
#include "../../system/include/gkwca.h"
#include "../../system/include/gkerr.h"
#include "gk0xwd.h"

#define crosssize 30

/*
 * We construct a struct which cointains pointers to routines for handling
 * various aspects of performing input. At present we have two routines, one
 * for handling echoing and the other for erasing the echo.
 *
 */
  static int crosshairs(),trackingcross(),rubberband(),rubberrect(),digital(),
  digitalopen(), digitalclose(), noaction(), cu_off(), cu_on();

struct ecb xlocator[]={
	cu_off, crosshairs, crosshairs, cu_on, /* PET 1 */
	cu_off, crosshairs, crosshairs, cu_on, /* PET 2 */
	cu_off, trackingcross, trackingcross, cu_on, /* PET 3 */
	noaction, rubberband, rubberband, noaction, /* PET 4 */
	noaction, rubberrect, rubberrect, noaction, /* PET 5 */
	digitalopen, digital, digital, digitalclose /* PET 6 */
  };

static box echoarea;			/* Box for echo area */
static window* echowin;			/* Saved digital echo window */
static f77_integer inta[10];		/* Integer data returned from GKS */
static f77_real reala[10];		/* Real data returned from GKS */

f77_integer gk0xrl_(n, xndc, yndc)		/* request locator input */
  f77_integer *n;
  f77_real *xndc, *yndc;
{
  f77_integer
    gk0xcc_();					/* Maps GKS -> ww colours */

  int line_mode,				/* Used to save line drawing
						 * mode */
      colour,					/* Used to save foreground
						 * colour */
      one = 1,					/* Constant for gk0xcc_() */
      y_max = 					/* Window height for coord. */
	gkywdt_.kdsry[gkywca_.kwkix - 1] - 1;	/* inversion */

  f77_real xdc, ydc, inixdc, iniydc;

  gkilc_(xndc, yndc, &inta[1], &reala[1], &xdc, &ydc);	/* get i/p data */
  ydc = y_max - ydc;				/* Invert Y for ww */
  inixdc = xdc; iniydc = ydc;                   /* Note initial position */
  gk0xlnstack(WWPUSH);				/* Stack current pen position */
  line_mode = dd->d_line;			/* Save current line mode */
  colour = dd->d_fore;			        /* Save current colour */
  dd->d_line = WWXOR;				/* Set XOR mode */
  dd->d_fore = (int)gk0xcc_(&one);		/* Set colour to 1 */
  if (inta[KIPE] == GECHO)			/* Open up echo area */
    (*xlocator[inta[KIPPRT]-1].open)();
  gk0xipset(IPON);					/* Switch on input */
  do {
	  gkyerr_.kerror = 0;			/* Ignore DC -> NDC errors */
	  if (inta[KIPE] == GECHO)
	    (*xlocator[inta[KIPPRT]-1].echo)
	      ((int) xdc, (int) ydc,
	       (int) inixdc,
	       (int) iniydc);	                /* echo the locator position */
	  gk0xipwait();				/* wait for an event */
	  if (inta[KIPE] == GECHO)
	    (*xlocator[inta[KIPPRT]-1].erase)
	      ((int) xdc, (int) ydc,
	       (int) inixdc,
	       (int) iniydc);		        /* erase the locator echo */
    	  xdc = dd->d_x;			/* get the position */
    	  ydc = y_max - dd->d_y;		/* Invert Y for GKS */
          gktdn_(n, &xdc, &ydc, xndc, yndc);	/* Convert to NDC - sets
						 * kerror if invalid */
	  ydc = y_max - (int)ydc;		/* Invert Y coord. for ww */
  } while ((gkyerr_.kerror != 0) ||
	   ((dd->d_event != IPOTHER || dd->d_buttons == 0) &&
	   (dd->d_event != IPKEY || dd->d_char != break_char)));
  gk0xbu();					/* Wait for button up */
  gk0xipset(IPOFF);					/* disable input */
  if (inta[KIPE] == GECHO)	/* Close down the echo area */
    (*xlocator[inta[KIPPRT]-1].close)();
  dd->d_line = line_mode;			/* restore line mode */
  dd->d_fore = colour;				/* restore colour */
  gk0xlnstack(WWPOP);				/* restore pen position */
  return (dd->d_event == IPKEY) ? (f77_integer)GNONE : (f77_integer)GOK;
}

  static crosshairs(x, y) int x, y; /* Display crosshairs */
{
  gk0xline(ddwin->w_bm->bm_box.b_left, y, LNMOVEABS);
  gk0xline(ddwin->w_bm->bm_box.b_right, y, LNDRAWABS);
  gk0xline(x, ddwin->w_bm->bm_box.b_bottom, LNMOVEABS);
  gk0xline(x, ddwin->w_bm->bm_box.b_top, LNDRAWABS);
}

  static rubberband(x,y,inix,iniy) int x,y,inix,iniy; /* Display rubberband line */
{
	gk0xline(inix,iniy,LNMOVEABS);
	gk0xline(x,y,LNDRAWABS);
}

  static rubberrect(x,y,inix,iniy) int x,y,inix,iniy; /* Display rubber rectangle */
{
	gk0xline(inix,iniy,LNMOVEABS);
	gk0xline(inix,y,LNDRAWABS);
	gk0xline(x,y,LNDRAWABS);
	if (inix != x && iniy !=y){
		gk0xline(x,iniy,LNDRAWABS);
		gk0xline(inix,iniy,LNDRAWABS);
	}
}

  static trackingcross(x,y) int x,y; /* Display a small tracking cross */
{
	gk0xline(x-crosssize/2,y,LNMOVEABS);
	gk0xline(crosssize,0,LNDRAWREL);
	gk0xline(x,y-crosssize/2,LNMOVEABS);
	gk0xline(0,crosssize,LNDRAWREL);
}

  static digitalopen()		/* Open echo area for PET 6 */
{
	echoarea = gk0xboxbuild((int) reala[KIPEXL],
			    (int)(gkywdt_.kdsry[gkywca_.kwkix - 1] - 1) -
			    (int) reala[KIPEYT],
			    (int) reala[KIPEXR],
			    (int)(gkywdt_.kdsry[gkywca_.kwkix - 1] - 1) -
			    (int) reala[KIPEYB]);
        gk0xoe(echoarea,"Locator device");
	echoarea = ddbm->bm_box;

	/*
	 * Now save echo area window for deletion by gk0xce() and pop the
	 * workstation window so events come from the display surface.
	 */

	echowin = ddwin;
	gk0xwinstack(WWPOP);
}

  static digitalclose()		/* Close echo area for PET 6 */
{
  gk0xwinstack(WWPUSH);		/* Push ddwin for gk0xce() to pop again! */
  ddwin = echowin;		/* So the echo window is deleted by gk0xce() */
  gk0xce();
}

  static digital(x,y) int x,y;	/* Display digital rep of locator */
{
  char *sprintf();		/* BSD Only! - ANSI C will make this int */
  char label[15];		/* Space to store text of echo */
  int				/* DC limits in X and Y */
    xmax = gkywdt_.kdsrx[gkywca_.kwkix - 1] - 1,
    ymax = gkywdt_.kdsry[gkywca_.kwkix - 1] - 1;
	if(x < 0) x = 0;
	else if(x > xmax) x = xmax;
	if(y < 0) y = 0;
	else if(y > ymax) y = ymax;
	(void) sprintf(label, "X=%4d, Y=%4d",
		       x,
		       ymax - y
		       );
	gk0xftprint(echoarea, label, FTCENTRE);
}

  static cu_off()		/* Blanks cursor for crosshair PETs */
{
  static char			/* Blank, transparent cursor */
    blank_cursor[41] =
    {
      WWOR, 0, 0, 0, 0,
      0, 16, 0, 16,
      0
    }; /* ww style */

  cursor
    *gk0xcudecode();		/* To translate blank_cursor for custack() */

  gk0xcustack(gk0xcudecode(blank_cursor, ENWWSTYLE), WWPUSH);
}

  static cu_on()		/* Restores cursor after use of cu_off */
{
  gk0xcustack((cursor *)0, WWPOP);
}

  static noaction()		/* Dummy routine */
{
}
