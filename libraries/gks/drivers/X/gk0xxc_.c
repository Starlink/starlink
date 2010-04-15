/*
 * RAL GKS SYSTEM
 *
 * gk0xxc_:  Outputs a raster character string given either the start position
 *           of the string or the centre of each character in the string.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1988
 *
 * Maintenance Log:
 *
 *     20/09/88	TAW	Copied from ../sun/gk9sxc_.c and dummied.
 *     27/11/88 TAW     Fleshed out to work with X strings.
 *     16/02/89 TAW     Change gc function for colour from XOR to COPY.
 */

#include <stdio.h>
#include <wwinfo.h>				/* For ww */
#include "../../system/include/f77_type.h"	/* For FORTRAN type matching */
#include "../../system/include/gks.h"		/* For GCHARP */
#include "../../system/include/gkerr.h"		/* For error reporting */
#include "../../system/include/gkdt.h"		/* Needed by the following */
#include "../../system/include/gkwca.h"		/* For WCA access */
#include "../../system/include/gkwdt.h"		/* For WDT access */
#include "../../system/include/gkwkd.h"		/* For WS workspace access */
#include "gk0xwd.h"				/* For WS workspace offsets */
#include <X11/Xlib.h>
#include "./ww/dd/x/font.h"			/* for jfontinfo */
/*
 * Types:
 *
 *  jfontinfo:  The structure used by ww to hold font information.
 */

/*
 * Errors:
 *
 *    300  Storage overflow has occurred in GKS
 *
 * Comments:
 *
 *  The size of the workstation colour table is used to decide whether the
 *  workstation's monochrome or colour.  The code to grab the pixfont from
 *  ww was supplied by M M Martin.
 */

f77_integer gk0xxc_(ifid, itxpr, nchars, ichstr, x, y)
  f77_integer *ifid,		/* Font identifier (Unused) */
	      *itxpr,		/* Text precision (In) */
	      *nchars,		/* Number of chracters in string (In) */
	      ichstr[];		/* String as an array of ASCII integers (In) */
  f77_real    x[],		/* Centre positions (X) / string origin (In) */
	      y[];		/* Centre positions (Y) / string origin (In) */
{
  f77_integer
    numchars = *nchars;		/* number of characters in string ichstr */
  extern char
    *malloc();			/* For getting memory for C copy of string */

  static char
    ch[2] = {'\0','\0'};	/* For passing single characters to X */

  char
    *str;			/* C copy of string */

  Font pf;			/* The current X font */
  int
    ws = 			/* Local copy of workstation index */
      gkywca_.kwkix - 1,
    y_max =			/* Display space size in Y */
      gkywdt_.kdsry[ws] - 1,
    xorig, yorig,		/* character origin for X */
    i;				/* Loop index variable */

  f77_integer
    b_left,			/* Changed display area left bound */
    b_right,			/* Changed display area right bound */
    b_top,			/* Changed display area top bound */
    b_bottom;			/* Changed display area bottom bound */

  f77_real
    x_offset =			/* Offset of left edge from character centre*/
      (gkywkd_.qwkdat[ws][ICHWD] / 2.0),
    y_offset =			/* From character centre to X baseline */
      ((gkywkd_.qwkdat[ws][ICHHT]) / 2.0) - gkywkd_.qwkdat[ws][ICHGS];

  static Display *display;	/* X display to draw on */
  Drawable *pixmap; 		/* of the above display */
  static GC LGC;		/* The GC to setup for drawing text */
  XGCValues LGCValues;          /* Values to set up in the GC */

  /*
   * Allocate space for C copy of string and convert it from integer to
   * character representation.  Set error 300 and return if we can't get
   * memory.
   */

  if((str = malloc((unsigned)numchars+1)) == (char *)0)
    gkyerr_.kerror = 300;
  else
  {
    /* Do the conversion */

    for(i = 0; i < numchars; i++)
      str[i] = (char)ichstr[i];
    str[numchars] = '\0';

    /*
     * Get the display and font for use by X.
     */
    display = (Display *)gk0xunportask(ddbm,040);
    pixmap = (Drawable *)gk0xunportask(ddbm,ASKBMMEMORY);
    if(LGC == (GC)0) LGC = DefaultGC(display, gk0xunportask(ddbm,050));
    if(LGCValues.font != ((jfontinfo *)(ddfont->f_junk))->jf_font)
      {
        LGCValues.font = ((jfontinfo *)(ddfont->f_junk))->jf_font;
	XSetFont(display, LGC, LGCValues.font);
       }

    /*
     * Select output routine and rasterop.  Use Set rasterop
     * for monochrome,  COPY for colour.
     */

    if(DefaultVisual(display,DefaultScreen(display))->class==PseudoColor)
       LGCValues.function= GXcopy;
    else LGCValues.function=GXset;
    XSetFunction(display, LGC, LGCValues.function);
    XSetForeground(display, LGC, dd->d_fore);

    /*
     * If we're using character precision,  render each character individually
     * given its centre,  otherwise render the entire string given the origin
     * of the string.
     */

    if (*itxpr == GCHARP)		/* Character precision */
    {
      /* Initialise the modified area of the bitmap */

      b_left = gkywdt_.kdsrx[ws];
      b_right = 0;
      b_top = gkywdt_.kdsry[ws];
      b_bottom = 0;

      for(i = 0; i < numchars; i++)
      {
	/*
	 * Calculate X output coordinates (left edge and baseline)
	 * and update modified area.
	 */

	 if((xorig = (int)(x[i] - x_offset)) < b_left)
           b_left = xorig;
         else if (xorig > b_right)
           b_right = xorig;
         if((yorig = y_max - (int)(y[i] - y_offset)) < b_top)
           b_top = yorig;
         else if (yorig > b_bottom)
           b_bottom = yorig;

	/* Output the character */
        ch[0] = str[i];
        XDrawString(display,pixmap,LGC,xorig,yorig,ch,1);
      }

      /* Modify right bound of changed area for character width */

      b_right += (int)gkywkd_.qwkdat[ws][ICHWD] - 1;
    }
    else				/* String precision */
    {
      /* Calculate X output coordinates of the first character */
      xorig = (int)(x[0] - x_offset);
      yorig = y_max - (int)(y[0] - y_offset);

      /*
       * Calculate left and right bounds of changed area and set both top and
       * bottom bounds to the Y output coordinate.
       */
      b_left = xorig;
      b_right = (int)(numchars * gkywkd_.qwkdat[ws][ICHWD]) +
			xorig - 1;
      b_top = b_bottom = yorig;

      /* Render the string */
      XDrawString(display,pixmap,LGC,xorig,yorig,str,numchars);
    }

    /* Update top and bottom bounds of changed area for character height */

    b_top -= (int)(gkywkd_.qwkdat[ws][ICHHT] +
			    gkywkd_.qwkdat[ws][ICHCT] +
			    gkywkd_.qwkdat[ws][ICHGS]) + 1;
    b_bottom += (int)(gkywkd_.qwkdat[ws][ICHBB] -
			       gkywkd_.qwkdat[ws][ICHGS]);

    /* Set up the display area to be updated in the workstation workspace */

    if(b_left < gkywkd_.kwkdat[ws][ILEFT])
      gkywkd_.kwkdat[ws][ILEFT] = (f77_integer)b_left;
    if(b_right > gkywkd_.kwkdat[ws][IRIGHT])
      gkywkd_.kwkdat[ws][IRIGHT] = (f77_integer)b_right;
    if(b_top < gkywkd_.kwkdat[ws][ITOP])
      gkywkd_.kwkdat[ws][ITOP] = (f77_integer)b_top;
    if(b_bottom > gkywkd_.kwkdat[ws][IBOTT])
      gkywkd_.kwkdat[ws][IBOTT] = (f77_integer)b_bottom;

    /* Free the string copy */

    (void)free(str);
  }





}
