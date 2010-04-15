/*
 * RAL GKS SYSTEM
 *
 * gk9sxc_:  Outputs a raster character string given either the start position
 *           of the string or the centre of each character in the string.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1987
 *
 * Maintenance Log:
 *
 *     ??/??/87  TAW   Created.
 *     06/05/87  PJWR  Corrected to use GKS drawing area of bitmap rather than
 *                     the entire bitmap.
 *     14/07/87  PJWR  IS: Changed error 301 to error 300.
 *     23/09/87  PJWR  Modified to use SunView text routines for speed.
 *     24/09/87  PJWR  Switched from pixwin routines to pixrect routines for
 *                     output and now call notescreen() to perform updates.
 *                     This was required because ww does its own repainting.
 *     09/10/87  PJWR  Modified Y offset calculation to remove algebraic
 *                     error.
 *     21/10/87  PJWR  The hardware text utility returns the character body
 *                     centre,  not the character box centre.  The Y offset
 *                     has been changed to reflect this.
 *     23/10/87  PJWR  Text was being drawn consistently one pixel too high.
 *                     A correction factor of 1.0 has been added to the Y
 *                     offset.  The problem is believed to be in SunView,  as
 *                     diagnostic output showed that the same baseline Y
 *                     coordinate was being used for both string and stroke
 *                     precision text,  but the string precision text appeared
 *                     one pixel higher.
 *     03/11/87  PJWR  Display updating now done in main driver.  Code changed
 *                     to leave relevant information in WS workspace rather
 *                     than calling notescreen().
 *     03/02/88  TAW   Changed PIX_SET to PIX_SRC  in pw_ttext call.
 *     31/01/89  TAW   Changed jfontinfo to reflect changes in ww.
 *     07/04/89  RMK   Changed to use transparent character box on monochrome
 *                     as well as colour workstations.
 *     15/03/90  PLP   Fixed the single character CHAR precision strings
 *                     problem (S377).
 */

#include <stdio.h>
#include "../../drivers/sun/varinc/wwinfo.h"				/* For ww */
#include <pixrect/pixrect_hs.h>			/* For pixrect operations */
#include "../../system/include/f77_type.h"	/* For FORTRAN type matching */
#include "../../system/include/gks.h"		/* For GCHARP */
#include "../../system/include/gkerr.h"		/* For error reporting */
#include "../../system/include/gkdt.h"		/* Needed by the following */
#include "../../system/include/gkwca.h"		/* For WCA access */
#include "../../system/include/gkwdt.h"		/* For WDT access */
#include "../../system/include/gkwkd.h"		/* For WS workspace access */
#include "../../drivers/sun/gk9swd.h"				/* For WS workspace offsets */

/*
 * Types:
 *
 *  jfontinfo:  The structure used by ww to hold font information.
 */

typedef struct
{
  int		 jf_links;
  int		 jf_flags;
  struct pixfont *jf_font;		/* Need this for SunView routines */
  int		 jf_offset;
  unsigned char	 jf_first,
		 jf_last;
}
jfontinfo;

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

f77_integer gk9sxc_(ifid, itxpr, nchars, ichstr, x, y)
  f77_integer *ifid,		/* Font identifier (Unused) */
	      *itxpr,		/* Text precision (In) */
	      *nchars,		/* Number of chracters in string (In) */
	      ichstr[];		/* String as an array of ASCII integers (In) */
  f77_real    x[],		/* Centre positions (X) / string origin (In) */
	      y[];		/* Centre positions (Y) / string origin (In) */
{
  extern char
    *malloc();			/* For getting memory for C copy of string */

  extern int
    pf_ttext();			/* Pixrect transparent text routine */

  struct pr_prpos
    where;			/* Output pixrect/coordinate pair */

  static char
    ch[2] = {'\0','\0'};	/* For passing single characters to SunView */

  char
    *str;			/* C copy of string */

  struct pixfont
    *pf;			/* The current raster font */

  int
    (*print)(),			/* Text routine (pf_ttext) */
    rasterop,			/* Rasterop for display */
    ws = 			/* Local copy of workstation index */
      gkywca_.kwkix - 1,
    y_max =			/* Display space size in Y */
      gkywdt_.kdsry[ws] - 1,
    i;				/* Loop index variable */

  f77_integer
    b_left,			/* Changed display area left bound */
    b_right,			/* Changed display area right bound */
    b_top,			/* Changed display area top bound */
    b_bottom;			/* Changed display area bottom bound */

  f77_real
    x_offset =			/* Offset of left edge from character centre */
      (gkywkd_.qwkdat[ws][ICHWD] / 2.0),
    y_offset =			/* From character centre to SunView baseline */
      ((gkywkd_.qwkdat[ws][ICHHT] / 2.0) + 1.0) + gkywkd_.qwkdat[ws][ICHGS];

  /*
   * Allocate space for C copy of string and convert it from integer to
   * character representation.  Set error 300 and return if we can't get
   * memory.
   */

  if((str = malloc((unsigned)*nchars+1)) == (char *)0)
    gkyerr_.kerror = 300;
  else
  {
    /* Do the conversion */

    for(i = 0; i < *nchars; i++)
      str[i] = (char)ichstr[i];
    str[*nchars] = '\0';

    /*
     * Get the ww bitmaps pixrect and pixfont for use by SunView.
     */

    where.pr = (struct pixrect *)unportask((void *)ddbm,ASKBMMEMORY);
    pf = ((jfontinfo *)(ddfont->f_junk))->jf_font;

    /*
     * Select output routine and rasterop.  Use pf_ttext.
     */

    print = pf_ttext;
    rasterop = PIX_SRC  | PIX_COLOR(dd->d_fore);

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

      for(i = 0; i < *nchars; i++)
      {
	/*
	 * Calculate SunView output coordinates (left edge and baseline)
	 * and update modified area.
	 */

	if((where.pos.x = (int)(x[i] - x_offset)) < b_left)
	  b_left = where.pos.x;
	if(where.pos.x > b_right)
	  b_right = where.pos.x;
	if((where.pos.y = y_max - (int)(y[i] - y_offset)) < b_top)
	  b_top = where.pos.y;
	if(where.pos.y > b_bottom)
	  b_bottom = where.pos.y;

	/* Output the character */

	ch[0] = str[i];
	print(where, rasterop, pf, ch);

      }

      /* Modify right bound of changed area for character width */

      b_right += (int)gkywkd_.qwkdat[ws][ICHWD] - 1;
    }
    else				/* String precision */
    {
      /* Calculate SunView output coordinates of the first character */

      where.pos.x = (int)(x[0] - x_offset);
      where.pos.y = y_max - (int)(y[0] - y_offset);

      /*
       * Calculate left and right bounds of changed area and set both top and
       * bottom bounds to the Y output coordinate.
       */

      b_left = where.pos.x;
      b_right = (int)(*nchars * gkywkd_.qwkdat[ws][ICHWD]) +
			 where.pos.x - 1;
      b_top = b_bottom = where.pos.y;

      /* Render the string using pw_ttext or pw_text (see above) */

      print(where, rasterop, pf, str);
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

  return((f77_integer)0);
}
