/*
 * RAL GKS SYSTEM
 *
 * gk0xsf_: Selects an appropriate raster font for STRING and CHAR precision
 *          text,  given the character height.
 *
 * Type of Function:  Part of WORKSTATION DRIVER
 * Author:            TAW
 *
 * Copyright (C) SERC 1988
 *
 * Maintenance Log:
 *
 *  20/09/88	TAW	Dummy routine, need implementing in Xlib.
 *  30/11/88    TAW     Implemented in Xlib and ww.
 */

#include <wwinfo.h>				/* For ww information. */
#include "../../system/include/f77_type.h"	/* For f77 type matching. */
#include "../../system/include/gkerr.h"		/* For GKS error reporting. */
#include "../../system/include/gkdt.h"		/* For KWK,  needed by ... */
#include "../../system/include/gkwca.h"		/* For WS comms. area. */
#include "../../system/include/gkwkd.h"		/* For WS derived data. */
#include "gk0xwd.h"				/* For WS workspace offsets. */

/*
 * Constants:
 *
 *  NFONT	The number of sizes in a given font style.
 *  FONT_TABLE	Geometry description table for this font style (see COMMENTS).
 */

#include "fonts.h"		/* For NFONT and FONT_TABLE */

/*
 * Types:
 *
 *  Font_info:  Holds raster font geometry information for use by GKS.
 */

typedef struct
{
  char *name;			/* Name of font description file. */
  short height,			/* Height of character body in DC. */
	width,			/* Width of font in DC. */
	cap_to_top,		/* Distance from cap to top lines in DC. */
	base_to_bottom,		/* Distance from base to bottom lines in DC. */
	gks_base_offset;	/* Distance from X base to GKS base */
}
gk0xFont_info ;

/*
 * Errors:
 *
 *  -1016  Internal error detected within GKS
 *
 * Comments:
 *
 *  The constant ICHSIZ above is dependant on the PARAMETER of the same name
 *  in GK0XWD(),  and should have a value one less than the PARAMETER.
 *
 *  The font geometry information is copied into QWKDAT,  and the index of the
 *  font in 'fonts' is stored in KWKDAT.
 *
 *  If the font style used is changed,  you must update the WDT files for the
 *  X workstation types.  The values that need changing are kchh, qmnchh and
 *  qmxchh in the WDT section (the number of fonts as given by NFONT and the
 *  minimum and maximum height of the font family as given in FONT_TABLE).
 *
 *  The quantities held in a Font_info structure relate to a X glyph
 *  as follows (a 'M' is used as an example):
 *
 *			. . . . . . .> cap_to_top (1)
 *		       <X . . . . X .
 *		       <X . . . . X .
 *		       <X X . . X X .
 *		       <X X . . X X .
 *	    height (9) <X . X X . X .
 *		       <X . X X . X .
 *		       <X . . . . X .
 *		       <X . . . . X .
 *		       <X . . . . X .<---- GKS baseline
 *			. . . . . . .><---- X baseline
 *			. . . . . . .> base_to_bottom (3)
 *			. . . . . . .>
 *			V V V V V V V
 *                        width (7)
 *
 *  The GKS baseline is the lowest pixel (excluding descenders) but the X
 *  baseline is the pixel below that.
 *
 *  Note that base_to_bottom is the number of pixels from below the GKS
 *  baseline to the lowest set pixel in a raster, NOT the bottom of the raster.
 */

f77_integer gk0xsf_(height)
  f77_real *height;	/* Required character height in DC (In). */
{
  static gk0xFont_info
    fonts[NFONT] = 	/* Descriptions of fonts currently used */
    {
     FONT_TABLE
    };

  fontinfo
    *gk0xftload();		/* To load the font. */

   int
     new_font,		/* The font to be loaded (0 to NFONT - 1). */
     wsid =		/* Local copy of WS index, decremented for C use */
       gkywca_.kwkix - 1,
     i;			/* Loop index. */

    double
      fabs(),		/* To get ablsolute value of height difference. */
      difference, 	/* Difference between requested height and height of
			   font geing examined for selection .*/
      min_difference;	/* Minimum value of the above. */

  /* First find the font with the height nearest to that selected.  Start
   * by initialising index of font in 'fonts' and minimum difference.
   */

   new_font = 0;
   min_difference = fabs((double)fonts[0].height - (double)*height);

  /* Then scan through 'fonts' looking for a closer match. */

   for (i = 1; i < NFONT; i++)
   {
     difference = fabs((double)fonts[i].height - (double)*height);
     if (difference < min_difference)
     {
       new_font = i;
       min_difference = difference;
     }
   }

  /* If the font found isn't the same as that currently selected,  load it. */

  if (new_font != (int)gkywkd_.kwkdat[wsid][ICHSIZ])
  {
    ddfont = gk0xftload(fonts[new_font].name);

    /* If there was an error,  report it.  Otherwise set up WS data. */
    if (ddfont == (fontinfo *)0)
      gkyerr_.kerror = -1016;
    else
    {
      gkywkd_.kwkdat[wsid][ICHSIZ] = new_font;
      gkywkd_.qwkdat[wsid][ICHHT] = (f77_real)fonts[new_font].height;
      gkywkd_.qwkdat[wsid][ICHWD] = (f77_real)fonts[new_font].width;
      gkywkd_.qwkdat[wsid][ICHCT] = (f77_real)fonts[new_font].cap_to_top;
      gkywkd_.qwkdat[wsid][ICHBB] = (f77_real)fonts[new_font].base_to_bottom;
      gkywkd_.qwkdat[wsid][ICHGS] = (f77_real)fonts[new_font].gks_base_offset;
    }
  }

  return((f77_integer)0);
}
