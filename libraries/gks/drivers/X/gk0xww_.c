/*
 * RAL GKS SYSTEM
 *
 * gk0xww_: Creates, selects and destroys ww windows for the X workstation
 *	    driver.
 *
 * Type of Routine:  Part of WORKSTATION DRIVER
 * Author:           TAW
 *
 * Copyright (C) SERC 1988
 *
 * Maintenance Log:
 *
 *   9/8/88	TAW  Copied from Sun ../sun/gk9sww.c and modified.
*/

#include <stdio.h>				/* For options file access */
#include <strings.h>				/* For index() */
#include <sys/file.h>				/* open() & access() flags */
#include <sys/ioctl.h>				/* For obtaining ... */
#include <wwinfo.h>				/* For ww */
#include "../../system/include/f77_type.h"	/* For FORTRAN type matching */
#include "../../system/include/gkerr.h"		/* For error reporting */
#include "../../system/include/gkdt.h"		/* Needed by the following */
#include "../../system/include/gkwca.h"		/* WS comms area */
#include "../../system/include/gkwdt.h"		/* WDT area */
#include "../../system/include/gkwkd.h"		/* WS derived data area */
#include "../../system/include/gkwsl.h"		/* WS state list area */
#include "gk0xwd.h"				/* For function codes */
#include  <X11/Xlib.h>				/* for screen info */

/*
 * Errors:
 *
 *     26  Specified workstation cannot be opened
 *  -1016  Internal error detected within GKS
 *  -2006  Value of internal enumerated type is invalid
 *
 * Comments:
 *
 *  Depending on the function code supplied,  gk0xww_() will:
 *
 *  CREATE a ww window,  obtaining window details from a window options
 *    file named by the environment variable GCONnn,  where nn is the user
 *    supplied connection identifier.  If no options file is supplied,
 *    a window containing a display surface of the size specified in the
 *    WDT entry for the workstation type is created,  centred on screen.
 *
 *    The window pointer created is held in an internal table and an index
 *    into this table is returned via 'key'.   The ww default pointers are
 *    reset to reference the window just created and the ww background and
 *    foreground colours are set to 0 and 1 respectively.
 *
 *    When the size of the window is calculated from the display surface size,
 *    the following correction factors are used:
 *
 *      X: +2*borderwidth for right frame
 *      Y: +2*borderwidth for bottom frame
 *
 *  SELECT a ww window.  The key supplied is used to retrieve the window for
 *    the workstation.  The ww default pointers ddwin and ddbm are updated
 *    using this pointer.
 *
 *  UPDATE a ww window.  The key supplied is used to select the window to
 *    update.  The area of the window to be updated is held in the workstation
 *    workspace area as a set of left, right, top and bottom boundaries.
 *    The ww internal routine 'notescreen' is used to perform the update.
 *
 *  DELETE a ww window.  The key is used to index the window to be deleted.
 */

f77_integer gk0xww_(function, key)
  f77_integer *function;	/* Function code (Input) */
  f77_integer *key;		/* Index for window table (Input/Output) */
{
  static window
    *windows[KWK] =		/* To hold return of gk0xwwxget() for each WS */
    {
      (window *)0
    };

  static char			/* Pattern for workstation icon */
    ws_icon[] =
    {
#include "ws_icon.h"
    };

  static char			/* Default cursor */
    gks_cursor[] =
    {
      WWXOR, 0, 0, 0, 0,
      0, 16, 0, 16,
      0377, 0200, 0377,    0, 0376,    0, 0374,    0,
      0376,    0, 0377,    0, 0357, 0200, 0307, 0300,
      0203, 0340,   01, 0360,    0, 0370,    0, 0174,
	 0,  076,    0,  037,    0,  016,    0,   04
    }; /* ww style */

  static short
    opened_ws = 0;		/* Open workstation counter */

  void
    gk0xnotescreen();		/* To update windows (ww internal routine) */

  cursor
    *gk0xcudecode();		/* To translate gks_cursor for custack() */

  box
    frame;			/* Size of window to be opened/update area */

  FILE
    *fopen(),			/* To open the description file */
    *option_stream;		/* For return value of fopen() */

  int
    open(),			/* For opening frame buffer to get details */
    access(),			/* For checking for colour device */
    getpgrp(),			/* For getting process group of application */
    setpgrp(),			/* For setting process group of application */
    sscanf(),			/* For interpreting the options file */
    wsid = gkywca_.kwkix - 1,	/* Modified WS index,  for C use */
    fd,				/* File descriptor returned by open() */
    pgrp,			/* Process group of application */
    colour_fb,	 		/* Colour frame buffer flag */
    x_origin = -1,		/* Optional user supplied origin values ... */
    y_origin = -1,		/* ... -1 forces using default. */
    x_size =			/* Optional user supplied bitmap size ... */
      gkywdt_.kdsrx[wsid],
    y_size =			/* ... default from WDT */
      gkywdt_.kdsry[wsid],
    fbwidth,			/* Width of window frame */
    fbheight,			/* Height of window frame */
    wwborderwidth = 2;		/* Border width ww sets all windows with */

  char
    *fgets(),			/* For reading lines from the options file */
    *sprintf(),			/* For creating option_tag from KCID(KWKIX) */
    *getenv(),			/* To obtain value of option_tag,  if any */
    option_tag[7],		/* Option environment variable name */
    *option_file,		/* For the return value from getenv() */
    option_entry[81];		/* For lines from the option file */


  if(*function == ICREAT)
  {
    /* Get details of screen */
    Display *display;
    int screen ;
    int needsclosing;
   if(ddbm==0) {
         display=XOpenDisplay((char *)0);
         needsclosing=TRUE;
       }
   else {
         display=(Display *)gk0xunportask(ddbm,ASKDISPLAY);
         needsclosing = FALSE;
         }
      screen=DefaultScreen(display);

      fbwidth = DisplayWidth(display, screen);
      fbheight = DisplayHeight(display, screen);

      /*
       * Check that the workstation can be supported if it's a colour type.
       * Monochrome workstations can always be opened.
       */

      if(gkywdt_.kpci[wsid] > 2)
	colour_fb = ( 1<<DisplayPlanes(display,screen)> 2);

      if(needsclosing) XCloseDisplay(display);

      if(gkywdt_.kpci[wsid] == 2 || (gkywdt_.kpci[wsid] > 2 && colour_fb))
      {
	/*
	 * Get window options from the environment,  unless the default
	 * FORTRAN streams are being used (this forces defaults).
	 */

	if(gkywsl_.kcid[wsid] != 5)
	{
	  (void)sprintf(option_tag, "GCON%02ld", gkywsl_.kcid[wsid]);
	  if(((option_file = getenv(option_tag)) != NULL)
	  && ((option_stream = fopen(option_file, "r")) != NULL))
	  {
	    while(fgets(option_entry, sizeof(option_entry), option_stream) != NULL)
	    {
	      (void)sscanf(option_entry, " position %d %d", &x_origin, &y_origin);
	      (void)sscanf(option_entry, " size %d %d", &x_size, &y_size);
	    }
	    (void)fclose(option_stream);
	  }
	  else
	  {

	    /* Must have a workstation description file! */

	    gkyerr_.kerror = 26;
	    return((f77_integer)0);
	  }
	}

	/*
	 * Calculate the total height and width of the window,  and construct
	 * a box for the window located at ww screen origin.
	 */

	frame.b_top = 0;
	frame.b_bottom = y_size + 2*wwborderwidth;
	frame.b_left = 0;
	frame.b_right = x_size + 2*wwborderwidth;

	/*
	 * Now shift the box to the required origin.  If the options have
	 * specified a negative origin or not been employed,  the default will
	 * be used.
	 */

	if (x_origin < 0 || y_origin < 0)
	{
	  /* Centre the window on the screen */

	  frame.b_top = fbheight / 2 - frame.b_bottom / 2;
	  frame.b_left = fbwidth / 2 - frame.b_right / 2;

	  /* Apply an offset to make the window partially visible */

	  frame.b_top += opened_ws * 10;
	  frame.b_left += opened_ws * 10;
	}
	else
	{
	  /* Use supplied origin */

	  frame.b_top = y_origin;
	  frame.b_left = x_origin;
	}

	/* Correct the right and bottom bounds of the box for the shift */

	frame.b_right += frame.b_left;
	frame.b_bottom += frame.b_top;

	/* Check that the window will fit onto the screen */

	if(frame.b_right < fbwidth && frame.b_bottom < fbheight)
	{
	  /*
	   * Open the window and reset defaults as needed.  On colour Suns,
	   * ww won't open colour windows if a monochrome window has been
	   * opened first because dd->d_colours will have been set to 2 so
	   * ww will happily return a monochrome window when asked for a
	   * colour one.  Thus the value of dd->d_colours is checked first
	   * and reset as appropriate.  dd is initialised by the first call
	   * to gk0xwwxget(),  so this is only done once dd is valid.
	   */

	  if(dd != (wwstate *)0)
	    if(gkywdt_.kpci[wsid] > 2)
	      dd->d_colours = 256;
	    else
	      dd->d_colours = 2;

	  pgrp = getpgrp(0);
	  windows[wsid] = gk0xwwxget(frame,gkywdt_.kpci[wsid],"GKS",0);
	  if(windows[wsid] != (window *)0)
	  {
	    if(setpgrp(0, pgrp) != -1)
	    {
	      /*
	       * Update the display surface size and workstation viewport in
	       * case a "size" option has been used.
	       */

	      gkywdt_.kdsrx[wsid] = windows[wsid]->w_bm->bm_box.b_right + 1;
	      gkywdt_.kdsry[wsid] = windows[wsid]->w_bm->bm_box.b_bottom + 1;
	      gkywdt_.qdsdx[wsid] = (f77_real)gkywdt_.kdsrx[wsid];
	      gkywdt_.qdsdy[wsid] = (f77_real)gkywdt_.kdsry[wsid];
	      gkywsl_.qrwvxr[wsid] = gkywdt_.qdsdx[wsid] - 1.0;
	      gkywsl_.qrwvyt[wsid] = gkywdt_.qdsdy[wsid] - 1.0;
	      gkywsl_.qcwvxr[wsid] = gkywdt_.qdsdx[wsid] - 1.0;
	      gkywsl_.qcwvyt[wsid] = gkywdt_.qdsdy[wsid] - 1.0;

	      /*
	       * Set up ww default pointers and workstation icon and ensure
	       * that foreground and background colours have reasonable values
	       */

	      ddwin = windows[wsid];
	      ddbm = ddwin->w_icon;
	      (void)gk0xbmcopy(gk0xbmdecode(ws_icon, ENWWSTYLE), ddbm->bm_box, BMFROM);
	      ddbm = ddwin->w_bm;
	      dd->d_back = 0;
	      dd->d_fore = 1;

	      /* Set up cursor,  changing rasterop to OR for colour displays */

	      if(gkywdt_.kpci[wsid] > 2)
		gks_cursor[0] = WWOR;
	      gk0xcustack(gk0xcudecode(gks_cursor, ENWWSTYLE), WWSET);

	      /*
	       * Return the subscript into windows[] for later use as a key
	       * and increment the number of workstations opened.
	       */

	      *key = wsid;
	      opened_ws++;
	    }
	    else

	      /* Couldn't reset process group */

	      gkyerr_.kerror = -1016;
	  }
	  else

	    /* ww failed to open the window */

	    gkyerr_.kerror = 26;
	}
	else

	  /* Window size exceeds screen size */

	  gkyerr_.kerror = 26;
      }
      else

	/* Sun doesn't support enough colours for workstation */

	gkyerr_.kerror = 26;
    }
  else if(*function == ISELCT)
  {
    /* Make sure bitmaps will be of the correct depth */

    if(gkywdt_.kpci[wsid] > 2)
      dd->d_colours = 256;
    else
      dd->d_colours = 2;

    /* Update default pointers */

    ddwin = windows[*key];
    ddbm = ddwin->w_bm;
  }
  else if(*function == IDELET)
  {
    /* Free window and collapse ww default pointers */

    gk0xwwfree(windows[*key]);
    ddwin = (window *)0;
    ddbm = (bitmap *)0;
  }
  else if(*function == IUPDAT)
  {
    /* Get area to be updated from workstation workspace area */

    frame.b_left = (int)gkywkd_.kwkdat[wsid][ILEFT];
    frame.b_right = (int)gkywkd_.kwkdat[wsid][IRIGHT];
    frame.b_top = (int)gkywkd_.kwkdat[wsid][ITOP];
    frame.b_bottom = (int)gkywkd_.kwkdat[wsid][IBOTT];

    /* Update the required window */

    gk0xnotescreen(windows[*key]->w_bm, frame);

    /*
     * Reset the update area to an impossible rectangle so subsequent
     * output primitives reset it correctly
     */

    gkywkd_.kwkdat[wsid][ILEFT] = gkywdt_.kdsrx[wsid] - 1;
    gkywkd_.kwkdat[wsid][IRIGHT] = 0;
    gkywkd_.kwkdat[wsid][ITOP] = gkywdt_.kdsry[wsid] - 1;
    gkywkd_.kwkdat[wsid][IBOTT] = 0;
  }
  else
  {
    /* Bad function code */

    gkyerr_.kerror = -2006;
  }

  return((f77_integer)0);
}
