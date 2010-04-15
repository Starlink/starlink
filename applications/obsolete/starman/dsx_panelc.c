/*******************************************************
 DSX_PANELC.C
-
 Contains:-
-
 DSX_P_INIT      Initialise panel window (Fortran interface) (XWindows)
 DSX_P_CLOSE     Close panel window (Fortran interface) (XWindows)
 DSX_P_PTTIT     Put title on panel (Fortran interface) (Xwindows)
 DSX_P_SCOL      Set panel line colour (Fortran interface) (XWindows)
 DSX_P_SCUR      Start the panel cursor (Fortran interface) (XWindows)
 DSX_P_SSCUR     Start the panel cursor (sections) (Fortran interface) (XWindows)
 DSX_P_ERASE     Erase panel display (Fortran interface) (XWindows)
 DSX_P_SBOX      Paint a panel box (sections) (Fortran interface) (XWindows)
 DSX_P_LINE      Put a line at a position (Fortran interface) (Xwindows)
 DSX_P_PUTTXT    Write text to panel (Fortran interface) (XWindows)
 DSX_P_PUTIM     Put an image into panel (Fortran interface) (XWindows)
 DSX_P_CAREA     Put a standard colour rectangle into panel (Fortran interface) (XWindows)
 DSX_P_HBOX      Paint a help panel box (Fortran interface) (XWindows)
 DSX_P_HSTAT     Write panel status in the help panel box (Fortran interface) (XWindows)

 DSX_P_HX_LOAD   Load help panel with start help (Fortran interface) (XWindows)
 DSX_P_HX_HLOAD  Put help panel information (Fortran interface) (XWindows)
-
 DSXC_P_INIT      Initialise panel window (XWindows)
 DSXC_P_BYTECOL   Calc panel byte equivalents of PC_P_ID (XWindows)
 DSXC_P_SCOLOURS  Set up panel colours (XWindows)
 DSXC_P_CICON     Set up panel corner icon (XWindows)
 DSXC_P_ISVIEW    Is panel viewable?
 DSXC_P_EVENT     Handle panel exposure event (XWindows)
 DSXC_P_SETCOL    Set up color display for panel image display (XWindows)
 DSXC_P_CLOSE     Close panel window (XWindows)
 DSXC_P_PTTIT     Put title on panel (Xwindows)
 DSXC_P_ERASE     Erase panel display (XWindows)
 DSXC_P_SSCUR     Start the panel cursor (sections) (XWindows)
 DSXC_P_SBOX      Paint a panel box (sections) (XWindows)
 DSXC_P_PUTIM     Put an image to panel (XWindows)
 DSXC_P_CAREA     Put a standard colour rectangle into panel (XWindows)
 DSXC_P_LINE      Put a line at a position (Xwindows)
 DSXC_P_HBOX      Paint a help panel box (XWindows)
 DSXC_P_PUTTXT    Write text to panel (XWindows)
 DSXC_P_SETTHECOL Set writing colour for panel (XWindows)
 DSXC_P_SCOLNUM   Calc panel colour pixel number from colour number (XWindows)
 DSXC_P_HSTAT     Write panel status in the help panel box (XWindows)
 DSXC_P_CLEAR     Clear panel (Xwindows )
 DSXC_P_PUTCOL    Put a colour into an array (XWindows)

 DSXC_P_HX_LOAD   Load help panel with start help (XWindows)
 DSXC_P_HX_HLOAD  Put help panel information (Fortran interface) (XWindows)
 DSXC_P_HX_CICON  Set up help panel corner icon
 DSXC_P_HX_PUTTXT Write text to help panel (XWindows)
 DSXC_P_HX_PUTIM  Put an image into help panel (XWindows)

 AJ_SCPY          Copy text

*/

/* Include files */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#if defined(vms)
#include "starman_x11_xlib"
#include "starman_x11_xutil"
#include "starman_x11_x"
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/X.h>
#include <malloc.h>
#endif

#include "f77.h"
#include "cnf.h"

#include "dsx_panel.h"
#include "ds_gen.h"
#include "ds_panel.h"
#include "image.h"

/* Definitions */

extern void c_printo ( char* );
extern void c_gcmdlst ( char*, char*, int, int*, int*, int*, int*, int*,
                        int*, int*, int* );
extern void c_tyhelp ( char*, int, char*, int, int, int, int );
extern void c_get1i ( char*, int*, int, int, int );
extern void c_get1b ( char*, Bool*, Bool );
extern void dsxc_waitbut ( Bool, Bool, int*, int*, int* );
extern int imax ( int, int );
extern int imin ( int, int );

/*
DECLARE_CHARACTER(in,2000);
DECLARE_CHARACTER(out,2000);
*/

/* Panel Globals */

extern int PNKXS;
extern int PNKYS;
extern int PNKXO;
extern int PNKYO;

char TEXTAA[200];
/*
  (void) sprintf ( TEXTAA, " F1XXX %d %d %d %d %d ",
                          PDSOPEN, DSNXS, DSNXE, DSNYS, DSNYE );
   (void) c_printo ( TEXTAA );
*/

/*******************************************************************************
 DSX_P_INIT -- Initialise panel window (Fortran interface) (XWindows)

  alan penny             ral          1990 Aug
  pat morris             leeds        1992 Jan
*/

F77_SUBROUTINE(dsx_p_init) ( INTEGER(kx), INTEGER(ky), CHARACTER(title),
                             INTEGER(kdoh), INTEGER(ierr)  TRAIL(title) )

{
      GENPTR_INTEGER(kx)		/* i: Panel X size */
      GENPTR_INTEGER(ky)		/* i: Panel Y size */
      GENPTR_CHARACTER(title)		/* i: Title of image */
      GENPTR_INTEGER(kdoh)		/* i: Use Help Panel? (0=no;1=yes) */
      GENPTR_INTEGER(ierr)		/* o: Error flag (0=ok;1=bad) */
/* C-- */
      char  *name;
      int iierr;
/* Cbegin */

      name = cnf_creim ( title, title_length );

      (void) dsxc_p_init ( *kx, *ky, name, *kdoh, &iierr );

      *ierr = iierr;
      cnf_free ( name );

}



/*************************************************************************
  DSX_P_CLOSE -- Close panel window (Fortran interface) (XWindows)

   alan penny             ral          1990 jan
  pat morris             leeds        1992 Jan
*/

F77_SUBROUTINE(dsx_p_close) ( INTEGER(kdoh), INTEGER(ierr) )

{
   GENPTR_INTEGER(kdoh)		/* i: Use Help Box? (0=no;1=yes) */
   GENPTR_INTEGER(ierr)		/* o: Error flag (0=ok;1=bad) */
/* C-- */
    int iierr;
/* Cbegin */

    (void) dsxc_p_close ( *kdoh, &iierr );

    *ierr = iierr;

}



/**************************************************************************
  DSX_P_PTTIT -- Put title on panel (Fortran interface) (XWindows)

    alan penny                ral              1990-01-31
  pat morris             leeds        1992 Jan
*/

F77_SUBROUTINE(dsx_p_pttit) ( CHARACTER(title) TRAIL(title) )

{
GENPTR_CHARACTER(title)		/* i: Title to put up */
/* C-- */
      char *name;
/* Cbegin */


      name = cnf_creim ( title, title_length );
      (void) dsxc_p_pttit ( name );
      cnf_free (name);

}


/***********************************************************************
  DSX_P_SCOL -- Set panel line colour (Fortran interface) (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

F77_SUBROUTINE(dsx_p_scol) ( REAL(w), INTEGER(kc) )

{
GENPTR_REAL(w)		/* i: Line width */
GENPTR_INTEGER(kc)      /* i: Colour (1=red;2=green;3=blue;4=yellow;
				5=cyan;6=mauve;7=tan;8=pink;
				9=black;10=white) */
/* C-- */
/* Cbegin */


}


/****************************************************************************
  DSX_P_SSCUR -- Start the panel cursor (sections) (Fortran interface) (XWindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 Jan
*/

F77_SUBROUTINE(dsx_p_sscur) ( INTEGER(kn) )

{
GENPTR_INTEGER(kn)		/* i: Button to place cursor on */
/* C-- */
/* Cbegin */



     (void) dsxc_p_sscur ( *kn );

}


/**************************************************************************
 DSX_P_ERASE -- Erase panel display (Fortran interface) (XWindows)

    a j penny                    ral         1990 jan
*/

F77_SUBROUTINE(dsx_p_erase) (void)

/* C-- */
{
/* Cbegin */

      (void) dsxc_p_clear ( 1 );

}



/**********************************************************************
 DSX_P_SBOX -- Paint a panel box (sections) (Fortran interface) (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

F77_SUBROUTINE(dsx_p_sbox) ( INTEGER(kn), CHARACTER(text),
                            INTEGER(kflagu), INTEGER(kflage) TRAIL(text) )
{
   GENPTR_INTEGER(kn)		/* i: Box number (if -1=COMMAND; -2=HELP BUTT) */
   GENPTR_CHARACTER(text)	/* i: Text in box */
   GENPTR_INTEGER(kflagu)	/* i: Down or up box (0/1) */
   GENPTR_INTEGER(kflage)	/* i: Whole or edge of box (0/1) */
/* C-- */
      char *name;
/* Cbegin */

      name = cnf_creim( text, text_length );

      (void) dsxc_p_sbox ( *kn, name, *kflagu, *kflage );

      cnf_free (name);

}



/**********************************************************************
 DSX_P_LINE -- Put a line at a position (Fortran interface) (XWindows)

  alan penny             ral             1990 May
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_p_line) ( REAL(xs), REAL(ys), REAL(xe), REAL(ye),
                            INTEGER(kc) )

{
    GENPTR_REAL(xs)		/* i: Line start X position(image pixels) */
    GENPTR_REAL(ys)		/* i: Line start Y position (image pixels) */
    GENPTR_REAL(xe)		/* i: Line end X position(image pixels) */
    GENPTR_REAL(ye)		/* i: Line end Y position (image pixels) */
    GENPTR_INTEGER(kc)		/* i: Colour (1=red;2=green;3=blue;4=yellow;
				           5=cyan;6=mauve;7=tan;8=pink)
				           9=black;10=white) */
/* C-- */
/* Cbegin */


     (void) dsxc_p_line ( *xs, *ys, *xe, *ye, *kc );


}


/***************************************************************************
 DSX_P_PUTTXT -- Write text to panel (Fortran interface) (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

F77_SUBROUTINE(dsx_p_puttxt) ( CHARACTER(text), INTEGER(kl), INTEGER(kx),
                               INTEGER(ky), INTEGER(kc) TRAIL(text) )

{
     GENPTR_CHARACTER(text)		/* i: Text to write */
     GENPTR_INTEGER(kl)			/* i: No of characters to write */
     GENPTR_INTEGER(kx)			/* i: X position */
     GENPTR_INTEGER(ky)       		/* i: Y position */
     GENPTR_INTEGER(kc)			/* i: Colour code (1=red;2=green;3=blue;
						4=yellow;5=cyan;6=mauve;7=tan;
				   		8=pink;9=black;10=white) */
/* C-- */
      char *name;
/* Cbegin */

      name = cnf_creim( text, text_length );

      (void) dsxc_p_puttxt ( name, *kl, *kx, *ky, *kc );

      cnf_free (name);

}


/**********************************************************************
 DSX_P_PUTIM -- Put an image into panel (Fortran interface) (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/


F77_SUBROUTINE(dsx_p_putim) ( BYTE_ARRAY(data), INTEGER(kxl), INTEGER(kyl),
                              INTEGER(kx), INTEGER(ky) )

{
     GENPTR_BYTE_ARRAY(data)		/* i: Input data */
     GENPTR_INTEGER(kxl)		/* i: X size of data */
     GENPTR_INTEGER(kyl)		/* i: Y size of data */
     GENPTR_INTEGER(kx)       		/* i: X start of data position */
     GENPTR_INTEGER(ky)       		/* i: Y start of data position */
/* C-- */
/* Cbegin */


      (void) dsxc_p_putim ( data, *kxl, *kyl, *kx, *ky );

}

/**********************************************************************
 DSX_P_CAREA -- Put a standard colour rectangle into panel (Fortran interface) (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/


F77_SUBROUTINE(dsx_p_carea) ( INTEGER(kx), INTEGER(ky), INTEGER(kxl),
                              INTEGER(kyl), INTEGER(kc) )

{
     GENPTR_INTEGER(kx)       		/* i: X start of area */
     GENPTR_INTEGER(ky)       		/* i: Y start of area */
     GENPTR_INTEGER(kxl)		/* i: X length of area */
     GENPTR_INTEGER(kyl)		/* i: Y length of area */
     GENPTR_INTEGER(kc)       		/* i: Colour code (0:1:2=medium:light:dark grey) */
/* C-- */
/* Cbegin */


      (void) dsxc_p_carea ( *kx, *ky, *kxl, *kyl, *kc );

}


/**********************************************************************
 DSX_P_HSTAT -- Write panel status in the help panel box (Fortran interface) (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/


F77_SUBROUTINE(dsx_p_hstat) ( INTEGER(kin) )

{
     GENPTR_INTEGER(kin)			/* i: Work/Wait/Help/Command/Pan/Zoom/Not Yet/Ready/One Got/ */
						/*    (0/1/2/3/4/5/6/7/8) */
/* C-- */
/* Cbegin */


      (void) dsxc_p_hstat ( *kin );

}

/**********************************************************************
 DSX_P_HX_LOAD -- Put help panel start information (Fortran interface) (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/


F77_SUBROUTINE(dsx_p_hx_load) ( CHARACTER(title) TRAIL(title) )

{
     GENPTR_CHARACTER(title)		/* i: Title of help */
/* C-- */
      char  *name;
/* Cbegin */


      name = cnf_creim ( title, title_length );

      (void) dsxc_p_hx_load ( name );

      cnf_free ( name );


}

/**********************************************************************
 DSX_P_HX_HLOAD -- Put help panel information (Fortran interface) (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/


F77_SUBROUTINE(dsx_p_hx_hload) ( CHARACTER(topt), CHARACTER(thelp)
                                 TRAIL(topt) TRAIL(thelp)  )
{
     GENPTR_CHARACTER(topt)		/* i: Help Panel title - 68 character description of Option */
     GENPTR_CHARACTER(thelp)		/* i: Help Panel text - six lines of 68 characters*/

/* C-- */
      char  *namea, *nameb;
/* Cbegin */


      namea = cnf_creim ( topt, topt_length );
      nameb = cnf_creim ( thelp, thelp_length );

      (void) dsxc_p_hx_hload ( namea, nameb );

      cnf_free ( namea );
      cnf_free ( nameb );

}


/**********************************************************************
 DSX_P_HBOX -- Paint a help panel box (Fortran interface) (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/


F77_SUBROUTINE(dsx_p_hbox) (void)

{
/* C-- */
/* Cbegin */


      (void) dsxc_p_hbox ();

}


/*******************************************************************************
 DSXC_P_INIT -- Initialise panel window (XWindows)

  alan penny             ral          1990 Aug
  pat morris             leeds        1992 Jan
*/

      dsxc_p_init ( kx, ky, title, kdoh, ierr )

      int    kx;		/* i: Panel X size */
      int    ky;		/* i: Panel Y size */
      char   *title;		/* i: Title of image */
      int    kdoh;		/* i: Use Help Panel? (0=no;1=yes) */
      int    *ierr;		/* o: Error flag (0=ok;1=bad) */
{
/* C-- */
      Bool func = True ;			/* Synchronous behaour */
      unsigned long attr_mask, event_mask;	/* attributes mask */
      XSetWindowAttributes xswda; 		/* window attributes */
      XGCValues       xgcvl;	    		/* gc values */
      XSizeHints      xszhn;	    		/* size hints */
/*
      char *font_name = "-adobe-new century schoolbook-medium-r-normal--*-120-*-*-p*";
      char *font_name = "-adobe-new century schoolbook-medium-r-normal--*-80-*-*-p*";
*/
      char *font_name = "-adobe-new century schoolbook-medium-r-normal--*-100-*-*-p*";
      char *display_name = NULL;
      char buf1[32], buf2[32], buf3[32], buf4[32], buf5[32], texta[50];
      char *argv[8];
      XEvent   event;
      int     k, nxa, nya, kxa, kya, khxa, khya, status, iout, fd[2];
      Bool loop;
/* Cbegin */

      *ierr = 0;

      if ( DSOPEN ) {
         if ( VD_ID == NULL ) {
            (void) c_printo ( "ERROR: Cant open Xwindows window" );
            *ierr = 1;
            return;
         }
      }
      else {
         if ( (VD_ID = XOpenDisplay(display_name)) == NULL ) {
            (void) c_printo ( "ERROR: Cant open Xwindows Display" );
            *ierr = 1;
            return;
         }
         (void) c_get1i ( "NUMBUTT", &iout, 3, 2, 3 );
         NUMXBUTTONS = iout;
         F77_NAMED_COMMON(ds_gen).numxbuttons = NUMXBUTTONS;
      }

      PDSOPEN = True;
      F77_NAMED_COMMON(ds_panelb).pdsopen = F77_TRUE;

      (void) XSynchronize ( VD_ID, func );
      SC_P_ID = DefaultScreen( VD_ID );
      RW_P_ID = RootWindow( VD_ID, SC_P_ID );

      nxa = DisplayWidth ( VD_ID, SC_P_ID) ;
      nya = DisplayHeight( VD_ID, SC_P_ID);

      PNSNX = kx;
      PNSNY = ky;

      F77_NAMED_COMMON(ds_panel).pnsnx = PNSNX;
      F77_NAMED_COMMON(ds_panel).pnsny = PNSNY;

      if ( (PNSNX>(nxa-6)) || (PNSNY>(nya-25)) ) {
         (void) c_printo ( "ERROR: Panel too large for display" );
         *ierr = 1;
         return;
      }

      PNNCOL = PNSNX/91;
      PNNROW = PNSNY/26;
      F77_NAMED_COMMON(ds_panel).pnncol = PNNCOL;
      F77_NAMED_COMMON(ds_panel).pnnrow = PNNROW;

      P_DEPTH = DefaultDepth(VD_ID, SC_P_ID);
      if ( P_DEPTH<8 ) {
         (void) c_printo ( "ERROR: Panel pixel depth must be at least 8" );
         *ierr = 1;
         return;
      }

      P_VISUAL = DefaultVisual( VD_ID, SC_P_ID );
      if ( P_VISUAL->class != PseudoColor ) {
         (void) c_printo ( "ERROR: Cant allocate colours - display type has to be pseudo color" );
         *ierr = 1;
         return;
      }

      if (!DSOPEN) (void) dsxc_opcolmap ();				/* Open colour map */

      attr_mask = CWEventMask | CWBackPixel | CWBorderPixel | CWColormap;
      xswda.event_mask = ExposureMask ;
      xswda.background_pixel = BlackPixel(VD_ID, SC_P_ID);
      xswda.border_pixel = WhitePixel(VD_ID, SC_P_ID);
      xswda.colormap = CM_ID;

      DSWINDX = F77_NAMED_COMMON(ds_gen).dswindx;
      DSWINDY = F77_NAMED_COMMON(ds_gen).dswindy;
      kxa = nxa - PNSNX - DSWINDX;					/* Position */
      kya = DSWINDY;

      WD_P_ID = XCreateWindow ( VD_ID, RW_P_ID, kxa, kya, PNSNX,
                                PNSNY, 0, P_DEPTH, InputOutput,
                                P_VISUAL, attr_mask, &xswda );		/* Open window */

      (void) dsxc_p_cicon (); 						/* Set up panel corner icon */

      (void) dsxc_p_scolours ();					/* Set up panel parts */

      DSWINDX = DSWINDX + PNSNX + 20;
      DSWINDXM = DSWINDX;
      F77_NAMED_COMMON(ds_gen).dswindx = DSWINDX;
      F77_NAMED_COMMON(ds_gen).dswindxm = DSWINDXM;

      xgcvl.foreground = BlackPixel(VD_ID, SC_P_ID);			/* Create graphics context*/
      xgcvl.background = WhitePixel(VD_ID, SC_P_ID);
      GC_P_ID = XCreateGC ( VD_ID, WD_P_ID, (GCForeground | GCBackground),
                            &xgcvl );

      if ( (FT_P_ID = XLoadQueryFont ( VD_ID, font_name ) ) == NULL ) {	/* Load the font for text */
         (void) c_printo (
         "WARNING: Couldn\'t open fonts - Text may be wrong size" );	/* writing */
      }else{
         XSetFont ( VD_ID, GC_P_ID, FT_P_ID->fid );
      }

      XSetForeground ( VD_ID, GC_P_ID, WhitePixel(VD_ID, SC_P_ID) );

      xszhn.x = kxa;							/* Define the size and */
      xszhn.y = kya;							/* name of the WD_P_ID */
      xszhn.width = PNSNX;						/* window */
      xszhn.height = PNSNY;
      xszhn.flags = PPosition | PSize;

      XSetNormalHints ( VD_ID, WD_P_ID, &xszhn );

      XFlush ( VD_ID );

      P_PIXMAP = XCreatePixmap ( VD_ID, RW_P_ID, PNSNX, PNSNY, P_DEPTH );
      (void) dsxc_p_clear ( 1 );

      XSelectInput ( VD_ID, WD_P_ID, StructureNotifyMask | ButtonPressMask
                     | ButtonReleaseMask | PointerMotionMask
                     | SubstructureNotifyMask | ExposureMask );

      XMapWindow ( VD_ID, WD_P_ID );					/* Map the windows */

      XFlush ( VD_ID );

      (void) dsxc_p_pttit ( title );

      event_mask = SubstructureNotifyMask | StructureNotifyMask ;
      XWindowEvent ( VD_ID, WD_P_ID, event_mask, &event );
									/* Wait for window to appear */


      PID = vfork();
      if ( PID != 0 ) {  				/* We are child so do exposure monitor */
         if ( PID == -1 ) (void) c_printo ( "ERROR: Panel Refresh not started" );
      } else {

         display_name = DisplayString ( VD_ID );
         argv[0] = "s_refresh";
         sprintf ( buf1, "%lu", WD_P_ID );
         argv[1] = buf1;
         sprintf ( buf2, "%lu", P_PIXMAP );
         argv[2] = buf2;
         argv[3] = display_name;
         argv[4] = '\0';
         execvp ( "s_refresh", argv );
         _exit ( errno );
      }

      XFlush ( VD_ID );

      if ( kdoh==1 ) {
         PHNX = 470;							/* Open Help window */
         PHNY = 120;
         khxa = kxa - (PHNX-PNSNX);
         khya = kya + PNSNY + 100;
         xswda.background_pixel = WhitePixel(VD_ID, SC_P_ID);
         xswda.border_pixel = BlackPixel(VD_ID, SC_P_ID);
         WD_PH_ID = XCreateWindow ( VD_ID, RW_P_ID, khxa, khya, PHNX,
                                    PHNY, 0, P_DEPTH, InputOutput,
                                    P_VISUAL, attr_mask, &xswda );

         (void) dsxc_p_hx_cicon (); 					/* Set up Help panel corner icon */

         GC_PH_ID = XCreateGC ( VD_ID, WD_PH_ID,
                                (GCForeground | GCBackground), &xgcvl );

									/* Load the font for text */
         if ( (FT_P_ID = XLoadQueryFont ( VD_ID, font_name ) )
              != NULL ) {
            XSetFont ( VD_ID, GC_PH_ID, FT_P_ID->fid );
         }

         XSetForeground (VD_ID,GC_PH_ID, WhitePixel(VD_ID, SC_P_ID) );

         xszhn.x = khxa;						/* Define the size and */
         xszhn.y = khya;						/* name of the WD_PH_ID */
         xszhn.width = PHNX;						/* window */
         xszhn.height = PHNY;
         XSetNormalHints ( VD_ID, WD_PH_ID, &xszhn );

         XFlush ( VD_ID );

         PH_PIXMAP = XCreatePixmap (VD_ID,RW_P_ID,PHNX,PHNY,P_DEPTH);
         (void) dsxc_p_clear ( 2 );

         XSelectInput ( VD_ID, WD_PH_ID,
                        StructureNotifyMask | ButtonPressMask
                        | ButtonReleaseMask | PointerMotionMask
                        | SubstructureNotifyMask | ExposureMask );

         XMapWindow ( VD_ID, WD_PH_ID );				/* Map the windows */

         XFlush ( VD_ID );

         event_mask = SubstructureNotifyMask | StructureNotifyMask ;
         XWindowEvent ( VD_ID, WD_PH_ID, event_mask, &event );
 									/* Wait for window to appear */
         PID = vfork();
         if ( PID != 0 ) {  						/* We are child so do exposure monitor */
            if ( PID == -1 ) (void) c_printo (
                             "ERROR: Help Panel Refresh not started" );
         } else {

            display_name = DisplayString ( VD_ID );
            argv[0] = "s_refresh";
            sprintf ( buf1, "%lu", WD_PH_ID );
            argv[1] = buf1;
            sprintf ( buf2, "%lu", PH_PIXMAP );
            argv[2] = buf2;
            argv[3] = display_name;
            argv[4] = '\0';
            execvp ( "s_refresh", argv );
            _exit ( errno );
         }

         (void) dsxc_p_hx_load ( title );
         XFlush ( VD_ID );

      }

}


/************************************************************************
   DSXC_P_SCOLOURS -- Set up panel colours

  alan penny             ral                    1990 Aug
  pat morris             leeds        1992 Jan
*/

dsxc_p_scolours ( )

{
/* C-- */
      int i, j, k, ka;
      char gcol[5];

      static int knu[59] = { 1, 1, 1, 3, 1, 1, 3, 1, 1, 3, 1, 2, 3,
     			     1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3,
                             1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3,
        1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3,
        1, 3, 3, 1, 3, 3, 1, 3, 3, 3 };

      static int knd[59] = { 3,
        3, 3, 1, 3, 3, 1,
        3, 3, 1, 3, 2, 1,
        3, 2, 1, 3, 2, 1,
        3, 2, 1, 3, 2, 1,
        3, 2, 1, 3, 2, 1,
        3, 2, 1, 3, 2, 1,
        3, 2, 1, 3, 2, 1,
        3, 2, 1, 3, 2, 1,
        3, 1, 1, 3, 1, 1,
        3, 1, 1,
        1 };

      static int knum[59] = { 86,
        1, 84, 1,   2, 82, 2,
        3, 80, 3,   4, 78, 4,
        5, 76, 5,   5, 76, 5,
        5, 76, 5,   5, 76, 5,
        5, 76, 5,   5, 76, 5,
        5, 76, 5,   5, 76, 5,
        5, 76, 5,   5, 76, 5,
        5, 76, 5,   4, 78, 4,
        3, 80, 3,   2, 82, 2,
        1, 84, 1,
        86 };

       static int knh[134] = { 1,
         1, 1, 3, 1, 1, 3,
         1, 1, 3, 1, 1, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 2, 3,
         1, 2, 3, 1, 3, 3,
         1, 3, 3, 1, 3, 3,
         3 };

      static int knumh[134] = { 86,
        1, 84, 1, 2, 82, 2,
        3, 80, 3, 4, 78, 4,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        5, 76, 5, 5, 76, 5,
        4, 78, 4, 3, 80, 3,
        2, 82, 2, 1, 84, 1,
        86 };

       static int knr[40] = { 4, 4, 4, 2, 4, 4, 2, 4,
        4, 2, 4, 4, 2, 4, 4, 2, 4,
        4, 2, 4, 4, 2, 4,
        4, 2, 4, 4, 2, 4, 4, 2, 4,
        4, 2, 4, 4, 2, 4, 4, 4 };

       static int kng[40] = { 5, 5, 5, 2, 5, 5, 2, 5,
        5, 2, 5, 5, 2, 5, 5, 2, 5,
        5, 2, 5, 5, 2, 5,
        5, 2, 5, 5, 2, 5, 5, 2, 5,
        5, 2, 5, 5, 2, 5, 5, 5 };

      static int knumr[40] = { 67, 67, 3, 61, 3, 3, 61, 3,
         3, 61, 3, 3, 61, 3, 3, 61, 3,
         3, 61, 3, 3, 61, 3,
         3, 61, 3, 3, 61, 3, 3, 61, 3,
         3, 61, 3, 3, 61, 3, 67, 67 };
/* Cbegin */


      gcol[0] = BY_PC_ID[13];
      gcol[1] = BY_PC_ID[12];
      gcol[2] = BY_PC_ID[11];
      gcol[3] = BY_PC_ID[2];
      gcol[4] = BY_PC_ID[3];

      GL = gcol[0];
      GM = gcol[1];
      GD = gcol[2];

      for ( j=0; j<500*500; j++ ) {
         XPM[j] = GM;
      }

      for ( j=0; j<2000 ; j++ ) {
         XPD[j] = GD;
         XPL[j] = GL;
      }

      k = 0;
      for ( j = 0; j<59; j++ ) {
         ka = knu[j]-1;
         for ( i=0; i<knum[j]; i++ )
             XDU[k+i] = gcol[ka];
         k = k + knum[j];
      }

      k = 0;
      for ( j=0; j<59; j++ ) {
         ka = knd[j]-1;
         for ( i=0; i<knum[j]; i++ )
             XDD[k+i] = gcol[ka];
         k = k + knum[j];
      }

      k = 0;
      for ( j=0; j<134; j++ ) {
         ka = knh[j]-1;
         for ( i=0; i<knumh[j]; i++ )
             XDH[k+i] = gcol[ka];
         k = k + knumh[j];
      }

      k = 0;
      for ( j=0; j<40; j++ ) {
         ka = knr[j]-1;
         for ( i=0; i<knumr[j]; i++ )
             IR[k+i] = gcol[ka];
         k = k + knumr[j];
      }

      k = 0;
      for ( j=0; j<40; j++ ) {
         ka = kng[j]-1;
         for ( i=0; i<knumr[j]; i++ )
             IG[k+i] = gcol[ka];
         k = k + knumr[j];
      }


}

/*********************************************************************
 DSXC_P_CICON -- Set up panel corner icon

  alan penny             ral                    1990 Aug
  pat morris             leeds        1992 Jan
*/

dsxc_p_cicon ( )

{
/* C-- */
      XWMHints        xwmhn;	    	/* Window manager hints */
      XWMHints        xwmhints;

      Pixmap icon_pixmap;
      Atom   wmatom;
      static unsigned char icon_bits[512] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x3c, 0x1f, 0x63, 0x23, 0x62, 0x88, 0x00, 0x00,
        0x02, 0x04, 0x43, 0x64, 0x63, 0x88, 0x00, 0x00,
        0x02, 0x84, 0x44, 0x64, 0x93, 0x98, 0x00, 0x00,
        0x02, 0x84, 0x64, 0xa3, 0x92, 0xa8, 0x00, 0x00,
        0x1c, 0x84, 0x44, 0x21, 0x92, 0xa8, 0x00, 0x00,
        0x20, 0x84, 0x47, 0x22, 0xf2, 0xc8, 0x00, 0x00,

        0x20, 0x44, 0x48, 0x22, 0x0a, 0x89, 0x00, 0x00,
        0x1e, 0x44, 0x48, 0x24, 0x0a, 0x89, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

        0x00, 0x1f, 0x8c, 0x90, 0x27, 0x00, 0x00, 0x00,
        0x00, 0x21, 0x8c, 0x91, 0x20, 0x00, 0x00, 0x00,
        0x00, 0x21, 0x92, 0x92, 0x20, 0x00, 0x00, 0x00,
        0x00, 0x21, 0x92, 0x92, 0x20, 0x00, 0x00, 0x00,
        0x00, 0x1f, 0x92, 0x94, 0x23, 0x00, 0x00, 0x00,
        0x00, 0x01, 0x9e, 0x94, 0x20, 0x00, 0x00, 0x00,
        0x00, 0x01, 0xa1, 0x98, 0x20, 0x00, 0x00, 0x00,
        0x00, 0x01, 0xa1, 0x90, 0xe7, 0x01, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xf0, 0xff, 0xff, 0xff, 0xff, 0x0f, 0x00, 0x00,
        0x00, 0xff, 0xff, 0xff, 0x0f, 0x00, 0x00, 0x00,
        0x00, 0x00, 0xff, 0x0f, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 } ;
/* Cbegin */


      icon_pixmap = XCreateBitmapFromData ( VD_ID, WD_P_ID, icon_bits, 64, 64 );

      xwmhn.flags  = IconPixmapHint | IconMaskHint;
      xwmhn.icon_pixmap = icon_pixmap;
      xwmhn.icon_mask   = icon_pixmap;
      XSetWMHints ( VD_ID, WD_P_ID, &xwmhn );

      wmatom = XInternAtom ( VD_ID, "XA_WM_HINTS", 0 );

      if ( wmatom != None ) {
         xwmhints.flags = IconPixmapHint;
         xwmhints.icon_pixmap = icon_pixmap;

         XChangeProperty ( VD_ID, WD_P_ID, wmatom, wmatom, 32,
                                 PropModeReplace, (char *) &xwmhints, 9 );
      }

      XFlush ( VD_ID );


}

/*********************************************************************
 DSXC_P_ISVIEW -- Is panel viewable?

  alan penny             ral                    1990 Aug
  pat morris             leeds        1992 Jan
*/

dsxc_p_isview ( kis )

       int   *kis;	/* o: Is Viewable (0=yes;1=no) */

{
/* C-- */
      XWindowAttributes  xwattr;	    	/* Window Attributes */

/* Cbegin */


      *kis = 1;

      XGetWindowAttributes ( VD_ID, WD_P_ID, & xwattr );
      if ( xwattr.map_state == IsViewable ) *kis = 0 ;

      XFlush ( VD_ID );


}

/*********************************************************************
 DSXC_P_HX_CICON -- Set up help panel corner icon

  alan penny             ral                    1990 Aug
  pat morris             leeds        1992 Jan
*/

dsxc_p_hx_cicon ( )

{
/* C-- */
      XWMHints        xwmhn;	    	/* Window manager hints */
      XWMHints        xwmhints;

      Pixmap icon_pixmap;
      Atom   wmatom;
      static unsigned char icon_bits[512] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x3c, 0x1f, 0x63, 0x23, 0x62, 0x88, 0x00, 0x00,
        0x02, 0x04, 0x43, 0x64, 0x63, 0x88, 0x00, 0x00,
        0x02, 0x84, 0x44, 0x64, 0x93, 0x98, 0x00, 0x00,
        0x02, 0x84, 0x64, 0xa3, 0x92, 0xa8, 0x00, 0x00,
        0x1c, 0x84, 0x44, 0x21, 0x92, 0xa8, 0x00, 0x00,
        0x20, 0x84, 0x47, 0x22, 0xf2, 0xc8, 0x00, 0x00,

        0x20, 0x44, 0x48, 0x22, 0x0a, 0x89, 0x00, 0x00,
        0x1e, 0x44, 0x48, 0x24, 0x0a, 0x89, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

        0x00, 0x40, 0xe4, 0x09, 0x3e, 0x00, 0x00, 0x00,
        0x00, 0x40, 0x24, 0x08, 0x42, 0x00, 0x00, 0x00,
        0x00, 0x40, 0x24, 0x08, 0x42, 0x00, 0x00, 0x00,
        0x00, 0x40, 0x24, 0x08, 0x42, 0x00, 0x00, 0x00,
        0x00, 0xc0, 0xe7, 0x08, 0x3e, 0x00, 0x00, 0x00,
        0x00, 0x40, 0x24, 0x08, 0x02, 0x00, 0x00, 0x00,
        0x00, 0x40, 0x24, 0x08, 0x02, 0x00, 0x00, 0x00,
        0x00, 0x40, 0xe4, 0x78, 0x02, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0xf0, 0xff, 0xff, 0xff, 0xff, 0x0f, 0x00, 0x00,
        0x00, 0xff, 0xff, 0xff, 0x0f, 0x00, 0x00, 0x00,
        0x00, 0x00, 0xff, 0x0f, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 } ;
/* Cbegin */


      icon_pixmap = XCreateBitmapFromData ( VD_ID, WD_PH_ID, icon_bits, 64, 64 );

      xwmhn.flags  = IconPixmapHint | IconMaskHint;
      xwmhn.icon_pixmap = icon_pixmap;
      xwmhn.icon_mask   = icon_pixmap;
      XSetWMHints ( VD_ID, WD_PH_ID, &xwmhn );

      wmatom = XInternAtom ( VD_ID, "XA_WM_HINTS", 0 );

      if ( wmatom != None ) {
         xwmhints.flags = IconPixmapHint;
         xwmhints.icon_pixmap = icon_pixmap;

         XChangeProperty ( VD_ID, WD_PH_ID, wmatom, wmatom, 32,
                                 PropModeReplace, (char *) &xwmhints, 9 );
      }

      XFlush ( VD_ID );


}


/*************************************************************************
  DSXC_P_CLOSE -- Close panel window (XWindows)

   alan penny             ral          1990 jan
  pat morris             leeds        1992 Jan
*/

dsxc_p_close ( kdoh, ierr )

       int    kdoh;	/* i: Use Help Panel? (0=no;1=yes) */
       int   *ierr;	/* o: Error flag (0=ok;1=bad) */
{
/* C-- */
/* Cbegin */


      *ierr = 0;					/* Cant fail! */

      DSOPEN = F77_NAMED_COMMON(ds_genb).dsopen;
      PDSOPEN = F77_NAMED_COMMON(ds_panelb).pdsopen;
      DSWINDX = F77_NAMED_COMMON(ds_gen).dswindx;
      DSWINDXM = F77_NAMED_COMMON(ds_gen).dswindxm;
      PNSNX = F77_NAMED_COMMON(ds_panel).pnsnx;

      if ( !PDSOPEN ) return;

      DSWINDX = DSWINDX - PNSNX - 20;

      XUnmapWindow ( VD_ID, WD_P_ID );
      XDestroyWindow ( VD_ID, WD_P_ID );
      XFreePixmap ( VD_ID, P_PIXMAP );

      if ( kdoh==1 ) {
         XUnmapWindow ( VD_ID, WD_PH_ID );
         XDestroyWindow ( VD_ID, WD_PH_ID );
         XFreePixmap ( VD_ID, PH_PIXMAP );
      }

      if ( !DSOPEN ) XCloseDisplay ( VD_ID );
      XFlush ( VD_ID );

      PDSOPEN = False;

      F77_NAMED_COMMON(ds_gen).dswindx = DSWINDX;
      F77_NAMED_COMMON(ds_panelb).pdsopen = F77_FALSE;


}


/**************************************************************************
  DSXC_P_PTTIT -- Put title on panel (XWindows)

    alan penny                ral              1990-01-31
  pat morris             leeds        1992 Jan
*/
dsxc_p_pttit ( title )

char *title;	/* i: Title to put up */
{
/* C-- */
/* Cbegin */


      XStoreName ( VD_ID, WD_P_ID, title );


}


/****************************************************************************
  DSXC_P_SSCUR -- Start the panel cursor (sections) (XWindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 Jan
*/

dsxc_p_sscur ( kn )

     int kn;		/* i: Box number to place cursor */
{
/* C-- */
      Cursor  cursor;
      Pixmap cursor_pixmap;
      static unsigned char cursor_bits[32] =  { 0x00, 0x00, 0x00, 0x00,
                        0x00, 0x00, 0xff, 0xff,
                        0xff, 0xff, 0xff, 0xff,
                        0xff, 0xff, 0xff, 0xff,
                        0xff, 0xff, 0xff, 0xff,
                        0xff, 0xff, 0xff, 0xff,
                        0xff, 0xff, 0x00, 0x00,
                        0x00, 0x00, 0x00, 0x00 } ;
      int     kx, ky, jx, jy;
      XColor  cursor_dummy;
      XColor  cursor_foreground;
      XColor  cursor_background;
/* Cbegin */


      cursor_pixmap = XCreatePixmapFromBitmapData ( VD_ID, RW_P_ID,
                             cursor_bits, 16, 16, 1, 0, 1 );

      XLookupColor ( VD_ID, DefaultColormap(VD_ID,SC_P_ID),
                           "red", &cursor_dummy, &cursor_foreground );
      XLookupColor ( VD_ID, DefaultColormap(VD_ID,SC_P_ID),
                           "black", &cursor_dummy, &cursor_background );

      cursor = XCreatePixmapCursor ( VD_ID, cursor_pixmap, cursor_pixmap,
                              &cursor_foreground, &cursor_background, 7, 7 );
                                	         			/* Use same bitmap for shape and mask
				                                         so cursor background is transparent */
      XDefineCursor ( VD_ID, WD_P_ID, cursor );
      XFreePixmap ( VD_ID, cursor_pixmap );

      jx = F77_NAMED_COMMON(ds_panel).pnx[kn-1];
      jy = F77_NAMED_COMMON(ds_panel).pny[kn-1];
      jx = jx + 68.0;
      jy = jy + 10.0;

      kx = jx - 1;
      ky = PNSNY - jy;

/*
      XWarpPointer ( VD_ID, None, WD_P_ID,  0, 0, 0, 0, kx, ky );
*/

      XFlush ( VD_ID );

}


/**********************************************************************
 DSXC_P_SBOX -- Paint a panel box (sections) (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

dsxc_p_sbox ( kn, text, kflagu, kflage )

   int  kn;		/* i: Box number (unless -1=COMMAND;-2=Help Buttons) */
   char *text;		/* i: Text in box */
   int  kflagu;		/* i: Down or up box (0/1) */
   int  kflage;		/* i: Whole or edge of box (0/1) */
{
/* C-- */
      int jx, jy, kl;
/* Cbegin */


      if ( kn==-1 ) {
         jx = F77_NAMED_COMMON(ds_panel).pncposx;
         jy = F77_NAMED_COMMON(ds_panel).pncposy;
      } else if ( kn==-2 ) {
         jx = F77_NAMED_COMMON(ds_panel).pnbhposx;
         jy = F77_NAMED_COMMON(ds_panel).pnbhposy;
      } else {
         jx = F77_NAMED_COMMON(ds_panel).pnx[kn-1];
         jy = F77_NAMED_COMMON(ds_panel).pny[kn-1];
      }

      if ( kflage == 0 ) {						/* Paint box */
         if ( kflagu == 1 )
            (void) dsxc_p_putim ( XDU, 86, 21, jx, jy );
         else
            (void) dsxc_p_putim ( XDD, 86, 21, jx, jy );
         kl = strlen(text);
         kl = imin (11, kl );
         if ( kl > 1 ) (void) dsxc_p_puttxt ( text, kl, jx+9, jy+5, 9 );
      }else {
         if ( kflagu == 1 ) {
            (void) dsxc_p_putim ( &XDU[86*17], 86, 4, jx, jy );
            (void) dsxc_p_putim ( XDU, 86, 4, jx, jy+17 );
            (void) dsxc_p_putim ( XPL, 5, 13, jx, jy+4 );
            (void) dsxc_p_putim ( XPD, 5, 13, jx+81, jy+4 );
         }else {
            (void) dsxc_p_putim ( &XDD[86*17], 86, 4, jx, jy );
            (void) dsxc_p_putim ( XDD, 86, 4, jx, jy+17 );
            (void) dsxc_p_putim ( XPD, 5, 13, jx, jy+4 );
            (void) dsxc_p_putim ( XPL, 5, 13, jx+81, jy+4 );
         }
      }


}


/**********************************************************************
 DSXC_P_PUTIM -- Put an image into panel (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

dsxc_p_putim ( data, kxl, kyl, kx, ky )

char  *data;		/* i: Input data */
int   kxl;		/* i: X size of data */
int   kyl;		/* i: Y size of data */
int   kx;		/* i: X start of data position */
int   ky;		/* i: Y start of data position */
{
/* C-- */

      int xkx, xky;
      XImage  *image;
      char *ipd;
      int i;
/* Cbegin */


      PNSNY = F77_NAMED_COMMON(ds_panel).pnsny;

      ipd = malloc ( kxl*kyl );
      for ( i=0;i<kxl*kyl;i++ )
          ipd[i] = data[i];

      image = XCreateImage ( VD_ID, P_VISUAL, P_DEPTH, ZPixmap, 0, ipd,
                              kxl, kyl, 8, kxl );
									/* Point to image */
      XFlush ( VD_ID );
      xkx = kx - 1;
      xky = PNSNY - ky - kyl + 2;
      XPutImage ( VD_ID, P_PIXMAP, GC_P_ID, image, 0, 0,
                  xkx, xky, kxl, kyl );					/* Put image in pixmap */
      XCopyArea ( VD_ID, P_PIXMAP, WD_P_ID, GC_P_ID,
		  xkx, xky, kxl, kyl, xkx, xky );			/* Put pixmap area on screen */

      XDestroyImage ( image );
      free (ipd);
      XFlush ( VD_ID );


}

/**********************************************************************
 DSXC_P_CAREA -- Put a standard colour rectangle into panel (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

dsxc_p_carea ( kx, ky, kxl, kyl, kc )

     int   kx;       		/* i: X start of area */
     int   ky;       		/* i: Y start of area */
     int   kxl;			/* i: X length of area */
     int   kyl;			/* i: Y length of area */
     int   kc;       		/* i: Colour code (0:1:2=medium:light:dark grey) */
{
/* C-- */
/* Cbegin */


      PNSNY = F77_NAMED_COMMON(ds_panel).pnsny;

      switch (kc) {
         case 0  : if ( kxl*kyl<=500*500 ) {
                      (void) dsxc_p_putim ( XPM, kxl, kyl, kx, ky );
                   }
                   break;
         case 1  : if ( kxl*kyl<=2000 ) {
                      (void) dsxc_p_putim ( XPL, kxl, kyl, kx, ky );
                   }
                   break;
         case 2  : if ( kxl*kyl<=2000 ) {
                      (void) dsxc_p_putim ( XPD, kxl, kyl, kx, ky );
                   }
                   break;
         default : return;
      }
}

/***************************************************************************
  DSXC_P_HBOX -- Paint a help panel box (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

dsxc_p_hbox (void)

{
/* C-- */

      int j, jx, jy;
/* Cbegin */


      jx = F77_NAMED_COMMON(ds_panel).pnhposx;
      jy = F77_NAMED_COMMON(ds_panel).pnhposy;

      (void) dsxc_p_putim ( XDH, 86, 46, jx, jy );

      (void) dsxc_p_puttxt ( "HELP", 4, jx+23, jy+9, 9 );


}


/***************************************************************************
 DSXC_P_PUTTXT -- Write text to panel (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

dsxc_p_puttxt ( text, kl, kx, ky, kc )

char    *text;			/* i: Text to write */
int     kl;			/* i: No of characters to write */
int     kx;			/* i: X position */
int     ky;       		/* i: Y position */
int     kc;			/* i: Colour code (1=red;2=green;3=blue;
				   4=yellow;5=cyan;6=mauve;7=tan;
				   8=pink;9=black;10=white) */
{
/* C-- */
      int jy;
/* Cbegin */


      PNSNY = F77_NAMED_COMMON(ds_panel).pnsny;

      (void) dsxc_p_setthecol ( kc );

      jy = PNSNY - ky;

      XDrawString ( VD_ID, WD_P_ID, GC_P_ID, kx, jy, text, kl );
      XDrawString ( VD_ID, P_PIXMAP, GC_P_ID, kx, jy, text, kl );
      XFlush ( VD_ID );


 }


/***************************************************************************
 DSXC_P_HX_PUTTXT -- Write text to help panel (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

dsxc_p_hx_puttxt ( text, kl, kx, ky, kc )

char    *text;			/* i: Text to write */
int     kl;			/* i: No of characters to write */
int     kx;			/* i: X position */
int     ky;       		/* i: Y position */
int     kc;			/* i: Colour code (1=red;2=green;3=blue;
				   4=yellow;5=cyan;6=mauve;7=tan;
				   8=pink;9=black;10=white) */
{
/* C-- */
      int jy, kkcol;
/* Cbegin */


      switch (kc) {
         case 9  : kkcol = BY_PC_ID[10];
                   break;
         case 10 : kkcol = BY_PC_ID[14];
                   break;
         default : kkcol = BY_PC_ID[1+imin(8,imax(0,kc))];
      }


      XSetForeground ( VD_ID, GC_PH_ID, kkcol );
      XFlush ( VD_ID );

      jy = PHNY - ky;

      XDrawString ( VD_ID, WD_PH_ID, GC_PH_ID, kx, jy, text, kl );
      XDrawString ( VD_ID, PH_PIXMAP, GC_PH_ID, kx, jy, text, kl );
      XFlush ( VD_ID );


 }



/**********************************************************************
 DSXC_P_LINE -- Put a line at a position (XWindows)

  alan penny             ral             1990 May
  pat morris             leeds        1992 jan
*/

dsxc_p_line ( xs, ys, xe, ye, kc )

    float   xs;		/* i: Line start X position(image pixels) */
    float   ys;		/* i: Line start Y position (image pixels) */
    float   xe;		/* i: Line end X position(image pixels) */
    float   ye;		/* i: Line end Y position (image pixels) */
    int     kc;		/* i: Colour (1=red;2=green;3=blue;4=yellow;
			              5=cyan;6=mauve;7=tan;8=pink)
			              9=black;10=white) */

/* C-- */
{
      int jxs, jxe, jys, jye, jysi, jyei;
/* Cbegin */


      PNSNY = F77_NAMED_COMMON(ds_panel).pnsny;

      (void) dsxc_p_setthecol ( kc );

      jxs = xs;
      jxe = xe;
      jys = ys;
      jye = ye;

      jysi = PNSNY - jys;
      jyei = PNSNY - jye;
      XDrawLine ( VD_ID,  WD_P_ID, GC_P_ID, jxs, jysi, jxe, jyei ); 	/* Paint line */
      XDrawLine ( VD_ID,  P_PIXMAP, GC_P_ID, jxs, jysi, jxe, jyei );
      XFlush ( VD_ID );


 }


/**************************************************************************
 DSXC_P_SETTHECOL -- Set writing colour for panel (XWindows)

  alan penny           ral                         1990-06-09
  pat morris             leeds        1992 Jan
*/

dsxc_p_setthecol ( kc )

int  kc;		/* i: Colour (1=red;2=green;3=blue;4=yellow;
				          5=cyan;6=mauve;7=tan;8=pink;
				          9=black;10=white) */
/* C-- */
{
      int kkcol;
/* Cbegin */


      (void) dsxc_p_scolnum ( kc, &kkcol );

      XSetForeground ( VD_ID, GC_P_ID, kkcol );
      XFlush ( VD_ID );


}


/***************************************************************************
 DSXC_P_SCOLNUM -- Calc panel colour pixel number from colour number (XWindows)

  alan penny             ral             1990 May
  pat morris             leeds        1992 Jan
*/

dsxc_p_scolnum ( kc, kkcol )

int   kc;		/* i: Colour number */
int   *kkcol;		/* o: Display colour pixel number */

/* C-- */
{
/* Cbegin */


      switch (kc) {
         case 9  : *kkcol = BY_PC_ID[10];
                   break;
         case 10 : *kkcol = BY_PC_ID[14];
                   break;
         default : *kkcol = BY_PC_ID[1+imin(8,imax(0,kc))];
      }


}



/**************************************************************************
 DSXC_P_HSTAT -- Write status in the help panel box (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

dsxc_p_hstat ( kin )

int   kin;		/* i: Work/Wait/Help/Command/Pan/Zoom/Not Yet/Ready/One Got/ */
			/*    (0/1/2/3/4/5/6/7/8) */
/* C-- */
{
      int kx, ky;
      static char *atext[9] = { "WORKING", "WAITING", "HELPING", "COMMAND",
                  "PANNING", "ZOOMING", "NOT YET", "READY  ", "ONE GOT" };
/* Cbegin */


      PDSOPEN = F77_NAMED_COMMON(ds_panelb).pdsopen;

      if ( (kin<0) || (kin>7) ) return;
      if ( !PDSOPEN ) return;

      kx = F77_NAMED_COMMON(ds_panel).pnhposx;
      ky = F77_NAMED_COMMON(ds_panel).pnhposy;
      kx = kx + 9;
      ky = ky + 24;

      if ( kin==1 )
         (void) dsxc_p_putim ( IG, 67, 16, kx, ky );
      else
         (void) dsxc_p_putim ( IR, 67, 16, kx, ky );

      (void) dsxc_p_puttxt ( atext[kin], 7, kx+3, ky+2, 9 );


}


/**************************************************************************
 DSXC_P_CLEAR -- Clear panel (XWindows)

    a j penny                    ral         1990 jan
*/

dsxc_p_clear ( kopt )

int     kopt;		/* i: panel or help panel (1:2)? */
/* C-- */
{
      int j, ks, nx, ny;
      char *ipd;
      XImage  *image;
/* Cbegin */


      if ( kopt==1 ) {
         nx = F77_NAMED_COMMON(ds_panel).pnsnx;
         ny = F77_NAMED_COMMON(ds_panel).pnsny;
      } else {
         nx = PHNX;
         ny = PHNY;
      }

      ks = (nx*ny)+1;
      ipd = calloc ( ks, sizeof(char) ); 				/* Set temp virtual screen */
      if ( ipd == NULL ) {
           if ( kopt==1 )
              (void) c_printo ("ERROR: P_CLEAR - Cant get workspace");
           else
              (void) c_printo ("ERROR: PH_CLEAR - Cant get workspace");
           return;
      }
      ( void ) dsxc_p_putcol ( ipd, ks, GL );

      image = XCreateImage( VD_ID, P_VISUAL, P_DEPTH, ZPixmap, 0, 	/* Point to image */
                            ipd, nx, ny, 8, nx );

      if ( kopt==1 ) {							/* Put image in pixmap and panel */
         XPutImage ( VD_ID, P_PIXMAP, GC_P_ID, image, 0, 0, 0, 0, nx, ny );
         XCopyArea ( VD_ID, P_PIXMAP, WD_P_ID, GC_P_ID, 0, 0, nx, ny, 0, 0 );
      }
      if ( kopt==2 ) {
         XPutImage ( VD_ID, PH_PIXMAP, GC_PH_ID, image, 0, 0, 0, 0, nx, ny );
         XCopyArea ( VD_ID, PH_PIXMAP, WD_PH_ID, GC_PH_ID, 0, 0, nx, ny, 0, 0 );
      }

      XDestroyImage ( image );
      XFlush ( VD_ID );
      free (ipd);


}


/**************************************************************************
 DSXC_P_PUTCOL -- Put a colour into an array (XWindows)

    a j penny                    ral         1990 jan
*/

dsxc_p_putcol ( data, ks, col )

char   data[];		/* o: Array to load */
int    ks;		/* i: Size of array */
char   col;		/* i: Colour */
/* C-- */
{
      int j;
/* Cbegin */

      for ( j=0; j<ks; j++ )
          data[j] = col;							/* to light grey */



}


/**********************************************************************
 DSXC_P_HX_PUTIM -- Put an image into help panel (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

dsxc_p_hx_putim ( data, kxl, kyl, kx, ky )

char  *data;		/* i: Input data */
int   kxl;		/* i: X size of data */
int   kyl;		/* i: Y size of data */
int   kx;		/* i: X start of data position */
int   ky;		/* i: Y start of data position */
{
/* C-- */

      int xkx, xky;
      XImage  *image;
      char *ipd;
      int i;
/* Cbegin */


      PNSNY = F77_NAMED_COMMON(ds_panel).pnsny;

      ipd = malloc ( kxl*kyl );
      for ( i=0;i<kxl*kyl;i++ )
          ipd[i] = data[i];

      image = XCreateImage ( VD_ID, P_VISUAL, P_DEPTH, ZPixmap, 0, ipd,
                              kxl, kyl, 8, kxl );
									/* Point to image */
      XFlush ( VD_ID );
      xkx = kx - 1;
      xky = PNSNY - ky - kyl + 2;
      XPutImage ( VD_ID, PH_PIXMAP, GC_PH_ID, image, 0, 0, xkx,
                  xky, kxl, kyl );					/* Put image in pixmap */
      XCopyArea ( VD_ID, PH_PIXMAP, WD_PH_ID, GC_PH_ID,
			    xkx, xky, kxl, kyl, xkx, xky );		/* Put pixmap area on screen */

      XDestroyImage ( image );
      XFlush ( VD_ID );
      free (ipd);


}



/**********************************************************************
 DSXC_P_HX_LOAD -- Put help panel start information (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

dsxc_p_hx_load ( title )

      char   *title;		/* i: Title of image */
{
/* C-- */
      static char *texta[6] = {
        "To execute a choice, click mouse with pointer in the panel button.",
        "To return from a choice, generally, click on mouse right-hand button.",
        "For guide to options displayed on buttons, click on HELP panel button.",
        "For help on individual options, position pointer on button, and a ",
        "    short help will automatically appear in the Panel Help window.",
        "(Usually cursor has to be in a relevant, ACTIVE, window.)" };

      int j, k, kl;
      char textb[200];
/* Cbegin */


      (void) dsxc_p_clear ( 2 );
      (void) dsxc_p_hx_putim ( XPM, PHNX, PHNY, 0, 0 );

      strcpy ( textb, "Help for:   " );
      strncat ( textb, title, 80 );
      XStoreName ( VD_ID, WD_PH_ID, textb );

      j = 86;
      for ( k=0; k<=5; k++ ) {
         strcpy ( textb, texta[k] );
         kl = strlen(textb);
         kl = imin(68,kl);

         (void) dsxc_p_hx_puttxt ( textb, kl, 5, j, 9 );
         j = j - 16;
      }


}


/**********************************************************************
 DSXC_P_HX_HLOAD -- Put help panel information (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 Jan
*/

dsxc_p_hx_hload ( topt, thelp )

      char   *topt;		/* i: One line description of Option */
      char   *thelp;		/* i: Help text */
{
/* C-- */
      char texta[68];
      int k, ka;
/* Cbegin */


      (void) dsxc_p_clear ( 2 );
      (void) dsxc_p_hx_putim ( XPM, PHNX, PHNY, 0, 0 );

      XStoreName ( VD_ID, WD_PH_ID, topt );

      for ( k=1; k<=2; k++ ) {
          (void) aj_scpy ( texta, thelp, k );
          ka = 108 - (k-1)*15;
         (void) dsxc_p_hx_puttxt ( texta, 68, 5, ka, 9 );
      }
      for ( k=3; k<=8; k++ ) {
          (void) aj_scpy ( texta, thelp, k );
          ka = 115 - (k-1)*15;
         (void) dsxc_p_hx_puttxt ( texta, 68, 5, ka, 9 );
      }

      XSetForeground ( VD_ID, GC_PH_ID, BY_PC_ID[10] );
      XDrawLine ( VD_ID,  WD_PH_ID, GC_PH_ID, 0, 20, PHNX, 20 ); 	/* Paint line */
      XDrawLine ( VD_ID,  PH_PIXMAP, GC_PH_ID, 0, 20, PHNX, 20 );
      XFlush ( VD_ID );

}



/**********************************************************************
 AJ_SCPY -- Copy text

    alan penny           ral                       1990-02-01
*/

aj_scpy ( texta, textb, k )

      char   *texta;		/* i: One line description of Option */
      char   *textb;		/* i: Help text */
      int    k;			/* i: Row to copy (k=1-6) */
{
/* C-- */
      int j, ja;
/* Cbegin */


      for ( j=0; j<=67; j++ ) {
          ja = j + (k-1)*68;
          texta[j] = textb[ja];
      }
      texta[68] = '\0';

}
