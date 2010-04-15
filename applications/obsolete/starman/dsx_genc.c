/******************************************************************************
* DSX_GENC.C
-
 Contains:-
-
 DSX_VTIM       Display part of virtual image - Fortran interface (Xwindows)
 DSX_PTTIT      Put image title on display - fortran interface (Xwindows)
 DSX_PSCUR      Put the cursor at screen position - fortran interface (Xwindows)
 DSX_GETCURPB   Get cursor posn and button state  - fortran interface (XWindows)
 DSX_PAINTOV(AB) Paint moving oval - erase last - Fortran interface (X windows)
 DSX_INIT       Initialise window - Fortran interface (Xwindows)
 DSX_ID_INIT    Initialise window IDs - Fortran interface (XWindows)
 DSX_SCOL       Set line colour - Fortran interface (XWindows)
 DSX_CLOSE      Close window - Fortran interface (XWindows)
 DSX_ERASE      Erase display - Fortran interface (XWindows)
 DSX_SCUR       Start the cursor - Fortran interface (XWindows)
 DSX_PCUR       Put the cursor at position - Fortran interface (XWindows)
 DSX_OVAL       Paint an oval - Fortran interface (Xwindows)
 DSX_OSIZE      X, Y size of tilted oval
 DSX_CROSS      Put a cross at a position - Fortran interface (Xwindows)
 DSX_LINE       Put a line at a position - Fortran interface (Xwindows)
 DSX_UPDOWN     Raise' or 'lower' a window - Fortran interface (Xwindows)
 DSX_ACIM(RS)   Display part of an actual (real:short) image - Fortran interface (X window)
 DSX_MSWAIT     Wait X millisecs  - Fortran interface (Xwindows)
-
 DSXC_INIT      Initialise window (Xwindows)
 DSXC_ID_INIT   Initialise window IDs (XWindows)
 DSXC_OPCOLMAP  Open colour map (Xwindows)
 DSXC_BYTECOL   Calc byte equivalents of PC_ID (Xwindows)
 DSXC_CICON     Set up corner icon (Xwindows)
 DSXC_SCOL      Set line colour (XWindows)
 DSXC_SETCOL    Set up colour display for image display (Xwindows)
 DSXC_VTIM      Display part of virtual image (Xwindows)
 DSXC_VTIMA     Display part of virtual image (Xwindows)
 DSXC_VTLOAD    Load part of virtual image into a zoomed array (Xwindows)
 DSXC_CLOSE     Close window (XWindows)
 DSXC_PTTIT     Put image title on display (XWindows)
 DSXC_ERASE     Erase display (XWindows)
 DSXC_CLEAR     Clear screen (XWindows)
 DSXC_CLEARPIX  Clear pixmap (XWindows)
 DSXC_SCUR      Start the cursor (XWindows)
 DSXC_PCUR      Put the cursor at position (XWindows)
 DSXC_PSCUR     Put the cursor at screen position (XWindows)
 DSXC_GETCURPB  Get cursor posn and button state (XWindows)
 DSXC_WAITBUT   Wait for button to be pressed or to be up (Xwindows)
-
 DSXC_PUTIM     Put an image (Xwindows)
-
 DSXC_PAINTOV(AB)  Paint moving oval - erase last (X windows)
 DSXC_OVAL      Paint an oval (Xwindows)
 DSXC_OVALA     Paint an oval at X/Y normals (Xwindows)
 DSXC_OVALB     Paint an oval at angle (Xwindows)
 DSXC_OSIZE     X, Y size of tilted oval
 DSXC_CROSS     Put a cross at a position (Xwindows)
 DSXC_SCOLNUM   Calc colour pixel number from colour number (Xwindows)
 DSXC_LINE      Put a line at a position (Xwindows)
 DSXC_SETTHECOL Set writing colour  (Xwindows)
-
 DSXC_UPDOWN    'Raise' or 'lower' a window (Xwindows)
 DSXC_ACIM(RS)   Display part of an actual (real:short) image (X window)
 DSXC_MSWAIT    Wait X millisecs  (Xwindows)

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/X.h>
#include <sys/time.h>
#include <malloc.h>


#include "f77.h"
#include "cnf.h"

#include "image.h"
#include "ds_gen.h"
#include "ds_lut.h"
#include "ds_panel.h"
#include "dsx_gen.h"

/* Prototypes */

int imax ( int, int );
int imin ( int, int );
float fmax ( float, float );
float fmin ( float, float );

/* External C functions */

extern void dsxc_lutcol ();
extern void dsxc_lutacol ();
extern void dsxc_p_putim ( char*, int, int, int, int );
extern void dsxc_p_puttxt ( char*, int, int, int, int );
extern void dsxc_p_hstat ( int );

extern void c_printo ( char* );
extern void c_get1i ( char*, int*, int, int, int );
extern void c_get2i ( char*, int*, int*, Bool, int, int );

extern void vtc_tis ( int, int, int*, int* );
extern void vtc_tsv ( int, int, int*, int* );
extern void vtc_tvs ( int, int, int*, int* );



/* Panel Globals */

int PNKXS;
int PNKYS;
int PNKXO;
int PNKYO;

int	KXO;		/*  X Last cursor X posn */
int	KYO;		/*  Y Last cursor Y posn */

char   TEXTAA[200];
/*
  (void) sprintf ( TEXTAA, " F1XXX %d %d %d %d %d ",
                          PDSOPEN, DSNXS, DSNXE, DSNYS, DSNYE );
   (void) c_printo ( TEXTAA );
*/

/***********************************************************************
  DSX_PTTIT -- Put image title on display - FORTRAN interface (Xwindows)

    alan penny                ral              1990-01-31
    pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_pttit) ( CHARACTER(title) TRAIL(title) )

{
       GENPTR_CHARACTER(title)		/*   i: Title to put up */
/* C-- */
       char *name;
/* Cbegin */


       name = cnf_creim ( title, title_length );

       (void) dsxc_pttit ( name );

       cnf_free (name);


}


/******************************************************************
 DSX_VTIM -- Display part of virtual image - Fortran interface (Xwindows)

    alan penny           ral                       1990-02-01
*/

F77_SUBROUTINE(dsx_vtim) ( INTEGER(jvxs), INTEGER(jvxe), INTEGER(jvys),
                           INTEGER(jvye), INTEGER(kcl) )

{
      GENPTR_INTEGER(jvxs)           /* i: X start of virtual area to display */
      GENPTR_INTEGER(jvxe)           /* i: X   end of virtual area to display */
      GENPTR_INTEGER(jvys)           /* i: Y start of virtual area to display */
      GENPTR_INTEGER(jvye)           /* i: Y   end of virtual area to display */
      GENPTR_INTEGER(kcl)            /* i: Flag to clear screen before (0=no) */
/* C-- */
/* Cbegin */

      (void) dsxc_vtim ( *jvxs, *jvxe, *jvys, *jvye, *kcl );
}


/**********************************************************************
     DSX_PSCUR -- Put the cursor at screen position - Fortran Interface (Xwindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_pscur) ( REAL(x), REAL(y) )

{
     GENPTR_REAL(x)		/* i: X position to put cursor at */
     GENPTR_REAL(y)		/* i: Y position to put cursor at */
/* C-- */
/* Cbegin */

      (void) dsxc_pscur ( *x, *y );

}


/*******************************************************************
   DSX_GETCURPB -- Get cursor posn and button state - Fortran interface (XWindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_getcurpb) ( LOGICAL(isimage), INTEGER(kx), INTEGER(ky),
                               INTEGER_ARRAY(kb), INTEGER(ierr) )

{
    GENPTR_LOGICAL(isimage)		/* i: Image (T) or panel (F) ? */
    GENPTR_INTEGER(kx)			/* o: Cursor X position */
    GENPTR_INTEGER(ky)			/* o: Cursor Y position */
    GENPTR_INTEGER_ARRAY(kb)		/* o: Cursor Button states (0=up;1=down) */
    GENPTR_INTEGER(ierr)	    	/* o: Error flag ( 0=ok; 1=bad) */
/* C-- */
    Bool la;
    int ikx, iky, iierr;
/* Cbegin */

       la = *isimage;
      (void) dsxc_getcurpb ( la, &ikx, &iky, kb, &iierr );
       *kx = ikx; *ky = iky; *ierr = iierr;

}



/*******************************************************************
   DSX_PAINTOVA -- Paint moving oval at X/Y normal - erase last - Fortran interface (XWindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_paintova) ( REAL(pxo), REAL(pyo), REAL(rxo), REAL(ryo),
                              REAL(px),  REAL(py),
                              REAL(rax1), REAL(ray1), INTEGER(kc1),
                              REAL_ARRAY(rax2), REAL_ARRAY(ray2),
                              INTEGER(kc2) )

{
    GENPTR_REAL(pxo)		/* i: Old screen X posn */
    GENPTR_REAL(pyo)		/* i: Old screen Y posn */
    GENPTR_REAL(rxo)		/* i: Old max X radius (image scale) */
    GENPTR_REAL(ryo)		/* i: Old max Y radius (image scale) */
    GENPTR_REAL(px)		/* i: New screen X posn */
    GENPTR_REAL(py)		/* i: New screen Y posn */
    GENPTR_REAL(rax1)		/* i: Inner X oval radius (image scale) */
    GENPTR_REAL(ray1)		/* i: Inner Y oval radius (image scale) */
    GENPTR_INTEGER(kc1)		/* i: Inner oval colour (1-8) */
    GENPTR_REAL_ARRAY(rax2)	/* i: Inner/outer annular X oval radii */
    GENPTR_REAL_ARRAY(ray2)	/* i: Inner/outer annular Y oval radii */
    GENPTR_INTEGER(kc2)		/* i: Outer oval colour (1-8) */
/* C-- */
/* Cbegin */


      (void) dsxc_paintova ( *pxo, *pyo, *rxo, *ryo, *px, *py, *rax1, *ray1,
                             *kc1, rax2, ray2, *kc2 );


}


/*******************************************************************
   DSX_PAINTOVB -- Paint moving oval - erase last - Fortran interface (XWindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_paintovb) ( INTEGER_ARRAY(kcx), INTEGER_ARRAY(kcy),
                               INTEGER(kc1), INTEGER(kc2),
                               REAL(pxo), REAL(pyo), REAL(rxo), REAL(ryo) )
{
    GENPTR_INTEGER_ARRAY(kcx)		/* i: Centre/Inner/outer annular X oval */
    GENPTR_INTEGER_ARRAY(kcy)		/* i: Centre/Inner/outer annular Y oval */
    GENPTR_INTEGER(kc1)			/* i: Centre oval colour (1-8) */
    GENPTR_INTEGER(kc2)			/* i: Inner/Outer oval colour (1-8) */
    GENPTR_REAL(pxo)        	    	/* i: Old screen X posn */
    GENPTR_REAL(pyo)            	/* i: Old screen Y posn */
    GENPTR_REAL(rxo)			/* i: Old max X radius (image scale) */
    GENPTR_REAL(ryo)			/* i: Old max Y radius (image scale) */
/* C-- */
/* Cbegin */


      (void) dsxc_paintovb ( kcx, kcy, *kc1, *kc2, *pxo, *pyo, *rxo, *ryo );


}


/***********************************************************************
  DSX_INIT -- Initialise window - Fortran interface (XWindows)

  alan penny             ral          1990 Aug
  patrick Morris         leeds                  1992 Jan
*/

F77_SUBROUTINE(dsx_init) ( CHARACTER(title), INTEGER(kscreen),
                           INTEGER(ierr) TRAIL(title) )

{
    GENPTR_CHARACTER(title)  	/* i: Title of image */
    GENPTR_INTEGER(kscreen)     /* i: Ask for screen size flag (0=yes;1=no) */
    GENPTR_INTEGER(ierr)     	/* o: Error flag (0=ok;1=bad) */
/* C-- */
     char *name;
     int iierr;
/* Cbegin */

      name = cnf_creim ( title, title_length );

      (void) dsxc_init ( name, *kscreen, &iierr );

      *ierr = iierr;
      cnf_free ( name );

}


/***********************************************************************
  DSX_ID_INIT -- Initialise window IDs - Fortran interface (XWindows)

  alan penny             ral          1990 Aug
  patrick Morris         leeds                  1992 Jan
*/

F77_SUBROUTINE(dsx_id_init) ( void )

{
/* C-- */
/* Cbegin */


      (void) dsxc_id_init ( );


}


/******************************************************************
      DSX_SCOL -- Set line colour - Fortran interface (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_scol) ( REAL(w), INTEGER(kc) )

{
    GENPTR_REAL(w)		/* i: Line width */
    GENPTR_INTEGER(kc)		/* i: Colour (1=red;2=green;3=blue;4=yellow
				   5=cyan;6=mauve;7=tan;8=pink) */
/* C-- */
/* Cbegin */

    (void)  dsxc_scol ( *w, *kc );


}


/***********************************************************************
        DSX_CLOSE -- Close window - Fortran interface (XWindows)

  alan penny             ral          1990 jan
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_close) ( INTEGER(ierr) )

{
    GENPTR_INTEGER(ierr)		/* o: Error flag (0=ok;1=bad) */
/* C-- */
    int iierr;
/* Cbegin */


      (void) dsxc_close ( &iierr );
       *ierr = iierr;


}



/**************************************************************************
        DSX_ERASE -- Erase display - Fortran interface (XWindows)

    a j penny                    ral         1990 jan
*/

F77_SUBROUTINE(dsx_erase) (void)

/* C-- */
{
/* Cbegin */


     (void) dsxc_erase ();


}



/*********************************************************************
        DSX_SCUR -- Start the cursor - Fortran interface (XWindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_scur) (void)

/* C-- */
{
/* Cbegin */


     (void) dsxc_scur ();

}



/**********************************************************************
        DSX_PCUR -- Put the cursor at position - Fortran interface (XWindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_pcur) ( INTEGER(kx), INTEGER(ky) )

{
    GENPTR_INTEGER(kx)		/*i: X position to put cursor at */
    GENPTR_INTEGER(ky)		/*i: Y position to put cursor at */
/* C-- */
/* Cbegin */

     (void) dsxc_pcur ( *kx, *ky );


}


/********************************************************************
     DSX_OVAL -- Paint an oval - Fortran interface (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_oval) ( REAL(xp), REAL(yp), REAL(radx), REAL(angle),
                           REAL(elli), INTEGER(kc) )

{
    GENPTR_REAL(xp)		/* i: Image X position */
    GENPTR_REAL(yp)		/* i: Image Y position */
    GENPTR_REAL(radx)		/* i: Image pixel X radius */
    GENPTR_REAL(angle)		/* i: Oval angle to X-axis */
    GENPTR_REAL(elli)		/* i: Oval ellipticity */
    GENPTR_INTEGER(kc)		/* i: Colour (1=red;2=green;3=blue;4=yellow;
				           5=cyan;6=mauve;7=tan;8=pink)
				           9=black;10=white) */
/* C-- */
/* Cbegin */

     (void) dsxc_oval ( *xp, *yp, *radx, *angle, *elli, *kc );


}

/********************************************************************
     DSX_OSIZE -- X, Y Size of tilted oval - Fortran interface (XWindows)

    alan penny           ral                       1994
*/

F77_SUBROUTINE(dsx_osize) ( REAL(rad), REAL(ang), REAL(elli), REAL(xs),
                            REAL(ys) )

{
    GENPTR_REAL(rad)		/* i: Oval maj radius */
    GENPTR_REAL(ang)		/* i: Angle to X-axis in degrees */
    GENPTR_REAL(elli)		/* i: Ellipticity */
    GENPTR_REAL(xs)		/* o: X coverage */
    GENPTR_REAL(ys)		/* o: Y coverage */
/* C-- */
    float xxs, yys;
/* Cbegin */


     (void) dsxc_osize ( *rad, *ang, *elli, &xxs, &yys );
     *xs = xxs;
     *ys = yys;

}


/****************************************************************
    DSX_CROSS -- Put a cross at a position - Fortran interface (XWindows)

  alan penny             ral             1990 Jan
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_cross) ( REAL(x), REAL(y), REAL(crs), INTEGER(kc) )

{
    GENPTR_REAL(x)		/* i: X position(image pixels) */
    GENPTR_REAL(y)		/* i: Y position (image pixels) */
    GENPTR_REAL(crs)		/* i: Cross size (end-to-end) (image pixels) */
    GENPTR_INTEGER(kc)		/* i: Colour (1=red;2=green;3=blue;4=yellow;
					      5=cyan;6=mauve;7=tan;8=pink)
					      9=black;10=white) */
/* C-- */
/* Cbegin */


     (void) dsxc_cross ( *x, *y, *crs, *kc );


}



/**********************************************************************
 DSX_LINE -- Put a line at a position - Fortran interface (XWindows)

  alan penny             ral             1990 May
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_line) ( REAL(xs), REAL(ys), REAL(xe), REAL(ye),
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


     (void) dsxc_line ( *xs, *ys, *xe, *ye, *kc );


}



/****************************************************************************
    DSX_UPDOWN -- Raise' or 'lower' a window - Fortran interface (Xwindows)

  alan penny           ral                         1990-06-09
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_updown) ( INTEGER(kf) )

{
    GENPTR_INTEGER(kf)		/* i: raise=1; lower=2 */
/* C-- */
/* Cbegin */


    (void) dsxc_updown ( *kf );


}


/**************************************************************************
  DSX_ACIMR -- Display part of an actual real image (loads virtual image)

  alan penny                  ral         1990 jan
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_acimr) ( REAL_ARRAY(im), INTEGER(mx), INTEGER(my),
                            INTEGER(ixs), INTEGER(ixe), INTEGER(iys), INTEGER(iye),
                            INTEGER(ix), INTEGER(iy), LOGICAL(wrap) )

GENPTR_INTEGER(mx) 		/* i: Actual image X size */
GENPTR_INTEGER(my)		/* i: Actual image Y size */
GENPTR_REAL_ARRAY(im)		/* i: Actual image */
GENPTR_INTEGER(ixs)		/* i: Actual image area X start */
GENPTR_INTEGER(ixe)		/* i: Actual image area X end */
GENPTR_INTEGER(iys)		/* i: Actual image area Y start */
GENPTR_INTEGER(iye)		/* i: Actual image area Y end */
GENPTR_INTEGER(ix)		/* i: Position in virtual image of X start */
GENPTR_INTEGER(iy)		/* i: Position in virtual image of Y start */
GENPTR_LOGICAL(wrap)		/* i: Flag to wrap values round display limits */

{

    (void) dsxc_acimr ( im, *mx, *my, *ixs, *ixe, *iys, *iye, *ix, *iy, *wrap );

}


/****************************************************************
    DSX_ACIMS -- Display part of an actual int*2 image (loads virtual image)

  alan penny             ral          1990 jan
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_acims) ( WORD_ARRAY(im), INTEGER(mx), INTEGER(my),
                            INTEGER(ixs), INTEGER(ixe), INTEGER(iys), INTEGER(iye),
                            INTEGER(ix), INTEGER(iy), LOGICAL(wrap) )

GENPTR_INTEGER(mx) 			/* i: Actual image X size */
GENPTR_INTEGER(my)			/* i: Actual image Y size */
GENPTR_WORD_ARRAY(im)			/* i: Actual image */
GENPTR_INTEGER(ixs)			/* i: Actual image area X start */
GENPTR_INTEGER(ixe)			/* i: Actual image area X end */
GENPTR_INTEGER(iys)			/* i: Actual image area Y start */
GENPTR_INTEGER(iye)			/* i: Actual image area Y end */
GENPTR_INTEGER(ix)			/* i: Position in virtual image of X start */
GENPTR_INTEGER(iy)			/* i: Position in virtual image of Y start */
GENPTR_LOGICAL(wrap)			/* i: Flag to wrap values round display limits*/

{

    (void) dsxc_acims ( im, *mx, *my, *ixs, *ixe, *iys, *iye, *ix, *iy, *wrap );

}


/****************************************************************************
    DSX_MSWAIT -- Wait N milliseconds an image - Fortran interface (Xwindows)

  alan penny           ral                         1990-06-09
  pat morris             leeds        1992 jan
*/

F77_SUBROUTINE(dsx_mswait) ( INTEGER(msecs) )

{
    GENPTR_INTEGER(msecs)		/* i: Milliseconds to wait */
/* C-- */

/* Cbegin */

    (void) dsxc_mswait ( *msecs );

}


/***********************************************************************
  DSXC_INIT -- Initialise window (XWindows)

  alan penny             ral          1990 Aug
  patrick Morris         leeds                  1992 Jan
*/

dsxc_init ( title, kscreenopt, ierr )

    char  *title;		/* i: Title of image */
    int   *kscreenopt;		/* i: Ask for screen size? (0=yes;1=no)*/
    int   *ierr;		/* o: Error flag (0=ok;1=bad) */

/* C-- */
{
      char                texta[100];

      Bool                func = True ;		/* Synchronous behaour */

      unsigned long       attr_mask;		/* attributes mask */
      long                event_mask;
      int     iv, ivx, ivy, jx, jy, lxy, nxa, nya, k, kka, kkb;
      int     kxa, kya, nnxa, nnya;
      float   rvx, rvy;
      XFontStruct         *font;		/* font structure */

      XSetWindowAttributes   xswda; 		/* window attributes */
      XGCValues           xgcvl;    		/* gc values */
      XSizeHints          xszhn;	    	/* size hints */
/*
      char   *font_name = "-adobe-new century schoolbook-medium-r-normal--*-120-*-*-p*";
      char   *font_name = "-adobe-new century schoolbook-medium-r-normal--*-80-*-*-p*";
*/
      char   *font_name = "-adobe-new century schoolbook-medium-r-normal--*-100-*-*-p*";
      char   *display_name = NULL;
      char buf1[32], buf2[32];
      char *argv[8];

      XEvent              event;
      Status              status;
      XVisualInfo         vinfo;
/* Cbegin */


      *ierr = 0;

      PDSOPEN = F77_NAMED_COMMON(ds_panelb).pdsopen;
      DSNXS = F77_NAMED_COMMON(ds_gen).dsnxs;
      DSNXE = F77_NAMED_COMMON(ds_gen).dsnxe;
      DSNYS = F77_NAMED_COMMON(ds_gen).dsnys;
      DSNYE = F77_NAMED_COMMON(ds_gen).dsnye;

      if ( PDSOPEN ) {
         if ( VD_ID == NULL ) {
            (void) c_printo ( "ERROR: Cant open Xwindows display" );
            *ierr = 1;
            return ;
         }
      }
      else {
         if ( (VD_ID = XOpenDisplay(display_name)) == NULL ) {
            (void) c_printo ( "ERROR: Cant open Xwindows display" );
            *ierr = 1;
            return;
         }
         (void) c_get1i ( "NUMBUTT", &iv, 3, 2, 3 );
         NUMXBUTTONS = iv;
         F77_NAMED_COMMON(ds_gen).numxbuttons = iv;
      }
      DSOPEN = True;
      F77_NAMED_COMMON(ds_genb).dsopen = F77_TRUE;

      (void) XSynchronize ( VD_ID, func );
      SC_ID = DefaultScreen ( VD_ID );
      RW_ID = RootWindow ( VD_ID, SC_ID );

      nxa = DisplayWidth ( VD_ID, SC_ID ) ;
      nya = DisplayHeight( VD_ID, SC_ID ) ;
      nnxa = nxa - 6 ;
      nnya = nya - 25 ;

      if ( nnxa*nnya > MAXVIRT ) {
         (void) sprintf ( texta,
         "  WARNING: Total screen size is %5d x %5d :", nxa, nya );
         (void) c_printo ( texta );
         (void) sprintf ( texta,
         "           Opening a window with more than 8%d pixels ", MAXVIRT );
         (void) c_printo ( texta );
         (void) c_printo ( "           will crash this program" );
      }

      jx  = DSNXE - DSNXS + 1 ;		/* Get screen pixel size */
      jy  = DSNYE - DSNYS + 1 ;
      if ( jx<16 || jy<16 ) {
         (void) sprintf ( texta,
         "  WARNING: Window size asked for was %5d x %5d :", jx, jy);
         (void) c_printo ( texta );
         (void) c_printo (
         "           For technical reasons, display sides must be > 15" );
         if ( jx<16 ) jx = 16;
         if ( jy<16 ) jy = 16;
         (void) sprintf ( texta,
         "           Window will be opened as size %5d x %5d :", jx, jy);
         (void) c_printo ( texta );
       }
      DSSNX = jx ;
      DSSNY = jy ;
      if ( (jx > nnxa) || (jy > nnya) ) {
         kka = jx - 1;
         if ( kka<0 ) kka = -1*kka;
         kkb = jy - 1;
         if ( kkb<0 ) kkb = -1*kkb;
         lxy = 1 + imax((kka/nnxa),(kkb/nnya));
         DSSNX = 1 + ((jx-1)/lxy);
         DSSNY = 1 + ((jy-1)/lxy);
         (void) sprintf ( texta,
                          " Image size %5d x %5d is too large for screen size - %5d x %5d",
                          jx, jy, nnxa, nnya );
         (void) c_printo ( texta );
         (void) sprintf ( texta, " Displayed image will be compressed by %5d x %5d",
                          lxy, lxy);
         (void) c_printo ( texta );
      }

      if ( kscreenopt==0 ) {
         k = imax(nnxa,nnya);
         ivx = DSSNX;
         ivy = DSSNY;
         (void) c_get2i ( "SCREEN", &ivx, &ivy, True, 1, k );
         DSSNX = ivx;
         DSSNY = ivy;
      }

      if ( DSSNX > nnxa ) {
         (void) sprintf ( texta, " X size too large - set to max X size of %d",
                                 nnxa);
         (void) c_printo ( texta );
         DSSNX = nnxa;
      }
      if ( DSSNY > nnya ) {
         (void) sprintf ( texta, " Y size too large - set to max Y size of %d",
                                nnya);
         (void) c_printo ( texta );
         DSSNY = nnya;
      }

      F77_NAMED_COMMON(ds_gen).dssnx = DSSNX;
      F77_NAMED_COMMON(ds_gen).dssny = DSSNY;

      if ( ( DEPTH = DefaultDepth (VD_ID, SC_ID) ) < 8 ) {
         (void) c_printo ( "ERROR: Screen pixel depth must be at least 8" );
         *ierr = 1;
         return ;
      }

      D_VISUAL = DefaultVisual (VD_ID, SC_ID);
      status = XMatchVisualInfo ( VD_ID, SC_ID, DEPTH, PseudoColor, &vinfo );
      if ( status == 0 ) {
         (void) c_printo (
         "ERROR: Cant allocate colours - display type has to be pseudo color" );
         *ierr = 1;
         return ;
      }

      if ( vinfo.visual != D_VISUAL )
	  (void) c_printo ( "Not default visual" );
      if (!PDSOPEN) (void) dsxc_opcolmap ();				/* Open colour map */

      attr_mask = CWEventMask | CWBackPixel | CWBorderPixel | CWColormap;
      xswda.event_mask = ExposureMask;
      xswda.background_pixel = BlackPixel(VD_ID,SC_ID);
      xswda.border_pixel = WhitePixel(VD_ID,SC_ID);
      xswda.colormap = CM_ID;

      kxa = nnxa - DSSNX - DSWINDX;					/* Position */
      kya = DSWINDY ;

      WD_ID = XCreateWindow ( VD_ID, RW_ID, kxa, kya, DSSNX, DSSNY, 0, DEPTH,
                             InputOutput, D_VISUAL, attr_mask, &xswda );

      (void) dsxc_cicon ();						/* Set up corner icon */

      DSWINDX = DSWINDX + DSSNX + 20;
      DSWINDXM = DSWINDX;

      F77_NAMED_COMMON(ds_gen).dswindx = DSWINDX;
      F77_NAMED_COMMON(ds_gen).dswindxm = DSWINDXM;

      xgcvl.foreground = WhitePixel(VD_ID,SC_ID);			/* Create graphics context */
      xgcvl.background = WhitePixel(VD_ID,SC_ID);
      GC_ID = XCreateGC ( VD_ID, WD_ID, (GCForeground | GCBackground), &xgcvl );

      if ( (font = XLoadQueryFont ( VD_ID, font_name ) ) == NULL ) {	/* Load the font for text writing */
         (void) c_printo (
         "WARNING: Couldn\'t open fonts - any text may be wrong size");
      }else{
         XSetFont ( VD_ID, GC_ID, font->fid );
      }

      XSetForeground ( VD_ID, GC_ID, WhitePixel(VD_ID,SC_ID) );

      xszhn.x = kxa;							/* Define the size and name of the WD_ID window */
      xszhn.y = kya;
      xszhn.width = DSSNX;
      xszhn.height = DSSNY;
      xszhn.flags = PPosition | PSize ;

      XSetNormalHints ( VD_ID, WD_ID, &xszhn );
      XFlush ( VD_ID );

      PIXMAP = XCreatePixmap ( VD_ID, RW_ID, DSSNX, DSSNY, DEPTH );
      (void) dsxc_clearpix () ;

      XSelectInput ( VD_ID, WD_ID, (  StructureNotifyMask    | ButtonReleaseMask
                                    | ButtonPressMask        | PointerMotionMask
                                    | SubstructureNotifyMask | ExposureMask) );


      PID = vfork();
      if ( PID != 0 ) {  						/* We are child so do exposure monitor */
         if ( PID == -1 ) (void) c_printo ( "ERROR: Refresh not started" );
      } else {
         display_name = DisplayString ( VD_ID );
         argv[0] = "s_refresh";
         sprintf ( buf1, "%lu", WD_ID );
         argv[1] = buf1;
         sprintf ( buf2, "%lu", PIXMAP );
         argv[2] = buf2;
         argv[3] = display_name;
         argv[4] = '\0';
         execvp ( "s_refresh", argv );
        _exit ( errno );
       }

      XMapWindow ( VD_ID, WD_ID );					/* Map the windows */
      XFlush ( VD_ID );

      (void) dsxc_pttit ( title );

      (void) dsxc_erase () ;

      event_mask = StructureNotifyMask | SubstructureNotifyMask;
      XWindowEvent ( VD_ID, WD_ID, event_mask, &event ); 		/* Wait for window to appear */

      XFlush ( VD_ID );

      rvx = DSSNX/2;							/* Cursor position */
      rvy = DSSNY/2;
      DSCURPOSX = rvx;
      DSCURPOSY = rvy;
      DSCURSET  = False;

      DSZM = 1;								/* Set Zoom factors */
      DSZPX = 1;
      DSZPY = 1;

      DSIXS  = 1;							/* Default image blh corner X */
      DSIYS  = 1;							/* and Y virtual image posn */

      F77_NAMED_COMMON(ds_gen).dscurposx = rvx;
      F77_NAMED_COMMON(ds_gen).dscurposy = rvy;
      F77_NAMED_COMMON(ds_genb).dscurset = F77_FALSE;
      F77_NAMED_COMMON(ds_gen).dszm = DSZM;
      F77_NAMED_COMMON(ds_gen).dszpx = DSZPX;
      F77_NAMED_COMMON(ds_gen).dszpy = DSZPY;
      F77_NAMED_COMMON(ds_gen).dsiys = DSIYS;
      F77_NAMED_COMMON(ds_gen).dsixs = DSIXS;

      DSCOMFX = F77_NAMED_COMMON(ds_gen).dscomfx;
      DSCOMFY = F77_NAMED_COMMON(ds_gen).dscomfy;
}


/**************************************************************************
  DSXC_ID_INIT -- Initialise window IDs (XWindows)

  alan penny             ral          1990 jan
  pat morris             leeds        1992 jan
*/

dsxc_id_init (void)
{

      VD_ID = NULL;

}


/**************************************************************************
 DSXC_OPCOLMAP -- Open colour map (Xwindows)

  alan penny             ral                    1990 Aug
  patrick Morris         leeds                  1992 Jan
*/

dsxc_opcolmap (void)

/* C-- */
{
      Status istat;
      int iv;
      unsigned long plane_mask;
/* Cbegin */


      CM_ID = DefaultColormap ( VD_ID, SC_ID );		/* Get colormap */

      OWNCOL = True;
      NUMDDCOL = NUMDCOL;
      istat = XAllocColorCells ( VD_ID, CM_ID, False, &plane_mask,
                                 0, PC_ID, (15+NUMDDCOL) );
      if ( istat == 0 ) {
         NUMDDCOL = NUMDCOL/2;
         istat = XAllocColorCells ( VD_ID, CM_ID, False, &plane_mask,
                                    0, PC_ID, 15+NUMDDCOL );
         if ( istat == 0 ) {
            NUMDDCOL = NUMDCOL/3;
            istat = XAllocColorCells ( VD_ID, CM_ID, False, &plane_mask,
                                       0, PC_ID, 15+NUMDDCOL );
            if ( istat == 0 ) {
               NUMDDCOL = NUMDCOL;
               (void) c_printo ( "WARNING: Cant allocate own colours - will take available ones" );
               (void) c_printo ( "        LUT Rotation and Scaling will not work" );
               (void) c_printo ( "        Any change to the LUT will also need to have the image redisplayed" );
               OWNCOL = False;
            }
            else
               (void) c_printo ( "WARNING: Only 50 steps in image display" );
         }
         else
            (void) c_printo ( "WARNING: Only 75 steps in image display" );
      }

      F77_NAMED_COMMON(lutacom).numddcol = NUMDDCOL;

      (void) dsxc_setcol ();
      (void) dsxc_bytecol ();


}


/*********************************************************************
 DSXC_BYTECOL -- Calc byte equivalents of PC_ID (Xwindows)

  alan penny             ral                    1990 Aug
  pat morris             leeds        1992 jan
*/

dsxc_bytecol (void)

/* C-- */
{
      int k, iv, ka, kdiv;
/* Cbegin */


      NUMDDCOL = F77_NAMED_COMMON(lutacom).numddcol;

      kdiv = NUMDCOL/NUMDDCOL ;
      for ( k = 1; k <= (15+NUMDCOL) ; k++ ) {
         ka = k;
         if ( k > 15 ) ka = 16 + ((ka-16)/kdiv);
         iv = PC_ID[ka-1];
         if ( iv>255 )
            BY_PC_ID[k-1] = -1;
         else if ( iv>127 )
            BY_PC_ID[k-1] = -256 + iv;
         else if ( iv<0 )
            BY_PC_ID[k-1] = 0;
	 else
            BY_PC_ID[k-1] = iv;

      }


}


/*************************************************************************
 DSXC_CICON -- Set up corner icon (Xwindows)

  alan penny             ral                    1990 Aug
  pat morris             leeds        1992 jan
*/

dsxc_cicon (void)

/* C-- */
{

      XWMHints        xwmhn;	    	/* Window manager hints */

      XWMHints        xwmhints;

      Pixmap          icon_pixmap;
      Atom            wmatom;
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

        0x80, 0x84, 0x20, 0x30, 0x1e, 0x00, 0x00, 0x00,
        0x80, 0xcc, 0x20, 0x48, 0x02, 0x00, 0x00, 0x00,
        0x80, 0xcc, 0x48, 0x08, 0x02, 0x00, 0x00, 0x00,
        0x80, 0xb4, 0x48, 0x04, 0x02, 0x00, 0x00, 0x00,
        0x80, 0x84, 0x48, 0x64, 0x0e, 0x00, 0x00, 0x00,
        0x80, 0x84, 0x68, 0x48, 0x02, 0x00, 0x00, 0x00,
        0x80, 0x84, 0x84, 0x48, 0x02, 0x00, 0x00, 0x00,
        0x80, 0x84, 0x84, 0x30, 0x1e, 0x00, 0x00, 0x00,
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


      icon_pixmap = XCreateBitmapFromData ( VD_ID, WD_ID, icon_bits, 64, 64 );

      xwmhn.flags  = IconPixmapHint | IconMaskHint ;
      xwmhn.icon_pixmap = icon_pixmap ;
      xwmhn.icon_mask   = icon_pixmap ;
      XSetWMHints ( VD_ID, WD_ID, &xwmhn );

      wmatom = XInternAtom ( VD_ID, "XA_WM_HINTS", 0 ) ;

      if ( wmatom != None ) {
         xwmhints.flags =  IconPixmapHint ;
         xwmhints.icon_pixmap = icon_pixmap ;
         XChangeProperty ( VD_ID, WD_ID, wmatom, wmatom, 32,
                           PropModeReplace, (char *) &xwmhints, 9 ) ;
      }

      XFlush ( VD_ID );


}


/******************************************************************
      DSXC_SCOL -- Set line colour (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 jan
*/

dsxc_scol ( w, kc )

    float     w;		/* i: Line width */
    int       kc;		/* i: Colour (1=red;2=green;3=blue;4=yellow
				   5=cyan;6=mauve;7=tan;8=pink) */
/* C-- */
{
/* Cbegin */



}


/*********************************************************************
  DSXC_SETCOL -- Set up color display for image display (Xwindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/

dsxc_setcol (void)

/* C-- */
{
      int          k, ka;
      Status       istat;
      char         flags;
      unsigned short int    j;

      static float rr[10]={0.8, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.5};
      static float gg[10]={0.8, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.5, 1.0};
      static float bb[10]={0.8, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0};
      static float sgr[5]={0.0,  0.5,  0.75,  0.89,  1.0};                        /* Starman greys */
      static float sgg[5]={0.0,  0.5,  0.75,  0.89,  1.0};
      static float sgb[5]={0.0,  0.5,  0.75,  0.89,  1.0};
/* Cbegin */


      flags = DoRed | DoGreen | DoBlue ;

      if ( OWNCOL ) {

         for ( k = 0; k<10; k++ ) {						/* Pgplot colours */
            COLOUR[k].pixel = PC_ID[k];
            COLOUR[k].flags = flags;
            j = (short) (rr[k]*65535.0) ;
            COLOUR[k].red = j;
            j = (short) (gg[k]*65535.0) ;
            COLOUR[k].green = j;
            j =  (short) (bb[k]*65535.0) ;
            COLOUR[k].blue = j;
         }

         for ( k = 10; k<15; k++ ) {						/* Starman greys */
            COLOUR[k].pixel = PC_ID[k];
            COLOUR[k].flags = flags;
            ka = k - 10;
            j = (short) (sgr[ka]*65535.0) ;
            COLOUR[k].red = j;
            j = (short) (sgg[ka]*65535.0) ;
            COLOUR[k].green = j;
            j = (short) (sgb[ka]*65535.0) ;
            COLOUR[k].blue = j;
         }

         for ( k = 15; k<15+NUMDDCOL ; k++ ) {					/* Starman grey scale */
            COLOUR[k].pixel = PC_ID[k];
            COLOUR[k].flags = flags;
            j =  65535.0 * (k-15)/(NUMDDCOL-1);
            COLOUR[k].red = j;
            COLOUR[k].green = j;
            COLOUR[k].blue = j;
         }


         XStoreColors ( VD_ID, CM_ID, COLOUR, (15+NUMDDCOL) );
         XInstallColormap ( VD_ID, CM_ID );
         XFlush ( VD_ID );

         (void) dsxc_lutacol () ;						/* Set up LUT */
         (void) dsxc_lutcol () ;

      }
      else {

         for ( k = 0; k<10 ; k++ ) {						/* Pgplot colours */
            COLOUR[k].flags = flags;
            j = (short) (rr[k]*65535.0);
            COLOUR[k].red = j;
            j = (short) (gg[k]*65535.0);
            COLOUR[k].green = j;
            j = (short) (bb[k]*65535.0);
            COLOUR[k].blue = j;
            istat = XAllocColor ( VD_ID, CM_ID, &COLOUR[k] );
            if ( istat == 0 ) {
               (void) c_printo ( "ERROR: Colour pixel not allocated" ) ;
               PC_ID[k] = 0 ;
            }
            else
               PC_ID[k] = COLOUR[k].pixel ;
         }

         for ( k = 10; k<15 ; k++ ) {						/* Starman greys */
            COLOUR[k].flags = flags;
            ka = k - 10;
            j = (short) (sgr[ka]*65535.0) ;
            COLOUR[k].red = j;
            j = (short) (sgg[ka]*65535.0) ;
            COLOUR[k].green = j;
            j = (short) (sgb[ka]*65535.0) ;
            COLOUR[k].blue = j;
            istat = XAllocColor ( VD_ID, CM_ID, &COLOUR[k] );
            if ( istat == 0 ) {
               (void) c_printo ( "ERROR: Colour pixel not allocated" );
               PC_ID[k] = 0;
            }
            else
               PC_ID[k] = COLOUR[k].pixel ;
         }

         for ( k = 15; k<15+NUMDCOL ; k++ ) {					/* Starman grey scale */
            COLOUR[k].flags = flags ;
            j = (short) (( (float) (k-15)/ (float) (NUMDCOL-1))*65535.0) ;
            COLOUR[k].red = j;
            COLOUR[k].green = j;
            COLOUR[k].blue = j;
            istat = XAllocColor ( VD_ID, CM_ID, &COLOUR[k] ) ;
            if ( istat == 0 ) {
               (void) c_printo ( "ERROR: Colour pixel not allocated" );
               PC_ID[k] = 0;
            }
            else
               PC_ID[k] = COLOUR[k].pixel;
         }

         XFlush ( VD_ID );

         (void) dsxc_lutacol () ; 						/* Set up LUT */
         (void) dsxc_lutcol () ;

      }


}



/******************************************************************
 DSXC_VTIM -- Display part of virtual image (Xwindows)

    alan penny           ral                       1990-02-01
*/

dsxc_vtim ( jvxs, jvxe, jvys, jvye, kcl )

    int    jvxs;           /* i: X start of virtual area to display */
    int    jvxe;           /* i: X   end of virtual area to display */
    int    jvys;           /* i: Y start of virtual area to display */
    int    jvye;           /* i: Y   end of virtual area to display */
    int    kcl;            /* i: Flag to clear screen before (0=no) */

/* C-- */
{

      int j, isxs, isxe, isys, isye, ksxs, ksxe, ksys, ksye,
             ivxs, ivxe, ivys, ivye, kvxs, kvxe, kvys, kvye;
      long nn;
      char *ptr;
      char texta[100];
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;

      ivxe = imax( jvxs, jvxe ); 					/* Get +ve version of area*/
      ivxs = imin( jvxs, jvxe );
      ivye = imax( jvys, jvye );
      ivys = imin( jvys, jvye );

      if ( ivxe<1 || ivxs>DSSNX || ivye<1 || ivys>DSSNY ) {		/* Check in virtual image */
         if ( kcl != 0 ) (void) dsxc_clear();				/* If told to, clear */
         return;
      }

      (void) vtc_tvs ( ivxs, ivys, &isxs, &isys );  			/*Get screen positions */
      isye = isye + (DSZM-1);
      (void) vtc_tvs ( ivxe, ivye, &isxe, &isye );
      isxe = isxe + (DSZM-1);

      if ( (isxe+DSZM-1)<1 || isxs>DSSNX || 				/* Check in screen image */
           (isye+DSZM-1)<1 || isys>DSSNY ) {
         if ( kcl != 0 ) (void) dsxc_clear();				/* If told to, clear */
         return;
      }

      kvxs = imax(1,ivxs);                    				/* Get area to display on screen*/
      kvys = imax(1,ivys);
      kvxe = imin(DSSNX,ivxe);
      kvye = imin(DSSNY,ivye);
      (void) vtc_tvs ( kvxs, kvys, &ksxs, &ksys );
      (void) vtc_tvs ( kvxe, kvye, &ksxe, &ksye );
      ksxe = ksxe + (DSZM-1);
      ksye = ksye + (DSZM-1);

      if ( ksys<1 ) {
         ksys = 1;
         (void) vtc_tsv ( 0, ksys, &j, &kvys );
      }

      if ( ksye>DSSNY ) {
         ksye = DSSNY;
         (void) vtc_tsv ( 0, ksye, &j, &kvye );
      }

      if ( ksxs<1 ) {
         ksxs = 1;
         (void) vtc_tsv ( ksxs, 0, &kvxs, &j );
      }

      if ( ksxe>DSSNX ) {
         ksxe = DSSNX;
         (void) vtc_tsv ( ksxe, 0, &kvxe, &j );
      }

      if ( DSZM==1 && kvxs==1 && kvxe==DSSNX && kvys==1 		/* Display whole vitual image */
           && kvye==DSSNY ) {
         nn = 1;
         (void) dsxc_vtima  ( ptr, nn, kcl, ksxs, ksxe,
                              ksys, ksye, kvxs, kvxe, kvys, kvye );
      } else {
         nn = DSZM*DSZM*(kvxe-kvxs+1)*(kvye-kvys+1);

         if ( nn<=0 ) {
         (void) sprintf ( texta,
            "ERROR:  %d size workspace asked in - s/r dsxc_vtim", nn);
            (void ) c_printo ( texta );
            return;
         }
         ptr = calloc ( nn, sizeof(char) );
         if ( ptr==NULL ) {
            (void ) c_printo (
                    "ERROR: cant get workspace - in s/r dsxc_vtim" );
            return;
         }
         (void) dsxc_vtima ( ptr, nn, kcl, ksxs, ksxe, ksys, ksye,
                                kvxs, kvxe, kvys, kvye );
         free ( ptr);
      }


}




/******************************************************************
  DSXC_VTIMA -- Display part of virtual image (Xwindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 jan
*/

dsxc_vtima ( vt, nn, kcl, ksxs, ksxe, ksys, ksye, kvxs, kvxe, kvys, kvye )

    char vt[];		/* o: Work area */
    long nn; 		/* i: size of work area */
    int kcl;		/* i: Flag to clear screen before display  (0=no) */
    int ksxs;		/* i: X start of screen area */
    int ksxe;		/* i: X   end of screen area */
    int ksys;		/* i: Y start of screen area */
    int ksye;		/* i: Y   end of screen area */
    int kvxs;		/* i: X start of virtual area to display */
    int kvxe;		/* i: X   end of virtual area to display */
    int kvys;		/* i: Y start of virtual area to display */
    int kvye;		/* i: Y   end of virtual area to display */

/* C-- */
{
      XImage    *image;
      int       k, kx, ky, jkx, jky;
      unsigned int  ukx, uky;
      char *ipd;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;

      if ( kcl != 0 && ( ksxs > 1 || ksxe < DSSNX || 			/* If not fill screen and told to, clear first */
           ksys > 1 || ksye < DSSNY ) ) (void) dsxc_clear();

      kx = ksxe - ksxs + 1;
      ky = ksye - ksys + 1;
      ukx = kx;
      uky = ky;

      jkx = ksxs - 1;
      jky = DSSNY - ksye;

      if ( (DSZM==1) && (kvxs==1) && (kvxe==DSSNX) &&			/* Display whole vitual image */
           (kvys==1) && (kvye==DSSNY) ) {
         ipd = malloc ( kx*ky );
         for ( k=0;k<kx*ky;k++ )
            ipd[k] = VT_IM[k];
      } else {
         (void) dsxc_vtload ( vt, nn, kvxs, kvxe, kvys, kvye, ksxs );	/* Load array */

         ipd = malloc ( kx*ky );
         for ( k=0; k<kx*ky; k++ )
            ipd[k] = vt[k];
      }

      image = XCreateImage ( VD_ID, D_VISUAL, DEPTH, ZPixmap, 0,	/* Point to image */
                             ipd, ukx, uky, 8, kx);
      if ( image == NULL ) {
         (void) c_printo (
                   "ERROR: Could not get image - in s/r dsxc_vtima" );
         return;
      }

      XPutImage ( VD_ID, PIXMAP,GC_ID, image, 0, 0, jkx, jky, ukx, 	/* Put image in pixmap */
                  uky );
      XCopyArea ( VD_ID, PIXMAP, WD_ID, GC_ID, jkx, jky, ukx, uky, 	/* Put pixmap area on screen */
                  jkx, jky );

      XDestroyImage ( image );
      XFlush ( VD_ID );
      free (ipd);


}

/***********************************************************************
 DSXC_VTLOAD -- Load part of virtual image into a zoomed array (Xwindows)

 Output array sides are size of input part array times the zoom factor.
 Output array starts at [0,0].

 The area is defined in the standard 'virtual' array - the 'VT_IM'
 input and 'vt' output are Y-reversed from this.

 If the 'right' of the new (perhaps enlarged) array lies
 outside the screen, then the array is filled up only to the edge.

 The output array is usually sized so that it is big enough
 to hold the whole zoomed aray.

 alan penny           ral                       1991 Aug
*/

dsxc_vtload ( vt, nn, kvxs, kvxe, kvys, kvye, ksxs )

    int   nn;			/* i: Size of work array */
    char  vt[];			/* o: Array to put image into */
    int   kvxs;			/* i: X start of virtual area */
    int   kvxe;			/* i: X   end of virtual area */
    int   kvys;			/* i: Y start of virtual area */
    int   kvye;			/* i: Y   end of virtual area */
    int   ksxs;			/* i: X screen start of output array */

/* C-- */
{
      int  j, k, jj, kk, ja, ka, kxd, kxf, kyd, kyf,
           kxs, kxe, kys, kye;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;

      kxs = kvxs - 1;						/* VT_IM and vt coordinates */
      kxe = kvxe - 1;
      kys = DSSNY - kvye;
      kye = DSSNY - kvys;

      if ( DSZM == 1 ) {

         ka = 0;						/* Unzoomed is easy */
         for ( j = kys; j <= kye; j++ ) {
             ja = j*DSSNX;
             for ( k = kxs; k <= kxe; k++ ) {
                 vt[ka] = VT_IM[ja+k];
                 ka = ka + 1;
             }
         }

      } else {

         kxf = DSZM;						/* Fraction of zoomed pixel */
         kxd = ksxs + (kvxe-kvxs+1)*DSZM - 1;			/* at right edge */
         if ( kxd>DSSNX ) kxf = DSZM - (kxd-DSSNX);

         ka = 0;
         for ( j = kys; j <= kye; j++ ) {			/* Y rows */
            ja = j*DSSNX;

            for ( jj=1; jj <= DSZM; jj++ ) {
               if ( kxs != kxe ) {				/* 1st N-1 X pixels */
                  for ( k = kxs; k <= kxe-1; k++ ) {
                     for ( kk = 1; kk<= DSZM; kk++ ) {
                        vt[ka] = VT_IM[ja+k];
                        ka = ka + 1;
                     }
                  }
               }
               if ( kxf >= 1 ) {
                  for ( kk = 1; kk <= kxf; kk++ ) {		/* Last X pixel (or fraction) */
                     vt[ka] = VT_IM[ja+kxe];
                     ka = ka + 1;
                  }
               }
            }

         }

      }


}




/***********************************************************************
        DSXC_CLOSE -- Close window (XWindows)

  alan penny             ral          1990 jan
  pat morris             leeds        1992 jan
*/

dsxc_close ( ierr )

    int    *ierr;		/* o: Error flag (0=ok;1=bad) */

/* C-- */
{
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSWINDX = F77_NAMED_COMMON(ds_gen).dswindx;
      DSOPEN = F77_NAMED_COMMON(ds_genb).dsopen;
      PDSOPEN = F77_NAMED_COMMON(ds_panelb).pdsopen;

      *ierr = 0;						/* Cant fail! */

      if (!DSOPEN) return;

      DSWINDX = DSWINDX - DSSNX - 20;

      XUnmapWindow ( VD_ID, WD_ID );
      XDestroyWindow ( VD_ID, WD_ID );
      XFreePixmap ( VD_ID, PIXMAP );
      if ( !PDSOPEN ) XCloseDisplay ( VD_ID );
      XFlush ( VD_ID );

      DSOPEN = False;
      DSSCUR = False;

      F77_NAMED_COMMON(ds_genb).dsopen = F77_FALSE;	/* Display not open */
      F77_NAMED_COMMON(ds_genb).dsscur = F77_FALSE;
      F77_NAMED_COMMON(ds_gen).dswindx = DSWINDX;


}


/***********************************************************************
   DSXC_PTTIT -- Put image title on display (XWindows)

    alan penny                ral              1990-01-31
  pat morris             leeds        1992 jan
*/

dsxc_pttit ( title )

    char       *title;	/*   i: Title to put up */

/* C-- */
{
/* Cbegin */


    XStoreName ( VD_ID, WD_ID, title );


}



/**************************************************************************
        DSXC_ERASE -- Erase display (XWindows)

    a j penny                    ral         1990 jan
*/

dsxc_erase ()

/* C-- */
{
      int k;
      unsigned int udssnx, udssny;
      XImage *image;
      char kbya;
      char *ipd;
/* Cbegin */


      DSOPEN = F77_NAMED_COMMON(ds_genb).dsopen;
      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;

      if (!DSOPEN) return;

      udssnx = DSSNX;
      udssny = DSSNY;
      kbya = BY_PC_ID[9+4];

      ipd = malloc (DSSNX*DSSNY);				/* Clear virtual screen */
      for ( k=0 ; k<(DSSNX*DSSNY) ; k++ ) {
         ipd[k] = kbya;
         VT_IM[k] = kbya;
      }

      image = XCreateImage ( VD_ID, D_VISUAL, DEPTH, ZPixmap, 0,	/* Point to image */
                             ipd, udssnx, udssny, 8, DSSNX );
      XPutImage ( VD_ID, PIXMAP, GC_ID, image, 0, 0, 0, 0, udssnx,	/* Put image in pixmap */
                  udssny );
      XCopyArea ( VD_ID, PIXMAP, WD_ID, GC_ID, 0, 0, udssnx, udssny, 	/* Put pixmap area on screen */
                  0, 0 );
      XDestroyImage ( image );
      XFlush ( VD_ID );
      free (ipd);


}


/************************************************************
   DSXC_CLEAR -- Clear screen (XWindows)

    a j penny                    ral         1990 jan
    patrick morris               leeds       1992 jan
*/

dsxc_clear (void)

/* C-- */
{
      int k;
      unsigned int udssnx, udssny;
      XImage  *image;
      char kbya;
      char *ipd;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;

      udssnx = DSSNX;
      udssny = DSSNY;
      kbya = BY_PC_ID[13];

      k = (DSSNX*DSSNY)+1;
      ipd = calloc( k, sizeof(char) );			/* Get workspace */
      if ( ipd == NULL ) {
         (void) c_printo ( "Error:Cant allocate memory in dsxc_clear" );
         return;
      }
      for ( k=0 ; k<DSSNX*DSSNY ; k++ )		/* Clear temp virtual screen */
          ipd[k] = kbya;

      image = XCreateImage ( VD_ID, D_VISUAL, DEPTH, ZPixmap, 0, 	/* Point to image */
                                     ipd, udssnx, udssny, 8, DSSNX );
      XPutImage ( VD_ID, PIXMAP, GC_ID, image, 0, 0, 0, 0, udssnx,	/* Put image in pixmap */
                  udssny );
      XCopyArea ( VD_ID, PIXMAP, WD_ID, GC_ID, 0, 0, udssnx, udssny, 	/* Put pixmap area on screen */
                  0, 0 );
      XDestroyImage ( image );
      XFlush ( VD_ID );
      free (ipd);



}

/***************************************************************
  DSXC_CLEARPIX -- Clear pixmap (XWindows)

    a j penny                    ral         1990 jan
    patrick morris               leeds       1992 feb
*/

dsxc_clearpix (void)

/* C-- */
{
      int k;
      unsigned int udssnx, udssny;
      XImage *image;
      char kbya;
      char *ipd;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;

      udssnx = DSSNX;
      udssny = DSSNY;
      kbya = BY_PC_ID[13];

      k = (DSSNX*DSSNY)+1 ;
      ipd = calloc( k, sizeof(char) );		/* Get workspace */
      if ( ipd == NULL ) {
         (void) c_printo ( "Error:Cant allocate memory in dsxc_clearpix" );
         return;
      }
      for ( k=0 ; k<DSSNX*DSSNY ; k++ )		/* Clear temp virtual screen */
          ipd[k] = kbya;

      image = XCreateImage ( VD_ID, D_VISUAL, DEPTH, ZPixmap, 0,	/* Point to image */
                                     ipd, udssnx, udssny, 8, DSSNX );
      XPutImage ( VD_ID, PIXMAP, GC_ID, image, 0, 0, 0, 0, udssnx, 	/* Put image in pixmap */
                  udssny );
      XDestroyImage ( image );
      XFlush ( VD_ID );
      free (ipd);


}


/*********************************************************************
        DSXC_SCUR -- Start the cursor (XWindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/

dsxc_scur (void)

/* C-- */
{
      int  kxi, kyi, kxo, kyo, jxo, jyo;
      float xo, yo;
      Cursor cursor;
      Pixmap   cursor_pixmap;
      static unsigned char cursor_bits[32] = {
                              0x80, 0x00, 0x80, 0x00,
                              0x80, 0x00, 0x80, 0x00,
                              0x80, 0x00, 0x80, 0x00,
                              0x80, 0x00, 0x80, 0x00,
                              0xff, 0xff, 0x80, 0x00,
                              0x80, 0x00, 0x80, 0x00,
                              0x80, 0x00, 0x80, 0x00,
                              0x80, 0x00, 0x80, 0x00 } ;
      XColor cursor_dummy;
      XColor cursor_foreground;
      XColor cursor_background;
/* Cbegin */


      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSNXS = F77_NAMED_COMMON(ds_gen).dsnxs;
      DSNXE = F77_NAMED_COMMON(ds_gen).dsnxe;
      DSNYS = F77_NAMED_COMMON(ds_gen).dsnys;
      DSNYE = F77_NAMED_COMMON(ds_gen).dsnye;

      cursor_pixmap = XCreatePixmapFromBitmapData ( VD_ID, RW_ID,
                                        cursor_bits, 16, 16, 1, 0, 1 );
      XLookupColor ( VD_ID, DefaultColormap(VD_ID, SC_ID),
                           "red", &cursor_dummy, &cursor_foreground );
      XLookupColor ( VD_ID, DefaultColormap(VD_ID, SC_ID),
                           "black", &cursor_dummy, &cursor_background );
      cursor = XCreatePixmapCursor ( VD_ID, cursor_pixmap, cursor_pixmap,
                               &cursor_foreground, &cursor_background, 7, 7 );
				/* Use same bitmap for shape and mask
   				 so cursor background is transparent */

      XDefineCursor ( VD_ID, WD_ID, cursor );
      XFreePixmap ( VD_ID, cursor_pixmap );

      kxi = (DSNXS+DSNXE)/2;				/*Set pointer position*/
      kyi = (DSNYS+DSNYE)/2;

      (void) vtc_tis ( kxi, kyi, &kxo, &kyo );

      jxo = kxo - 1;
      jyo = DSSNY - kyo;
      XWarpPointer ( VD_ID, None, WD_ID,  0, 0, 0, 0, jxo, jyo );
      XFlush ( VD_ID );

      xo = kxo;
      yo = kyo;

      DSCURPOSX = xo;
      DSCURPOSY = yo;
      DSSCUR = True;

      F77_NAMED_COMMON(ds_gen).dscurposx = xo;
      F77_NAMED_COMMON(ds_gen).dscurposy = yo;
      F77_NAMED_COMMON(ds_genb).dsscur = F77_TRUE;

}


/**********************************************************************
        DSXC_PCUR -- Put the cursor at position (XWindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/

dsxc_pcur ( kx, ky )

    int   kx;		/*i: X position to put cursor at */
    int   ky;		/*i: Y position to put cursor at */

/* C-- */
{
      int  kxo, kyo, jxo, jyo;
      float     xo, yo;
/* Cbegin */


      DSOPEN = F77_NAMED_COMMON(ds_genb).dsopen;
      DSSCUR = F77_NAMED_COMMON(ds_genb).dsscur;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;

      if (!DSOPEN) return;
      if (!DSSCUR) (void) dsxc_scur ();

      (void) vtc_tis ( kx, ky, &kxo, &kyo );
      xo = kxo;
      yo = kyo;

      jxo = kxo - 1;
      jyo = DSSNY - kyo;
      XWarpPointer ( VD_ID, None, WD_ID,  0, 0, 0, 0, jxo, jyo );

      XFlush ( VD_ID );

      DSCURPOSX = xo;
      DSCURPOSY = yo;
      DSCURSET = True;

      F77_NAMED_COMMON(ds_gen).dscurposx = xo;
      F77_NAMED_COMMON(ds_gen).dscurposy = yo;
      F77_NAMED_COMMON(ds_genb).dscurset = F77_TRUE;

}


/**********************************************************************
  DSXC_PSCUR -- Put the cursor at screen position (XWindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/

dsxc_pscur ( x, y )

    float      x;		/* i: X position to put cursor at */
    float      y;		/* i: Y position to put cursor at */

/* C-- */
{
      int kx, ky;
/* Cbegin */


      DSOPEN = F77_NAMED_COMMON(ds_genb).dsopen;
      DSSCUR = F77_NAMED_COMMON(ds_genb).dsscur;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;

      if (!DSOPEN) return;
      if (!DSSCUR) (void) dsxc_scur ();

      kx = x - 1;
      ky = DSSNY - y;
      XWarpPointer ( VD_ID, None, WD_ID,  0, 0, 0, 0, kx, ky );

      XFlush ( VD_ID );

      DSCURPOSX = x;
      DSCURPOSY = y;
      DSCURSET = True;

      F77_NAMED_COMMON(ds_gen).dscurposx = x;
      F77_NAMED_COMMON(ds_gen).dscurposy = y;
      F77_NAMED_COMMON(ds_genb).dscurset = F77_TRUE;


}



/*******************************************************************
  DSXC_GETCURPB -- Get cursor posn and button state (XWindows)

   alan penny                  ral                        1990-02-03
   pat morris             leeds        1992 jan
*/

dsxc_getcurpb ( isimage, kx, ky, kb, ierr )

Bool   isimage;          /* i: Image (T) or panel (F) ? */
int    *kx;              /* o: Cursor X position (start at 1,1) */
int    *ky;              /* o: Cursor Y position (start at 1,1) */
int    kb[];             /* o: Cursor Button states (0=up;1=down) */
int    *ierr;            /* o: Error flag ( 0=ok; 1=bad) */
/* C-- */
{
      int n, kwx, kwy, jkx, jky;
      Window win, root, child;
      XEvent event;
      XWindowAttributes  xwattr;                /* Window Attributes */
      unsigned int kbstat;
      Bool istat, loop;
/* Cbegin */


      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      PNSNY = F77_NAMED_COMMON(ds_panel).pnsny;
      NUMXBUTTONS = F77_NAMED_COMMON(ds_gen).numxbuttons;

      *ierr = 0;

      if ( isimage )
         win = WD_ID;
      else
         win = WD_P_ID;

      XGetWindowAttributes ( VD_ID, win, & xwattr );
      if ( xwattr.map_state != IsViewable ) {
         *kx = 0 ;
         *ky = 0 ;
         kb[0] = 0 ;
         kb[1] = 0 ;
         kb[2] = 0 ;
         return;
      }

      istat = XQueryPointer ( VD_ID, win, &root, &child, &kwx, &kwy,
                              &jkx, &jky, &kbstat );
      XFlush ( VD_ID );

      jkx = jkx + 1;
      if ( isimage )
         jky = DSSNY - jky;
      else
         jky = PNSNY - jky;

      *kx = jkx;
      *ky = jky;
      if ( istat!=1 ) *ierr = 1;

      kb[0] = (kbstat-512*(kbstat/512))/256;
      kb[1] = (kbstat-1024*(kbstat/1024))/512;
      kb[2] = (kbstat-2048*(kbstat/2048))/1024;

      if ( *ierr == 0 && NUMXBUTTONS==2 && kb[1]==1 ) {
         (void) dsxc_mswait ( 400 );
         istat = XQueryPointer ( VD_ID, win, &root, &child, &kwx, &kwy,
                                 &jkx, &jky, &kbstat );
         XFlush ( VD_ID );

         jkx = jkx + 1;
         if ( isimage )
             jky = DSSNY - jky;
         else
             jky = PNSNY - jky;

         *kx = jkx;
         *ky = jky;
         if ( istat!=1 ) *ierr = 1;

         kb[0] = (kbstat-512*(kbstat/512))/256;
         kb[1] = (kbstat-1204*(kbstat/1024))/512;

         if ( kb[0]==1 && kb[1]==1 ) {
            kb[0] = 0;
            kb[1] = 0;
            kb[2] = 1;
         }
      }


}


/***********************************************************
  DSXC_WAITBUT -- Wait for button to be pressed or to be up (Xwindows)

   alan penny                  ral                        1990-02-03
  pat morris             leeds        1992 jan
*/


dsxc_waitbut ( isimage, down, kbut, kpx, kpy )

Bool  isimage;          /* i: Looking in the image (T) or panel (F)? */
Bool  down;		/* i: Flag to wait till button pressed
			    (True) or all up (False) */
int   *kbut;		/* o: Which button pressed */
int   *kpx;		/* o: X screen position */
int   *kpy;		/* o: Y screen position */
/* C-- */
{
      int kb[3], istat, kpxa, kpya;
      Bool loop;
/* Cbegin */


      loop = True;
      while ( loop ) {
         (void) dsxc_mswait ( 20 );
         (void) dsxc_getcurpb ( isimage, &kpxa, &kpya, kb, &istat );

         if ( istat == 0 ) {
           if ( down ) {
              if ( (kb[0]==1) || (kb[1]==1) || (kb[2]==1) ) loop = False;
              if ( kb[0]==1 ) *kbut = 1;
              if ( kb[1]==1 ) *kbut = 2;
              if ( kb[2]==1 ) *kbut = 3;
           }
           else {
              *kbut = 0 ;
              if ( (kb[0]==0) && (kb[1]==0) && (kb[2]==0) ) loop= False;
           }
         }
      }

      *kpx = kpxa;
      *kpy = kpya;

}

/**************************************************************
    DSXC_PUTIM -- Put an image (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 jan
*/

void dsxc_putim ( data, kx, ky, ksxs, ksys )

    char  *data;		/* i: Input data */
    int   kx;			/* i: X size of data */
    int   ky;			/* i: Y size of data */
    int   ksxs;			/* i: X screen start of data position */
    int   ksys;			/* i: Y screen start of data position */

/* C-- */
{
      int k, jkx, jky;
      unsigned int ukx, uky;
      XImage *image;
      char *ipd;
/* Cbegin */


      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      ukx = kx;
      uky = ky;

      ipd = malloc (kx*ky);
      for ( k=0;k<kx*ky;k++ )
          ipd[k] = data[k];

      image = XCreateImage ( VD_ID, D_VISUAL, DEPTH, ZPixmap, 0, ipd,	/* Point to image */
                             ukx, uky, 8, kx );
      XFlush ( VD_ID );

      jkx = ksxs - 1;
      jky = DSSNY - (ky-1) - ksys;
      XPutImage ( VD_ID, PIXMAP, GC_ID, image, 0, 0, jkx, jky, ukx, uky ); 	/* Put image in pixmap */
      XCopyArea ( VD_ID, PIXMAP, WD_ID, GC_ID, jkx, jky, ukx, uky, jkx, jky );	/* Put pixmap area on screen */
      XDestroyImage ( image );
      XFlush ( VD_ID );
      free ( ipd);


}


/************************************************************************
     DSXC_PAINTOVA -- Paint moving oval  at X/Y normals - erase last (Xwindows)

  alan penny             ral             1990 Jan
  pat morris             leeds        1992 jan
*/

dsxc_paintova ( pxo, pyo, rxo, ryo, px, py, rx1, ry1, kc1, rx2, ry2, kc2 )

    float   pxo;	/* i: Old screen X posn */
    float   pyo;	/* i: Old screen Y posn */
    float   rxo;	/* i: Old max X oval radius (image scale) */
    float   ryo;	/* i: Old max Y oval radius (image scale) */
    float   px;		/* i: New screen X posn */
    float   py;		/* i: New screen Y posn */
    float   rx1;	/* i: Inner X oval radius (image scale) */
    float   ry1;	/* i: Inner Y oval radius (image scale) */
    int     kc1;	/* i: Inner oval colour (1-8) */
    float   rx2[];	/* i: Inner/outer annular X oval radii */
    float   ry2[];	/* i: Inner/outer annular Y oval radii */
    int     kc2;	/* i: Outer oval colour (1-8) */

/* C-- */
{
      int      kvxs, kvxe, kvys, kvye, kvx, kvy, kpxo, kpyo,
               jx, jy, jxs, jys, jxe, jye;
      unsigned int  krx, kry;
      float    rx, ry, pxi, pyi;
/* Cbegin */



      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;
      DSCOMFX = F77_NAMED_COMMON(ds_gen).dscomfx;
      DSCOMFY = F77_NAMED_COMMON(ds_gen).dscomfy;

      kpxo = pxo;
      kpyo = pyo;
      (void) vtc_tsv ( kpxo, kpyo, &kvx, &kvy ); 			/* Get old image pos */

      kvxs = kvx - (int) ((rxo/DSCOMFX)+6.0);				/* Restore old area */
      kvxe = kvxs + (int) ((2.0*rxo/DSCOMFX)+12.0);
      kvys = kvy - (int) ((ryo/DSCOMFY)+6.0);
      kvye = kvys + (int) ((2.0*ryo/DSCOMFY)+12.0);
      (void) dsxc_vtim ( kvxs, kvxe, kvys, kvye, 0 );

      if ( kc1!=-1 ) {							/* Paint new oval */
         pxi = px + (DSZM/2);
         pyi = py + (DSZM/2);
         (void) dsxc_setthecol ( kc1 );

         rx = (float) (DSZM) * rx1/ (float) (DSCOMFX);
         ry = (float) (DSZM) * ry1/ (float) (DSCOMFY);
         krx = 2.0*rx;
         kry = 2.0*ry;
         jx = pxi - rx - 1;
         jy = DSSNY - pyi - ry;
         XDrawArc ( VD_ID, WD_ID, GC_ID, jx, jy, krx, kry, 0, 64*360 );
      }

      if ( kc2!=-1 ) {							/* Paint new oval */
         pxi = px + (DSZM/2);
         pyi = py + (DSZM/2);
         (void) dsxc_setthecol ( kc2 );

         rx = (float) (DSZM) * rx2[0]/ (float) (DSCOMFX);
         ry = (float) (DSZM) * ry2[0]/ (float) (DSCOMFY);
         krx = 2.0*rx;
         kry = 2.0*ry;
         jx = pxi - rx - 1;
         jy = DSSNY - pyi - ry;
         XDrawArc ( VD_ID, WD_ID, GC_ID, jx, jy, krx, kry, 0, 64*360 );

         rx = (float) (DSZM)* rx2[1]/(float) (DSCOMFX);
         ry = (float) (DSZM)* ry2[1]/(float) (DSCOMFY);
         krx = 2.0*rx;
         kry = 2.0*ry;
         jx = pxi - rx - 1;
         jy = DSSNY - pyi - ry;
         XDrawArc ( VD_ID, WD_ID, GC_ID, jx, jy, krx, kry, 0, 64*360 );

         XFlush ( VD_ID );
      }

}


/************************************************************************
     DSXC_PAINTOVB -- Paint moving oval (XY) - erase last (Xwindows)

  alan penny             ral             1990 Jan
  pat morris             leeds        1992 jan
*/

dsxc_paintovb ( kcx, kcy, kc1, kc2, pxo, pyo, rxo, ryo )


    int     kcx[];	/* i: Centre/Inner/Outer oval X posns (image scale) */
    int     kcy[];	/* i: Centre/Inner/Outer oval Y posns (image scale) */
    int     kc1;	/* i: Inner oval colour (1-8) */
    int     kc2;	/* i: Outer oval colour (1-8) */
    float   pxo;        /* i: Old screen X posn */
    float   pyo;        /* i: Old screen Y posn */
    float   rxo;	/* i: Old max X oval radius (image scale) */
    float   ryo;	/* i: Old max Y oval radius (image scale) */

/* C-- */
{
      int      kvxs, kvxe, kvys, kvye, kvx, kvy, kpxo, kpyo,
               kpxi, kpyi, k;
      XPoint   theline[185];
/* Cbegin */


      kpxo = pxo;
      kpyo = pyo;
      (void) vtc_tsv ( kpxo, kpyo, &kvx, &kvy ); 			/* Get old image pos */

      kvxs = kvx - (int) ((rxo/DSCOMFX)+6.0);				/* Restore old area */
      kvxe = kvxs + (int) ((2.0*rxo/DSCOMFX)+12.0);
      kvys = kvy - (int) ((ryo/DSCOMFY)+6.0);
      kvye = kvys + (int) ((2.0*ryo/DSCOMFY)+12.0);
      (void) dsxc_vtim ( kvxs, kvxe, kvys, kvye, 0 );
      XFlush ( VD_ID );

      if ( kc1!=-1 ) {							/* Paint new oval */
         (void) dsxc_setthecol ( kc1 );
         for ( k=0; k<=180; k++ ) {
            theline[k].x = kcx[k];
            theline[k].y = DSSNY - kcy[k];
         }
         XDrawLines ( VD_ID, WD_ID, GC_ID, theline, 181, CoordModeOrigin );
         XFlush ( VD_ID );
      }

      if ( kc2!=-1 ) {							/* Paint new oval */
         (void) dsxc_setthecol ( kc2 );

         for ( k=0; k<=180; k++ ) {
            theline[k].x = kcx[181+k];
            theline[k].y = DSSNY - kcy[181+k];
         }
         XDrawLines ( VD_ID, WD_ID, GC_ID, theline, 181, CoordModeOrigin );
         for ( k=0; k<=180; k++ ) {
            theline[k].x = kcx[362+k];
            theline[k].y = DSSNY - kcy[362+k];
         }
         XDrawLines ( VD_ID, WD_ID, GC_ID, theline, 181, CoordModeOrigin );

         XFlush ( VD_ID );
      }


}


/********************************************************************
     DSXC_OVAL -- Paint an oval (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 jan
*/

dsxc_oval ( xp, yp, radx, rangle, elli, kc )

    float     xp;		/* i: Image X position */
    float     yp;		/* i: Image Y position */
    float     radx;		/* i: Image pixel X radius */
    float     rangle;		/* i: Oval angle to X-axis */
    float     elli;		/* i: Oval ellipticity */
    int       kc;		/* i: Colour (1=red;2=green;3=blue;4=yellow;
				           5=cyan;6=mauve;7=tan;8=pink)
				           9=black;10=white) */
/* C-- */
{
      float angle;
      int k;
/* Cbegin */


      DSZM = F77_NAMED_COMMON(ds_gen).dszm;

      if ( radx<=0.0 || elli<0.0 || elli>1.0 ) return ;

      angle = rangle;
      if ( rangle<0.0 ) angle = -1.0*rangle;
      k = angle/180.0;
      angle = angle - 180.0*(float)k;
      if ( angle>90.0 ) angle = angle - 180.0;
      if ( rangle<0.0 ) angle = -1.0*angle;

      (void) dsxc_setthecol ( kc );

      if ( angle==0.0 || angle==90.0 || angle==-90.0 || elli==0.0 ) {
         (void) dsxc_ovala ( xp, yp, radx, angle, elli, kc );
      } else {
         (void) dsxc_ovalb ( xp, yp, radx, angle, elli, kc );
      }


}


/********************************************************************
     DSXC_OVALA -- Paint an oval at X/Y normals (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 jan
*/

dsxc_ovala ( xp, yp, radx, angle, elli, kc )

    float     xp;		/* i: Image X position */
    float     yp;		/* i: Image Y position */
    float     radx;		/* i: Image pixel X radius */
    float     angle;		/* i: Oval angle to X-axis */
    float     elli;		/* i: Oval ellipticity */
    int       kc;		/* i: Colour (1=red;2=green;3=blue;4=yellow;
				           5=cyan;6=mauve;7=tan;8=pink)
				           9=black;10=white) */
/* C-- */
{
      float rx, ry, da, rxx, ryy, dx1, dy1, rx1, ry1, tt, sdx[5], sdy[5];
      int j, k, nxv, nyv, kkcol, kp, jx, jy, krv, krva, ivx, ivy,
          kvx, kvy, kxp, kyp, kpx, kpy, kpxi, kpyi, krx, kry, ns;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;
      DSCOMFX = F77_NAMED_COMMON(ds_gen).dscomfx;
      DSCOMFY = F77_NAMED_COMMON(ds_gen).dscomfy;

      kxp = xp;
      kyp = yp;
      (void) vtc_tis ( kxp, kyp, &kpx, &kpy );				/* Get screen pos, rad */
      kpx = kpx + (DSZM/2);
      kpy = kpy + (DSZM/2);

      if ( angle==0.0 ) {
         rx = radx;
         ry = radx*(1.0-elli);
      } else if ( elli==0.0) {
         rx = radx;
         ry = radx;
      } else {
         rx = radx*(1.0-elli);
         ry = radx;
      }
      rx = (float) (DSZM)*(rx)/(float) (DSCOMFX);
      ry = (float) (DSZM)*(ry)/(float) (DSCOMFY);

      krx = rx;								/* Paint oval */
      kry = ry;
      jx = kpx - rx - 1;
      jy = DSSNY - kpy - ry;
      XDrawArc ( VD_ID, WD_ID, GC_ID, jx, jy, 2*krx, 2*kry, 0, 64*360 );

      (void) vtc_tsv ( kpx, kpy, &kvx, &kvy );				/*Put oval in virtual memory */
      ivx = kpx + (int) rx;
      ivy = kpy + (int) ry;
      (void) vtc_tsv ( ivx, ivy, &krv, &krva );
      rx1 = krv - kvx;
      ry1 = krva - kvy;

      (void) dsxc_scolnum ( kc, &kkcol );

      if ( radx<=0.0 ) {
         nxv = kvx;
         nyv = kvy;
         kp = nxv + (DSSNY-nyv)*DSSNX;
         if ( (nxv>=1) && (nxv<=DSSNX) && (nyv>=1) && (nyv<=DSSNY) ) {
            VT_IM[kp-1] = (char) kkcol;
         }
      } else {
         nxv = kvx + rx1;
         nyv = kvy;
         kp = nxv + (DSSNY-nyv)*DSSNX;
         if ( (nxv>=1) && (nxv<=DSSNX) && (nyv>=1) && (nyv<=DSSNY) ) {
            VT_IM[kp-1] = (char) kkcol;
         }

         sdx[1] = 1.0;
         sdx[2] = 1.0;
         sdx[3] = -1.0;
         sdx[4] = -1.0;
         sdy[1] = 1.0;
         sdy[2] = -1.0;
         sdy[3] = -1.0;
         sdy[4] = 1.0;

         da = 0.3/rx1;
         ns = 3.14159/(2.0*da);
         ns = imin(90,ns);
         da = (3.14159/2.0)/(float) (ns);
         rxx = 1.0/(rx1*rx1);
         ryy = 1.0/(ry1*ry1);
         for ( j=1; j<=4; j++ ) {
            for ( k = 1; k<=ns+1 ; k++ ) {
               tt = tan(da*(float)(k));
               tt = tt*tt;
               dx1 = sqrt(1.0/(rxx+(tt*ryy)));
               dy1 = sqrt(1.0/(ryy+(rxx/tt)));
               nxv = kvx + sdx[j]*dx1;
               nyv = kvy + sdy[j]*dy1;
               kp = nxv + (DSSNY-nyv)*DSSNX;
               if ( (nxv>=1) && (nxv<=DSSNX) && (nyv>=1) && (nyv<=DSSNY) ){
                  VT_IM[kp-1] = (char) kkcol;
               }
            }
         }
      }

      XFlush ( VD_ID );

}


/********************************************************************
     DSXC_OVALB -- Paint an oval at an angle (XWindows)

    alan penny           ral                       1990-02-01
  pat morris             leeds        1992 jan
*/

dsxc_ovalb ( xp, yp, radx, angle, elli, kc )

    float     xp;		/* i: Image X position */
    float     yp;		/* i: Image Y position */
    float     radx;		/* i: Image pixel X radius */
    float     angle;		/* i: Oval angle to X-axis */
    float     elli;		/* i: Oval ellipticity */
    int       kc;		/* i: Colour (1=red;2=green;3=blue;4=yellow;
				           5=cyan;6=mauve;7=tan;8=pink)
				           9=black;10=white) */
/* C-- */
{
      float ry, sav, cav, ryy, trv, ttrv, dx, dy, rv, afx, afy, ax1, ax2,
            ay1, ay2, dfx, dfy, dax[48], day[48], sdx[5], sdy[5],
            f1, f2, f3, f4;
      int   k, kk, kkcol, kp, ka, kcx[185], kcy[185], kcxa[185], kcya[185],
            kpx, kpy, kxp, kyp, nxv, nyv, nxva, nyva;
      XPoint  theline[185];
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;
      DSZPX = F77_NAMED_COMMON(ds_gen).dszpx;
      DSZPY = F77_NAMED_COMMON(ds_gen).dszpy;
      DSCOMFX = F77_NAMED_COMMON(ds_gen).dscomfx;
      DSCOMFY = F77_NAMED_COMMON(ds_gen).dscomfy;

      kxp = xp;
      kyp = yp;
      (void) vtc_tis ( kxp, kyp, &kpx, &kpy );				/* Get screen pos, rad */
      kpx = kpx + (DSZM/2);
      kpy = kpy + (DSZM/2);

      sav = -1.0*sin(angle*3.14159/180.0);
      cav = cos(angle*3.14159/180.0);

      sdx[1] = 1.0;
      sdx[2] = 1.0;
      sdx[3] = -1.0;
      sdx[4] = -1.0;
      sdy[1] = 1.0;
      sdy[2] = -1.0;
      sdy[3] = -1.0;
      sdy[4] = 1.0;

      ry = 1.0 - elli;
      ry = fmin(1.0,fmax(1.0e-6,ry));
      ryy = 1.0/(ry*ry);
      for ( k=1; k<=45; k++ ) {
         rv = ((float)(k-1))*2.0*3.14159/180.0;
         trv = tan(rv);
         ttrv = trv*trv;

         if ( trv>-1.0e-5 && trv<1.0e-5) {
            dax[k] = 0.0;
            day[k] = ry;
         } else if ( trv>1.0e5 || trv<-1.0e5 ) {
            dax[k] = 1.0;
            day[k] = 0.0;
         } else {
            dax[k] = sqrt(1.0/(1.0+(ryy/ttrv)));
            day[k] = sqrt(1.0/(ryy+(1.0*ttrv)));
         }
      }

      afx = (float)DSZM/(float)DSCOMFX;
      afy = (float)DSZM/(float)DSCOMFY;
      dfx = kpx;
      dfy = kpy;

      ax1 = (float) DSZPX;
      ax2 = (float) DSZM;
      ay1 = (float) DSZPY;
      ay2 = (float) DSZM;

      for ( kk=1; kk<=4; kk++ ) {
         ka = (kk-1)*45 - 1;
         f1 = cav*radx*sdx[kk];
         f2 = sav*radx*sdy[kk];
         f3 = -1.0*sav*radx*sdx[kk];
         f4 = cav*radx*sdy[kk];
         for ( k=1; k<=45; k++ ) {
            if ( kk==2 || kk==4 ) {
               dx = f1*dax[46-k] + f2*day[46-k];
               dy = f3*dax[46-k] + f4*day[46-k];
            } else {
               dx = f1*dax[k] + f2*day[k];
               dy = f3*dax[k] + f4*day[k];
            }
            kcx[k+ka] = dx*afx + dfx;
            kcy[k+ka] = dy*afy + dfy;
            kcxa[k+ka] = ax1 + (float) (kcx[k+ka]-1)/ax2;
            kcya[k+ka] = ay1 + (float) (kcy[k+ka]-1)/ay2;
         }
      }
      kcxa[180] = kcxa[0];
      kcya[180] = kcya[0];
      kcx[180] = kcx[0];
      kcy[180] = kcy[0];

      for ( k=0; k<=180; k++ ) {
         theline[k].x = kcx[k];
         theline[k].y = DSSNY - kcy[k];
      }
      XDrawLines ( VD_ID, WD_ID, GC_ID, theline, 181, CoordModeOrigin );

      (void) dsxc_scolnum ( kc, &kkcol );				/* Put in virtual image */

      for ( k = 0; k<=180 ; k++ ) {
         nxv = kcxa[k];
         nyv = kcya[k];
         if ( (nxv>=1) && (nxv<=DSSNX) && (nyv>=1) && (nyv<=DSSNY) ) {
            kp = nxv + (DSSNY-nyv)*DSSNX;
            VT_IM[kp-1] = (char) kkcol;
         }
      }

      XFlush ( VD_ID );

}


/********************************************************************
     DSXC_OSIZE -- X, Y size of tilted oval

    alan penny           ral                       1994
*/

dsxc_osize ( rad, ang, ell, xs, ys )

    float     rad;		/* i: Major axis size */
    float     ang;		/* i: Angle to x-axis in degrees */
    float     ell;		/* i: Ellipticity */
    float     *xs;		/* o: X size (total) */
    float     *ys;		/* o: Y size (total) */

/* C-- */
{
/* Cbegin */


      *xs = 2.0*rad;
      *ys = 2.0*rad;


}


/****************************************************************
    DSXC_CROSS -- Put a cross at a position (XWindows)

  alan penny             ral             1990 Jan
  pat morris             leeds        1992 jan
*/

dsxc_cross ( x, y, crs, kc )

    float      x;		/* i: X position(image pixels) */
    float      y;		/* i: Y position (image pixels) */
    float      crs;		/* i: Cross size (end-to-end) (image pixels) */
    int        kc;		/* i: Colour (1=red;2=green;3=blue;4=yellow;
					      5=cyan;6=mauve;7=tan;8=pink)
					      9=black;10=white) */
/* C-- */
{
      int iv, jx, jxs, jxe, jy, jys, jye, k, kxa, kxb, kya, kyb,
          kkcol, kjs, kje, kx, ky, kpx, kpy, kdx, kdy;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;

      (void) dsxc_setthecol ( kc );

      kx = x;
      ky = y;
      (void) vtc_tis ( kx, ky, &kpx, &kpy );				/* Get screen position*/
      kpx = kpx + (DSZM/2);
      kpy = kpy + (DSZM/2);
      kdx = (float) (DSZM)*(crs/2.0);
      kdy = (float) (DSZM)*(crs/2.0);

      jxs = kpx - kdx - 1;						/* Paint cross  */
      jxe = kpx + kdx - 1;
      jy = DSSNY - kpy;
      XDrawLine ( VD_ID, WD_ID, GC_ID, jxs, jy, jxe, jy );
      XDrawLine ( VD_ID, PIXMAP, GC_ID,	jxs, jy, jxe, jy );
      jx = kpx - 1;
      jys = DSSNY - (kpy-kdy);
      jye = DSSNY - (kpy+kdy);
      XDrawLine ( VD_ID, WD_ID, GC_ID, jx, jye, jx, jys );
      XDrawLine ( VD_ID, PIXMAP, GC_ID, jx, jye, jx, jys );
      XFlush ( VD_ID );

      iv = kpx - kdx;
      (void) vtc_tsv ( iv, kpy, &kxa, &kya );				/*Put in virtual image*/
      iv = kpx + kdx;
      (void) vtc_tsv ( iv, kpy, &kxb, &kya );

      (void) dsxc_scolnum ( kc, &kkcol );

      if( (kxa<=DSSNX) && (kxb>=1) && (kya>=1) && (kya<=DSSNY) ) {
         kxa = imin(DSSNX,imax(1,kxa));
         kxb = imin(DSSNX,imax(1,kxb));
         kya = imin(DSSNY,imax(1,kya));
         kjs = kxa + (DSSNY-kya)*DSSNX;
         kje = kjs + kxb - kxa ;
         for ( k = kjs; k<=kje ; k++ )
            VT_IM[k-1] = (char) kkcol;
      }

      iv = kpy - kdy;
      (void) vtc_tsv ( kpx, iv, &kxa, &kya );
      iv = kpy + kdy;
      (void) vtc_tsv ( kpx, iv, &kxa, &kyb );
      if( (kxa<=DSSNX) && (kxb>=1) && (kya>=1) && (kya<=DSSNY) ) {
         kxa = imin(DSSNX,imax(1,kxa));
         kya = imin(DSSNY,imax(1,kya));
         kyb = imin(DSSNY,imax(1,kyb));
         kje = (DSSNY-kya)*DSSNX + kxa;
         kjs = (DSSNY-kyb)*DSSNX + kxa;
         for ( k = kjs; k<=kje; k += DSSNX )
            VT_IM[k-1] = (char) kkcol;
      }


}


/**********************************************************************
  DSXC_SCOLNUM -- Calc colour pixel number from colour number (XWindows)

  alan penny             ral             1990 May
  pat morris             leeds        1992 jan
*/

dsxc_scolnum ( kc, kkcol )

    int   kc;		/* i: Colour number */
    int   *kkcol;	/* o: Display colour pixel number */

/* C-- */
{
/* Cbegin */


      switch (kc) {
         case 9 :  *kkcol = BY_PC_ID[10];
                   break;
         case 10 : *kkcol = BY_PC_ID[14];
                   break;
         default : *kkcol = BY_PC_ID[1+imin(8,imax(0,kc))];
      }


}


/**********************************************************************
 DSXC_LINE -- Put a line at a position (XWindows)

  alan penny             ral             1990 May
  pat morris             leeds        1992 jan
*/

dsxc_line ( xs, ys, xe, ye, kc )

    float   xs;		/* i: Line start X position(image pixels) */
    float   ys;		/* i: Line start Y position (image pixels) */
    float   xe;		/* i: Line end X position(image pixels) */
    float   ye;		/* i: Line end Y position (image pixels) */
    int     kc;		/* i: Colour (1=red;2=green;3=blue;4=yellow;
			              5=cyan;6=mauve;7=tan;8=pink)
			              9=black;10=white) */

/* C-- */
{
      float ds, axc, ayc, dx, dy, ax, ay;
      int k, kxa, kxb, kya, kyb, kkcol, kx, ky, kn, kp,
          kjs, kje, kt, kxs, kys, kpxs, kpys, kxe, kye, kpxe, kpye,
          jxs, jxe, jys, jye;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;

      (void) dsxc_setthecol ( kc );

      kxs = xs;
      kys = ys;
      (void) vtc_tis ( kxs, kys, &kpxs, &kpys );				/*Get screen position*/
      kxe = xe;
      kye = ye;
      (void) vtc_tis ( kxe, kye, &kpxe, &kpye );				/*Get screen position*/

      kpxs = kpxs + (DSZM/2);
      kpys = kpys + (DSZM/2);

      kpxe = kpxe + (DSZM/2);
      kpye = kpye + (DSZM/2);

      jxs = kpxs - 1;
      jxe = kpxe - 1;
      jys = DSSNY - kpys;
      jye = DSSNY - kpye;
      XDrawLine ( VD_ID,  WD_ID, GC_ID, jxs, jys, jxe, jye );			/* Paint line */
      XDrawLine ( VD_ID, PIXMAP, GC_ID, jxs, jys, jxe, jye );
      XFlush ( VD_ID );

      (void) vtc_tsv ( kpxs, kpys, &kxa, &kya );				/* Put in virtual image */
      (void) vtc_tsv ( kpxe, kpye, &kxb, &kyb );

      (void) dsxc_scolnum ( kc, &kkcol );

      if ( kxa==kxb ) {
         if ( (kxa>=1) && (kxa<=DSSNX) ) {
            kxa = imin(DSSNX,imax(1,kxa));
            kjs  = kxa + (DSSNY-kya)*DSSNX;
            kje  = kxa + (DSSNY-kyb)*DSSNX;
            if ( kjs>kje ) {
               kt = kjs;
               kjs = kje;
               kje = kt;
            }
            if ( (kjs<=MAXVIRT) && (kje>=1) ) {
               kjs = imax(1,imin(MAXVIRT,kjs));
               kje = imax(1,imin(MAXVIRT,kje));
               for ( k = kjs; k<=kje; k+=DSSNX )
                  VT_IM[k-1] = (char) kkcol;
            }
         }
      }
      else if ( kya==kyb ) {
         if ( (kya>=1) && (kya<=DSSNY) ) {
            kya = imin(DSSNY,imax(1,kya));
            if ( kxa>kxb ) {
               kt = kxa;
               kxa = kxb;
               kxb = kxa;
            }
            if ( (kxa<=DSSNX) && (kya>=1) ) {
               kxa = imin(DSSNX,imax(1,kxa));
               kxb = imin(DSSNX,imax(1,kxb));
               kjs  = kxa + (DSSNY-kya)*DSSNX;
               kje  = kxb + (DSSNY-kya)*DSSNX;
               for ( k = kjs; k<=kje; k++ )
                  VT_IM[k-1] = (char) kkcol;
            }
         }
      }
      else {
         dx = kxb - kxa;
         dy = kyb - kya;
         ds = sqrt(dx*dx+dy*dy);
         axc = dx/ds;
         ayc = dy/ds;
         ax = (float) (kxa) - 0.25;
         ay = (float) (kya) - 0.25;
         kn = ds*4.0;
         for ( k = 1; k<=kn; k++ ) {
            ax = ax + 0.25*axc;
            ay = ay + 0.25*ayc;
            kx = ax;
            ky = ay;
            if ( (kx>=1) && (kx<=DSSNX) && (ky>=1) && (ky<=DSSNY) ) {
               kp = kx + (DSSNY-ky)*DSSNX;
               VT_IM[kp-1] = (char) kkcol;
            }
         }
      }


}



/************************************************************************
    DSXC_SETTHECOL -- Set writing colour (Xwindows)

  alan penny           ral                         1990-06-09
  pat morris             leeds        1992 jan
*/

dsxc_setthecol ( kc )

    int  kc;		/* i: Colour (1=red;2=green;3=blue;4=yellow;
				           5=cyan;6=mauve;7=tan;8=pink;
				           9=black;10=white) */

/* C-- */
{
      int kkcol;
/* Cbegin */


      (void) dsxc_scolnum ( kc, &kkcol );

      XSetForeground ( VD_ID, GC_ID, kkcol );
      XFlush ( VD_ID );


}


/****************************************************************************
    DSXC_UPDOWN -- Raise' or 'lower' a window (Xwindows)

  alan penny           ral                         1990-06-09
  pat morris             leeds        1992 jan
*/

dsxc_updown ( kf )

    int     kf;			/* i: raise=1; lower=2 */

/* C-- */
{
/* Cbegin */


      DSOPEN = F77_NAMED_COMMON(ds_genb).dsopen;

      if (!DSOPEN) return;

      if ( kf==1 ) {
         XRaiseWindow ( VD_ID, WD_ID );
         XFlush ( VD_ID );
      }
      else {
         XLowerWindow ( VD_ID, WD_ID );
         XFlush ( VD_ID );
      }


}



/********************************************************************************
   DSXC_ACIMR -- Display part of an actual real image (loads virtual image) (X window)

  alan penny                  ral         1990 jan
*/
      dsxc_acimr ( im, mx, my, ixs, ixe, iys, iye, ix, iy, wrap )

      int       mx;		/* i: Actual image X size */
      int       my;		/* i: Actual image Y size */
      float     im[];		/* i: Actual image  */
      int       ixs;		/* i: Actual image area X start */
      int       ixe;		/* i: Actual image area X end */
      int       iys;		/* i: Actual image area Y start */
      int       iye;		/* i: Actual image area Y end */
      int       ix;		/* i: Position in virtual image of X start */
      int       iy;		/* i: Position in virtual image of Y start */
      Bool      wrap;		/* i: Flag to wrap values round display limits */
/* C--*/
{
      int     iv, j, jj, ja, jx, jy, k, ka, kxoff, kyoff, kvxs,
              kvxe, kvys, kvye, knv, kas, kbs, kcs, nxs, nxe, nys, nye;
      float   zero, delta, rv, rva;
      char    byteim[2048];
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;
      DSCOMFX = F77_NAMED_COMMON(ds_gen).dscomfx;
      DSCOMFY = F77_NAMED_COMMON(ds_gen).dscomfy;
      DSVMIN = F77_NAMED_COMMON(ds_gen).dsvmin;
      DSVMAX = F77_NAMED_COMMON(ds_gen).dsvmax;
      RINVAL = F77_NAMED_COMMON(imagecoma).rinval;

      if ( ixs>mx || ixe<1 || iys>my || iye<1 ) return;			/* Check in image */

      nxs = imax(1,ixs);						/* Check all in image */
      nxe = imin(mx,ixe);
      nys = imax(1,iys);
      nye = imin(my,iye);

      jx = 1 + (((nxe-nxs+1)-1)/DSCOMFX);
      jy = 1 + (((nye-nys+1)-1)/DSCOMFY);

      kvxs = ix;
      kvxe = ix + jx - 1;
      kvys = iy;
      kvye = iy + jy - 1;

      if ( kvxs>DSSNX || kvys>DSSNY || kvxe<1 || kvye<1 ) return;	/* Check any in virtual screen */

      kxoff = 0;
      kyoff = 0;
      if ( kvxs<1 ) kxoff = 1 - kvxs;
      if ( kvys<1 ) kyoff = 1 - kvys;

      kvxs = imax(1,kvxs);
      kvxe = imin(DSSNX,kvxe);
      kvys = imax(1,kvys);
      kvye = imin(DSSNY,kvye);

      rv = DSVMAX - DSVMIN;
      if ( rv==0.0 ) rv = 1.0;
      delta = (float) (NUMDCOL-1)/rv;					/* Set up display factors */
      zero = -1.0*DSVMIN*delta;

      knv = kvxe - kvxs + 1;

      ka = nys + kyoff;							/* Load in virtual image */
      for ( k=kvys; k<=kvye; k++ ) {

         ja = nxs + kxoff;
         for ( j=kvxs; j<=kvxe; j++ ) {

            rv = im[ja-1+(ka-1)*mx];
            ja = ja + DSCOMFX;
            if ( rv==RINVAL ) {
               byteim[j-1] = BY_PC_ID[15+NUMDCOL];
            }else{
               rva = rv*delta + zero;
               rva = fmin(1.0e6,fmax(-1.0e6,rva));
               jj = (int)rva;
               if ( wrap ) {
                  jj = jj % (NUMDCOL-1);
                  if ( jj<0 ) jj = jj + NUMDCOL;
                  iv = 16 + jj;
               }else{
                  iv = 16 + imax(0,imin(jj,(NUMDCOL-1)));
               }
               byteim[j-1] = BY_PC_ID[iv];
            }
         }
         ka = ka + DSCOMFY;

         for ( kas=0; kas<knv; kas++ ) {
            kbs = kvxs + (DSSNY-k)*DSSNX - 1;
            kcs = kvxs - 1;
            VT_IM[kbs+kas] = byteim[kcs+kas];
         }

      }

      dsxc_vtim ( kvxs, kvxe, kvys, kvye, 0 );		/* Display section of virtual image */


}



/********************************************************************************
   DSXC_ACIMS -- Display part of an actual short image (loads virtual image) (X window)

  alan penny                  ral         1990 jan
*/
      dsxc_acims ( im, mx, my, ixs, ixe, iys, iye, ix, iy, wrap )

      int       mx;		/* i: Actual image X size */
      int       my;		/* i: Actual image Y size */
      short int im[];		/* i: Actual image  */
      int       ixs;		/* i: Actual image area X start */
      int       ixe;		/* i: Actual image area X end */
      int       iys;		/* i: Actual image area Y start */
      int       iye;		/* i: Actual image area Y end */
      int       ix;		/* i: Position in virtual image of X start */
      int       iy;		/* i: Position in virtual image of Y start */
      Bool      wrap;		/* i: Flag to wrap values round display limits */
/* C--*/
{
      int   iv, j, ja, jj, jx, jy, kk, k, ka, kv, kxoff, kyoff, kvxs,
            kvxe, kvys, kvye, knv, kas, kbs, kcs, nxs, nxe, nys, nye;
      float zero, delta, rv, rva;
      char  byteim[2048];
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      DSZM = F77_NAMED_COMMON(ds_gen).dszm;
      DSCOMFX = F77_NAMED_COMMON(ds_gen).dscomfx;
      DSCOMFY = F77_NAMED_COMMON(ds_gen).dscomfy;
      DSVMIN = F77_NAMED_COMMON(ds_gen).dsvmin;
      DSVMAX = F77_NAMED_COMMON(ds_gen).dsvmax;
      INVAL = F77_NAMED_COMMON(imagecoma).inval;

      if ( ixs>mx || ixe<1 || iys>my || iye<1 ) return;			/* Check in image */

      nxs = imax(1,ixs);						/* Check all in image */
      nxe = imin(mx,ixe);
      nys = imax(1,iys);
      nye = imin(my,iye);

      jx = 1 + (((nxe-nxs+1)-1)/DSCOMFX);
      jy = 1 + (((nye-nys+1)-1)/DSCOMFY);

      kvxs = ix;
      kvxe = ix + jx - 1;
      kvys = iy;
      kvye = iy + jy - 1;

      if ( kvxs>DSSNX || kvys>DSSNY || kvxe<1 || kvye<1 ) return;	/* Check any in virtual screen */

      kxoff = 0;
      kyoff = 0;
      if ( kvxs<1 ) kxoff = 1 - kvxs;
      if ( kvys<1 ) kyoff = 1 - kvys;

      kvxs = imax(1,kvxs);
      kvxe = imin(DSSNX,kvxe);
      kvys = imax(1,kvys);
      kvye = imin(DSSNY,kvye);

      rv = DSVMAX - DSVMIN;
      if ( rv==0.0 ) rv = 1.0;
      delta = (float) (NUMDCOL-1)/rv;					/* Set up display factors */
      zero = -1.0*DSVMIN*delta;

      knv = kvxe - kvxs + 1;

      ka = nys + kyoff;							/* Load in virtual image */
      for ( k=kvys; k<=kvye; k++ ) {

         ja = nxs + kxoff;
         for ( j=kvxs; j<=kvxe; j++ ) {
            kv = (int) im[ja-1+(ka-1)*mx];
            ja = ja + DSCOMFX;
            if ( kv==INVAL ) {
               byteim[j-1] = BY_PC_ID[15+NUMDCOL];
            }else{
               rva = (float) kv*delta + zero;
               rva = fmin(1.0e6,fmax(-1.0e6,rva));
               jj = (int) rva;
               if ( wrap ) {
                  jj = jj % (NUMDCOL-1);
                  if ( jj<0 ) jj = jj + NUMDCOL;
                  iv = 16 + jj;
               }else{
                  iv = 16 + imax(0,imin(jj,(NUMDCOL-1)));
               }
               byteim[j-1] = BY_PC_ID[iv];
            }
         }
         ka = ka + DSCOMFY;

         for ( kas=0; kas<knv; kas++ ) {
            kbs = kvxs + (DSSNY-k)*DSSNX - 1;
            kcs = kvxs - 1;
            VT_IM[kbs+kas] = byteim[kcs+kas];
         }
      }

      dsxc_vtim ( kvxs, kvxe, kvys, kvye, 0 );		/* Display section of virtual image */


}



/********************************************************************************
IMIN

*/
int imin ( int f1, int f2 )
{
     int smaller;
     if ( f1 < f2 )
        smaller = f1;
     else
        smaller = f2;
     return smaller ;
}
/********************************************************************************
IMAX

*/
int imax ( int f1, int f2 )
{
     int bigger;
     if ( f1 > f2 )
        bigger = f1;
     else
        bigger = f2;
     return bigger ;
}
/********************************************************************************
FMIN

*/
float fmin ( float f1, float f2 )
{
     float smaller;
     if ( f1 < f2 )
        smaller = f1;
     else
        smaller = f2;
     return smaller ;
}
/********************************************************************************
FMAX

*/
float fmax ( float f1, float f2 )
{
     float bigger;
     if ( f1 > f2 )
        bigger = f1;
     else
        bigger = f2;
     return bigger ;
}


/****************************************************************************
 DSXC_MSWAIT -- Wait X millisecs  (Xwindows)

  Authors: Alan Penny  (RAL)              2 Mar 1993  Convert to Starman
*/

   dsxc_mswait ( msecs )

    int     msecs;			/* i: no of milliseconds to wait */
/* C-- */
{
/* Cbegin */


    (void) dsxc_unix_mswait ( msecs );

}

/****************************************************************************
 DSXC_UNIX_MSWAIT -- Wait X millisecs  (Xwindows)
     This function causes the program to go to sleep for a given number of
     milliseconds. This function in needed to provides sub-second sleep
     intervals as the standard C function, sleep, has a resolution of 1 second.

  Notes:
     -  If a negative time interval is given, the routine returns immediately.
     -  The resolution of the timer on the Sun is 10ms.
     -  The resolution of the timer on the DECstation is 3.906ms.
     -  All actual sleep times will be in units of the timer resolution and
        will be rounded up. E.g. a request to sleep for 15ms on a Sun will
        result in an actual sleep time of 20ms.

  Copyright: Copyright (C) 1992 Science & Engineering Research Council

  Authors: PMA: Peter Allan (Starlink, RAL)   18 Jun 1992  Original
                 Alan Penny  (RAL)              2 Mar 1993  Convert to Starman
*/

   dsxc_unix_mswait ( msecs )

    int     msecs;			/* i: no of milliseconds to wait */
/* C-- */
{
    struct timeval time_struct;    /* The time structure to give to select.  */
    int ret;                       /* The return code from select.           */
    fd_set ia, ib, ic;
/* Cbegin */


    if ( msecs <= 0 ) return;					/* Check for a positive time interval.  */

    if ( msecs < 1000 ) {  					/* Set up the time structure, allowing */
       time_struct.tv_sec = 0; 					/* for times longer than 1 second.     */
       time_struct.tv_usec = msecs * 1000;
    } else {
       time_struct.tv_sec = msecs / 1000;
       time_struct.tv_usec = ( msecs % 1000 ) * 1000;
    }

    FD_ZERO (&ia); FD_ZERO (&ib); FD_ZERO (&ic);
    ret = select ( 0, &ia, &ib, &ic, &time_struct );	  	/* Call select with null file descriptor sets */
								/* to give the effect of going   */
								/* to sleep for a given time.  */


}

