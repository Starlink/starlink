C*XEDRIV -- PGPLOT driver for VAX workstations with DECWindows software
C+
      SUBROUTINE XEDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      IMPLICIT NONE
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for VAX workstations running DECWindows.
C
C Version 1.0 - 1989 Apr 15 - S. C. Allendorf
C                             Initial try (DWDRIVER.FOR).
C Version 2.0 - 1990 Mar 25 - S. C. Allendorf
C                             Merge functionality with Al Fey's C
C                             version for XWindows (XWDRIVER.C).
C Version 2.1 - 1990 Apr 03 - S. C. Allendorf
C                             Add asynchronous event handler and add
C                             code to implement a backing store.
C Version 2.2 - 1990 Apr 05 - S. C. Allendorf
C                             Modify so that only the necessary portion
C                             of the display is redrawn when flushed and
C                             when we receive an expose event.
C Version 3.0 - 1990 Apr 15 - S. C. Allendorf
C                             Merge functionality with WEW Jr.'s
C                             XE driver (XEDRIVER.FOR).
C Version 3.1 - 1990 Oct  5 - T. J. Pearson
C                             Restore input focus after using cursor.
C                             Report interactive, not hardcopy.
C Version 3.2 - 1991 Dec  5 - T. J. Pearson
C                             Change name from X11 to XWINDOW
C=======================================================================
C
C Supported device: This driver should work with all VAX/VMS
C workstations running the DECWindows software.
C
C Device type code: /XWINDOW.
C
C Default device name: PGPLOT.  Output is always directed to device
C DECW$DISPLAY; the "device name" provided by the user is used to label
C the PGPLOT window.
C
C Default view surface dimensions: Depends on the monitor, but nominally
C 10.5 inches horizontally by 8.0 inches vertically.  If you prefer a
C vertical orientation, execute the following command before running the
C program:
C
C $ DEFINE PGPLOT_XWIN_ASPECT PORTRAIT
C
C Substitute LANDSCAPE for PORTRAIT to revert to a horizontal 
C orientation.
C
C Resolution: Nominally 75 dpi, but depends on the monitor.
C
C Color capability: This driver will use as many colors as the 
C DECWindows server will allow, up to a maximum of 145 colors.  This
C maximum comes from the maximum number of colors that PGPLOT will use
C internally, and a desire to avoid hogging the resources of the server.
C
C Input capability: The cursor is controlled by the mouse. The user
C positions the cursor, and then types any key on the controlling
C keyboard.  The buttons on the mouse are also defined to return the 
C following characters:
C
C      Button  Character
C      ------  ---------
C         1        A
C         2        D
C        >2        X
C
C File format: It is not possible to send workstation plots to a disk 
C file using PGPLOT, but this may be accomplished using the standard
C X Windows utility xwd.  The format of the resulting file is documented
C in the X Windows documentation.
C
C Obtaining hardcopy: Not possible using PGPLOT, but may be achieved 
C using the standard X Windows utilities xwd and xpr or the Print Screen
C menu in DECWindows.
C
C NOTE: There is a bug in the early versions of DECWindows that cause
C the OPEN_DISPLAY call to sometimes abort the calling program. This may
C happen if you have used SET DISPLAY to define a display using the 
C local transport mechanism and you do not have access to it.  The 
C routine should return 0 in such a case, but does not currently.  If 
C you are going to use the SET DISPLAY command, use /TRANSPORT = DECNET 
C instead of /TRANSPORT = LOCAL or make sure that you will be able to 
C write to the display.  This means that someone must be logged into the 
C workstation display and must have security set to allow you to write 
C to it.  This bug has existed in all versions of DECWindows up through
C the version shipped with VMS 5.3-1.
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE
      PARAMETER  (TYPE='XWINDOW (Xwindow display)')
      INCLUDE    'SYS$LIBRARY:DECW$XLIBDEF'
      LOGICAL    INIT, LANDSCAPE, LOPIX, MONO
      BYTE       IMAGE(1280), KBYTE(2), XLOGO(128)
      INTEGER*2  JWORD(2), KWORD
      INTEGER*4  ARGS(4), BACK, BUFLEN, CMAP, CONTIG, CURS, DC$_TERM
      INTEGER*4  DEFX, DEFY, DEPTH, DEVCLASS, DISPLAY, DVI$_DEVCLASS
      INTEGER*4  FORE, GC, GCB, HEIGHT, I, IC, ICON, IER, IMAX, IMIN
      INTEGER*4  ISTAT, I0, I1, JLONG, JMAX, JMIN, J0, J1, KEYSYM, L
      INTEGER*4  GRFMEM, LIB$GETDVI, LIB$GET_COMMAND, GRGMEM
      INTEGER*4  LWIN, MAXCOL, MAXX, MAXY, NPTS, PARENT
      INTEGER*4  PIXELS(145), PIXMAP, PLANE_MASKS(145), POINTS, REMCAL
      INTEGER*4  SCREEN, SS$_NORMAL, WIDTH, WINDOW, XMAX, XMIN, XMM
      INTEGER*4  XOFF, XPIX, YMAX, YMIN, YMM, YOFF, YPIX
      INTEGER*4  FWINDOW, FREVERT
      REAL*4     CTABLE(3, 16), FACTOR, RESOL(2)
      CHARACTER  ASPECT*20, BUFFER*10, ICON_NAME*13, MESS*4, MSG*3
      CHARACTER  WINNAME*80
      RECORD     /X$COLOR/              BLACK, COLOR, RED
      RECORD     /X$EVENT/              REPORT
      RECORD     /X$GC_VALUES/          VALUES
      RECORD     /X$IMAGE/              XI
      RECORD     /X$POINT/              POINT
      RECORD     /X$SET_WIN_ATTRIBUTES/ SETWINATTR
      RECORD     /X$SIZE_HINTS/         SIZE_HINTS
      RECORD     /X$VISUAL/             VISUAL
C                                       Declare the asynchronous expose
C                                       event handler.
      EXTERNAL GRXE03
C                                       Setup the arguments passed to
C                                       the asynchronous expose event
C                                       routine.
      EQUIVALENCE (DISPLAY, ARGS(1))
      EQUIVALENCE (PIXMAP,  ARGS(2))
      EQUIVALENCE (WINDOW,  ARGS(3))
      EQUIVALENCE (GC,      ARGS(4))
C                                       We need these because FORTRAN
C                                       does not have unsigned types.
      EQUIVALENCE (JWORD(1), JLONG), (KBYTE(1), KWORD)
C                                       Define some parameters to avoid
C                                       having to use include files.
      PARAMETER (DC$_TERM = 66)
      PARAMETER (DVI$_DEVCLASS = 4)
      PARAMETER (SS$_NORMAL = 1)
C                                       Initialize a couple of things.
      DATA ICON_NAME, INIT /'PGPLOT Window', .TRUE./
C                                       Define the PGPLOT color table.
      DATA CTABLE /0.0,0.0,0.0, 1.0,1.0,1.0, 1.0,0.0,0.0, 0.0,1.0,0.0,
     +             0.0,0.0,1.0, 0.0,1.0,1.0, 1.0,0.0,1.0, 1.0,1.0,0.0,
     +             1.0,0.5,0.0, 0.5,1.0,0.0, 0.0,1.0,0.5, 0.0,0.5,1.0,
     +             0.5,0.0,1.0, 1.0,0.0,0.5, 0.333,0.333,0.333,
     +             0.667,0.667,0.667/
C                                       Define the X Windows logo.
      DATA XLOGO /  -1,    0,    0,  -64,   -2,    1,    0,  -64,   -4,
     +         3,    0,   96,   -8,    7,    0,   48,   -8,    7,    0,
     +        24,  -16,   15,    0,   12,  -32,   31,    0,    6,  -64,
     +        63,    0,    6,  -64,   63,    0,    3, -128,  127, -128,
     +         1,    0,   -1,  -64,    0,    0,   -2,   97,    0,    0,
     +        -2,   49,    0,    0,   -4,   51,    0,    0,   -8,   27,
     +         0,    0,  -16,   13,    0,    0,  -16,   14,    0,    0,
     +        96,   31,    0,    0,  -80,   63,    0,    0, -104,  127,
     +         0,    0, -104,  127,    0,    0,   12,   -1,    0,    0,
     +         6,   -2,    1,    0,    3,   -4,    3, -128,    1,   -4,
     +         3,  -64,    0,   -8,    7,  -64,    0,  -16,   15,   96,
     +         0,  -32,   31,   48,    0,  -32,   31,   24,    0,  -64,
     +        63,   12,    0, -128,  127,    6,    0,    0,   -1/
C-----------------------------------------------------------------------
C                                       On the first call, find out what
C                                       sort of workstation we have.
      IF (INIT) THEN
         INIT = .FALSE.
C                                       Attempt to open a DECWindows
C                                       display.  See note above
C                                       about DECWindows bug.
         DISPLAY = X$OPEN_DISPLAY ()
C                                       Only do the following if we
C                                       actually have a DECWindows
C                                       display.
         IF (DISPLAY .NE. 0) THEN
C                                       Get the default screen that is
C                                       associated with the display.
            SCREEN = X$DEFAULT_SCREEN (DISPLAY)
C                                       Find the root window.
            PARENT = X$ROOT_WINDOW (DISPLAY, SCREEN)
C                                       Get the number of planes.
            DEPTH = X$DISPLAY_PLANES (DISPLAY, SCREEN)
C                                       Get the visual type.
            CALL X$DEFAULT_VISUAL (DISPLAY, SCREEN, VISUAL)
C                                       Classify the display.
            MONO = (VISUAL.X$L_VISU_CLASS .EQ. X$C_STATIC_GRAY) .OR.
     +             (VISUAL.X$L_VISU_CLASS .EQ. X$C_STATIC_COLOR) .OR.
     +             (DEPTH .EQ. 1)
C                                       Get the size of the display.
            XPIX = X$DISPLAY_WIDTH (DISPLAY, SCREEN)
            YPIX = X$DISPLAY_HEIGHT (DISPLAY, SCREEN)
            XMM = X$DISPLAY_WIDTH_MM (DISPLAY, SCREEN)
            YMM = X$DISPLAY_HEIGHT_MM (DISPLAY, SCREEN)
C                                       Calculate the resolution of the
C                                       display.
            RESOL(1) = 25.4 * REAL (XPIX) / REAL (XMM)
            RESOL(2) = 25.4 * REAL (YPIX) / REAL (YMM)
C                                       Set the aspect ratio of the
C                                       window.
            FACTOR = 8.5 / 11.0
C                                       See what orientation we want.
            CALL GRGENV ('XWIN_ASPECT', ASPECT, L)
C                                       Calculate the window size.
            IF (ASPECT(1:1) .EQ. 'P') THEN
C                                       Potrait mode (pixels).
               HEIGHT = 828 * YPIX / 1024
               WIDTH = NINT (FACTOR * HEIGHT)
               LANDSCAPE = .FALSE.
            ELSE
C                                       Landscape mode (pixels).
               WIDTH = 828 * XPIX / 1024
               HEIGHT = NINT (FACTOR * WIDTH)
               LANDSCAPE = .TRUE.
            END IF
C                                       Calculate the size of the border
C                                       around the plot.
            IMIN = NINT (0.25 * RESOL(1))
            JMIN = NINT (0.25 * RESOL(2))
C                                       Set the maximum coordinates.
            IMAX = WIDTH - IMIN - 1
            JMAX = HEIGHT - JMIN - 1
C                                       Define the maximum allowed plot
C                                       size.  This is a bit of a hack
C                                       to handle extra things that the
C                                       window manager might do to the
C                                       window.
            MAXX = XPIX - 2 * IMIN - 10
            MAXY = YPIX - 2 * JMIN - 30
C                                       Define the default width and
C                                       height of the plot.
            DEFX = IMAX - IMIN
            DEFY = JMAX - JMIN
C                                       Center the window in the
C                                       display.
            XOFF = (XPIX - WIDTH) / 2
            YOFF = (YPIX - HEIGHT) / 2
C                                       Find the default colormap.
            CMAP = X$DEFAULT_COLORMAP (DISPLAY, SCREEN)
C                                       See if we will be able to use
C                                       the colors.
            IF (MONO) THEN
C                                       On static displays and
C                                       monochrome displays we will only
C                                       be able to use two colors.
               MAXCOL = 1
            ELSE
C                                       Determine the maximum number of
C                                       colors available.  Make sure we
C                                       only grab a reasonable number.
               MAXCOL = MIN (X$DISPLAY_CELLS (DISPLAY, SCREEN), 145)
C                                       Grab as many color cells as we
C                                       need (or X will allow us).
               DO I = MAXCOL, 2, -1
                  ISTAT = X$ALLOC_COLOR_CELLS (DISPLAY, CMAP, CONTIG,
     +                                      PLANE_MASKS, 0, PIXELS, I)
                  MAXCOL = I
                  IF (ISTAT .EQ. 1) GOTO 5
               END DO
C                                       Set the value of the maximum
C                                       color index.  If we found two or
C                                       fewer colors, revert to 
C                                       monochrome.
C       
    5          MAXCOL = MAXCOL - 1
               IF (MAXCOL .EQ. 1) MONO = .TRUE.
            END IF
         ELSE
C                                       Deal with the error on the open
C                                       workstation call.
            MAXCOL = 1
            LANDSCAPE = .TRUE.
         END IF
C                                       Set the machine characteristics.
         LOPIX = .FALSE.
         IF (MAXCOL .GT. 33) LOPIX = .TRUE.
      END IF
C                                       Branch on opcode.
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     +     110,120,130,140,150,160,170,180,190,200,
     +     210,220,230,240,250,260), IFUNC
C
  900 WRITE (MSG, '(I10)') IFUNC
      CALL GRWARN ('Unimplemented function in DECWindows device driver:'
     +              // MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CONTINUE
      CHR = TYPE
      LCHR = LEN(TYPE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 CONTINUE
      RBUF(1) = 0.0
      RBUF(2) = REAL (MAXX)
      RBUF(3) = 0.0
      RBUF(4) = REAL (MAXY)
      RBUF(5) = 0.0
      RBUF(6) = REAL (MAXCOL)
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 CONTINUE
      RBUF(1) = RESOL(1)
      RBUF(2) = RESOL(2)
      RBUF(3) = 1.0                     ! Device coordinates per pixel
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is No Hardcopy, Cursor, No dashed lines, Area fill,
C    No thick lines, Rectangle fill, and possibly Line of pixels.)
C
   40 CONTINUE
      IF (LOPIX) THEN
         CHR = 'ICNANRPNNN'
      ELSE
         CHR = 'ICNANRNNNN'
      END IF
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CONTINUE
      CHR = 'PGPLOT'
      LCHR = 6
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 CONTINUE
      RBUF(1) = 0.0
      RBUF(2) = REAL (DEFX)
      RBUF(3) = 0.0
      RBUF(4) = REAL (DEFY)
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 CONTINUE
      RBUF(1) = 1.0
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot --------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation ---------------------------------------
C
   90 CONTINUE
C                                       Return an error if the display
C                                       couldn't be opened.  The display
C                                       should be set with the VMS SET
C                                       DISPLAY command.
      IF (DISPLAY .EQ. 0) THEN
         CALL GRWARN ('Cannot open the specified DECWindows display.')
         RBUF(2) = 0.0
         RETURN
      END IF
C                                       Stash away the passed window
C                                       name for later use.
      WINNAME = CHR(:LCHR)
      LWIN = LCHR
C                                       Define the foreground and
C                                       background colors to be black
C                                       and white respectively.
      FORE = X$BLACK_PIXEL (DISPLAY, SCREEN)
      BACK = X$WHITE_PIXEL (DISPLAY, SCREEN)
C                                       Create a window in the display.
      WINDOW = X$CREATE_WINDOW (DISPLAY, PARENT, XOFF, YOFF,
     +            WIDTH, HEIGHT, 0, DEPTH, X$C_INPUT_OUTPUT,
     +            VISUAL, 0, SETWINATTR)
C                                       Load the PGPLOT palette.
      IF (.NOT. MONO) THEN
C                                       Define color indices 0-15;
C                                       background (CI = 0) black,
C                                       write (CI = 1) in white.
         DO I = 0, MIN (15, MAXCOL)
            COLOR.X$L_COLR_PIXEL = PIXELS(I + 1)
            JLONG = NINT (CTABLE (1, I + 1) * 65535.0)
            COLOR.X$W_COLR_RED = JWORD(1)
            JLONG = NINT (CTABLE (2, I + 1) * 65535.0)
            COLOR.X$W_COLR_GREEN = JWORD(1)
            JLONG = NINT (CTABLE (3, I + 1) * 65535.0)
            COLOR.X$W_COLR_BLUE = JWORD(1)
            COLOR.X$B_COLR_FLAGS = X$M_DO_RED .OR.
     +                                    X$M_DO_GREEN .OR. X$M_DO_BLUE
C                                       Load our color table into the
C                                       color map.
            CALL X$STORE_COLOR (DISPLAY, CMAP, COLOR)
         END DO
C                                       Redefine the background and
C                                       foreground colors to point at
C                                       our definitions.
         BACK = PIXELS(1)
         FORE = PIXELS(2)
C                                       Get color structures for the
C                                       cursor colors.
         RED.X$L_COLR_PIXEL = PIXELS(3)
         BLACK.X$L_COLR_PIXEL = PIXELS(1)
         CALL X$QUERY_COLOR (DISPLAY, CMAP, BLACK)
         CALL X$QUERY_COLOR (DISPLAY, CMAP, RED)
      END IF
C                                       Set the window colors.
      CALL X$SET_WINDOW_BACKGROUND (DISPLAY, WINDOW, BACK)
      CALL X$SET_WINDOW_BORDER (DISPLAY, WINDOW, FORE)
C                                       Initialize size hint property
C                                       for the window manager.
      SIZE_HINTS.X$L_SZHN_FLAGS = X$M_P_POSITION .OR. X$M_P_SIZE .OR.
     +                            X$M_P_MIN_SIZE .OR. X$M_P_MAX_SIZE
      SIZE_HINTS.X$L_SZHN_X = XOFF
      SIZE_HINTS.X$L_SZHN_Y = YOFF
      SIZE_HINTS.X$L_SZHN_WIDTH = WIDTH
      SIZE_HINTS.X$L_SZHN_HEIGHT = HEIGHT
      SIZE_HINTS.X$L_SZHN_MIN_WIDTH = WIDTH
      SIZE_HINTS.X$L_SZHN_MIN_HEIGHT = HEIGHT
      SIZE_HINTS.X$L_SZHN_MAX_WIDTH = WIDTH
      SIZE_HINTS.X$L_SZHN_MAX_HEIGHT = HEIGHT
C                                       Create an icon.
      ICON = X$CREATE_BITMAP_FROM_DATA (DISPLAY, WINDOW, XLOGO, 32, 32)
C                                       Set the necessary properties.
      CALL X$SET_STANDARD_PROPERTIES (DISPLAY, WINDOW, WINNAME(:LWIN),
     +        ICON_NAME, ICON, 0, 0, SIZE_HINTS)
C                                       Create a pixmap.
      PIXMAP = X$CREATE_PIXMAP (DISPLAY, WINDOW, WIDTH, HEIGHT, DEPTH)
C                                       Create default graphics contexts
C                                       for foreground and background.
      GC  = X$CREATE_GC (DISPLAY, PIXMAP, 0, VALUES)
      GCB = X$CREATE_GC (DISPLAY, PIXMAP, 0, VALUES)
C                                       Set the foreground colors in the
C                                       graphics contexts.
      CALL X$SET_FOREGROUND (DISPLAY, GC, FORE)
      CALL X$SET_FOREGROUND (DISPLAY, GCB, BACK)
C                                       Ask for mapping notification.
      CALL X$SELECT_INPUT (DISPLAY, WINDOW, X$M_STRUCTURE_NOTIFY)
C                                       Display the window.
      CALL X$MAP_RAISED (DISPLAY, WINDOW)
C                                       Eat the mapping notification.
C                                       The loop is necessary because
C                                       the DECWindows window manager
C                                       reparents everything and sends
C                                       reparenting events before the
C                                       mapping event.
   95 CALL X$NEXT_EVENT (DISPLAY, REPORT)
      IF (REPORT.EVNT_TYPE .NE. X$C_MAP_NOTIFY) GOTO 95
C                                       Set up the asynchronous expose
C                                       event handler.
      CALL X$SELECT_ASYNC_EVENT (DISPLAY, WINDOW, X$C_EXPOSE,
     +                              GRXE03, %LOC (ARGS))
C                                       Turn on exposure events.
      CALL X$SELECT_INPUT (DISPLAY, WINDOW, X$M_EXPOSURE)
C                                       Initialize the damaged region.
      CALL GRXE02 (WIDTH, HEIGHT, XMIN, XMAX, YMIN, YMAX)
C                                       Successful-- return display
      RBUF(1) = DISPLAY
      RBUF(2) = 1.0
      NBUF = 2
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
C                                       See if we are attached to a
C                                       real terminal.
      IER = LIB$GETDVI (DVI$_DEVCLASS, , 'SYS$COMMAND', DEVCLASS)
C                                       Wait for user acknowledgement.
      IF (IER .EQ. 1 .AND. DEVCLASS .EQ. DC$_TERM)
     +    CALL LIB$GET_COMMAND (MESS,
     +       CHAR (7) // 'Type <RETURN> to remove PGPLOT window: ', L)
C                                       Clean up resources.
      CALL X$SELECT_INPUT (DISPLAY, WINDOW, 0)
      CALL X$SELECT_ASYNC_EVENT (DISPLAY, WINDOW, X$C_EXPOSE, 0, 0)
      CALL X$UNMAP_WINDOW (DISPLAY, WINDOW)
      CALL X$FREE_GC (DISPLAY, GC)
      CALL X$FREE_GC (DISPLAY, GCB)
      CALL X$DESTROY_WINDOW (DISPLAY, WINDOW)
      CALL X$FREE_PIXMAP (DISPLAY, PIXMAP)
      CALL X$CLOSE_DISPLAY (DISPLAY)
C                                       Reset the initialization
C                                       variable.
      INIT = .TRUE.
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
C                                       See if the user wants a
C                                       nonstandard size window.
      I0 = NINT (RBUF(1)) + 2 * IMIN + 1
      J0 = NINT (RBUF(2)) + 2 * JMIN + 1
C                                       See if it is different than what
C                                       we already have.
      IF (I0 .NE. WIDTH .OR. J0 .NE. HEIGHT) THEN
C                                       Recompute the size and position
C                                       parameters.
         WIDTH = I0
         HEIGHT = J0
         IMAX = WIDTH - IMIN - 1
         JMAX = HEIGHT - JMIN -1
         XOFF = (XPIX - WIDTH) / 2
         YOFF = (YPIX - HEIGHT) / 2
C                                       Turn off expose events to avoid
C                                       PIXMAP being invalid to the
C                                       asynchronous expose event
C                                       handler.
         CALL X$SELECT_INPUT (DISPLAY, WINDOW, 0)
C                                       Destroy the old pixmap.
         CALL X$FREE_PIXMAP (DISPLAY, PIXMAP)
C                                       Create a new pixmap.
         PIXMAP = X$CREATE_PIXMAP (DISPLAY, WINDOW, WIDTH, HEIGHT,
     +                             DEPTH)
C                                       Reset the size hints for the
C                                       window manager.
         SIZE_HINTS.X$L_SZHN_FLAGS = X$M_P_POSITION .OR. X$M_P_SIZE .OR.
     +                               X$M_P_MIN_SIZE .OR. X$M_P_MAX_SIZE
         SIZE_HINTS.X$L_SZHN_X = XOFF
         SIZE_HINTS.X$L_SZHN_Y = YOFF
         SIZE_HINTS.X$L_SZHN_WIDTH = WIDTH
         SIZE_HINTS.X$L_SZHN_HEIGHT = HEIGHT
         SIZE_HINTS.X$L_SZHN_MIN_WIDTH = WIDTH
         SIZE_HINTS.X$L_SZHN_MIN_HEIGHT = HEIGHT
         SIZE_HINTS.X$L_SZHN_MAX_WIDTH = WIDTH
         SIZE_HINTS.X$L_SZHN_MAX_HEIGHT = HEIGHT
C                                       Send the hints to the window
C                                       manager.
         CALL X$SET_STANDARD_PROPERTIES (DISPLAY, WINDOW,
     +           WINNAME(:LWIN), ICON_NAME, ICON, 0, 0, SIZE_HINTS)
C                                       Resize the window.
         CALL X$RESIZE_WINDOW (DISPLAY, WINDOW, WIDTH, HEIGHT)
C                                       Wait for the server to catch
C                                       up.
         CALL X$SYNC (DISPLAY, .FALSE.)
C                                       Turn on exposure events.
         CALL X$SELECT_INPUT (DISPLAY, WINDOW, X$M_EXPOSURE)
      END IF
C                                       Clear the pixmap.
      CALL X$FILL_RECTANGLE (DISPLAY, PIXMAP, GCB, 0, 0, WIDTH, HEIGHT)
C                                       Clear the window.
      CALL X$CLEAR_WINDOW (DISPLAY, WINDOW)
C                                       Reset the damaged region.
      CALL GRXE02 (WIDTH, HEIGHT, XMIN, XMAX, YMIN, YMAX)
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
C                                       Transform the input coordinates.
      I0 = NINT (RBUF(1)) + IMIN
      J0 = JMAX - NINT (RBUF(2))
      I1 = NINT (RBUF(3)) + IMIN
      J1 = JMAX - NINT (RBUF(4))
C                                       Draw the line.
      CALL X$DRAW_LINE (DISPLAY, PIXMAP, GC, I0, J0, I1, J1)
C                                       Update the damaged region.
      CALL GRXE01 (1, I0, J0, I1, J1, XMIN, XMAX, YMIN, YMAX)
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
C                                       Transform the input coordinates.
      I0 = NINT (RBUF(1)) + IMIN
      J0 = JMAX - NINT (RBUF(2))
C                                       Draw the point.
      CALL X$DRAW_POINT (DISPLAY, PIXMAP, GC, I0, J0)
C                                       Update the damaged region.
      CALL GRXE01 (0, I0, J0, I0, J0, XMIN, XMAX, YMIN, YMAX)
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
C                                       Make sure the server is caught 
C                                       up.
      CALL X$SYNC (DISPLAY, .FALSE.)
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = NINT (RBUF(1))
C                                       Handle monochrome displays
C                                       properly.
      IF (.NOT. MONO) THEN
         CALL X$SET_FOREGROUND (DISPLAY, GC, PIXELS(IC + 1))
      ELSE IF (IC .EQ. 1) THEN
         CALL X$SET_FOREGROUND (DISPLAY, GC, FORE)
      ELSE
         CALL X$SET_FOREGROUND (DISPLAY, GC, BACK)
      END IF
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
C                                       Copy pixmap to server.
      IF (XMAX .NE. -1) CALL X$COPY_AREA (DISPLAY, PIXMAP, WINDOW, GC,
     +       XMIN, YMIN, XMAX - XMIN + 1, YMAX - YMIN + 1, XMIN, YMIN)
C                                       Make sure the server is caught 
C                                       up.
      CALL X$SYNC (DISPLAY, .FALSE.)
C                                       Reset damaged region.
      CALL GRXE02 (WIDTH, HEIGHT, XMIN, XMAX, YMIN, YMAX)
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C
  170 CONTINUE
C                                       Create and display the graphics
C                                       cursor.
      CURS = X$CREATE_FONT_CURSOR (DISPLAY, X$C_CROSS_HAIR_CURSOR)
      CALL X$DEFINE_CURSOR (DISPLAY, WINDOW, CURS)
      IF (.NOT. MONO) CALL X$RECOLOR_CURSOR (DISPLAY, CURS, RED, BLACK)
C                                       Convert input coordinates.
      I0 = NINT (RBUF(1)) + IMIN
      J0 = JMAX - NINT (RBUF(2))
C                                       Set input focus to avoid
C                                       unwanted data entry.
      CALL X$GET_INPUT_FOCUS (DISPLAY, FWINDOW, FREVERT)
      CALL X$SET_INPUT_FOCUS (DISPLAY, WINDOW, X$C_REVERT_TO_PARENT,
     +                           X$C_CURRENT_TIME)
C                                       Set cursor to the correct spot.
      CALL X$WARP_POINTER (DISPLAY, X$C_NONE, WINDOW,
     +                                        0, 0, 0, 0, I0, J0)
C                                       Turn on event processing.
      CALL X$SELECT_INPUT (DISPLAY, WINDOW, X$M_KEY_PRESS .OR.
     +        X$M_BUTTON_PRESS .OR. X$M_EXPOSURE)
C                                       Make sure the server is caught
C                                       up.
      CALL X$SYNC (DISPLAY, .FALSE.)
C                                       Loop until we get an entry from
C                                       the user.
      DO WHILE (.TRUE.)
C                                       Wait for an event to occur.  We
C                                       ignore no expose events and
C                                       graphics expose events.
         CALL X$NEXT_EVENT (DISPLAY, REPORT)
C                                       Process the window exposure.
         IF (REPORT.EVNT_TYPE .EQ. X$C_EXPOSE) THEN
             CALL X$COPY_AREA (DISPLAY, PIXMAP, WINDOW, GC,
     +               REPORT.EVNT_EXPOSE.X$L_EXEV_X,
     +               REPORT.EVNT_EXPOSE.X$L_EXEV_Y,
     +               REPORT.EVNT_EXPOSE.X$L_EXEV_WIDTH,
     +               REPORT.EVNT_EXPOSE.X$L_EXEV_HEIGHT,
     +               REPORT.EVNT_EXPOSE.X$L_EXEV_X,
     +               REPORT.EVNT_EXPOSE.X$L_EXEV_Y)
C                                       The user pressed a mouse button.
         ELSE IF (REPORT.EVNT_TYPE .EQ. X$C_BUTTON_PRESS) THEN
C                                       Record the position
            I0 = REPORT.EVNT_BUTTON.X$L_BTEV_X
            J0 = REPORT.EVNT_BUTTON.X$L_BTEV_Y
C                                       Translate the mouse buttons to
C                                       the common letters Add, Delete, 
C                                       and eXit.
            IF (REPORT.EVNT_BUTTON.X$L_BTEV_BUTTON .EQ.
     +                  X$C_BUTTON1) THEN
               BUFFER(1:1) = 'A'
            ELSE IF (REPORT.EVNT_BUTTON.X$L_BTEV_BUTTON .EQ.
     +                  X$C_BUTTON2) THEN
               BUFFER(1:1) = 'D'
            ELSE
               BUFFER(1:1) = 'X'
            END IF
C                                       Ignore this event if it is
C                                       outside the graphics boundaries.
            IF (I0 .GE. IMIN .AND. I0 .LE. IMAX .AND.
     +          J0 .GE. JMIN .AND. J0 .LE. JMAX) GOTO 175
            CALL X$BELL (DISPLAY, 0)
            CALL X$SYNC (DISPLAY, .FALSE.)
C                                       Translate the key pressed by the
C                                       user.
         ELSE IF (REPORT.EVNT_TYPE .EQ. X$C_KEY_PRESS) THEN
            I0 = REPORT.EVNT_KEY.X$L_KYEV_X
            J0 = REPORT.EVNT_KEY.X$L_KYEV_Y
            BUFLEN = X$LOOKUP_STRING (REPORT.EVNT_KEY,
     +                                    BUFFER, 10, KEYSYM, )
C                                       Ignore this event if it did not
C                                       produce a single character.
            IF (BUFLEN .EQ. 1) THEN
C                                       Ignore this event if it is
C                                       outside the graphics boundaries.
               IF (I0 .GE. IMIN .AND. I0 .LE. IMAX .AND.
     +             J0 .GE. JMIN .AND. J0 .LE. JMAX) GOTO 175
               CALL X$BELL (DISPLAY, 0)
               CALL X$SYNC (DISPLAY, .FALSE.)
            END IF
         END IF
      END DO
C                                       Reset event processing.
  175 CALL X$SELECT_INPUT (DISPLAY, WINDOW, X$M_EXPOSURE)
C                                       Return the cursor to its
C                                       original state.
      CALL X$UNDEFINE_CURSOR (DISPLAY, WINDOW)
      CALL X$FREE_CURSOR (DISPLAY, CURS)
      CALL X$SET_INPUT_FOCUS (DISPLAY, FWINDOW, FREVERT, 
     +                           X$C_CURRENT_TIME)
C                                       Make sure the server is caught
C                                       up.
      CALL X$SYNC (DISPLAY, .FALSE.)
C                                       Set the return values.
      CHR(1:1) = BUFFER(1:1)
      RBUF(1) = REAL (I0 - IMIN)
      RBUF(2) = REAL (JMAX - J0)
      NBUF = 2
      LCHR = 1
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Not implemented: no alpha screen)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called)
C
  190 CONTINUE
      GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
C                                       First time, set number of points
C                                       in polygon and allocate the
C                                       memory for the array.
      IF (REMCAL .EQ. 0) THEN
         NPTS = NINT (RBUF(1))
         REMCAL = NPTS
         IER = GRGMEM (SIZEOF (POINT) * NPTS, POINTS)
         IF (IER .NE. SS$_NORMAL) THEN
            CALL GRGMSG (IER)
            CALL GRQUIT ('Failed to allocate temporary buffer.')
         END IF
      ELSE
C                                       Second and succeeding calls,
C                                       change counter and load arrays.
         REMCAL = REMCAL - 1
         I0 = NINT (RBUF(1)) + IMIN
         J0 = JMAX - NINT (RBUF(2))
         CALL GRXE00 (NPTS, %VAL (POINTS), REMCAL, I0, J0)
C                                       Calculate the damaged region.
         CALL GRXE01 (0, I0, J0, I0, J0, XMIN, XMAX, YMIN, YMAX)
C                                       If last call, fill the area and
C                                       deallocate the memory.
         IF (REMCAL .EQ. 0) THEN
            CALL X$FILL_POLYGON (DISPLAY, PIXMAP, GC, %VAL (POINTS),
     +              NPTS, X$C_POLYCOMPLEX, X$C_COORD_MODE_ORIGIN)
            IER = GRFMEM (SIZEOF (POINT) * NPTS, POINTS)
            IF (IER .NE. SS$_NORMAL) THEN
               CALL GRGMSG (IER)
               CALL GRQUIT ('Failed to deallocate temporary buffer.')
            END IF
         END IF
      END IF
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
C                                       Ignore for a static or 
C                                       monochrome device.
      IF (.NOT. MONO) THEN
C                                       Determine the color index.
         IC = NINT (RBUF(1))
C                                       Load the color structure.
         COLOR.X$L_COLR_PIXEL = PIXELS(IC + 1)
         JLONG = NINT (RBUF(2) * 65535.0)
         COLOR.X$W_COLR_RED = JWORD(1)
         JLONG = NINT (RBUF(3) * 65535.0)
         COLOR.X$W_COLR_GREEN = JWORD(1)
         JLONG = NINT (RBUF(4) * 65535.0)
         COLOR.X$W_COLR_BLUE = JWORD(1)
         COLOR.X$B_COLR_FLAGS = X$M_DO_RED .OR. X$M_DO_GREEN .OR.
     +                              X$M_DO_BLUE
C                                       Tell the server about the new
C                                       definition.
         CALL X$STORE_COLOR (DISPLAY, CMAP, COLOR)
      END IF
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called)
C
  220 CONTINUE
      GOTO 900
C
C--- IFUNC=23, Escape --------------------------------------------------
C    (Not implemented: ignored)
C
  230 CONTINUE
      RETURN
C
C--- IFUNC=24, Rectangle Fill. -----------------------------------------
C
  240 CONTINUE
C                                       Figure out the position of the
C                                       rectangle.
      I0 = NINT (RBUF(1)) + IMIN
      J0 = JMAX - NINT (RBUF(4))
C                                       Determine the size of the
C                                       rectangle.
      I1 = NINT (RBUF(3) - RBUF(1) + 1.0)
      J1 = NINT (RBUF(4) - RBUF(2) + 1.0)
C                                       Draw the rectangle into the
C                                       display.
      CALL X$FILL_RECTANGLE (DISPLAY, PIXMAP, GC, I0, J0, I1, J1)
C                                       Calculate the damaged region.
      CALL GRXE01 (1, I0, J0, I0 + I1 - 1, J0 + J1 - 1,
     +                XMIN, XMAX, YMIN, YMAX)
      RETURN
C
C--- IFUNC=25, ---------------------------------------------------------
C    (Not implemented: ignored)
C
  250 CONTINUE
      RETURN
C
C--- IFUNC=26, Line of pixels ------------------------------------------
C
  260 CONTINUE
C                                       This should only be called if
C                                       there are more than four planes
C                                       present and the display can
C                                       handle defining colors.
      IF (.NOT. LOPIX) THEN
         GOTO 900
      ELSE
C                                       Calculate where to put the line.
         I0 = NINT (RBUF(1)) + IMIN
         J0 = JMAX - NINT (RBUF(2))
C                                       Load the image data into the
C                                       the array.
         DO 265 IC = 1, NBUF - 2
            KWORD = PIXELS (NINT (RBUF(IC + 2)) + 1)
            IMAGE(IC) = KBYTE(1)
  265    CONTINUE
C                                       Create an image structure.
         CALL X$CREATE_IMAGE (DISPLAY, VISUAL, DEPTH, X$C_Z_PIXMAP, 0,
     +           IMAGE, NBUF - 2, 1, 8, 0, XI)
C                                       Draw the line into the display.
         CALL X$PUT_IMAGE (DISPLAY, PIXMAP, GC, XI, 0, 0, I0, J0,
     +           NBUF - 2, 1)
C                                       Calculate the damaged region.
         CALL GRXE01 (1, I0, J0, I0 + NBUF - 3, J0,
     +                   XMIN, XMAX, YMIN, YMAX)
      END IF
      RETURN
C-----------------------------------------------------------------------
      END

C*GRXE00 -- PGPLOT XE driver, load polygon array
C+
      SUBROUTINE GRXE00 (N0, POINTS, N, X, Y)
      IMPLICIT NONE
      INCLUDE    'SYS$LIBRARY:DECW$XLIBDEF'
      INTEGER*4  N, N0, X, Y
      RECORD     /X$POINT/ POINTS(N0)
C-----------------------------------------------------------------------
C                                       Load the polygon array with the
C                                       passed vertex.
      POINTS(N0 - N).X$W_GPNT_X = X
      POINTS(N0 - N).X$W_GPNT_Y = Y
C-----------------------------------------------------------------------
      RETURN
      END

C*GRXE01 -- PGPLOT XE driver, calculate 'damaged' region.
C+
      SUBROUTINE GRXE01 (LINE, I0, J0, I1, J1, XMIN, XMAX, YMIN, YMAX)
      IMPLICIT NONE
      INTEGER*4  I0, I1, J0, J1, LINE, XMAX, XMIN, YMAX, YMIN
C-----------------------------------------------------------------------
C                                       Update the damaged region.
      IF (I0 .GT. XMAX) XMAX = I0
      IF (I0 .LT. XMIN) XMIN = I0
      IF (J0 .GT. YMAX) YMAX = J0
      IF (J0 .LT. YMIN) YMIN = J0
C                                       See if we were passed a
C                                       rectangle and update the
C                                       damaged region accordingly.
      IF (LINE .EQ. 1) THEN
         IF (I1 .GT. XMAX) XMAX = I1
         IF (I1 .LT. XMIN) XMIN = I1
         IF (J1 .GT. YMAX) YMAX = J1
         IF (J1 .LT. YMIN) YMIN = J1
      END IF
C-----------------------------------------------------------------------
      RETURN
      END

C*GRXE02 -- PGPLOT XE driver, reset 'damaged' region.
C+
      SUBROUTINE GRXE02 (WIDTH, HEIGHT, XMIN, XMAX, YMIN, YMAX)
      IMPLICIT NONE
      INTEGER*4  HEIGHT, WIDTH, XMAX, XMIN, YMAX, YMIN
C-----------------------------------------------------------------------
C                                       Reset the boundaries of the
C                                       damaged region.
      XMAX = -1
      YMAX = -1
      XMIN = WIDTH + 1
      YMIN = HEIGHT + 1
C-----------------------------------------------------------------------
      RETURN
      END

C*GRXE03 -- PGPLOT XE driver, aysynchronous redrawing routine.
C+
      SUBROUTINE GRXE03 (ARGS)
      IMPLICIT NONE
      INCLUDE    'SYS$LIBRARY:DECW$XLIBDEF'
      INTEGER*4  ARGS(4)
      RECORD     /X$EVENT/ EVENT
C-----------------------------------------------------------------------
C                                       Get all of the exposure events.
      DO WHILE (X$CHECK_WINDOW_EVENT (ARGS(1), ARGS(3),
     +              X$M_EXPOSURE, EVENT))
C                                       If part of the window has been
C                                       exposed, redraw that part.  We
C                                       ignore no expose events and 
C                                       graphics expose events.
         IF (EVENT.EVNT_TYPE .EQ. X$C_EXPOSE) THEN
             CALL X$COPY_AREA (ARGS(1), ARGS(2), ARGS(3), ARGS(4),
     +               EVENT.EVNT_EXPOSE.X$L_EXEV_X,
     +               EVENT.EVNT_EXPOSE.X$L_EXEV_Y,
     +               EVENT.EVNT_EXPOSE.X$L_EXEV_WIDTH,
     +               EVENT.EVNT_EXPOSE.X$L_EXEV_HEIGHT,
     +               EVENT.EVNT_EXPOSE.X$L_EXEV_X,
     +               EVENT.EVNT_EXPOSE.X$L_EXEV_Y)
         END IF
      END DO
C-----------------------------------------------------------------------
      RETURN
      END
