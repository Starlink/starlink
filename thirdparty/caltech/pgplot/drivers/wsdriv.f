C*WSDRIV -- PGPLOT driver for VAX workstations running VWS software
C+
      SUBROUTINE WSDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for VAX workstations.
C
C Version 1.0 - 1988 Feb 3  - T. J. Pearson (after an original by
C                             John Biretta).
C
C Version 1.1 - 1988 Mar 14 - S. C. Allendorf 
C                             Make work for 4 plane devices and some
C                             other minor changes/bugs.
C
C Version 1.1001 - 1988 Mar 17 - S. L. Morris
C                             Number of color entries corrected for 8 
C                             plane system.
C
C Version 1.2 - 1988 Mar 18 - S. C. Allendorf
C                             Change PAUSE to LIB$GET_COMMAND and change
C                             cursor routines.
C
C Version 2.0 - 1988 Mar 23 - S. C. Allendorf
C                             Add hardware area and rectangle fills
C                             (OPCODE = 24).  General cleanup.
C
C Version 3.0 - 1988 Apr 1  - S. C. Allendorf
C                             Use absolute device coordinates and add
C                             keypad cursor control.
C
C Version 3.1 - 1988 Nov 24 - S. C. Allendorf
C                             Change cursor so that it can be seen 
C                             against all choices of background colors.
C
C Version 3.2 - 1989 Jan 6  - S. C. Allendorf
C                             Disable display list entries to gain a 
C                             little more speed (30% in a test program).
C
C Version 3.3 - 1989 Jan 8  - S. C. Allendorf
C                             Remove all magic numbers from the code and
C                             query the hardware to get them.
C
C Version 4.0 - 1989 Apr 5  - S. C. Allendorf
C                             Add support for line of pixels and correct
C                             error in device selection support.
C
C Version 4.1 - 1989 Jun 07 - S. C. Allendorf
C                             Fix varying resolution support and other
C                             minor changes.  General cleanup.
C
C Version 4.2 - 1990 May 30 - S. C. Allendorf
C                             Fix cursor routine to function properly
C                             in a workstation environment.
C
C Version 4.3 - 1993 Apr 23 - T. J. Pearson
C                             This driver crashes when used with UISX.
C                             Changed so that it only crashes if the
C                             caller tries to use it.
C=======================================================================
C
C Supported device: This driver should work with all VAX/VMS 
C workstations running VWS software; it requires the UISSHR
C shareable image provided by DEC.
C
C Device type code: /WS.
C
C Default device name: PGPLOT.  Output is always directed to device 
C SYS$WORKSTATION; the "device name" provided by the user is used to 
C label the PGPLOT window.
C
C Default view surface dimensions: Depends on monitor.
C
C Resolution: Depends on monitor.
C
C Color capability: VAX workstations can have 1, 4, or 8 bitplanes.
C On 1-plane devices, there are only two colors (background = white,
C color index 1 = black). On 4-plane devices, color indices 0-11
C are available (4 indices are reserved for text windows and pointers).
C On 8-plane systems, color indices 0-249 are available (6 indices
C are reserved for text windows and pointers).
C
C Input capability: The cursor is controlled by the mouse or the keypad
C available on the controlling (DEC-like) keyboard. The user positions 
C the cursor, and then types any key on the controlling keyboard.
C
C File format: It is not possible (at present) to send workstation
C plots to a disk file.
C
C Obtaining hardcopy: Not possible (at present).
C-----------------------------------------------------------------------
C
C PGPLOT can be used in three modes on VAX Workstations. 
C 
C (1) Tektronix emulation. If you run a process in a Tektronix emulation
C window, you can use device specification "/TEK" to tell PGPLOT to
C plot in Tektronix mode within the same window. If you run in a VT220
C window, you can tell PGPLOT to create a new Tektronix window and plot
C in it by giving a device specification "TK:/TEK". (TK: is the VMS
C device name of the Tektronix emulator.) This has one problem: the
C window will be deleted as soon as your program calls PGEND or exits;
C you may need to add a user-prompt in your program before the call of
C PGEND. 
C 
C (2) UIS mode. In UIS mode, PGPLOT calls the UIS subroutines for
C creating graphics on the workstation. This has some advantages over
C Tektronix emulation; e.g., it is faster, can use colors, and can
C erase. The number of colors available depends on the VAXstation
C model. Use device specification "/WS" to tell PGPLOT to create a new
C window and plot using UIS calls. Again, the window is deleted on
C program exit. PGPLOT executes a LIB$GET_COMMAND statement before
C exiting, however, so that you can view the picture before it
C disappears. Type <RETURN> at the prompt when you are ready to
C continue. This also makes it impossible to overlay a plot created by
C one program on a plot created by another. (The /APPEND qualifier
C which allows this for other devices has no effect on device /WS.)
C PGPLOT uses a window which is nominally 11 inches wide by 8.5 inches
C tall, i.e., the same size as you would get in a hardcopy. If you
C prefer a vertical orientation, execute the following command before
C running the program: 
C 
C $ DEFINE PGPLOT_WS_ASPECT PORTRAIT 
C 
C Substitute LANDSCAPE for PORTRAIT to revert to horizontal
C orientation. 
C 
C The PGPLOT cursor is controlled by the mouse or the keypad on the
C controlling keyboard. Type any keyboard key to notify PGPLOT when you
C have positioned the cursor. The mouse buttons are ignored (at 
C present).
C
C (3) DECWindows mode. In DECWindows mode, PGPLOT calls the XLIB
C functions for creating graphics on the workstation.
C-----------------------------------------------------------------------
      LOGICAL    CHEAP, INIT, LANDSCAPE, MONO
      BYTE       KBYTE, PIXEL(1024)
      INTEGER*2  CROSS(32), KWORD
      INTEGER*4  HEIGHT, IC, ICH, IER, IMAX, IMIN, I0, I1, JMAX, JMIN
      INTEGER*4  J0, J1, KBID, L, GRFMEM, GRGMEM, LMESS, MAXCOL
      INTEGER*4  NPTS, REMCAL, RESCOL, SMG$CREATE_VIRTUAL_KEYBOARD
      INTEGER*4  SMG$DELETE_VIRTUAL_KEYBOARD, SMG$READ_KEYSTROKE
      INTEGER*4  SMG$SET_KEYPAD_MODE, STEP, UISDC$SET_POINTER_POSITION
      INTEGER*4  UIS$CREATE_COLOR_MAP, UIS$CREATE_DISPLAY
      INTEGER*4  UIS$CREATE_WINDOW, UIS$PRESENT, VCMID, VDID, WDID
      INTEGER*4  WIDTH, XBUF, YBUF
      REAL*4     CTABLE(3, 16), PIXEL_X, PIXEL_Y, RESOL(2), SCALE, XHGHT
      REAL*4     XWDTH
      CHARACTER  ASPECT*20, MESS*4, MSG*10, NAME*3
      EQUIVALENCE (KBYTE, KWORD)
      DATA NAME /'WS '/
      DATA INIT, STEP /.TRUE., 4/
C                                       Set up the cursor bitmap
      DATA CROSS /6 * 256, 256, 65534, 256, 6 * 256, 0,
     +            6 * 256,   0, 64638,   0, 6 * 256, 0/
C                                       Initialize the color table
      DATA CTABLE /0.0,0.0,0.0, 1.0,1.0,1.0, 1.0,0.0,0.0, 0.0,1.0,0.0,
     1             0.0,0.0,1.0, 0.0,1.0,1.0, 1.0,0.0,1.0, 1.0,1.0,0.0,
     2             1.0,0.5,0.0, 0.5,1.0,0.0, 0.0,1.0,0.5, 0.0,0.5,1.0,
     3             0.5,0.0,1.0, 1.0,0.0,0.5, 0.333,0.333,0.333, 
     5             0.667,0.667,0.667/
C                                       These avoid using the includes
      PARAMETER  PATT$C_FOREGROUND = 2
      PARAMETER  SMG$K_TRM_PF1 = 256
      PARAMETER  SMG$K_TRM_PF2 = 257
      PARAMETER  SMG$K_TRM_PF3 = 258
      PARAMETER  SMG$K_TRM_PF4 = 259
      PARAMETER  SMG$K_TRM_KP1 = 261
      PARAMETER  SMG$K_TRM_KP2 = 262
      PARAMETER  SMG$K_TRM_KP3 = 263
      PARAMETER  SMG$K_TRM_KP4 = 264
      PARAMETER  SMG$K_TRM_KP5 = 265
      PARAMETER  SMG$K_TRM_KP6 = 266
      PARAMETER  SMG$K_TRM_KP7 = 267
      PARAMETER  SMG$K_TRM_KP8 = 268
      PARAMETER  SMG$K_TRM_KP9 = 269
      PARAMETER  SMG$K_TRM_UP = 274
      PARAMETER  SMG$K_TRM_DOWN = 275
      PARAMETER  SMG$K_TRM_LEFT = 276
      PARAMETER  SMG$K_TRM_RIGHT = 277
      PARAMETER  SS$_NORMAL = 1
      PARAMETER  UIS$C_MODE_COPY = 2
C-----------------------------------------------------------------------
C                                       On first call, find out what 
C                                       sort of workstation we have.
C
      IF (INIT .AND. IFUNC.NE.1) THEN
         INIT = .FALSE.
C                                       Check for the UIS library.
         IER = UIS$PRESENT ()
C                                       Only do the following if we
C                                       actually have a UIS workstation.
         IF (IER .EQ. SS$_NORMAL) THEN
C                                       Get the number of planes.
C                                       NOTE: This may only work for
C                                       monochrome and color displays.
C                                       The code may not work for
C                                       intensity displays.
            CALL UIS$GET_HW_COLOR_INFO ('SYS$WORKSTATION', , MAXCOL,
     1                                  , , , , , , RESCOL)
C                                       Find the display resolution.
            CALL UIS$GET_DISPLAY_SIZE ('SYS$WORKSTATION', XWDTH, XHGHT,
     1                                 PIXEL_X, PIXEL_Y)
            RESOL(1) = PIXEL_X * 2.54
            RESOL(2) = PIXEL_Y * 2.54
C                                       Calculate a scale factor to 
C                                       handle display devices with
C                                       different resolutions.
            SCALE = 77.446785 / MAX (RESOL(1), RESOL(2))
C                                       Calculate the size of the border
C                                       around the plot.
            IMIN = NINT (0.25 * RESOL(1) * SCALE)
            JMIN = NINT (0.25 * RESOL(2) * SCALE)
C                                       See what orientation we want.
            CALL GRGENV ('WS_ASPECT', ASPECT, L)
            IF (ASPECT(1:1) .EQ. 'P') THEN
C                                       Portrait mode (pixels).
               WIDTH = NINT (8.113636 * RESOL(1) * SCALE)
               HEIGHT = NINT (10.5 * RESOL(2) * SCALE)
               LANDSCAPE = .FALSE.
            ELSE
C                                       Landscape mode (pixels).
               WIDTH = NINT (11.0 * RESOL(1) * SCALE)
               HEIGHT = NINT (8.5 * RESOL(2) * SCALE)
               LANDSCAPE = .TRUE.
            END IF
C                                       Set the other border.
            IMAX = WIDTH - IMIN - 1
            JMAX = HEIGHT - JMIN - 1
C                                       Calculate the size of the window
C                                       in centimeters.
            XWDTH = FLOAT (WIDTH) / PIXEL_X
            XHGHT = FLOAT (HEIGHT) / PIXEL_Y
         ELSE
C                                       Deal with error on the open
C                                       workstation call.
            MAXCOL = 1
            RESCOL = 0
         END IF
C                                       Set the machine characteristics.
         IF (MAXCOL .EQ. 256) THEN
            NAME = 'WS8'
            MONO = .FALSE.
            CHEAP = .FALSE.
         ELSE IF (MAXCOL .EQ. 16) THEN
            NAME = 'WS4'
            MONO = .FALSE.
            CHEAP = .TRUE.
         ELSE IF (MAXCOL .EQ. 2) THEN
            NAME = 'WS1'
            MONO = .TRUE.
            CHEAP = .TRUE.
         ELSE
            NAME = 'WS0'
            MONO = .TRUE.
            CHEAP = .TRUE.
         END IF
C                                       Set maximum color index.
         MAXCOL = MAXCOL - RESCOL - 1
      END IF
C                                       Branch on opcode.
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260), IFUNC
  900 WRITE (MSG, '(I10)') IFUNC
      CALL GRWARN ('Unimplemented function in VAX/WS device driver:'
     1              // MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = NAME // '   (VAX UIS workstation)'
      LCHR =  27
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0.0
      RBUF(2) = FLOAT (IMAX - IMIN)
      RBUF(3) = 0.0
      RBUF(4) = FLOAT (JMAX - JMIN)
      RBUF(5) = 0.0
      RBUF(6) = FLOAT (MAXCOL)
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = RESOL(1)
      RBUF(2) = RESOL(2)
      RBUF(3) = 1.0
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Interactive, Cursor, No dashed lines, Area fill,
C    No thick lines, Rectangle fill, Line of pixels.)
C
   40 CONTINUE
      IF (CHEAP) THEN
         CHR = 'ICNANRNNNN'
      ELSE
         CHR = 'ICNANRPNNN'
      END IF
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = 'PGPLOT'
      LCHR = 6
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 RBUF(1) = 0.0
      RBUF(2) = FLOAT (IMAX - IMIN)
      RBUF(3) = 0.0
      RBUF(4) = FLOAT (JMAX - JMIN)
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 1.0
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
C                                       Return an error if UIS software 
C                                       is missing.
      IER = UIS$PRESENT ()
      IF (IER .NE. SS$_NORMAL) THEN
         CALL GRGMSG (IER)
         CALL GRWARN ('UIS is not installed on this system.')
         RBUF(2) = IER
         RETURN
      END IF
C                                       Open device.  First allocate a
C                                       color map.
      VCMID = UIS$CREATE_COLOR_MAP (MAXCOL + 1)
C                                       Create a display.
      VDID = UIS$CREATE_DISPLAY (0.0, 0.0, FLOAT (IMAX + IMIN), 
     1                         FLOAT (JMAX + JMIN), XWDTH, XHGHT, VCMID)
C                                       Disable display list entries
C                                       (~30% speed improvement).
      CALL UIS$DISABLE_DISPLAY_LIST (VDID)
C                                       Open a window.
      WDID = UIS$CREATE_WINDOW (VDID, 'SYS$WORKSTATION', CHR(:LCHR),
     1              0.0, 0.0, FLOAT (IMAX + IMIN), FLOAT (JMAX + JMIN),
     2              XWDTH, XHGHT)
C                                       Initialize device.  First
C                                       set the color registers.
      IF (MONO) THEN
C                                       Background (CI = 0) white, 
C                                       write (CI = 1) in black
         CALL UIS$SET_COLOR (VDID, 0, 1.0, 1.0, 1.0)
         CALL UIS$SET_COLOR (VDID, 1, 0.0, 0.0, 0.0)
      ELSE 
C                                       Define color indices 0-15;
C                                       background (CI = 0) black,
C                                       write (CI = 1) in white.
         DO 95 IC = 0, MIN (15, MAXCOL)
            CALL UIS$SET_COLOR (VDID, IC, CTABLE(1, IC + 1), 
     1                          CTABLE(2, IC + 1), CTABLE(3, IC + 1))
   95    CONTINUE
      END IF
C                                       Set the background color.
      CALL UIS$SET_BACKGROUND_INDEX (VDID, 0, 1, 0)
C                                       For some reason, this does not
C                                       work on monochrome systems.
      IF (.NOT. MONO) THEN
         CALL UIS$SET_WRITING_MODE (VDID, 1, 1, UIS$C_MODE_COPY)
      END IF
C                                       Set the font for fill patterns.
      CALL UIS$SET_FONT (VDID, 1, 1, 'UIS$FILL_PATTERNS')
C                                       Successful-- return wd_id.
      RBUF(1) = WDID
      RBUF(2) = 1.0
      NBUF = 2
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CALL GRGCOM (MESS, CHAR (7)//'Type <RETURN> to continue: ', LMESS)
C                                       Clean up resources.
      CALL UIS$DELETE_WINDOW (WDID)
      CALL UIS$DELETE_DISPLAY (VDID)
      CALL UIS$DELETE_COLOR_MAP (VCMID)
C                                       Reset the initialization
C                                       variable.
      INIT = .TRUE.
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
C                                       Clear the screen.
      CALL UISDC$ERASE (WDID)
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      I0 = INT (RBUF(1) + 0.5) + IMIN
      J0 = INT (RBUF(2) + 0.5) + JMIN
      I1 = INT (RBUF(3) + 0.5) + IMIN
      J1 = INT (RBUF(4) + 0.5) + JMIN
      CALL UISDC$PLOT (WDID, 1, I0, J0, I1, J1)
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      I0 = INT (RBUF(1) + 0.5) + IMIN
      J0 = INT (RBUF(2) + 0.5) + JMIN
      CALL UISDC$PLOT (WDID, 1, I0, J0)
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = RBUF(1)
      CALL UIS$SET_WRITING_INDEX (VDID, 1, 1, IC)
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
C                                       Create a virtual keyboard for 
C                                       cursor control.
      IER = SMG$CREATE_VIRTUAL_KEYBOARD (KBID, 'SYS$COMMAND')
      IF (IER .NE. SS$_NORMAL) THEN
         CALL GRGMSG (IER)
         CALL GRQUIT ('Failed to create a virtual keyboard.')
      END IF
C                                       Set the keyboard to keypad mode.
      IER = SMG$SET_KEYPAD_MODE (KBID, 1)
      IF (IER .NE. SS$_NORMAL) THEN
         CALL GRGMSG (IER)
         CALL GRQUIT ('Failed to set keypad mode.')
      END IF
C                                       Set cursor pattern to a cross.
      CALL UISDC$SET_POINTER_PATTERN (WDID, CROSS, 2, 8, 8, IMIN, JMIN, 
     1        IMAX, JMAX)
C                                       Convert input coordinates.
      I0 = INT (RBUF(1) + 0.5) + IMIN
      J0 = INT (RBUF(2) + 0.5) + JMIN
C                                       Set cursor to correct spot (if
C                                       it is in the PGPLOT window).
  175 IF (I0 .GE. IMIN .AND. I0 .LE. IMAX .AND.
     1    J0 .GE. JMIN .AND. J0 .LE. JMAX) THEN
         CALL UIS$POP_VIEWPORT (WDID)
         IER = UISDC$SET_POINTER_POSITION (WDID, I0, J0)
         IF (IER .NE. SS$_NORMAL) THEN
            CALL GRGMSG (IER)
            CALL GRQUIT ('Failed to set the pointer position.')
         END IF
      END IF
C                                       Wait for a keystroke.
      IER = SMG$READ_KEYSTROKE (KBID, ICH)
C                                       Read cursor location (this
C                                       covers the case where the user
C                                       moved the cursor with the 
C                                       mouse).
      CALL UISDC$GET_POINTER_POSITION (WDID, I0, J0)
C                                       Catch error returns.
      IF (IER .NE. SS$_NORMAL) ICH = 0
C                                       Handle the keypad keys.
      IF (ICH .EQ. SMG$K_TRM_UP .OR. ICH .EQ. SMG$K_TRM_KP8) THEN
         J0 = MIN (JMAX, J0 + STEP)
      ELSE IF (ICH .EQ. SMG$K_TRM_DOWN .OR. ICH .EQ. SMG$K_TRM_KP2) THEN
         J0 = MAX (JMIN, J0 - STEP)
      ELSE IF (ICH .EQ. SMG$K_TRM_LEFT .OR. ICH .EQ. SMG$K_TRM_KP4) THEN
         I0 = MAX (IMIN, I0 - STEP)
      ELSE IF (ICH .EQ. SMG$K_TRM_RIGHT .OR. 
     1         ICH .EQ. SMG$K_TRM_KP6) THEN
         I0 = MIN (IMAX, I0 + STEP)
      ELSE IF (ICH .EQ. SMG$K_TRM_KP7) THEN
         I0 = MAX (IMIN, I0 - STEP)
         J0 = MIN (JMAX, J0 + STEP)
      ELSE IF (ICH .EQ. SMG$K_TRM_KP9) THEN
         I0 = MIN (IMAX, I0 + STEP)
         J0 = MIN (JMAX, J0 + STEP)
      ELSE IF (ICH .EQ. SMG$K_TRM_KP3) THEN
         I0 = MIN (IMAX, I0 + STEP)
         J0 = MAX (JMIN, J0 - STEP)
      ELSE IF (ICH .EQ. SMG$K_TRM_KP1) THEN
         I0 = MAX (IMIN, I0 - STEP)
         J0 = MAX (JMIN, J0 - STEP)
      ELSE IF (ICH .EQ. SMG$K_TRM_KP5) THEN
         I0 = WIDTH / 2
         J0 = HEIGHT / 2
      ELSE IF (ICH .EQ. SMG$K_TRM_PF1) THEN
         STEP = 1
      ELSE IF (ICH .EQ. SMG$K_TRM_PF2) THEN
         STEP = 4
      ELSE IF (ICH .EQ. SMG$K_TRM_PF3) THEN
         STEP = 16
      ELSE IF (ICH .EQ. SMG$K_TRM_PF4) THEN
         STEP = 64
      END IF
C                                       Toss out unacceptable 
C                                       characters.
      IF (ICH .LT. 0 .OR. ICH .GT. 255) GOTO 175
C                                       Make sure the pointer is in the
C                                       PGPLOT window.
      IF (I0 .LT. IMIN .OR. I0 .GT. IMAX) GOTO 175
      IF (J0 .LT. JMIN .OR. J0 .GT. JMAX) GOTO 175
C                                       Delete the virtual keyboard.
      IER = SMG$DELETE_VIRTUAL_KEYBOARD (KBID)
      IF (IER .NE. SS$_NORMAL) THEN
         CALL GRGMSG (IER)
         CALL GRWARN ('Failed to delete virtual keyboard.')
      END IF
C                                       Return the cursor to normal.
      CALL UISDC$SET_POINTER_PATTERN (WDID, , , , , IMIN, JMIN, 
     1        IMAX, JMAX)
C                                       Set the return values.
      CHR(1:1) = CHAR (ICH)
      RBUF(1) = FLOAT (I0 - IMIN)
      RBUF(2) = FLOAT (J0 - JMIN)
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
      IF (REMCAL .EQ. 0) THEN
C                                       First time, set number of points
C                                       in polygon and allocate the 
C                                       memory for the arrays.
         NPTS = RBUF(1)
         REMCAL = NPTS
         IER = GRGMEM (4 * NPTS, XBUF)
         IF (IER .NE. SS$_NORMAL) THEN
            CALL GRGMSG (IER)
            CALL GRQUIT ('Failed to allocate temporary buffer.')
         END IF
         IER = GRGMEM (4 * NPTS, YBUF)
         IF (IER .NE. SS$_NORMAL) THEN
            CALL GRGMSG (IER)
            CALL GRQUIT ('Failed to allocate temporary buffer.')
         END IF
      ELSE
C                                       Second and succeeding calls,
C                                       change counter and load arrays.
         REMCAL = REMCAL - 1
         I0 = INT (RBUF(1) + 0.5) + IMIN
         J0 = INT (RBUF(2) + 0.5) + JMIN
         CALL GRWS00 (NPTS, %VAL (XBUF), %VAL (YBUF), REMCAL, I0, J0)
C                                       If last call, fill the area and
C                                       deallocate the memory.
         IF (REMCAL .EQ. 0) THEN
            CALL UIS$SET_FILL_PATTERN (VDID, 1, 1, PATT$C_FOREGROUND)
            CALL UISDC$PLOT_ARRAY (WDID, 1, NPTS, %VAL (XBUF), 
     1                                            %VAL (YBUF))
            CALL UIS$SET_FILL_PATTERN (VDID, 1, 1)
            IER = GRFMEM (4 * NPTS, XBUF)
            IF (IER .NE. SS$_NORMAL) THEN
               CALL GRGMSG (IER)
               CALL GRWARN ('Failed to deallocate temporary buffer.')
            END IF
            IER = GRFMEM (4 * NPTS, YBUF)
            IF (IER .NE. SS$_NORMAL) THEN
               CALL GRGMSG (IER)
               CALL GRWARN ('Failed to deallocate temporary buffer.')
            END IF
         END IF 
      END IF
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
C                                       Ignore for a monochrome device.
      IF (.NOT. MONO) THEN
         IC = RBUF(1)
         CALL UIS$SET_COLOR (VDID, IC, RBUF(2), RBUF(3), RBUF(4))
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
      CALL UIS$SET_FILL_PATTERN (VDID, 1, 1, PATT$C_FOREGROUND)
      I0 = INT (RBUF(1) + 0.5) + IMIN
      J0 = INT (RBUF(2) + 0.5) + JMIN
      I1 = INT (RBUF(3) + 0.5) + IMIN
      J1 = INT (RBUF(4) + 0.5) + JMIN
      CALL UISDC$PLOT (WDID, 1, I0, J0, I1, J0, I1, J1, I0, J1)
      CALL UIS$SET_FILL_PATTERN (VDID, 1, 1)
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
      IF (CHEAP) THEN
          GOTO 900
      ELSE
         I0 = INT (RBUF(1) + 0.5) + IMIN
         J0 = INT (RBUF(2) + 0.5) + JMIN
         I1 = I0 + NBUF - 3
         DO 265 IC = 1, NBUF - 2
            KWORD = INT (RBUF(IC + 2) + 0.5)
            PIXEL(IC) = KBYTE
  265    CONTINUE
         CALL UISDC$IMAGE (WDID, 1, I0, J0, I1, J0, 
     1                                  NBUF - 2, 1, 8, PIXEL)
      END IF
      RETURN
C-----------------------------------------------------------------------
      END
 
C*GRWS00 -- PGPLOT WS driver, load polygon arrays
C+
      SUBROUTINE GRWS00 (N0, XBUF, YBUF, N, X, Y)
      INTEGER N, N0, X, XBUF(N0), Y, YBUF(N0)
C--
      XBUF(N0 - N) = X
      YBUF(N0 - N) = Y
      RETURN
      END
