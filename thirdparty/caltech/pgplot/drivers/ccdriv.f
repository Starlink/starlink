C*CCDRIV -- PGPLOT DEC LJ250 Color Companion driver
C+
      SUBROUTINE CCDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      IMPLICIT NONE
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for DEC LJ250 Color Companion device.
C
C Version 1.0  - 1989 Jun 04 - S. C. Allendorf
C=======================================================================
C
C Supported device: DEC LJ250 Color Companion printer.
C
C Device type code: /CCP (portrait) or /CCL (landscape).
C
C Default device name: PGPLOT.CCPLT.
C
C Default view surface dimensions: 8.0 inches by 10.5 inches.
C
C Resolution: 90 dots/inch.
C
C Color capability: Color indices 0-15 are supported. It is not (yet)
C possible to change color representation.
C
C Input capability: None.
C
C File format: DEC color sixel format.
C
C Obtaining hardcopy: Use the VMS PRINT command.
C-----------------------------------------------------------------------
C 
C To choose portrait mode, you must execute a DCL command of the 
C following form before executing your program:
C
C $ DEFINE PGPLOT_CC_MODE PORTRAIT
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE
      PARAMETER (TYPE='CC (DEC LJ250 Color Companion printer)')
      BYTE       CTAB(3, 256), FF
      LOGICAL    HIRES, INIT, LANDSCAPE
      INTEGER*4  BUFFER, BX, BY, I, IC, IER, GRFMEM, GRGMEM
      INTEGER*4  LUN, MAXCOL, NPICT
      REAL*4     XBUF(4)
      CHARACTER  DEFNAM*12, MODE*20, MSG*10
      PARAMETER  (FF = 12)
      PARAMETER  (DEFNAM = 'PGPLOT.CCPLT')
      DATA INIT  /.TRUE./
      DATA CTAB  /100, 100, 100,      0,   0,   0,    100,   0,   0,
     1              0, 100,   0,      0,   0, 100,      0, 100, 100,
     2            100,   0, 100,    100, 100,   0,    100,  50,   0,
     3             50, 100,   0,      0, 100,  50,      0,  50, 100,
     4             50,   0, 100,    100,   0,  50,     33,  33,  33,
     5             67,  67,  67,    720 * 0/
C-----------------------------------------------------------------------
C                                       First time, do some one-time
C                                       initialization.
      IF (INIT) THEN
C                                       Make sure we only do this once.
         INIT = .FALSE.
C                                       Initialize the maximum color
C                                       index currently used.
         MAXCOL = 0
C                                       The default is low resolution,
C                                       landscape orientation.
         LANDSCAPE = .TRUE.
         HIRES = .FALSE.
C                                       Select mode based on logical.
         CALL GRGENV ('CC_MODE', MODE, I)
         IF (MODE(1:1) .EQ. 'P') LANDSCAPE = .FALSE.
         IF (MODE(2:2) .EQ. 'H') HIRES = .TRUE.
      END IF
C                                       Branch on opcode.
      GOTO ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     1      110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     2      210, 220, 230, 240, 250, 260), IFUNC
C                                       Signal an error.
  900 WRITE (MSG, '(I10)') IFUNC
      CALL GRWARN ('Unimplemented function in LJ250 device driver:' 
     1             // MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CONTINUE
      CHR = TYPE
      NBUF = 0
      LCHR = LEN(TYPE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 CONTINUE
      RBUF(1) = 0.0
      IF (HIRES .AND. LANDSCAPE)             RBUF(2) = 1889.0
      IF (HIRES .AND. .NOT. LANDSCAPE)       RBUF(2) = 1439.0
      IF (.NOT. HIRES .AND. LANDSCAPE)       RBUF(2) = 944.0
      IF (.NOT. HIRES .AND. .NOT. LANDSCAPE) RBUF(2) = 719.0
      RBUF(3) = 0.0
      IF (HIRES .AND. LANDSCAPE)             RBUF(4) = 1439.0
      IF (HIRES .AND. .NOT. LANDSCAPE)       RBUF(4) = 1889.0
      IF (.NOT. HIRES .AND. LANDSCAPE)       RBUF(4) = 719.0
      IF (.NOT. HIRES .AND. .NOT. LANDSCAPE) RBUF(4) = 944.0
      RBUF(5) = 0.0
      IF (HIRES) THEN
         RBUF(6) = 7.0
      ELSE
         RBUF(6) = 255.0
      END IF
      NBUF = 6
      LCHR = 0
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 CONTINUE
      IF (HIRES) THEN
         RBUF(1) = 180.0
      ELSE
         RBUF(1) = 90.0
      END IF
      RBUF(2) = RBUF(1)
      RBUF(3) = 1.0
      NBUF = 3
      LCHR = 0
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    no thick lines)
C
   40 CONTINUE
      CHR = 'HNNNNNNNNN'
      NBUF = 0
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CONTINUE
      CHR = DEFNAM
      NBUF = 0
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 CONTINUE
      RBUF(1) = 0.0
      IF (HIRES .AND. LANDSCAPE)             RBUF(2) = 1889.0
      IF (HIRES .AND. .NOT. LANDSCAPE)       RBUF(2) = 1439.0
      IF (.NOT. HIRES .AND. LANDSCAPE)       RBUF(2) = 944.0
      IF (.NOT. HIRES .AND. .NOT. LANDSCAPE) RBUF(2) = 719.0
      RBUF(3) = 0.0
      IF (HIRES .AND. LANDSCAPE)             RBUF(4) = 1439.0
      IF (HIRES .AND. .NOT. LANDSCAPE)       RBUF(4) = 1889.0
      IF (.NOT. HIRES .AND. LANDSCAPE)       RBUF(4) = 719.0
      IF (.NOT. HIRES .AND. .NOT. LANDSCAPE) RBUF(4) = 944.0
      NBUF = 4
      LCHR = 0
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 CONTINUE
      RBUF(1) = 1.0
      NBUF = 1
      LCHR = 0
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
C                                       Assume success.
      RBUF(2) = 1.0
C                                       Obtain a logical unit number.
      CALL GRGLUN (LUN)
C                                       Check for an error.
      IF (LUN .EQ. -1) THEN
          CALL GRWARN ('Cannot allocate a logical unit.')
          RBUF(2) = 0
          RETURN
      ELSE
         RBUF(1) = LUN
      END IF
C                                       Open the output file.
      OPEN (UNIT = LUN, FILE = CHR(:LCHR), CARRIAGECONTROL = 'NONE',
     1      DEFAULTFILE = DEFNAM, STATUS = 'NEW',
     2      RECL = 362, FORM = 'UNFORMATTED', RECORDTYPE = 'VARIABLE', 
     3      IOSTAT = IER)
C                                       Check for an error and cleanup if
C                                       one occurred.
      IF (IER .NE. 0) THEN
          CALL GRWARN ('Cannot open output file for LJ250 plot: ' //
     1                 CHR(:LCHR))
          RBUF(2) = 0
          CALL GRFLUN (LUN)
          RETURN
      ELSE
C                                       Get the full file specification
C                                       and calculate the length of the 
C                                       string
          INQUIRE (UNIT = LUN, NAME = CHR)
          LCHR = LEN (CHR)
   91     IF (CHR (LCHR:LCHR) .EQ. ' ') THEN
              LCHR = LCHR - 1
              GOTO 91
          END IF
      END IF
C                                       Initialize the page counter.
      NPICT = 0
      RETURN
C
C--- IFUNC = 10, Close workstation -------------------------------------
C
  100 CONTINUE
C                                       Close the file.
      CLOSE (LUN, STATUS = 'KEEP')
C                                       Deallocate the logical unit.
      CALL GRFLUN (LUN)
C
      RETURN
C
C--- IFUNC = 11, Begin picture -----------------------------------------
C
  110 CONTINUE
C                                       Calculate the dimensions of the
C                                       plot buffer.
      IF (LANDSCAPE) THEN
         XBUF(1) = RBUF(2)
         XBUF(2) = RBUF(1)
      ELSE
         XBUF(1) = RBUF(1)
         XBUF(2) = RBUF(2)
      END IF
      BX = INT (XBUF(1)) + 1
      BY = (INT (XBUF(2)) / 6 + 1) * 6
C                                       Allocate a plot buffer.      
      IER = GRGMEM (BX * BY, BUFFER)
C                                       Check for error and clean up
C                                       if one was found.
      IF (IER .NE. 1) THEN
          CALL GRGMSG (IER)
          CALL GRQUIT ('Failed to allocate a plot buffer.')
      END IF
C                                       Increment the page number.
      NPICT = NPICT + 1
C                                       Eject the page from the printer.
      IF (NPICT .GT. 1) WRITE (LUN) FF
C                                       Zero out the plot buffer.
      CALL GRCC04 (BX * BY, %VAL(BUFFER))
      RETURN
C
C--- IFUNC = 12, Draw line ---------------------------------------------
C
  120 CONTINUE
C                                       Apply any needed tranformation.
      IF (LANDSCAPE) THEN
         XBUF(1) = RBUF(2)
         XBUF(2) = (BY - 1) - RBUF(1)
         XBUF(3) = RBUF(4)
         XBUF(4) = (BY - 1) - RBUF(3)
      ELSE
         XBUF(1) = RBUF(1)
         XBUF(2) = RBUF(2)
         XBUF(3) = RBUF(3)
         XBUF(4) = RBUF(4)
      END IF
C                                       Draw the point into the bitmap.
      CALL GRCC00 (1, XBUF, IC, BX, BY, %VAL (BUFFER))
      RETURN
C
C--- IFUNC = 13, Draw dot ----------------------------------------------
C
  130 CONTINUE
C                                       Apply any needed tranformation.
      IF (LANDSCAPE) THEN
         XBUF(1) = RBUF(2)
         XBUF(2) = (BY - 1) - RBUF(1)
      ELSE
         XBUF(1) = RBUF(1)
         XBUF(2) = RBUF(2)
      END IF
C                                       Draw the point into the bitmap.
      CALL GRCC00 (0, XBUF, IC, BX, BY, %VAL(BUFFER))
      RETURN
C
C--- IFUNC = 14, End picture -------------------------------------------
C
  140 CONTINUE
C                                       Write out the bitmap.
      CALL GRCC01 (LUN, BX, BY, %VAL (BUFFER), MAXCOL, HIRES, CTAB)
C                                       Deallocate the plot buffer.
      IER = GRFMEM (BX * BY, BUFFER)
C                                       Check for an error.
      IF (IER .NE. 1) THEN
          CALL GRGMSG (IER)
          CALL GRWARN ('Failed to deallocate plot buffer.')
      END IF
      RETURN
C
C--- IFUNC = 15, Select color index ------------------------------------
C
  150 CONTINUE
C                                       Save the requested color index.
      IC = RBUF(1)
      MAXCOL = MAX (IC, MAXCOL)
      RETURN
C
C--- IFUNC = 16, Flush buffer. -----------------------------------------
C    (Not implemented: ignored.)
C
  160 CONTINUE
      RETURN
C
C--- IFUNC = 17, Read cursor. ------------------------------------------
C    (Not implemented: should not be called.)
C
  170 CONTINUE
      GOTO 900
C
C--- IFUNC = 18, Erase alpha screen. -----------------------------------
C    (Not implemented: ignored.)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC = 19, Set line style. ---------------------------------------
C    (Not implemented: should not be called.)
C
  190 CONTINUE
      GOTO 900
C
C--- IFUNC = 20, Polygon fill. -----------------------------------------
C    (Not implemented: should not be called.)
C
  200 CONTINUE
      GOTO 900
C
C--- IFUNC = 21, Set color representation. -----------------------------
C
  210 CONTINUE
      I = INT (RBUF(1) + 1.5)
      CTAB(1, I) = INT (RBUF(2) * 100.0 + 0.5)
      CTAB(2, I) = INT (RBUF(3) * 100.0 + 0.5)
      CTAB(3, I) = INT (RBUF(4) * 100.0 + 0.5)
      RETURN
C
C--- IFUNC = 22, Set line width. ---------------------------------------
C    (Not implemented: should not be called.)
C
  220 CONTINUE
      GOTO 900
C
C--- IFUNC = 23, Escape ------------------------------------------------
C    (Not implemented: ignored.)
C
  230 CONTINUE
      RETURN
C
C--- IFUNC = 24, Rectangle fill. ---------------------------------------
C    (Not implemented: should not be called.)
C
  240 CONTINUE
      GOTO 900
C
C--- IFUNC = 25, -------------------------------------------------------
C    (Not implemented: should not be called.)
C
  250 CONTINUE
      GOTO 900
C
C--- IFUNC = 26, Line of pixels. ---------------------------------------
C    (Not implemented: should not be called.)
C
  260 CONTINUE
      GOTO 900
C-----------------------------------------------------------------------
      END

C*GRCC00 -- PGPLOT LJ250 driver, draw a colored line
C+
      SUBROUTINE GRCC00 (LINE, RBUF, ICOL, BX, BY, BITMAP)
      IMPLICIT NONE
      INTEGER*4  BX, BY, ICOL, LINE
      BYTE       BITMAP(BX, BY)
      REAL*4     RBUF(4)
C
C Draw a straight line segment from absolute pixel coordinates (RBUF(1),
C RBUF(2)) to (RBUF(3), RBUF(4)).  The line overwrites the previous 
C contents of the bitmap with the current color index.  The line is 
C generated with a Simple Digital Differential Analyser (ref: Newman & 
C Sproull). 
C
C Arguments:
C
C LINE            I I      =0 for dot, =1 for line.
C RBUF(1),RBUF(2) I R      Starting point of line.
C RBUF(3),RBUF(4) I R      Ending point of line.
C ICOL            I I      Color index
C BITMAP        I/O B      (address of) the frame buffer.
C
C-----------------------------------------------------------------------
      INTEGER*4  K, KX, KY, LENGTH
      REAL*4     D, XINC, XP, YINC, YP
C-----------------------------------------------------------------------
      IF (LINE .GT. 0) THEN
         D = MAX (ABS (RBUF(3) - RBUF(1)), ABS (RBUF(4) - RBUF(2)))
         LENGTH = D
         IF (LENGTH .EQ. 0) THEN
            XINC = 0.0
            YINC = 0.0
         ELSE
            XINC = (RBUF(3) - RBUF(1)) / D
            YINC = (RBUF(4) - RBUF(2)) / D
         END IF
      ELSE
         LENGTH = 0
         XINC = 0.0
         YINC = 0.0
      END IF
      XP = RBUF(1) + 0.5
      YP = RBUF(2) + 0.5
      DO K = 0, LENGTH
         KX = XP
         KY = (BY - 1) - INT (YP)
         BITMAP(KX + 1, KY + 1) = ICOL
         XP = XP + XINC
         YP = YP + YINC
      END DO
C-----------------------------------------------------------------------
      RETURN
      END

C*GRCC01 -- PGPLOT LJ250 driver, copy bitmap to Sixel output file
C+
      SUBROUTINE GRCC01 (LUN, BX, BY, BITMAP, NC, HIRES, CTAB)
      IMPLICIT NONE
      LOGICAL  HIRES
      INTEGER  BX, BY, LUN, NC
      BYTE     BITMAP(BX, BY), CTAB(3, 256)
C
C Arguments:
C
C  LUN    (input)  Fortran unit number for output
C  BX, BY (input)  dimensions of BITMAP (BY MUST be a multiple of 6)
C  BITMAP (input)  the bitmap array
C  NC     (input)  the maximum color index used in the bitmap
C  CTAB   (input)  the color table
C-----------------------------------------------------------------------
      BYTE       ESC
      INTEGER*4  BUFF, GRCC03, I, IER, J, K, L, GRGMEM, M
      CHARACTER  BLUE*3, COL*3, GREEN*3, RED*3
      PARAMETER  (ESC = 27)
C-----------------------------------------------------------------------
C                                       Start Sixel graphics mode.
      IF (HIRES) THEN
         WRITE (LUN) ESC, 'P;1;;q"1;1;;-------'
      ELSE
         WRITE (LUN) ESC, 'P;1;8;q"1;1;;---'
      END IF
C                                       Write out the color table.
      DO I = 1, NC + 1
         J = GRCC03 (I - 1)
         K = CTAB(1, I)
         K = GRCC03 (K)
         L = CTAB(2, I)
         L = GRCC03 (L)
         M = CTAB(3, I)
         M = GRCC03 (M)
         WRITE (COL,   '(I3)') I - 1
         WRITE (RED,   '(I3)') CTAB(1, I)
         WRITE (GREEN, '(I3)') CTAB(2, I)
         WRITE (BLUE,  '(I3)') CTAB(3, I)
         WRITE (LUN) '#', COL(4 - J : 3), ';2;', RED(4 - K : 3), ';',
     1                    GREEN(4 - L : 3), ';', BLUE(4 - M : 3)
      END DO
C                                       Allocate a work array.
      IER = GRGMEM (BX * (NC + 1), BUFF)
C                                       Check for an error.
      IF (IER .NE. 1) THEN
         CALL GRGMSG (IER)
         CALL GRQUIT ('Failed to allocate temporary buffer.')
      END IF
C                                       Output the Sixel data.
      CALL GRCC02 (LUN, BX, BY, BITMAP, NC + 1, %VAL (BUFF))
C                                       Turn off Sixel graphics mode.
      WRITE (LUN) ESC, CHAR(92)
C-----------------------------------------------------------------------
      RETURN
      END

C*GRCC02 -- PGPLOT LJ250 driver, output the bitmap
C+
      SUBROUTINE GRCC02 (LUN, BX, BY, BITMAP, NC, SIXEL)
      IMPLICIT NONE
      INTEGER  BX, BY, LUN, NC
      BYTE     BITMAP(BX, BY), SIXEL(BX, NC)
C
C Version 1.0  18-Jun-1989  S. C. Allendorf
C-----------------------------------------------------------------------
      BYTE       CH, QMASK(6)
      LOGICAL    OUTPUT
      INTEGER*4  GRCC03, I, J, K, L, M, N, REPCNT
      CHARACTER  COL*3, OUTLINE*1445, REP*4
      DATA QMASK /'01'X, '02'X, '04'X, '08'X, '10'X, '20'X/
C-----------------------------------------------------------------------
C                                       Output the Sixel data.
      DO I = 1, BY / 6
C                                       Zero out the work array.
         CALL GRCC04 (BX * NC, SIXEL)
C                                       Create a Sixel line.
         DO J = 1, 6
            DO K = 1, BX
               L = BITMAP(K, (I - 1) * 6 + J) + 1
               SIXEL(K, L) = SIXEL(K, L) .OR. QMASK(J)
            END DO
         END DO
C                                       Loop through each color plane.
         DO J = 1, NC
C                                       Add the Sixel offset.
            DO K = 1, BX
               SIXEL(K, J) = SIXEL(K, J) + 63
            END DO
C                                       Initialize some variables for
C                                       run-length encoding.
            K = 1
            L = 1
            M = 1
            OUTPUT = .FALSE.
C                                       Stop if we are at the end of the
C                                       line.
   10       IF (K .LE. BX) THEN
C                                       Find the next character.
               CH = SIXEL(K, J)
C                                       Count the repeats.
   20          IF (M .LE. BX .AND. CH .EQ. SIXEL(M, J)) THEN
                  M = M + 1
                  GOTO 20
               END IF
C                                       Determine the length.
               REPCNT = M - K
C                                       See if there is any printable
C                                       data in this buffer.
               IF (REPCNT .NE. BX .OR. SIXEL(M - 1, J) .NE. 63) THEN
C                                       Mark the buffer as containing
C                                       printable data.
                  OUTPUT = .TRUE.
C                                       Fill the output buffer.               
                  IF (REPCNT .GE. 3) THEN
                     WRITE (REP, '(I4)') REPCNT
                     N = GRCC03 (REPCNT)
                     OUTLINE(L : L) = '!'
                     OUTLINE(L + 1 : L + N) = REP (5 - N : 4)
                     OUTLINE(L + N + 1 : L + N + 1) = 
     1                                            CHAR (SIXEL(M - 1, J))
                     L = L + N + 2
                  ELSE
                     DO N = 0, REPCNT - 1
                        OUTLINE(L + N : L + N) = CHAR (SIXEL(M - 1, J))
                     END DO
                     L = L + REPCNT
                  END IF
               END IF
C                                       Reinitialize the starting point
C                                       for the next string and jump to 
C                                       start of run length encoding. 
               K = M
               GOTO 10
            END IF
C                                       Write out the buffer if there is
C                                       any data in it.
            IF (OUTPUT) THEN
               WRITE (COL, '(I3)') J - 1
               N = GRCC03 (J - 1)
               WRITE (LUN) '#', COL(4 - N : 3), OUTLINE(1 : L - 1), '$'
            END IF
         END DO
C                                       Output a graphics linefeed.
         WRITE (LUN) '-'
      END DO
C------------------------------------------------------------------------
      RETURN
      END

C*GRCC03 -- PGPLOT LJ250 driver, calculate length of an integer
C+
      INTEGER FUNCTION GRCC03 (I)
      INTEGER I
C
C This function calculates the number of digits in a supplied integer.
C
C Arguments:
C
C I               I I      Integer value of number
C GRCC03          O I      Length of printed representation of I
C
C Version 1.0  10-Feb-1988  S. C. Allendorf
C-----------------------------------------------------------------------
      IF (I .GE. 10) THEN
         IF (I .GE. 100) THEN
            IF (I .GE. 1000) THEN
               GRCC03 = 4
            ELSE
               GRCC03 = 3
            END IF
         ELSE
            GRCC03 = 2
         END IF
      ELSE
         GRCC03 = 1
      END IF
C-----------------------------------------------------------------------
      RETURN
      END

C*GRCC04 -- zero fill buffer
C+
      SUBROUTINE GRCC04 (BUFSIZ,BUFFER)
C
C GRPCKG (internal routine): fill a buffer with a given character.
C
C Arguments:
C
C BUFFER (byte array, input): (address of) the buffer.
C BUFSIZ (integer, input): number of bytes in BUFFER.
C-----------------------------------------------------------------------
      INTEGER  BUFSIZ, I
      BYTE     BUFFER(BUFSIZ), FILL
      DATA     FILL /0/
C
      DO 10 I=1,BUFSIZ
          BUFFER(I) = FILL
   10 CONTINUE
      END
