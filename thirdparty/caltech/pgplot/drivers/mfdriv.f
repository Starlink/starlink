C*MFDRIV -- PGPLOT Graphics MetaFile driver
C+
      SUBROUTINE MFDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Graphics MetaFile device.
C
C Version 1.0 - 1989 May 09 - S. C. Allendorf
C                             First attempt at recreating the old
C                             MetaFile device.  Code based on original
C                             version written by Tim Pearson.
C Version 1.1 - 1989 May 20 - S. C. Allendorf
C                             Make driver conform as closely as possible
C                             to the standard without breaking GMFPLOT
C                             and/or PGPLOT.  Deviations from the 
C                             standard are marked with 
C                             *** DEVIATION ***.  GMFPLOT and/or PGPLOT 
C                             would need to be changed to correct these
C                             parts.
C=======================================================================
C
C Supported device: The MetaFile device can be used to a store graphic 
C image in a device-independent disk file.
C
C Device type code: /FILE.
C
C Default device name: PGPLOT.GMF.
C
C Default view surface dimensions: Undefined (nominally 8 inches
C square).
C
C Resolution: Undefined.
C
C Color capability: Color indices 0-255 are accepted and the
C representation of all colors may be changed.  The actual colors used
C depend upon the output device chosen when the file is rendered.
C
C Input capability: None.
C
C File format: The metafile generated follow the "GSPC Metafile 
C Proposal" described in Computer Graphics (A.C.M.), Volume 13, number 3
C (August 1979).
C
C Obtaining hardcopy: Use the translator program GMFPLOT.
C-----------------------------------------------------------------------
      LOGICAL    CONT
      INTEGER*2  BUFFER(360), COMBUF(5), I0, I1, IB, IC, IG, IR, J0, J1
      INTEGER*2  LASTI, LASTJ, NPICT, NPTS
      INTEGER*4  HW, IER, LUN, REMCAL
      REAL*4     RATIO, SCALE, XMAX, YMAX
      CHARACTER  MSG*10
      CHARACTER*(*) DEFNAM, TYPE
      PARAMETER  (DEFNAM = 'PGPLOT.GMF')
      PARAMETER  (TYPE = 'FILE  (PGPLOT graphics metafile)')
C-----------------------------------------------------------------------
C                                      Branch on opcode.
      GOTO ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     1      110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     2      210, 220, 230, 240, 250, 260), IFUNC
C                                      Signal an error.
  900 WRITE (MSG, '(I10)') IFUNC
      CALL GRWARN ('Unimplemented function in MetaFile device driver:' 
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
      RBUF(2) = 32767.0
      RBUF(3) = 0.0
      RBUF(4) = 32767.0
      RBUF(5) = 0.0
      RBUF(6) = 255.0
      NBUF = 6
      LCHR = 0
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 CONTINUE
      RBUF(1) = 4096.0
      RBUF(2) = 4096.0
      RBUF(3) = 1.0
      NBUF = 3
      LCHR = 0
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, No cursor, Dashed lines, Area fill,
C    Thick lines, Rectangle fill, No line of pixels)
C
   40 CONTINUE
      CHR = 'HNDATRNNNN'
      NBUF = 0
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CONTINUE
      CHR = DEFNAM
      NBUF = 0
      LCHR = LEN (DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 CONTINUE
      RBUF(1) = 0.0
      RBUF(2) = 32767.0
      RBUF(3) = 0.0
      RBUF(4) = 32767.0
      NBUF = 4
      LCHR = 0
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 CONTINUE
      RBUF(1) = 20.0
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
C                                      Assume success.
      RBUF(2) = 1.0
C                                      Obtain a logical unit number.
      CALL GRGLUN (LUN)
C                                      Check for an error.
      IF (LUN .EQ. -1) THEN
         CALL GRWARN ('Cannot allocate a logical unit.')
         RBUF(2) = 0.0
         RETURN
      ELSE
         RBUF(1) = LUN
      END IF
C                                      Open the output file.
      OPEN (UNIT = LUN, FILE = CHR(:LCHR), CARRIAGECONTROL = 'NONE',
     1      DEFAULTFILE = DEFNAM, DISPOSE = 'DELETE', STATUS = 'NEW',
     2      RECL = 180, FORM = 'UNFORMATTED', RECORDTYPE = 'FIXED', 
     3      IOSTAT = IER)
C                                      Check for an error and cleanup if
C                                      one occurred.
      IF (IER .NE. 0) THEN
         CALL GRWARN ('Cannot open output file for MetaFile plot: ' //
     1                CHR(:LCHR))
         CALL GRFLUN (LUN)
         RBUF(2) = 0
         RETURN
      ELSE
C                                      Get the full file specification
C                                      and calculate the length of the 
C                                      string
         INQUIRE (UNIT = LUN, NAME = CHR)
         LCHR = LEN (CHR)
   95    IF (CHR (LCHR:LCHR) .EQ. ' ') THEN
            LCHR = LCHR - 1
            GOTO 95
         END IF
      END IF
C                                      Initialize the page counter.
      NPICT = 0
C                                      Initialize the high water mark.
      HW = 0
C                                      Send the BEGIN_METAFILE command,
C                                      requesting 15-bit precision.
      COMBUF(1) = '8001'X
      COMBUF(2) = '0001'X
      CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
      RETURN
C
C--- IFUNC = 10, Close workstation -------------------------------------
C
  100 CONTINUE
C                                      Send the END_METAFILE command.
      CALL GRMF01 (1, '8100'X, BUFFER, LUN, HW)
C                                      Flush the buffer.
      CALL GRMF02 (LUN, HW, BUFFER)
C                                      Close the file.
      CLOSE (LUN, DISPOSE = 'KEEP')
C                                      Deallocate the logical unit.
      CALL GRFLUN (LUN)
C
      RETURN
C
C--- IFUNC = 11, Begin picture -----------------------------------------
C
  110 CONTINUE
C                                      Increment the page number.
      NPICT = NPICT + 1
C                                      *** DEVIATION ***
C                                      The MetaFile standard defines 
C                                      the initial pen position to be at
C                                      (0, 0).  This causes problems for
C                                      PGPLOT.
C
C                                      Set the last position to unknown.
      LASTI = -1
      LASTJ = -1
C                                      Check to see if this is the first 
C                                      picture.
      IF (NPICT .EQ. 1) THEN
C                                      Initialize the requested size and
C                                      and scale factor.
         XMAX = INT (RBUF(1) + 0.5)
         YMAX = INT (RBUF(2) + 0.5)
         SCALE = 1.0
C                                      See if the user has requested a
C                                      specific size.
         IF (XMAX .NE. 32767.0 .OR. YMAX .NE. 32767.0) THEN
C                                      Calculate the the maximum 
C                                      coordinates and the scale factor.
            COMBUF(2) = 32767
            COMBUF(3) = 32767
            RATIO = (YMAX + 1.0) / (XMAX + 1.0)
            IF (RATIO .LT. 1.0) THEN
               SCALE = 32767.0 / XMAX
               XMAX = 32767.0
               YMAX = INT (32768.0 * RATIO - 0.5)
               COMBUF(3) = YMAX
            ELSE IF (RATIO .GT. 1.0) THEN
               SCALE = 32767.0 / YMAX
               XMAX = INT (32768.0 / RATIO - 0.5)
               YMAX = 32767.0
               COMBUF(2) = XMAX
            ELSE
               SCALE = 32767.0 / XMAX
               XMAX = 32767.0
               YMAX = 32767.0
            END IF
C                                      Send DEFINE_NDC_SPACE command 
C                                      along with X, Y, and Z ranges if
C                                      the user hasn't requested a
C                                      square plot.
            IF (RATIO .NE. 1.0) THEN
               COMBUF(1) = '8203'X
               COMBUF(4) = 0
               CALL GRMF01 (4, COMBUF, BUFFER, LUN, HW)
            END IF
         END IF
      END IF
C                                      Flush buffer to get to a record
C                                      boundary.
      CALL GRMF02 (LUN, HW, BUFFER)
C                                      Send BEGIN_PICTURE command with
C                                      the picture number.
      COMBUF(1) = '9001'X
      COMBUF(2) = NPICT
      CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
      RETURN
C
C--- IFUNC = 12, Draw line ---------------------------------------------
C
  120 CONTINUE
C                                      Scale and convert to integer.
      I0 = INT (MIN (RBUF(1) * SCALE + 0.5, XMAX))
      J0 = INT (MIN (RBUF(2) * SCALE + 0.5, YMAX))
      I1 = INT (MIN (RBUF(3) * SCALE + 0.5, XMAX))
      J1 = INT (MIN (RBUF(4) * SCALE + 0.5, YMAX))
C                                      See if this is a continuation.
      CONT = (LASTI .EQ. I0) .AND. (LASTJ .EQ. J0)
C                                      Draw the line.
      CALL GRMF00 (I0, J0, I1, J1, CONT, BUFFER, LUN, HW)
C                                      Update the last position
      LASTI = I1
      LASTJ = J1
      RETURN
C
C--- IFUNC = 13, Draw dot ----------------------------------------------
C
  130 CONTINUE
C                                      Convert to integer.
      I0 = INT (MIN (RBUF(1) * SCALE + 0.5, XMAX))
      J0 = INT (MIN (RBUF(2) * SCALE + 0.5, YMAX))
C                                      Draw the dot.
      CALL GRMF00 (I0, J0, I0, J0, .FALSE., BUFFER, LUN ,HW)
C                                      Update the last position.
      LASTI = I0
      LASTJ = J0
      RETURN
C
C--- IFUNC = 14, End picture -------------------------------------------
C
  140 CONTINUE
C                                      Send a END_PICTURE command.
      CALL GRMF01 (1, '9100'X, BUFFER, LUN, HW)
      RETURN
C
C--- IFUNC = 15, Select color index ------------------------------------
C
  150 CONTINUE
C                                      Save the requested color index.
      IC = RBUF(1)
C                                      *** DEVIATION ***
C                                      The MetaFile standard defines 
C                                      indices 0-7 and they are 
C                                      different than those defined by 
C                                      PGPLOT.
C
C                                      Send the SET_COLOR command along 
C                                      with the color index.
      COMBUF(1) = 'C101'X
      COMBUF(2) = IC
      CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
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
C
  190 CONTINUE
C                                      Convert to an integer.
      IC = RBUF(1)
C                                      Send SET_LINESTYLE command along
C                                      width the requested linestyle.
      COMBUF(1) = 'C301'X
      COMBUF(2) = IC
      CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
      RETURN
C
C--- IFUNC = 20, Polygon fill. -----------------------------------------
C
  200 CONTINUE
      IF (REMCAL .EQ. 0) THEN
C                                      First time, send DRAW_POLYGON and
C                                      the number of points.
         NPTS = RBUF(1)
         REMCAL = NPTS
         COMBUF(1) = 'A701'X
         COMBUF(2) = NPTS
         CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
      ELSE
C                                      Second and succeeding calls,
C                                      MOVE to first point, DRAW to the
C                                      rest, and decrement the counter.
         COMBUF(1) = INT (MIN (RBUF(1) * SCALE + 0.5, XMAX))
         COMBUF(2) = INT (MIN (RBUF(2) * SCALE + 0.5, YMAX))
         IF (REMCAL .NE. NPTS) COMBUF(2) = IBSET (COMBUF(2), 15)
         CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
         REMCAL = REMCAL - 1
C                                      *** DEVIATION ***
C                                      The MetaFile standard defines 
C                                      the pen position after a polygon
C                                      draw to be at the first point.
C                                      This causes problems for PGPLOT.
C
C                                      Set the pen position to unknown.
         IF (REMCAL .EQ. 0) LASTI = -1
      END IF
      RETURN
C
C--- IFUNC = 21, Set color representation. -----------------------------
C
  210 CONTINUE
C                                      *** DEVIATION ***
C                                      The MetaFile standard defines 
C                                      indices 0-7 and does not allow 
C                                      them to be changed.
C
C                                      Convert input to integer
      IC = RBUF(1)
      IR = INT (MIN (32767.0, MAX (RBUF(2) * 32767.0, 0.0)))
      IG = INT (MIN (32767.0, MAX (RBUF(3) * 32767.0, 0.0)))
      IB = INT (MIN (32767.0, MAX (RBUF(4) * 32767.0, 0.0)))
C                                      Send DEFINE_COLOR_INDEX command
C                                      along with the index to be 
C                                      defined and its definition.
      COMBUF(1) = 'C004'X
      COMBUF(2) = IC
      COMBUF(3) = IR
      COMBUF(4) = IG
      COMBUF(5) = IB
      CALL GRMF01 (5, COMBUF, BUFFER, LUN, HW)
      RETURN
C
C--- IFUNC = 22, Set line width. ---------------------------------------
C
  220 CONTINUE
C                                      *** DEVIATION ***
C                                      The MetaFile standard defines 
C                                      linewidths differently than
C                                      PGPLOT.
C
C                                      Convert to an integer.
      IC = RBUF(1)
C                                      Send SET_LINEWIDTH command along
C                                      with the requested line width.
      COMBUF(1) = 'C401'X
      COMBUF(2) = IC
      CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
      RETURN
C
C--- IFUNC = 23, Escape ------------------------------------------------
C    (Not implemented: ignored.)
C
  230 CONTINUE
      RETURN
C
C--- IFUNC = 24, Rectangle fill. ---------------------------------------
C
  240 CONTINUE
C                                      Scale and convert to integer.
      I0 = INT (MIN (RBUF(1) * SCALE + 0.5, XMAX))
      J0 = INT (MIN (RBUF(2) * SCALE + 0.5, YMAX))
      I1 = INT (MIN (RBUF(3) * SCALE + 0.5, XMAX))
      J1 = INT (MIN (RBUF(4) * SCALE + 0.5, YMAX))
C                                      Simulate a hardware area fill.
      COMBUF(1) = 'A701'X
      COMBUF(2) = 4
      CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
      COMBUF(1) = I0
      COMBUF(2) = J0
      CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
      COMBUF(1) = I1
      COMBUF(2) = IBSET (J0, 15)
      CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
      COMBUF(1) = I1
      COMBUF(2) = IBSET (J1, 15)
      CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
      COMBUF(1) = I0
      COMBUF(2) = IBSET (J1, 15)
      CALL GRMF01 (2, COMBUF, BUFFER, LUN, HW)
C                                      *** DEVIATION ***
C                                      The MetaFile standard defines 
C                                      the pen position after a polygon
C                                      draw to be at the first point.
C                                      This causes problems for PGPLOT.
C
C                                      Set the pen position to unknown.
      LASTI = -1
      RETURN
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

C*GRMF00 -- PGPLOT MetaFile driver, draw a line segment
C+
      SUBROUTINE GRMF00 (I0, J0, I1, J1, CONT, BUFFER, LUN, HW)
      LOGICAL    CONT
      INTEGER*2  BUFFER(360), I0, I1, J0, J1
      INTEGER*4  HW, LUN
C-----------------------------------------------------------------------
C Draw a line. This requires a MOVE command (unless the starting point 
C is the same point as the end point of the last line) followed by a 
C DRAW command.
C
C Arguments:
C
C  I0, J0 (input)         The absolute device coordinates of the 
C                            starting point of the line
C  I1, J1 (input)         The absolute device coordinates of the ending
C                            point of the line
C  CONT   (input)         Flag denoting whether the line is a 
C                            continuation
C  BUFFER (input/output)  The buffer
C-----------------------------------------------------------------------
      INTEGER*2  OUTPUT(4)
      INTEGER*4  K
C-----------------------------------------------------------------------
C                                      Initialize the counter.
      K = 0
C                                      See if we need to MOVE first.
      IF (.NOT. CONT) THEN
C                                      Increment the counter.
         K = 2
C                                      Output the coordinates.
         OUTPUT(1) = I0
         OUTPUT(2) = J0
      END IF
C                                      Send the x coordinate.
      OUTPUT(K + 1) = I1
C                                      Mark the y coordinate as a DRAW
C                                      command and output it.
      OUTPUT(K + 2) = IBSET (J1, 15)
C                                      Increment the counter.
      K = K + 2
C                                      Transfer the coordinates to the
C                                      buffer.
      CALL GRMF01 (K, OUTPUT, BUFFER, LUN, HW)
C-----------------------------------------------------------------------
      RETURN
      END

C*GRMF01 -- PGPLOT MetaFile driver, transfer chunks to output buffer
C+
      SUBROUTINE GRMF01 (N, CHUNKS, BUFFER, LUN, HW)
      INTEGER*4  HW, LUN, N
      INTEGER*2  CHUNKS(N), BUFFER(360)
C
C Transfer metafile chunks to output buffer. If the command would
C overflow, it is flushed to the output device using routine GRMF02.
C
C Arguments:
C
C  N      (input)         The number of chunks to transfer
C  CHUNKS (input)         The chunks to transfer
C  BUFFER (input/output)  The buffer
C  LUN    (input)         Fortran unit number for output
C  HW     (input/output)  Number of elements used in BUFFER
C-----------------------------------------------------------------------
      INTEGER*4  I
C-----------------------------------------------------------------------
C                                      Flush the buffer if the command
C                                      would overflow it.
      IF (HW + N .GT. 360) CALL GRMF02 (LUN, HW, BUFFER)
C                                      Transfer the chunks to the
C                                      buffer.
      DO 10 I = 1, N
C                                      Increment the high water mark.
         HW = HW + 1
C                                      Move the chunk to the buffer.
         BUFFER(HW) = CHUNKS(I)
   10 CONTINUE
C-----------------------------------------------------------------------
      RETURN
      END

C*GRMF02 -- PGPLOT MetaFile driver, flush metafile buffer contents
C+
      SUBROUTINE GRMF02 (LUN, HW, BUFFER)
      INTEGER*2  BUFFER(360)
      INTEGER*4  HW, LUN
C
C Flush metafile buffer contents. If the buffer is not full, it is 
C padded with NO_OPERATION commands.
C
C Arguments:
C
C  LUN    (input)         Fortran unit number for output
C  HW     (input/output)  Number of elements used in BUFFER
C  BUFFER (input/output)  The buffer
C-----------------------------------------------------------------------
      INTEGER*4  I
C-----------------------------------------------------------------------
C                                      See if the buffer has anything in
C                                      it.
      IF (HW .GT. 0) THEN
C                                      Fill buffer with NO_OPERATION
C                                      commands.
         DO 10 I = HW + 1 ,360
            BUFFER(I) = '8400'X
   10    CONTINUE
C                                      Write out the buffer.
         WRITE (LUN) BUFFER
C                                      Reset the high water mark.
         HW = 0
      END IF
C-----------------------------------------------------------------------
      RETURN
      END
