C*EXDRIV -- PGPLOT Talaris/EXCL driver (landscape mode)
C+
      SUBROUTINE    EXDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER       IFUNC, NBUF, LCHR, MODE
      REAL          RBUF(*)
      CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for EXCL devices (Talaris)
C-----------------------------------------------------------------------
C Version 1.0 - 1989 Nov 10 - A. L. Fey.
C         2.0 - 1994 Nov 08
C-----------------------------------------------------------------------
C
C Supported device: Any Talaris printer that accepts the EXCL
C                   page description language. 7-bit mode is used.
C
C Device type code: /EXCL  (landscape)
C                   /VEXCL (portrait)
C
C Default file name: pgplot.explot.
C
C Default view surface dimensions: 10.25 inches horizontal x 7.75 inches
C                                  vertical (landscape mode).
C Default view surface dimensions: 7.75 inches horizontal x 10.25 inches
C                                  vertical (portrait mode).
C
C Resolution: The driver uses coordinate increments of 1/1000 inch.
C             The true resolution is device-dependent; at time of
C             writing, it is typically 300 dots per inch.
C
C Color capability: Color indices 0 (erase), and 1 (black) are 
C                   supported. Requests for other color indices are
C                   converted to 1. It is not possible to change color
C                   representation. 
C
C Input capability: None.
C
C File format: Ascii, variable length records (max 80 bytes).
C-----------------------------------------------------------------------
C
      CHARACTER*(*) DEFNAM
      REAL WIDTH, HEIGHT, MARGIN
C--
      PARAMETER    (DEFNAM = 'pgplot.explot')
      PARAMETER    (WIDTH  = 11000.0)
      PARAMETER    (HEIGHT = 8500.0)
      PARAMETER    (MARGIN = 375)
C--
      CHARACTER*(1) ESC, SP
      CHARACTER*(2) ESCP, ESCLB, ESCBS
C      PARAMETER    (ESC    = CHAR (27))
C      PARAMETER    (SP     = CHAR (32))
C      PARAMETER    (ESCP   = CHAR (27)//CHAR (80))
C      PARAMETER    (ESCLB  = CHAR (27)//CHAR (91))
C      PARAMETER    (ESCBS  = CHAR (27)//CHAR (92))
C
      CHARACTER*80  BUFFER
      CHARACTER*80  INSTR
      CHARACTER*10  MSG
      INTEGER       UNIT, I0, J0, I1, J1, IC
      INTEGER       L, NPAGE, NPTS, PENWID
      REAL          EXSCAL, EYSCAL
      REAL          EXSIZE, EYSIZE
      REAL          XRESOL, YRESOL
      LOGICAL       NOTHIN
      INTEGER GRTRIM, GROPTX
C
C ---- BUFFER should not exceed 80 - this makes well formed EXCL files.
C ---- The E*SIZE parameters are the physical size of the plot (used
C ---- more than once here) in resolution units (1/1000 inch).  The
C ---- *OFF parameters are offsets from the physical origin of the
C ---- page assuming a page size of 8.5 x 11 inches. The E*SCAL
C ---- parameters are PGPLOT-modifiable scale factors.
C
      PARAMETER    (XRESOL = 1000.00,
     :              YRESOL = 1000.00,
     :              PENWID = 5)
      SAVE EXSIZE, EYSIZE

C ---- Linux can not have these in a parameter statement
      ESC    = CHAR (27)
      SP     = CHAR (32)
      ESCP   = CHAR (27)//CHAR (80)
      ESCLB  = CHAR (27)//CHAR (91)
      ESCBS  = CHAR (27)//CHAR (92)

C
C=======================================================================
C
C ---- Do the best one can in F77 for a "case" statement. --------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
C
C ---- Unknown opcode/function; most likely a logic error somewhere ----
C
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in EXCL device driver:'//MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 IF(MODE .EQ. 1) THEN
        CHR = 'EXCL  (Talaris/EXCL printers, landscape)'
      ELSE
        CHR = 'VEXCL (Talaris/EXCL printers, portrait)'
      END IF
      LCHR = GRTRIM(CHR)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C               Units are in device co-ordinates (1/1000 inches)
C
   20 RBUF(1) = 0.0
      RBUF(3) = 0.0
      RBUF(5) = 0.0
      RBUF(6) = 1.0
      IF(MODE .EQ. 1) THEN
        RBUF(2) = WIDTH - 2*MARGIN
        RBUF(4) = HEIGHT - 2*MARGIN
      ELSE
        RBUF(2) = HEIGHT - 2*MARGIN
        RBUF(4) = WIDTH - 2*MARGIN
      END IF
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C    (Nominal values)
C
   30 RBUF(1) = XRESOL
      RBUF(2) = YRESOL
C
C      (multiple strokes are spaced by PENWID/*RESOL inches)
C
      RBUF(3) = PENWID
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (Hardcopy, No cursor, No Dashed lines, Area fill, No Thick lines) 
C
   40 CHR = 'HNNANNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR  = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C    (in device coordinates).
C
   60 RBUF(1) = 0.0
      RBUF(3) = 0.0
      IF(MODE .EQ. 1) THEN
        RBUF(2) = WIDTH - 2*MARGIN
        RBUF(4) = HEIGHT - 2*MARGIN
      ELSE
        RBUF(2) = HEIGHT - 2*MARGIN
        RBUF(4) = WIDTH - 2*MARGIN
      END IF
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 1.0
      NBUF=1
      RETURN
C
C--- IFUNC = 8, Select plot --------------------------------------------
C    Future option, nothing done yet.  (Multiple devices open at one 
C    time will be allowed later; this opcode will select active device).
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation ---------------------------------------
C
   90 CONTINUE
C
C     -- Get a Unit number.
C
      CALL GRGLUN(UNIT)
C
C     -- Open the file.
C
      NBUF = 2
      RBUF(1) = UNIT
      IF(GROPTX(UNIT, CHR(1:LCHR), DEFNAM, 1) .NE. 0) THEN
          CALL GRWARN('Cannot open output file for EXCL plot')
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
          RETURN
      ELSE
          INQUIRE (UNIT=UNIT, NAME=CHR)
          LCHR = GRTRIM(CHR)
          RBUF(2) = 1
      END IF
C
C     -- initialization
C
      EXSCAL = 1.0
      EYSCAL = 1.0
      IF(MODE .EQ. 1) THEN
        EXSIZE = WIDTH  - 2 * MARGIN
        EYSIZE = HEIGHT - 2 * MARGIN
      ELSE
        EXSIZE = HEIGHT - 2 * MARGIN
        EYSIZE = WIDTH  - 2 * MARGIN
      END IF
      NOTHIN = .TRUE.
      NPAGE = 0
      NPTS = 0
C
C     -- Start vector graphics mode.
C        Initialize EXCL, set ANSI mode to EXCL (TALAMS), reset the
C        printstation to power-up state (TALMOD), set copy count
C        to one (TALCCNT), set paper size to letter (TALFCTL),
C        turn on Absorb Forms Control (TALASF).
C
      BUFFER = ESCLB//'0*s'
     :       //ESCLB//'1;0r'
     :       //ESCLB//'1;0u'
     :       //ESCLB//';3x'
     :       //ESCLB//'1'//SP//'z'
      CALL GREX00 (UNIT, BUFFER, 27)
C
C     -- Comments; write user and date created to file.
C
      CALL GREX00 (UNIT,
     :   ESC//'Q ------------------------------ '//ESC//'R', 36)
      CALL GRUSER (INSTR, L)
      BUFFER =
     :   ESC//'Q -- EXCL plot created by '//INSTR(1:L)//' -- '//ESC//'R'
      CALL GREX00 (UNIT, BUFFER, 33+L)
      CALL GRDATE (INSTR, L)
      BUFFER =
     :   ESC//'Q -- EXCL plot created on '//INSTR(1:L)//' -- '//ESC//'R'
      CALL GREX00 (UNIT, BUFFER, 33+L)
      CALL GREX00 (UNIT,
     :     ESC//'Q ------------------------------ '//ESC//'R', 36)
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
C
      IF (NOTHIN) THEN
C
C        -- Nothing was plotted so no need to keep the file around.
C
         CLOSE (UNIT)
C
      ELSE
C
C        -- Don't need to formfeed; end picture will do that.
C           Reset printstation to its power-up state.
C
         BUFFER = ESC//'Q -- End of File -- '//ESC//'R'//ESC//'c'
         CALL GREX00 (UNIT, BUFFER, 25)
         CLOSE (UNIT)
      ENDIF
C
C     -- Return UNIT to free pool.
C
      CALL GRFLUN(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture and possibly rescale -----------------------
C
  110 CONTINUE
      NPAGE = NPAGE + 1
C
C     --  Define the unit of measure to be mils (TALPRM), define the page
C         orientation to be landscape or portrait (TALPGO), set page
C         clipping window to default page size (TALPCW), and set the
C         origin to the top left of the physical page at MARGIN,MARGIN 
C         (TALORG).
C
      BUFFER = ESCLB//'3y'
     :       //ESCLB//'0p'
     :       //ESCLB//'0;;;0*c'
      IF(MODE .EQ. 1) THEN
        BUFFER(7:7)='1'
      ELSE
        BUFFER(7:7)='0'
      END IF
      WRITE (BUFFER (18:31), 1000) ESCLB, MARGIN, MARGIN
 1000 FORMAT (A2, I5.5, ';', I5.5, 'o')
      CALL GREX00 (UNIT, BUFFER, 31)
C
C     --  Set the line defaults (TALGLP, TALGLT) and set fill
C         pattern (TALGRP).
C
      BUFFER = ESCLB//'  ;  ;8;1;112;5279;1*w'
     :       //ESCLB//'112;5279;0*r'
     :       //ESCLB//'*t'
      WRITE (BUFFER (3:4), '(I2.2)') PENWID
      WRITE (BUFFER (6:7), '(I2.2)') PENWID
      CALL GREX00 (UNIT, BUFFER, 42)
C
C     -- Rescale if needed. 
C
      EXSCAL = MIN (1.0, RBUF(1) / EXSIZE)
      EYSCAL = MIN (1.0, RBUF(2) / EYSIZE)
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
C     -- Move with TALGMV, draw with TALGDW.
C
  120 CONTINUE
      IF (NOTHIN) NOTHIN = .FALSE.
      I0 = NINT(RBUF(1) * EXSCAL)
      J0 = NINT((EYSIZE - RBUF(2)) * EYSCAL)
      I1 = NINT(RBUF(3) * EXSCAL)
      J1 = NINT((EYSIZE - RBUF(4)) * EYSCAL)
  125 CONTINUE
      IF(MODE.EQ.1) THEN
        WRITE (BUFFER( 1:15), 2000) ESCLB, I0, J0, '*m'
        WRITE (BUFFER(16:30), 2000) ESCLB, I1, J1, '*d'
      ELSE
        WRITE (BUFFER( 1:15), 2001) ESCLB, I0, J0, '*m'
        WRITE (BUFFER(16:30), 2001) ESCLB, I1, J1, '*d'
      ENDIF
 2000 FORMAT (A2, ';', I5.5, ';', I4.4, A2)
 2001 FORMAT (A2, ';', I4.4, ';', I5.5, A2)
      CALL GREX00 (UNIT, BUFFER, 30)
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C    EXCL takes care of dot size by the pen width command so we 
C    don't have to worry about it here.  Just draw to same point and
C    let the "draw line" code handle it.
C
  130 CONTINUE
      IF (NOTHIN) NOTHIN = .FALSE.
      I0 = NINT(RBUF(1) * EXSCAL)
      J0 = NINT((EYSIZE - RBUF(2)) * EYSCAL)
      I1 = I0
      J1 = J0
      GOTO 125
C
C--- IFUNC=14, End picture ---------------------------------------------
C
C     -- Eject page and clear bitmap (TALFPO).
C
  140 CONTINUE
C
      IF (.NOT. NOTHIN) THEN
         CALL GRFAO ('Q -- End Page: # ', L, INSTR, NPAGE, 0, 0, 0)
         BUFFER = ESCLB//'0*F'//ESC//INSTR(1:L)//' -- '//ESC//'R'
         CALL GREX00 (UNIT, BUFFER, 12+L)
      ENDIF
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
C        Use TALGLP.
C
  150 CONTINUE
      IC = RBUF(1)
C
      IF (IC .EQ. 0) THEN
C
C     -- Color index 0 is erase.
C
         BUFFER = ESCLB//'  ;  ;11;1;112;5279;1*w'
     :          //ESCLB//'112;5279;11*r'
     :          //ESCLB//'*t'
         WRITE (BUFFER (3:4), '(I2.2)') PENWID
         WRITE (BUFFER (6:7), '(I2.2)') PENWID
         CALL GREX00 (UNIT, BUFFER, 44)
      ELSE IF (IC .EQ. 1) THEN
C
C     -- Color index 1 is black.
C
         BUFFER = ESCLB//'  ;  ;8;1;112;5279;1*w'
     :          //ESCLB//'112;5279;0*r'
     :          //ESCLB//'*t'
         WRITE (BUFFER (3:4), '(I2.2)') PENWID
         WRITE (BUFFER (6:7), '(I2.2)') PENWID
         CALL GREX00 (UNIT, BUFFER, 42)
      ELSE
          IC = 1
          RBUF(1) = IC
      END IF
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C    Hardcopy so ignore it
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           Not implemented, hardcopy device.  Return error code.
C
  170 CONTINUE
      NBUF = -1
      GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Not implemented: no alpha screen so ignore it).
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C
  190 CONTINUE
      RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
C     -- Use TALPFL to fill polygon.
C
  200 CONTINUE
C
C     -- Use NPTS as our indicator of whether this is first time or not.
C
      IF (NPTS .EQ. 0) THEN
C
C     -- First time so set number of points in polygon and start polygon
C        command.
C
         NPTS = RBUF (1)
         BUFFER = ESCP//'4;1;0;1}'
         CALL GREX00 (UNIT, BUFFER, 10)
      ELSE
C
C     -- Second or other time so decrement NPTS and draw to next vertex.
C
         IF (NOTHIN) NOTHIN = .FALSE.
         NPTS = NPTS - 1
         I0 = NINT(RBUF(1) * EXSCAL)
         J0 = NINT((EYSIZE - RBUF(2)) * EYSCAL)
         IF(MODE .EQ. 1) THEN
           WRITE (BUFFER, 3000) I0, J0
         ELSE
           WRITE (BUFFER, 3001) I0, J0
         END IF
 3000    FORMAT (I5.5, ':', I4.4, ';')
 3001    FORMAT (I4.4, ':', I5.5, ';')
         CALL GREX00 (UNIT, BUFFER, 11)
C
C     -- Give the polygon fill command on last call.
C
         IF (NPTS .EQ. 0) THEN
            BUFFER = ESCBS
            CALL GREX00 (UNIT, BUFFER, 2)
         END IF
      END IF
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C    (Not implemented: ignored.  Will we ever get color laser printers?)
C
  210 CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      RETURN
C
C--- IFUNC=23, Escape --------------------------------------------------
C    Note that the NOTHIN flag which indicates if there is anything 
C    written on the paper is set here regardless of the content of
C    the escape characters.
C
  230 CONTINUE
      IF (NOTHIN) NOTHIN = .FALSE.
      RETURN
C-----------------------------------------------------------------------
      END

C*GREX00 -- PGPLOT Talaris/EXCL driver, flush buffer
C+
      SUBROUTINE GREX00 (LUN, BUF, SIZ)
      CHARACTER*(*) BUF
      INTEGER LUN, SIZ
C--
      WRITE (LUN, '(A)') BUF(1:SIZ)
      END
