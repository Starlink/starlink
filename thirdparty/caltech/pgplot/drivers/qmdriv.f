C*GRQM00 -- PGPLOT QMS/QUIC driver

      SUBROUTINE    QMDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER       IFUNC, NBUF, LCHR, MODE
      REAL          RBUF(*)
      CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for QUIC devices (QMS and Talaris 800/1200/1500/2400)
C-----------------------------------------------------------------------
C Version 0.1  - 1987 Oct 22 - Patrick P. Murphy, NRAO/VLA [PPM]
C Version 0.2  - 1987 Oct 28 - [PPM] Fix backwards and scale bugs
C Version 1.0  - 1987 Nov 03 - [PPM] Don't form feed if nothing drawn.
C Version 1.1  - 1987 Nov 03 - [PPM] No formfeed at very end of file
C Version 2.0  - 1987 Nov 18 - [PPM] Get scaling done right.
C Version 2.1  - 1991 Jun 28 - [TJP] Standardization.
C Version 2.2  - 1991 Nov  6 - [TJP] Standardization.
C Version 3.0  - 1994 Feb 25 - [TJP] Combine portrait and landscape
C                                    modes in one file.
C-----------------------------------------------------------------------
C
C Supported device: Any QMS or Talaris printer that accepts the QUIC 
C                   page description language.  4-bit mode is used.
C
C Device type code: /QMS (landscape mode 1)
C                   /VQMS (portrait mode 2)
C
C Default file name: PGPLOT.QMPLOT.
C
C Default view surface dimensions:
C      10.25 inches horizontal x  7.75 inches vertical (landscape mode), 
C       7.75 inches horizontal x 10.25 inches vertical (portrait mode),
C      margins of 0.5 inches on top and left of page.
C
C Resolution: The driver uses coordinate increments of 1/1000 inch.
C             The true resolution is device-dependent; at time of
C             writing, it is typically 300 dots per inch.
C
C Color capability: Color indices 0 (erase), and 1 (black) are 
C                   supported.  Requests for other color indices are
C                   converted to 1.  It is not possible to change color
C                   representation. 
C
C Input capability: None.
C
C File format: Ascii, variable length records (max 130 bytes); carriage
C              return ("LIST") carriage control.  This length can be
C              easily changed if needed.
C
C Obtaining hardcopy:  send the file to an appropriate printer.
C-----------------------------------------------------------------------
C
      CHARACTER*(*) DEVTPL, DEVTPP, DEFNAM
      PARAMETER    (DEFNAM='PGPLOT.QMPLOT')
      PARAMETER (DEVTPL='QMS   (QUIC/QMS file, landscape orientation)')
      PARAMETER (DEVTPP='VQMS  (QUIC/QMS file, portrait orientation)')
C
      CHARACTER*130 BUFFER
      CHARACTER*16  HEXSTR
      CHARACTER*10  MSG
      CHARACTER*40  TEMP
      INTEGER       UNIT, IER, BUFLEN, MAXLEN, I0, J0, I1, J1, NPTS, IC,
     :              ISTYLE, LINWID, GROPTX
      REAL          QXSIZE, QYSIZE, QXSCAL, QYSCAL
      LOGICAL       NOTHIN, ENDFIL
C
C ---- Change MAXLEN if you want a shorter or longer max output line 
C ---- length.  Also change the declared length of BUFFER too!  The
C ---- Q*SIZE parameters are the physical size of the plot (used more
C ---- than once here) in resolution units (1/1000 inch).  The Q*SCAL 
C ---- parameters are PGPLOT-modifiable scale factors.
C
      PARAMETER    (MAXLEN = 130,
     :              QXSIZE = 10250.0,
     :              QYSIZE = 7750.00)
C
      SAVE UNIT, IC, BUFFER, BUFLEN, NPTS, QXSCAL, QYSCAL, NOTHIN,
     :     ENDFIL
C
      DATA HEXSTR /'0123456789ABCDEF'/
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
      CALL GRWARN('Unimplemented function in QMS'//
     :            ' device driver:'//MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 IF (MODE.EQ.1) THEN
          CHR = DEVTPL
          LCHR = LEN(DEVTPL)
      ELSE IF (MODE.EQ.2) THEN
          CHR = DEVTPP
          LCHR = LEN(DEVTPP)
      ELSE
          CALL GRWARN('Internal error in QMDRIV')
      END IF
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C               Units are in device co-ordinates (1/1000 inches)
C
   20 IF (MODE.EQ.1) THEN
          RBUF(2) = QXSIZE
          RBUF(4) = QYSIZE
      ELSE
          RBUF(2) = QYSIZE
          RBUF(4) = QXSIZE
      END IF
      RBUF(1) = 0.0
      RBUF(3) = 0.0
      RBUF(5) = 0.0
      RBUF(6) = 1.0
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C    (Nominal values)
C
   30 RBUF(1) = 1000.0
      RBUF(2) = 1000.0
C
C      (multiple strokes are spaced by 3.333 pixels, or 1/300 inch)
C
      RBUF(3) = 3.333
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (Hardcopy, No cursor, Dashed lines, Area fill, Thick lines) 
C
   40 CHR = 'HNDATNNNNN'
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
   60 IF (MODE.EQ.1) THEN
          RBUF(2) = QXSIZE
          RBUF(4) = QYSIZE
      ELSE
          RBUF(2) = QYSIZE
          RBUF(4) = QXSIZE
      END IF
      RBUF(1) = 0.0
      RBUF(3) = 0.0
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C    Currently scale factor for "obsolete" character set of old GRPCKG
C    routines (not used by PGPLOT routines).  Value copied from IMAGEN
C    driver -- I assume this is a good value (PPM 871026).
C
   70 RBUF(1) = 8.0
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
      IER = GROPTX(UNIT, CHR(1:LCHR), DEFNAM, 1)
      IF (IER.NE.0) THEN
          TEMP = CHR(1:LCHR)
          CALL GRWARN('Cannot open output file for QMS'//
     :                ' plot: '//TEMP)
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
          RETURN
      ELSE
          INQUIRE (UNIT=UNIT, NAME=CHR)
          LCHR = LEN(CHR)
   91     IF (CHR(LCHR:LCHR).EQ.' ') THEN
              LCHR = LCHR-1
              GOTO 91
          END IF
          RBUF(2) = 1
      END IF
C
C     -- initialization
C
      IC = 1
      BUFFER = ' '
      BUFLEN = 0
      NPTS = 0
      QXSCAL = 1.0
      QYSCAL = 1.0
      NOTHIN = .TRUE.
C
C     -- Initialize QUIC, get into free format, out of other possible 
C     -- modes (vector graphics, word processing), reset interpretation:
C     -- Set landscape/portrait mode, set margins, enter vector
C        graphics mode
C
      BUFLEN = 1
      CALL GRQM00 (UNIT, BUFFER, BUFLEN)
      BUFFER = '^PY^-'
      BUFLEN = 5
      CALL GRQM00 (UNIT, BUFFER, BUFLEN)
      BUFFER(1:38)  = '^F^IGE^G^IWE^G^IP0000^G^ISYNTAX00000^G'
      IF (MODE.EQ.1) THEN
          BUFFER(39:80) = '^IOL^G^IMH0050010750^G^IMV0050008250^G^IGV'
      ELSE
          BUFFER(39:80) = '^IOP^G^IMH0050008250^G^IMV0050010750^G^IGV'
      END IF
      BUFLEN = 80
      CALL GRQM00 (UNIT, BUFFER, BUFLEN)
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      IF (NOTHIN) THEN
C
C        -- Nothing was plotted so no need to keep the file around.
C
         CLOSE (UNIT)
C
      ELSE
C
C        -- see if the last call was end picture; if so, remove formfeed
C           (this assumes the printer/queue combination will supply the
C            form feeds; if not, comment out this next line).
C
         IF (ENDFIL) BUFLEN = BUFLEN - 2
C
C        -- Flush out anything left in the buffer
C
         IF (BUFLEN .GT. 0) CALL GRQM00 (UNIT, BUFFER, BUFLEN)
C
C        -- Don't need to formfeed; end picture will do that.
C
         BUFFER = '^IGE^G^O^-'
         BUFLEN = 10
         CALL GRQM00 (UNIT, BUFFER, BUFLEN)
         BUFFER = '^PN^-'
         BUFLEN = 5
         CALL GRQM00 (UNIT, BUFFER, BUFLEN)
         CLOSE (UNIT, STATUS='KEEP')
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
      ENDFIL = .FALSE.
      IF (MODE.EQ.1) THEN
          QXSCAL = MIN (1., RBUF(1) / QXSIZE)
          QYSCAL = MIN (1., RBUF(2) / QYSIZE)
      ELSE
          QXSCAL = MIN (1., RBUF(2) / QXSIZE)
          QYSCAL = MIN (1., RBUF(1) / QYSIZE)
      END IF
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C    When I copied the Imagen driver, I got output backwards in the
C    X direction (mirrored).  Hence I mirror it back now.
C
  120 CONTINUE
      IF (NOTHIN) NOTHIN = .FALSE.
      IF (IC.EQ.0) RETURN
      IF (MODE.EQ.1) THEN
          I0 = NINT((QXSIZE - RBUF(1)) * QXSCAL)
          J0 = NINT(RBUF(2) * QYSCAL)
          I1 = NINT((QXSIZE - RBUF(3)) * QXSCAL)
          J1 = NINT(RBUF(4) * QYSCAL)
      ELSE
          I0 = NINT(RBUF(1) * QYSCAL)
          J0 = NINT((QXSIZE - RBUF(2)) * QXSCAL)
          I1 = NINT(RBUF(3) * QYSCAL)
          J1 = NINT((QXSIZE - RBUF(4)) * QXSCAL)
      END IF
  125 CONTINUE
      IF (BUFLEN+13 .GE. MAXLEN) CALL GRQM00 (UNIT, BUFFER, BUFLEN)
      BUFFER(BUFLEN+1:BUFLEN+2) = '^U'
      WRITE (BUFFER(BUFLEN+3:BUFLEN+13), '(I5.5,1H:,I5.5)') I0, J0
      BUFLEN = BUFLEN + 13
      IF (BUFLEN+13 .GE. MAXLEN) CALL GRQM00 (UNIT, BUFFER, BUFLEN)
      BUFFER(BUFLEN+1:BUFLEN+2) = '^D'
      WRITE (BUFFER(BUFLEN+3:BUFLEN+13), '(I5.5,1H:,I5.5)') I1, J1
      BUFLEN = BUFLEN + 13
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C    QUIC takes care of dot size by the ^PW (pen width) command so we 
C    don't have to worry about it here.  Just draw to same point and
C    let the "draw line" code handle it.
C
  130 CONTINUE
      IF (NOTHIN) NOTHIN = .FALSE.
      IF (IC.EQ.0) RETURN
      IF (MODE.EQ.1) THEN
          I0 = NINT((QXSIZE - RBUF(1)) * QXSCAL)
          J0 = NINT(RBUF(2) * QYSCAL)
      ELSE
          I0 = NINT(RBUF(1) * QYSCAL)
          J0 = NINT((QXSIZE - RBUF(2)) * QXSCAL)
      END IF
      I1 = I0
      J1 = J0
      GOTO 125
C
C--- IFUNC=14, End picture ---------------------------------------------
C    This means do a form feed.  QUIC allows form feeds within vector
C    graphics mode so just put it in the stream.
C    Changed 871103 [PPM] so that no formfeed done if "NOTHIN" is true.
C    That means there is nothing on the paper.
C    Changed again (same date, person): set flag for end workstation
C
  140 CONTINUE
      ENDFIL = .TRUE.
      IF (.NOT. NOTHIN) THEN
         IF (BUFLEN+2 .GE. MAXLEN) CALL GRQM00 (UNIT, BUFFER, BUFLEN)
         BUFFER(BUFLEN+1:BUFLEN+2) = '^,'
         BUFLEN = BUFLEN + 2
      ENDIF
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = RBUF(1)
      IF (IC.LT.0 .OR. IC.GT.1) THEN
          IC = 1
          RBUF(1) = IC
      END IF
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C    Hardcopy so ignore it
C
  160 CONTINUE
C     CALL GRQM00 (UNIT, BUFFER, BUFLEN) Not needed!
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           Not implemented, hardcopy device.  Return error code.
C
  170 CONTINUE
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
      ISTYLE = NINT(RBUF(1))
      IF (ISTYLE .LT. 1) ISTYLE = 1
      IF (ISTYLE .GT. 5) ISTYLE = 5
C
C     -- Convert PGPLOT line styles 1 thru 5 to QUIC equivalents
C
      GOTO (191,192,193,194,195) ISTYLE
C
C     Select ISTYLE in CASE:
C     Full line
  191    ISTYLE = 0
         GOTO 196
C     Long dashes
  192    ISTYLE = 1
         GOTO 196
C     Dash-dot
  193    ISTYLE = 7
         GOTO 196
C     Dotted
  194    ISTYLE = 2
         GOTO 196
C     Dash-dot-dot-dot
  195    ISTYLE = 9
         GOTO 196
C     End SELECT/CASE on ISTYLE
  196 CONTINUE
C
C     -- I use HEXSTR here for system-independence and also in case the
C     -- PGPLOT package ever adds more line styles.
C
      IF (BUFLEN+3 .GE. MAXLEN) CALL GRQM00 (UNIT, BUFFER, BUFLEN)
      BUFFER(BUFLEN+1:BUFLEN+2) = '^V'
      ISTYLE = ISTYLE + 1
      BUFFER(BUFLEN+3:BUFLEN+3) = HEXSTR(ISTYLE:ISTYLE)
      BUFLEN = BUFLEN + 3
      RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      IF (IC .EQ. 0) RETURN
C
C     -- Use NPTS as our indicator of whether this is first time or not
C
      IF (NPTS.EQ.0) THEN
C
C        -- First time so set number of points in polygon
C
         NPTS = RBUF(1)
         IF (BUFLEN+8 .GE. MAXLEN) CALL GRQM00 (UNIT, BUFFER, BUFLEN)
C
C        -- Use black fill, no border (in case PGPLOT doesn't go back to
C           the last point) --------------------------------------------
C
         BUFFER (BUFLEN+1:BUFLEN+8) = '^PF020^U'
         BUFLEN = BUFLEN + 8
      ELSE
C
C        -- Second or other time so bump NPTS and draw to next vertex
C
         IF (NOTHIN) NOTHIN = .FALSE.
         NPTS = NPTS - 1
         IF (MODE.EQ.1) THEN
             I0 = NINT((QXSIZE - RBUF(1)) * QXSCAL)
             J0 = NINT(RBUF(2) * QYSCAL)
         ELSE
             I0 = NINT(RBUF(1) * QYSCAL)
             J0 = NINT((QXSIZE - RBUF(2)) * QXSCAL)
         END IF
         IF (BUFLEN+13 .GE. MAXLEN) CALL GRQM00 (UNIT, BUFFER, BUFLEN)
         WRITE (BUFFER(BUFLEN+1:BUFLEN+11), '(I5.5,1H:,I5.5)') I0, J0
         BUFFER(BUFLEN+12:BUFLEN+13) = '^D'
         BUFLEN = BUFLEN + 13
         IF (NPTS .EQ. 0) THEN
C
C           -- get rid of last ^D and give the Polygon fill command
C
            BUFLEN = BUFLEN - 2
            BUFFER(BUFLEN+1:BUFLEN+2) = '  '
            IF (BUFLEN+3 .GE. MAXLEN) CALL GRQM00 (UNIT, BUFFER, BUFLEN)
            BUFFER(BUFLEN+1:BUFLEN+3) = '^PS'
            BUFLEN = BUFLEN + 3
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
C
C     -- QUIC pen width is in dots (1/300 inch) so convert from 1/200's.
C
      LINWID = NINT(RBUF(1) * 1.5)
      IF (LINWID .LT. 1) LINWID = 1
      IF (LINWID .GT. 31) LINWID = 31
      IF (BUFLEN+5 .GE. MAXLEN) CALL GRQM00 (UNIT, BUFFER, BUFLEN)
      BUFFER(BUFLEN+1:BUFLEN+3) = '^PW'
      WRITE (BUFFER(BUFLEN+4:BUFLEN+5), '(I2.2)') LINWID
      BUFLEN = BUFLEN + 5
      RETURN
C
C--- IFUNC=23, Escape --------------------------------------------------
C    Note that the NOTHIN flag which indicates if there is anything 
C    written on the paper is set here regardless of the content of
C    the escape characters.
C
  230 CONTINUE
      IF (NOTHIN) NOTHIN = .FALSE.
      IF (LCHR .GT. MAXLEN) THEN
         WRITE (MSG(1:4), '(I4)') MAXLEN
         CALL GRWARN('Sorry, maximum line size ('//MSG(1:4)//
     :               ') exceeded for device type QMS')
         NBUF = -1
      ELSE
C
C        -- WARNING!  Anyone using the escape mechanism to send stuff
C           to the QMS had better remember (a) the QMS is ASSUMED by 
C           the driver to be in vector graphics mode, and (b) you better
C           darn well put it back in the same vector mode!!!  If not, 
C           well, you get what you deserve then.
C
         IF (BUFLEN+LCHR .GE. MAXLEN) CALL GRQM00 (UNIT, BUFFER, BUFLEN)
         BUFFER(BUFLEN+1:BUFLEN+LCHR) = CHR(1:LCHR)
         BUFLEN = BUFLEN + LCHR
      ENDIF
C
      RETURN
C-----------------------------------------------------------------------
      END

C*GRQM00 -- PGPLOT QMS/QUIC driver, flush buffer
C+
      SUBROUTINE GRQM00 (LUN, BUF, SIZ)
      CHARACTER*(*) BUF
      INTEGER LUN, SIZ
C--
      WRITE (LUN, '(A)') BUF(1:SIZ)
      BUF(1:LEN(BUF)) = ' '
      SIZ = 0
      END
