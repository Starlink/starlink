C*LADRIV -- PGPLOT Driver for SIXEL code
C+ (tested on DEC LA50, should in practice work on a LA100 or LN03)
      SUBROUTINE LADRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
      BYTE ESC/27/,FF/12/
      LOGICAL ALLERR
C
C PGPLOT driver for LA50 (SIXEL) device.
C
C Version 1.0  - 1988 Dec - B. H. Toby
C=======================================================================
C
C Supported device: DEC LA50
C
C Device type code: /LA50
C
C Default device name: LA50:PGPLOT.LAPLOT
C
C Default view surface dimensions: 9.5 in (horizontal) by  6 in (vertical).
C Maximum view surface dimensions: (none) (horizontal) by 7.9 in (vertical).
C Note: the 11 inch limit is software imposed.
C
C Resolution: 72 (x) x 144 (y) pixels/inch.
C
C Color capability: Color indices 0 (erase, white) and 1 (black) are
C supported. It is not possible to change color representation.
C
C Input capability: None.
C
C File format: Variable-length records, maximum 80 bytes, with
C "list" carriage-control.
C
C Obtaining hardcopy:
C   if the LA50 is connected to the user's VT2xx or VT3xx terminal
C    then the printer can be accessed by sending the file
C      directly to the terminal: use PGPLOT device TT:/LA50
C    or DEFINE PGPLOT_LA50 TT: (be sure to do a SET TERM/FORM if
C    page spacing is important)
C   if the LA50 is attached to a different terminal port, e.g. TXZ99:,
C    which preferably has been set spooled, use PGPLOT device TXZ99:/LA50
C    or DEFINE PGPLOT_LA50 TXZ99: (be sure that TXZ99 has been set
C    /FORM if page spacing is important)
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (TYPE='LA50 (DEC LA50 printer)')
      PARAMETER (DEFNAM='PGPLOT_LA50:PGPLOT.LAPLOT')
C
      INTEGER UNIT, IER, IC, BX, BY, NPICT
      INTEGER GRGMEM, GRFMEM
      CHARACTER*10 MSG
C define pointers for dynamically allocated arrays (used with same names)
      INTEGER BITMAP,buf
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in '//TYPE//' device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = TYPE
      LCHR = LEN(TYPE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = -1
      RBUF(3) = 0
      RBUF(4) = 1140
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 72.0
      RBUF(2) = 144.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    no thick lines)
C
   40 CHR = 'HNNNNNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = DEFNAM
      LCHR = LEN(DEFNAM) + 1
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 720    !   10 inch
      RBUF(3) = 0
      RBUF(4) = 1008    !   7 inch
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 1
      NBUF=1
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
      ALLERR = .TRUE.
      CALL GRGLUN(UNIT)
      RBUF(1) = UNIT
      NPICT = 0
      IC = 1
      OPEN (UNIT=UNIT, FILE=CHR(:LCHR), CARRIAGECONTROL='LIST',
     1      DEFAULTFILE=DEFNAM, STATUS='NEW',
     3      FORM='FORMATTED', RECORDTYPE='VARIABLE', IOSTAT=IER)
      IF (IER.NE.0) THEN
          CALL GRWARN('Cannot open output file for '//TYPE//' plot: '//
     1                CHR(:LCHR))
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
          RETURN
      ENDIF
     
      INQUIRE (UNIT=UNIT, NAME=CHR)
      LCHR = LEN(CHR)
   91 IF (CHR(LCHR:LCHR).EQ.' ') THEN
          LCHR = LCHR-1
          GOTO 91
      END IF
      RBUF(2) = 1
      RETURN
     
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CLOSE (UNIT=UNIT, STATUS='KEEP')
      CALL GRFLUN(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
C Allocate space based on the actual size of the plot, since there is no
C way to signal an error, if it is not possible to allocate the space
C set a flag which prevents plotting
C
C dynamically allocate memory for the plot bitmap
C     -- dimensions of plot buffer
      BX = 2. + (RBUF(1)+5.)/6.    ! (six bits encoded in each byte)
      BY = 2. + RBUF(2)
      IER = GRGMEM(BX*BY, BITMAP)
      IF (IER.NE.1) then
        CALL GRGMSG(IER)
        CALL GRWARN('Failed to allocate plot bitmap.')
        GOTO 92
      ENDIF
C allocate space for a buffer
      IER = GRGMEM(BY, BUF)
      IF (IER.NE.1) then
        CALL GRGMSG(IER)
        CALL GRWARN('Failed to allocate plot buffer.')
        IER = GRFMEM(BX*BY, BITMAP)
        IF (IER.NE.1) CALL GRWARN('Failed to deallocate plot bitmap.')
        GOTO 92
      ENDIF
     
      ALLERR = .FALSE.
      IC = 1
      NPICT = NPICT+1
C%    type *,'Begin picture',NPICT
C initialize the buffer
      CALL GRLA03(BX*BY, %val(BITMAP))
      RETURN
C error in space allocation
92    CALL GRWARN('Plot type has been set to /NULL.')
      CLOSE (UNIT=UNIT, STATUS='DELETE')
      CALL GRFLUN(UNIT)
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      IF (ALLERR) RETURN
C note: the following routine is shared with the PRINTRONIX driver
      CALL GRLA02(1, RBUF, IC, BX, BY, %val(BITMAP))
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      IF (ALLERR) RETURN
C note: the following routine is shared with the PRINTRONIX driver
      CALL GRLA02(0, RBUF, IC, BX, BY, %val(BITMAP))
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
C%    type *,'End picture  ',NPICT
C
      IF (ALLERR) RETURN
C Assume the LA50 is attached to a terminal, if so send the terminal
C into CONTROLLER mode while sending the plot. The extra escape sequences
C should not cause any problems if the LA50 is directly connected.
C Put terminal into controller mode, then enter graphics mode.
      IF (NPICT.EQ.1) THEN
        WRITE (UNIT,'(5a)') ESC,'[5i',ESC,'Pq'
      ELSE
C     if this is not the first plot, insert a form feed to advance the page
        WRITE (UNIT,'(5a)') ESC,'[5i',FF,ESC,'Pq'
      ENDIF
      CALL GRLA01(UNIT, BX, BY, %val(BITMAP),%val(BUF))
C leave graphics mode; put terminal into normal mode
      WRITE (UNIT,'(4a)') ESC,CHAR(92),ESC,'[4i'
C deallocate space for bitmap and buffer
      IER = GRFMEM(BX*BY, BITMAP)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to deallocate plot bitmap.')
      END IF
      IER = GRFMEM(BY, BUF)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to deallocate plot buffer.')
      END IF
      ALLERR = .true.
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
C    (Not used.)
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called)
C
  170 CONTINUE
      GOTO 900
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
C    (Not implemented: should not be called)
C
  200 CONTINUE
      GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C    (Not implemented: ignored)
C
  210 CONTINUE
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
C-----------------------------------------------------------------------
      END
     
C*GRLA01 -- PGPLOT LA50 driver, copy bitmap to output file
C+
      SUBROUTINE GRLA01 (UNIT, BX, BY, BITMAP, BUF)
      INTEGER UNIT, BX, BY
      BYTE BITMAP(BX,BY), BUF(BY)
C
C Arguments:
C  UNIT   (input)  Fortran unit number for output
C  BX, BY (input)  dimensions of BITMAP
C  BITMAP (input)  the bitmap array
C  BUF    (temp.)  buffer used to compress line of print
C-----------------------------------------------------------------------
      INTEGER I, J, K, JMAX, M, ndup, jj, XMAX
C
C Create SIXEL code and write bitmap to LA50, line by line.
C
C   The x-axis is the direction that paper advances, each line
C   that is sent contains six rows of bits
C
C find last non-zero line
      DO K=BX,1,-1
      XMAX = K
        DO J=1,by
          if (BITMAP(K,J) .ne. '00'X) goto 1
      ENDDO
      ENDDO
C  process each line
1     DO K=1,XMAX
C    copy each column of the bitmap to the buffer, reversing the order
C    also find the last non-zero array element
        JMAX = 1
        jj = 0
        DO J=by,1,-1
          jj = jj + 1
          BUF(jj) = BITMAP(K,J)
          IF (BUF(jj) .NE.'00'X) JMAX = jj
        END DO
C compress the line and send it to the printer
        M = 0
        ndup = 1
        do 10 i=1,JMAX
C   can the current character be compressed with the next?
        if (i .ne. JMAX) THEN
C   if the next character the same as current increment the counter and go on
          IF (BUF(i) .eq. BUF(i+1)) then
             ndup = ndup + 1
             goto 10
          ENDIF
        ENDIF
C   The next character is different from current or this is the last character.
C     How many characters are repeated?
       if (ndup .le. 4) then
C       if less than five, don't bother to compress
         do j = 1,ndup
           M = M + 1
           BUF(M) = BUF(i)+63
         enddo
         ndup = 1
         goto 10
       elseif (ndup .le. 9) then
         BUF(M+1) = 33
         encode(1,31,BUF(M+2)) ndup
31         format(i1)
         M = M + 2 + 1
       elseif (ndup .le. 99) then
         BUF(M+1) = 33
         encode(2,32,BUF(M+2)) ndup
32         format(i2)
         M = M + 2 + 2
       elseif (ndup .le. 999) then
         BUF(M+1) = 33
         encode(3,33,BUF(M+2)) ndup
33         format(i3)
         M = M + 2 + 3
       else
         BUF(M+1) = 33
         encode(4,34,BUF(M+2)) ndup
34         format(i4)
         M = M + 2 +  4
       endif
       BUF(M) = BUF(i)+63
       ndup = 1
10     continue
     
      WRITE (UNIT,'(80a1)') (BUF(j),j=1,M),'-'
      END DO
      END
     
C*GRLA02 -- PGPLOT Printronix driver, draw line
C+
      SUBROUTINE GRLA02 (LINE,RBUF,ICOL, BX, BY, BITMAP)
      INTEGER LINE
      REAL RBUF(4)
      INTEGER ICOL, BX, BY
      BYTE BITMAP(BX,BY)
C
C Draw a straight-line segment from absolute pixel coordinates
C (RBUF(1),RBUF(2)) to (RBUF(3),RBUF(4)).  The line either overwrites
C (sets to black) or erases (sets to white) the previous contents
C of the bitmap, depending on the current color index. Setting bits
C is accomplished with a VMS BISB2 instruction, expressed in
C Fortran as .OR.; clearing bits is accomplished with a VMS BICB2
C instruction, expressed in Fortran as .AND..NOT.. The line is
C generated with a Simple Digital Differential Analyser (ref:
C Newman & Sproull).
C
C Arguments:
C
C LINE            I I      =0 for dot, =1 for line.
C RBUF(1),RBUF(2) I R      Starting point of line.
C RBUF(3),RBUF(4) I R      End point of line.
C ICOL            I I      =0 for erase, =1 for write.
C BITMAP        I/O B      (address of) the frame buffer.
C
C-----------------------------------------------------------------------
      BYTE    QMASK(0:5)
      INTEGER LENGTH, KX, KY, K
      REAL    D, XINC, YINC, XP, YP
      DATA    QMASK /'01'x,'02'x,'04'x,'08'x,'10'x,'20'x/
C
      IF (LINE.GT.0) THEN
          D = MAX(ABS(RBUF(3)-RBUF(1)), ABS(RBUF(4)-RBUF(2)))
          LENGTH = D
          IF (LENGTH.EQ.0) THEN
              XINC = 0.
              YINC = 0.
          ELSE
              XINC = (RBUF(3)-RBUF(1))/D
              YINC = (RBUF(4)-RBUF(2))/D
          END IF
      ELSE
          LENGTH = 0
          XINC = 0.
          YINC = 0.
      END IF
      XP = RBUF(1)+0.5
      YP = RBUF(2)+0.5
      IF (ICOL.NE.0) THEN
          DO K=0,LENGTH
              KX = XP
              KY = (BY-1)-INT(YP)
              BITMAP(KX/6+1,KY+1) = BITMAP(KX/6+1,KY+1) .OR.
     1                              QMASK(MOD(KX,6))
              XP = XP + XINC
              YP = YP + YINC
          END DO
      ELSE
          DO K=0,LENGTH
              KX = XP
              KY = (BY-1)-INT(YP)
              BITMAP(KX/6+1,KY+1) = BITMAP(KX/6+1,KY+1) .AND.
     1                              (.NOT.QMASK(MOD(KX,6)))
              XP = XP + XINC
              YP = YP + YINC
          END DO
      END IF
      END

C*GRLA03 -- zero fill buffer
C+
      SUBROUTINE GRLA03 (BUFSIZ,BUFFER)
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
