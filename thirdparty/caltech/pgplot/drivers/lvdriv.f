C*LVDRIV -- PGPLOT driver for Digital LN03 printer (portrait mode)

      SUBROUTINE LVDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for Digital LN03 Laser Printer (PORTRAIT orientation)
C File : LVDRIVER.FOR
C-----------------------------------------------------------------------
C Version 1.0  - 1989 Nov. Sid Penstone, Queen's University
C Last Revision Dec.1.1989: Removed call to dot routine, now do it as
C a zero length vector
C-----------------------------------------------------------------------
C      This routine has been written specifically for the LN03-PLUS
C      Laser Printer
C
C      NAME: '/LVN03'
C
C
C       In all case, the initialization sequences are written out,
C      whether or not the plotter is connected as a terminal,
C      or driven from an intermediate file.
C
C      If there is more than one plot and the plot is on a terminal,
C      the page is ejected before the next one
C      
C      ref. Digital LN03 Programmer Reference Manual, P/N EK-OLN03-002
C
C      PHYICAL SIZE IS 7" BY 9"
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE
      PARAMETER (TYPE='LVN03 (Digital LN03 Laser Printer, portrait)')
C
      INTEGER MARGIN, NXPIX, NYPIX, NSIXROWS, NSIXCOLS
      PARAMETER(MARGIN=150)
      PARAMETER(NXPIX=2400)
      PARAMETER(NYPIX=3000)
      PARAMETER(NSIXROWS=(NYPIX/6)+2)
      PARAMETER(NSIXCOLS=NXPIX)
      CHARACTER*10 MSG
      INTEGER WIDTH,XLEFT,XRIGHT,YBOT,YTOP,INTENS, XMAX, YMAX, XMIN
      INTEGER UNIT, IER
      INTEGER I0, J0, I1, J1
      INTEGER IK1, IK2, IK3, IK4, IK5, PLOTNO
      CHARACTER*1 ESC
      DATA XLEFT,XRIGHT,YTOP,YBOT/0,NXPIX,0,NYPIX/
      DATA ESC /27/
      DATA WIDTH /2/
      LOGICAL ACTIVE(0:NSIXROWS)
C Data for the allocation routines
      INTEGER GRGMEM, GRFMEM
      INTEGER BUFLEN, IPOINTS, IERR
      LOGICAL ALLOC
      SAVE BUFLEN, IPOINTS, ALLOC
      DATA ALLOC /.FALSE./
      DATA IPOINTS /-1/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
      GOTO 900
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
 10   CHR = TYPE
      LCHR = LEN(TYPE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
 20   RBUF(1) = 0
      RBUF(2) = NXPIX - 2*MARGIN
      RBUF(3) = 0
      RBUF(4) = NYPIX - 2*MARGIN
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
 30   RBUF(1) = 300.0
      RBUF(2) = 300.0
      RBUF(3) = WIDTH
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    No thick lines)
C
 40   CHR = 'HNNNNNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
 50   CHR = 'PGPLOT.LN3'
      LCHR = 11
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
 60   RBUF(1) = 0
      RBUF(2) = NXPIX-2*MARGIN
      RBUF(3) = 0
      RBUF(4) = NYPIX-2*MARGIN
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
 70   RBUF(1) = 10
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
 80   CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
 90   CONTINUE
C Try to open the graphics device
      CALL GRGLUN(UNIT)
      OPEN (UNIT=UNIT,FILE=CHR(:LCHR),STATUS='NEW',
     1     FORM='FORMATTED', CARRIAGECONTROL='LIST',
     1     RECL=512,IOSTAT=IER)
      IF (IER.NE.0) THEN
         CALL ERRSNS(IK1,IK2,IK3,IK4,IK5)
         CALL GRWARN('Cannot open graphics device '
     1        //CHR(1:LCHR))
         IF (IK2.NE.0 .AND. IK2.NE.1) CALL GRGMSG(IK2)
         IF (IK5.NE.0 .AND. IK5.NE.1) CALL GRGMSG(IK5)
         RBUF(2) = 0 
         RETURN
      ENDIF
      RBUF(1) = UNIT
      RBUF(2) = 1
      NBUF = 2
C Now allocate the bitmap buffers (Assume integer*2)
      IF (.NOT. ALLOC) THEN
         BUFLEN = NSIXROWS * NSIXCOLS
         IERR = GRGMEM(2*BUFLEN, IPOINTS)
         IF (IERR .NE. 1 ) THEN
            CALL GRGMSG(IERR)
            CALL GRWARN('Memory allocation failure')
            RETURN
         ENDIF
         ALLOC = .TRUE.
      ENDIF
C Clear the row flags (and the bit map)
      CALL LN03_CLEAR(%VAL(IPOINTS),BUFLEN,ACTIVE,NSIXROWS)
C
C always write the preamble
C this resets the plotter
      WRITE (UNIT, '(A)') ESC//'c'
C this sets it for portrait, origin at corner 
      WRITE (UNIT, '(A)') ESC//'[?20 J'
      PLOTNO = 0
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
 100  CONTINUE
C always turn it off
      CLOSE (UNIT)
      CALL GRFLUN(UNIT)
C Deallocate the buffer
      IF (ALLOC .OR. IPOINTS .GE. 0) THEN
         IERR = GRFMEM(2*BUFLEN, IPOINTS)
         IF (IERR .NE. 1 ) THEN
            CALL GRGMSG(IERR)
            CALL GRWARN('Deallocation failure')
            RETURN
         ENDIF
         ALLOC = .FALSE.
         IPOINTS = -1
      ENDIF
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
 110  CONTINUE
C WE COULD GET THE VALUE OF XMAX AND YMAX HERE
      YMAX = YBOT - 2*MARGIN
      XMIN = XLEFT + MARGIN
      XMAX = XRIGHT - MARGIN
      PLOTNO = PLOTNO + 1
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
 120  CONTINUE
      I0 = XMIN + NINT(RBUF(1)) 
      J0 = YMAX  - NINT(RBUF(2))
      I1 = XMIN + NINT(RBUF(3))
      J1 = YMAX  - NINT(RBUF(4))
      CALL LN03_VECTOR(I0,J0,I1,J1,WIDTH,XLEFT,XRIGHT,
     1     YTOP,YBOT,%val(IPOINTS),ACTIVE,NSIXROWS,NSIXCOLS,INTENS)
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
 130  CONTINUE
      I0 = XLEFT + NINT(RBUF(1))
      J0 = YBOT  - NINT(RBUF(2))
      CALL LN03_VECTOR(I0,J0,I0,J0,WIDTH,XLEFT,XRIGHT,
     1     YTOP,YBOT,%VAL(IPOINTS),ACTIVE,NSIXROWS,NSIXCOLS,INTENS)
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
 140  CONTINUE
      CALL LN03_DUMP(UNIT,XLEFT,XMAX+WIDTH,YTOP+MARGIN,YMAX+WIDTH,
     1     %val(IPOINTS),ACTIVE,NSIXROWS,NSIXCOLS)
      IF(ALLOC) THEN
         CALL LN03_CLEAR(%VAL(IPOINTS),BUFLEN,ACTIVE,NSIXROWS)
      ENDIF
C  Eject the paper with a form feed
C     WRITE (UNIT, '(A)') CHAR(12)
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
 150  INTENS = NINT(RBUF(1))
      IF (INTENS .GT.1 ) INTENS = 1 
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C    (Null operation: buffering is not implemented.)
C
 160  CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
 170  GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Null operation: there is no alpha screen.)
C
 180  CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called.)
C
 190  GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C    (Not implemented: should not be called.)
C
 200  GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C
 210  RETURN
C      Other colors are not implemented
C
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called.)
C
 220  GOTO 900
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
 230  CONTINUE
      WRITE (UNIT, '(A)') CHR(:LCHR)
      RETURN
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
 900  WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in LN03 device driver: '//MSG)
      NBUF = -1
      RETURN
C-----------------------------------------------------------------------
      END
