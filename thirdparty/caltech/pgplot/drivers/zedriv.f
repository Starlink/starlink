* Date:     27-MAR-1987 11:28:46
* From:     AFT%UK.AC.CAM.AST-STAR@AC.UK
* To:       TJP@CITPHOBO
* Subject:  ZEDRIVER.FOR (3)

C*ZEDRIV -- PGPLOT Zeta Plotter driver
     
      SUBROUTINE ZEDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
C--- GRPCKG driver for ZETA plotter.
C----
C Supported device:  Zeta 8 Digital Plotter.
C Device type code:  /ZEta
C Default file name:  PGPLOT.ZET
C Default view surface dimensions:  11 inches by 11 inches.  Current
C   version does not allow larger plots although the manual indicates
C   plots up to 144 feet are possible.
C Resolution:  This version is written for the case where the resolution
C   switch is set to .025 mm.  Actual resolution depends on thickness
C   of pen tip.
C Color capability:  Color indices 1 to 8 are supported corresponding
C   to pens 1-8.  It is not possible to erase lines.
C Input capability:  None.
C File format:  Variable length records with Carriage control of LIST.
C Obtaining hardcopy:  On Starlink print the file on the queue associated
C   with the Zeta plotter.  If the Plotter is attached to a terminal
C   line, then TYPEing the file at the terminal will produce a plot.
C   On Starlink:
C   $ PRINT/NOFEED/QUE=ZETA PGPLOT.ZET
C
C   To stop a Zeta plot job, once it has been started, use the buttons
C   on the plotter.  Press PAUSE, NEXT PLOT and CLEAR.  Only after
C   this sequence is it safe to delete the job from the ZETA Queue.
C   Failing to press the NEXT PLOT button will not correctly advance
C   the paper.  Failing to press CLEAR but, deleteing the current
C   job can prevent the following plot from being plotted.
C
C  5-Aug-1986 - [AFT].
C-----------------------------------------------------------------------
C     IMPLICIT NONE
      CHARACTER*(*) TYPE
      PARAMETER (TYPE='ZETA (Zeta 8 Digital Plotter)')
      INTEGER   IFUNC,NBUF,LCHR,I0,J0,I1,J1
      REAL      RBUF(6)
      CHARACTER CHR*(*)
      INTEGER   GRGE00
      CHARACTER COL(0:7)*2
      INTEGER   LUN,MXCNT,ICNT,IBADR
      SAVE      LUN,MXCNT,ICNT,IBADR
      DATA COL/'6A','61','62','63','64','65','66','67'/
C---
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     :     110,120,130,140,150,160) IFUNC
      GOTO 999
C---
C--- IFUNC= 1, Return device name.
 10   CHR=TYPE
      LCHR=LEN(TYPE)
      RETURN
C---
C--- IFUNC= 2, Return Physical min and max for plot device.
 20   RBUF(1)=0
      RBUF(2)=11175
      RBUF(3)=0
      RBUF(4)=11175
      RBUF(5)=1
      RBUF(6)=8
      NBUF=4
      RETURN
C---
C--- IFUNC= 3, Return device resolution.
 30   RBUF(1)=1007.0
      RBUF(2)=1007.0
      RBUF(3)=10
      NBUF=3
      RETURN
C---
C--- IFUNC= 4, Return misc device info.
 40   CHR='HNNNNNNNNN'
      LCHR=10
      RETURN
C---
C--- IFUNC= 5, Return default file name.
 50   CHR='PGPLOT.ZET'
      LCHR=LEN(CHR)
      RETURN
C---
C--- IFUNC= 6, Return default physical size of plot.
 60   RBUF(1)=0
      RBUF(2)=11175
      RBUF(3)=0
      RBUF(4)=11175
      RETURN
C---
C--- IFUNC= 7, Return misc defaults.
 70   RBUF(1)=15
      NBUF=1
      RETURN
C---
C--- IFUNC= 8, Set active plot.
 80   CALL INIT03(0,LUN,0)
      RETURN
C---
C--- IFUNC= 9, Open workstation.
 90   RBUF(2)=GRGE00('FFL',LUN,CHR,LCHR)
      RBUF(1)=LUN
      IF(RBUF(2).EQ.1) THEN
         MXCNT=130
         CALL GRGMEM(MXCNT,IBADR)
         ICNT=0
         CALL INIT03(0,LUN,0)
      END IF
      RETURN
C---
C--- IFUNC=10, Close workstation.
 100  CLOSE(UNIT=LUN)
      CALL GRFLUN(LUN)
      CALL GRFMEM(MXCNT,IBADR)
      RETURN
C---
C--- IFUNC=11, Begin Picture.
 110  CALL GRGE02(%ref('ZZZZZZZZZZ'), 10, %val(IBADR),ICNT,MXCNT)
      CALL GRGE02(%ref('0000000000CIII'), 14, %val(IBADR),ICNT,MXCNT)
      CALL INZE01
      RETURN
C---
C--- IFUNC=12, Draw line.
 120  I0=NINT(RBUF(1))
      J0=NINT(RBUF(2))
      I1=NINT(RBUF(3))
      J1=NINT(RBUF(4))
      CALL GRZE01(I0,J0,I1,J1,%val(IBADR),ICNT,MXCNT)
      RETURN
C---
C--- IFUNC=13, Draw dot.
 130  I0=NINT(RBUF(1))
      J0=NINT(RBUF(2))
      CALL GRZE01(I0,J0,I0,J0,%val(IBADR),ICNT,MXCNT)
      RETURN
C---
C--- IFUNC=14, End picture.
C--- Move pen to origin,
C--- Advance paper by 15 inches,
C--- Reset.
 140  CALL GRZE01(0,0,0,0,%val(IBADR),ICNT,MXCNT)
      CALL GRGE02(%ref('1OGUE'),5,%val(IBADR),ICNT,MXCNT)
      CALL GRGE02(%ref('70Z')  ,3,%val(IBADR),ICNT,MXCNT)
      RETURN
C---
C--- IFUNC=15, Select pen.
 150  I0=MAX(0,MIN(NINT(RBUF(1)),7))
      RBUF(1)=I0
      CALL GRGE02(%ref(COL(I0)),2,%val(IBADR),ICNT,MXCNT)
      RETURN
C---
C--- IFUNC=16, Flush buffer.
 160  CALL GRGE03(%val(IBADR),ICNT)
      RETURN
C---
C--- Flag function not implemented.
 999  NBUF=-1
      RETURN
C---
      END

C*GRZE01 -- PGPLOT Zeta Plotter driver, line segment

      SUBROUTINE GRZE01 (I0,J0,I1,J1,IBUF,ICNT,MXCNT)
C-----------------------------------------------------------------------
C GRPCKG (internal routine, ZETA): draw a line segment.
C
C Arguments:
C
C I0,J0 (integer, input): the column and row numbers of the starting
C       point.
C I1,J1 (integer, input): the column and row numbers of the end point.
C
C 15-NOV-83
C-----------------------------------------------------------------------
C     IMPLICIT NONE
      INTEGER    ISIZE
      PARAMETER (ISIZE=11176)
      INTEGER   I0, I1, J0, J1, IBUF(*), ICNT, MXCNT
      CHARACTER CPEN(2), CSTR*8
      INTEGER   II0, II1, JJ0, JJ1, I
      INTEGER   IDX(2), IDY(2), LASTX, LASTY
      SAVE      LASTX,LASTY
      DATA CSTR(2:2)/'R'/, CPEN/'1','2'/
C---
      II0= MOD(I0, ISIZE)
      II1= MOD(I1, ISIZE)
      JJ0= MOD(J0, ISIZE)
      JJ1= MOD(J1, ISIZE)
C
      IDX(1)= II0-LASTX
      IDY(1)= JJ0-LASTY
      IDX(2)= II1-II0
      IDY(2)= JJ1-JJ0
C
C  First iteration moves to starting point, second draws line.
C
      DO 100 I= 1, 2
         CSTR(1:1)= CPEN(I)
         IF(IDX(I).NE.0  .OR. IDY(I).NE.0) THEN
            CALL GRZE04(IDX(I), CSTR, 3)
            CALL GRZE04(IDY(I), CSTR, 6)
            CALL GRGE02(%ref(CSTR), 8, IBUF,ICNT,MXCNT)
         ELSE IF(I .EQ. 2) THEN
            CALL GRGE02(%ref(CSTR), 1, IBUF,ICNT,MXCNT)
         END IF
 100  CONTINUE
C
      LASTX= II1
      LASTY= JJ1
      RETURN
C---
      ENTRY INZE01
C
C  This entry is called by to initialize a new plot.
C
      LASTX= 0
      LASTY= 0
      RETURN
      END

C*GRZE04 -- PGPLOT Zeta Plotter driver, string generation

      SUBROUTINE GRZE04(NUM, CSTR, NC)
C-----------------------------------------------------------------
C  Generate strings for sending to Zeta plotter.
C
C- NUM          I   I   Number to be converted.
C- CSTR         I/O C   Output character array.
C- NC           I/O I   Start location in CSTR
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     IMPLICIT NONE
      INTEGER   NUM,NC
      CHARACTER CSTR*(*)
      INTEGER   ITMP, I, IDIV, IND
      CHARACTER CFIG(0:31)
C
      DATA CFIG/'0','1','2','3','4','5','6','7','A',
     :    'B','C','D','E','F','G','H','I','J','K','L','M','N','O',
     :    'P','Q','R','S','T','U','V','W','X'/
C
      ITMP=NUM
      IF(NUM .LT. 0) ITMP= NUM+32768
      IDIV= 1
      DO 100 I=NC+2,NC,-1
         IND= MOD(ITMP/IDIV, 32)
         IF(IND .LT. 0) IND= 32+IND
         CSTR(I:I)= CFIG(IND)
         IDIV= IDIV*32
 100  CONTINUE
      END
