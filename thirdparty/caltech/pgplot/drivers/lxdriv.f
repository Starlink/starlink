C*LXDRIV -- PGPLOT driver for LaTeX Picture Environment
C+
      SUBROUTINE LXDRIV(OPCODE,RBUF,NBUF,CHR,LCHR)
      INTEGER OPCODE, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C Supported device:
C  This driver creates a text file containing commands for drawing
C  in the LaTeX picture environment, bracketted by \begin{picture} 
C  and \end{picture}. The file can be included in a LaTeX document.
C
C  If you have the option of including a PostScript file in your
C  LaTeX document, then that will usually give much better results
C  than using this driver, which has very limited capabilities.
C
C Device type code:
C  /LATEX
C
C Default file name:
C  pgplot.tex
C
C Default view surface dimensions:
C  The default picture size is 6 inches by 6 inches (which corresponds 
C  to 1728x1728 units where a unit is 0.25pt = 1/288 inch). The picture
C  size can be changed by using PGPAP in the PGPLOT program.
C
C Resolution:
C  The driver rounds coordinates to multiples of 0.25pt (1/288 inch).
C
C Limitations:
C  The LaTeX picture environment has a very limited set of primitives.
C  In particular, diagonal lines must be composed out of dots. This
C  can lead to very large files. For some graphs (especially with a 
C  lot of shaded areas), the capacity of many LaTeX systems can easily
C  be exceeded.
C
C Author:
C  Written by Grant McIntosh  95/02/14 (gmcint@relay.drev.dnd.ca).
C
C  Revised by T. Pearson 95/06/19.
C  Revised to allow picture size to be adjusted by PGPAP: TJP 97/5/16.
C-----------------------------------------------------------------------
      INTEGER LUN, IXO, IYO, IXPS, IYPS, I, J
      INTEGER INCR, NINC, IER, ISIGN, LENGTH, BX, BY, STATE
      INTEGER GROPTX
      REAL X1, Y1, X2, Y2, DELX, DELY, SLOPE
      CHARACTER*128 MSG
      CHARACTER*(*) DEVNAM
      PARAMETER (DEVNAM='LATEX (LaTeX picture environment)')
      CHARACTER*1   BS
      SAVE LUN, BS, BX, BY, STATE
C-----------------------------------------------------------------------
      GOTO(10,20,30,40,50,60,70,80,90,100,110,120,130,140)OPCODE
      NBUF=-1
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 1, Return device name -------------------------------------
   10 CHR=DEVNAM
      LCHR=LEN(DEVNAM)
      BS=CHAR(92)
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
   20 RBUF(1)=0
      RBUF(2)=32767
      RBUF(3)=0
      RBUF(4)=32767
      RBUF(5)=0
      RBUF(6)=1
      NBUF=6
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 3, Return device resolution -------------------------------
   30 RBUF(1)=72./0.25
      RBUF(2)=72./0.25
      RBUF(3)=1.
      NBUF=3
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 4, Return misc device info --------------------------------
   40 CHR='HNNNNNNNNN'
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 5, Return default file name -------------------------------
   50 CHR='pgplot.tex'
      LCHR=10
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 6, Return default physical size of plot -------------------
   60 RBUF(1)=0
      RBUF(2)=BX
      RBUF(3)=0
      RBUF(4)=BY
      NBUF=4
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 7, Return misc defaults -----------------------------------
   70 RBUF(1)=1
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 8, Select plot --------------------------------------------
 80   RETURN
C-----------------------------------------------------------------------
C--- IFUNC = 9, Open workstation ---------------------------------------
 90   CONTINUE
      NBUF=2
C     -- check for concurrent access
      IF (STATE.EQ.1) THEN
         CALL GRWARN('a PGPLOT LaTeX file is already open')
         RBUF(1) = 0
         RBUF(2) = 0
         RETURN
      END IF
      CALL GRGLUN(LUN)
      IER = GROPTX(LUN, CHR(1:LCHR), 'pgplot.tex', 1)
      IF (IER.NE.0) THEN
         MSG = 'Cannot open output file for LaTeX picture: '//
     :         CHR(:LCHR)
         CALL GRWARN(MSG)
         RBUF(1)=0
         RBUF(2)=0
         CALL GRFLUN(LUN)
      ELSE
         RBUF(2)=1
         RBUF(1)=LUN
         STATE=1
         BX=1728
         BY=1728
      END IF
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC=10, Close workstation ---------------------------------------
 100  CLOSE(UNIT=LUN)
      CALL GRFLUN(LUN)
      STATE=0
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC=11, Begin picture -------------------------------------------
  110 CONTINUE
      BX = NINT(RBUF(1))
      BY = NINT(RBUF(2))
      WRITE(LUN,'(A)') BS//'setlength{'//BS//'unitlength}{0.25pt}'
      WRITE(LUN,'(A)') BS//'linethickness{1pt}'
      WRITE(LUN,'(A,I6,A,I6,A)')
     :     BS//'begin{picture}(',BX,',',BY,')(0,0)'
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC=12, Draw line -----------------------------------------------
  120 X1=RBUF(1)
      Y1=RBUF(2)
      X2=RBUF(3)
      Y2=RBUF(4)
      IXO=X1
      IYO=Y1
      IXPS=X2
      IYPS=Y2
C vertical lines
      IF(IXPS.EQ.IXO) THEN
         LENGTH=ABS(IYPS-IYO)
         ISIGN=1
         IF(LENGTH.NE.0) ISIGN=(IYPS-IYO)/LENGTH
         WRITE(LUN,5000) BS,IXO,IYO,BS,ISIGN,LENGTH
 5000    FORMAT(A1,'put(',I4,',',I4,'){',A1,'line(0,',I4,'){',I4,'}}')
         RETURN
      ENDIF
C horizontal lines
      IF(IYPS.EQ.IYO) THEN
         LENGTH=ABS(IXPS-IXO)
         ISIGN=1
         IF(LENGTH.NE.0) ISIGN=(IXPS-IXO)/LENGTH
         WRITE(LUN,5100) BS,IXO,IYO,BS,ISIGN,LENGTH
 5100    FORMAT(A1,'put(',I4,',',I4,'){',A1,'line(',I4,',0){',I4,'}}')
         RETURN
      ENDIF
C other lines
      SLOPE=FLOAT(IYPS-IYO)/FLOAT(IXPS-IXO)
      INCR=1
      IF(IXPS.LT.IXO) INCR=-1
      NINC=MAX(1,ABS(IXPS-IXO))
      DELX=INCR
      DELY=SLOPE*INCR
  125 CONTINUE
      IF(ABS(DELY).GT.1) THEN
         NINC=NINC*2
         DELX=DELX/2.
         DELY=SLOPE*DELX
         GOTO 125
      ENDIF
      WRITE(LUN,5200) BS,IXO,IYO,DELX,DELY,NINC,BS
 5200 FORMAT(A1,'multiput(',I4,',',I4,')(',F8.3,',',F8.3,'){',I4,
     *     '}{',A1,'circle*{1}}')
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC=13, Draw dot ------------------------------------------------
  130 I=NINT(RBUF(1))
      J=NINT(RBUF(2))
      WRITE(LUN,5300) BS,I,J,BS
 5300 FORMAT(A1,'put(',I4,',',I4,'){',A1,'circle*{1}}')
      RETURN
C-----------------------------------------------------------------------
C--- IFUNC=14, End picture ---------------------------------------------
  140 WRITE(LUN,'(A)') BS//'end{picture}'
      RETURN
C-----------------------------------------------------------------------
      END
