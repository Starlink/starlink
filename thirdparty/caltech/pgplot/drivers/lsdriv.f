C*LSDRIV -- PGPLOT driver for Canon LaserShot

      SUBROUTINE LSDRIV(IFUNC,RBUF,NBUF,CHR,LCHR,MODE)
      INTEGER   IFUNC, NBUF, LCHR, MODE
      REAL      RBUF(*)
      CHARACTER CHR*(*)
C
C PGPLOT driver for Canon LaserShot printer (LIPS2/2+).
C
C Supported device:  Canon LaserShot (LIPS2/2+).
C   Conforms to ISO646,2022,2375 and 6429 specifications.
C   VDM (graphics) conforms to proposed American National
C   Standard VDM mode.
C
C Device type code:  /LIPS2 (landscape, mode 1)
C                    /VLIPS2 (portrait, mode 2).
C
C Default file name:  PGPLOT.LPS
C
C Default view surface dimensions:
C                    23 cm by 18 cm (landcsape)
C                    18 cm by 23 cm (portrait)
C
C Resolution:  240 pixels per inch in both directions.
C
C Color capability:  Color indices 0 (erase) and 1 (black) are
C   supported.  Note, hardware polygon fill is used and colors
C   0-11 control the fill pattern.
C
C Input capability:  None.
C
C File format:  Variable length text records.
C
C Obtaining hardcopy:  use lpr (unix) or print (dos) command.
C
C 17-Aug-1994 - [M.Hamabe] modified from cadriver.f
C 18-Aug-1994 - [T.Pearson] merge landscape and portrait drivers
C-----------------------------------------------------------------------
      CHARACTER*(*) DEFNAM
      PARAMETER (DEFNAM='PGPLOT.LPS')
      INTEGER    IS2,    IVESC
      PARAMETER (IS2=30, IVESC=125)
C- The maximum physical size of the plot in units of 0.1 mm.
      INTEGER    MXLEN,      MXWID
      PARAMETER (MXLEN=2870, MXWID=1900)
C- Default size of plot.
      INTEGER    IDEFL,      IDEFW
      PARAMETER (IDEFL=2300, IDEFW=1800)
C
      CHARACTER CBUF*256
      CHARACTER MSG*10
      CHARACTER CDASH(5),CFILL(0:11)
      INTEGER   GROPTX
      INTEGER   I0, J0, I1, J1, IER
      INTEGER   LUN, ICOL, NPTS, LBUF, LASX, LASY
      SAVE      LUN, ICOL, NPTS, LBUF, LASX, LASY
C---
C- Patterns defined with 2 and " appear the same on our Canon
C- so only one is used.  Pattern 0 causes the polygon not to
C- be filled.  Pattern ) erases interior of polygon is the
C- last character in list as all colors > max are set to
C- this pattern.
      DATA CFILL/')','1','(','''','&','%',
     :          '$','2','#','!','0',')'/
      DATA CDASH/'0','1','3','"','4'/
C---
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     :     110,120,130,140,150,160,900,180,190,200,
     :     210) IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in LS device driver: '//MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC= 1, Return device name. -------------------------------------
10      IF (MODE.EQ.1) THEN
            CHR='LIPS2 (Canon LIPS2 file, landscape orientation)'
        ELSE
            CHR='VLIPS2 (Canon LIPS2 file, portrait orientation)'
        END IF
        LCHR=48
        RETURN
C
C--- IFUNC= 2, Return Physical min and max for plot device. ------------
20      IF (MODE.EQ.1) THEN
            RBUF(2)=MXLEN
            RBUF(4)=MXWID
        ELSE
            RBUF(2)=MXWID
            RBUF(4)=MXLEN
        END IF
        RBUF(1)=0
        RBUF(3)=0
        RBUF(5)=0
        RBUF(6)=11
        NBUF=6
        RETURN
C
C--- IFUNC= 3, Return device resolution. -------------------------------
30      RBUF(1)=254.0
        RBUF(2)=254.0
        RBUF(3)=1
        NBUF=3
        RETURN
C
C--- IFUNC= 4, Return misc device info. --------------------------------
40      CHR='HNNANNNNNN'
        LCHR=10
        RETURN
C
C--- IFUNC= 5, Return default file name. -------------------------------
50      CHR=DEFNAM
        LCHR=LEN(DEFNAM)
        RETURN
C
C--- IFUNC= 6, Return default physical size of plot. -------------------
60      IF (MODE.EQ.1) THEN
            RBUF(2)=IDEFL
            RBUF(4)=IDEFW
        ELSE
            RBUF(2)=IDEFW
            RBUF(4)=IDEFL
        ENDIF
        RBUF(1)=0
        RBUF(3)=0
        NBUF=4
        RETURN
C
C--- IFUNC= 7, Return misc defaults. -----------------------------------
70      RBUF(1)=1
        NBUF=1
        RETURN
C
C--- IFUNC= 8, Select plot. --------------------------------------------
80      RETURN
C
C--- IFUNC= 9, Open workstation. ---------------------------------------
90      CALL GRGLUN (LUN)
        IER = GROPTX(LUN, CHR(1:LCHR), DEFNAM, 1)
        IF (IER.EQ.0) THEN
          RBUF(2)=1
        ELSE
          CALL GRWARN('Cannot open output file for LPS plot')
          RBUF(2) = 0
          CALL GRFLUN(LUN)
        ENDIF
        RBUF(1)=LUN
        RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
100     CLOSE(UNIT=LUN)
        CALL GRFLUN (LUN)
        RETURN
C
C--- IFUNC=11, Begin Picture. ------------------------------------------
110     CALL GRLS03(LUN,1,MODE)
C- Use the origin transfer command to ensure that the picture is
C- centered on the page.
        IF (MODE.EQ.1) THEN
            I0=(MXLEN-NINT(RBUF(1)))/2
            J0=(MXWID-NINT(RBUF(2)))/2
        ELSE
            I0=(MXWID-NINT(RBUF(1)))/2
            J0=(MXLEN-NINT(RBUF(2)))/2
        END IF
        CBUF(1:2)=CHAR(IVESC)//'"'
        LBUF=2
        CALL GRLS04(J0,CBUF,LBUF)
        CALL GRLS04(I0,CBUF,LBUF)
        LBUF=LBUF+1
        CBUF(LBUF:LBUF)=CHAR(IS2)
        WRITE(LUN,11) CBUF(:LBUF)
        RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
120     I0=NINT(RBUF(1))
        J0=NINT(RBUF(2))
        I1=NINT(RBUF(3))
        J1=NINT(RBUF(4))
        CALL GRLS01(LUN,I0,J0,I1,J1)
        RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
130     I0=NINT(RBUF(1))
        J0=NINT(RBUF(2))
        CALL GRLS01(LUN,I0,J0,I0,J0)
        RETURN
C
C--- IFUNC=14, End Picture. --------------------------------------------
140     CALL GRLS03(LUN,2,MODE)
        RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C- Save pen number (up to 11) for possible use in pattern interior.
150     ICOL=MAX(0,MIN(NINT(RBUF(1)),11))
        RBUF(1)=MAX(0,MIN(ICOL,1))
        IF(ICOL.EQ.0) THEN
          CBUF(1:4)=CHAR(IVESC)//'G2'//CHAR(IS2)
        ELSE
          CBUF(1:4)=CHAR(IVESC)//'G0'//CHAR(IS2)
        END IF
        WRITE(LUN,11) CBUF(:4)
        RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
160     RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
180     RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C- Currently turned off, since pattern is reset at beginning of
C- every new line segment.  Note, if GRLS01 was modified to
C- properly use polylines then dash pattern may work better.
190     CBUF(1:4)='E1'//CDASH(NINT(RBUF(1)))//CHAR(IS2)
        WRITE(LUN,11) CBUF(:4)
        RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
200     IF(NPTS.EQ.0) THEN
            NPTS=RBUF(1)
            CBUF(1:5)='I'//CFILL(ICOL)//'0'//CHAR(IS2)//'2'
            LBUF=5
            LASX=0
            LASY=0
        ELSE
            NPTS=NPTS-1
            I0=NINT(RBUF(1))
            J0=NINT(RBUF(2))
            CALL GRLS04(J0-LASY,CBUF,LBUF)
            CALL GRLS04(I0-LASX,CBUF,LBUF)
            LASX=I0
            LASY=J0
            IF(NPTS.EQ.0) THEN
                LBUF=LBUF+1
                CBUF(LBUF:LBUF)=CHAR(IS2)
                WRITE(LUN,11) CBUF(:LBUF)
11              FORMAT(A)
                LBUF=0
            END IF
        END IF
        RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C- (not possible but can be called).
210     RETURN
C-----------------------------------------------------------------------
        END

C*GRLS01 -- PGPLOT driver for Canon LaserShot, line segment

      SUBROUTINE GRLS01 (LUN,I0,J0,I1,J1)
C-----------------------------------------------------------------------
C Canon device driver support routine.  Draws a line segment.
C Current routine plots end line segment as a separate polyline.
C This can be improved.
C
C I0,J0         I  I    The coordinate of the start point.
C I1,J1         I  I    The coordinate of the end point.
C
C 26-JUN-86 - [AFT]
C-----------------------------------------------------------------------
      INTEGER    IS2
      PARAMETER (IS2=30)
      INTEGER   LUN, I0, J0, I1, J1
      INTEGER   LBUF, IX, IY
      CHARACTER CBUF*64
C---
      CBUF(1:1)='1'
      LBUF=1
      CALL GRLS04(J0,CBUF,LBUF)
      CALL GRLS04(I0,CBUF,LBUF)
      IX=I1-I0
      IY=J1-J0
      CALL GRLS04(IY,CBUF,LBUF)
      CALL GRLS04(IX,CBUF,LBUF)
      LBUF=LBUF+1
      CBUF(LBUF:LBUF)=CHAR(IS2)
      WRITE(LUN,11) CBUF(1:LBUF)
 11   FORMAT(A)
      RETURN
      END

C*GRLS03 -- PGPLOT driver for Canon LaserShot, begin/end picture

      SUBROUTINE GRLS03(LUN,ICMD,MODE)
C-----------------------------------------------------------------------
C Canon LIPS2 device driver support routine.  Outputs to LUN the string
C that begins a new picture (ICMD=1) or ends the current picture (ICMD=2).
C
C- LUN          I  I    Logical unit of output file.
C- ICMD         I  I    =1 to begin plot, =2 to terminate plot.
C- MODE         I  I    =1 for landcsape, =2 for portrait.
C
C 19-Aug-1994 - [M.Hamabe, Inst.of Astron., U.Tokyo, Japan]
C               Modified version of grca03.f (for LIPS2 command)
C-----------------------------------------------------------------------
      INTEGER    IESC,    IS2
      PARAMETER (IESC=27, IS2=30)
      INTEGER   LUN,ICMD,MODE
      CHARACTER CBUF*35
C---
      IF(ICMD.EQ.1) THEN
C           - Go to ISO mode (ignored if in ISO mode already), Hard reset,
C           - and then go to ISO again (in case dip switches set to Diablo).
         CBUF( 1: 4)=CHAR(IESC)//';'//CHAR(IESC)//'c'
         CBUF( 5: 6)=CHAR(IESC)//';'
C           - Define paper orientation
         IF (MODE.EQ.1) THEN
            CBUF( 7:11)=CHAR(IESC)//CHAR(91)//'14p'
         ELSE
            CBUF( 7:11)=CHAR(IESC)//CHAR(91)//'15p'
         END IF
C           - Enable full paint mode.
         CBUF(12:16)=CHAR(IESC)//CHAR(91)//'2&z'
C           - Go to vector mode.
         CBUF(17:20)=CHAR(IESC)//CHAR(91)//'&'//CHAR(125)
C           - Begin picture
         CBUF(21:28)='#PGPLOT'//CHAR(IS2)
C           - Scaling mode 1 pixel, Begin picture body.
         CBUF(29:35)='!0#1'//CHAR(IS2)//'$'//CHAR(IS2)
         WRITE(LUN,'(A)') CBUF(1:35)
      ELSE IF(ICMD.EQ.2) THEN
C           - End picture, Return to text (0,0)
         CBUF(1:7)='%'//CHAR(IS2)//CHAR(125)//'p00'//CHAR(IS2)
         WRITE(LUN,'(A)') CBUF(1: 7)
      END IF
      RETURN
      END

C*GRLS04 -- PGPLOT driver for Canon LaserShot, convert integer

      SUBROUTINE GRLS04(NUM,CBUF,LBUF)
C-----------------------------------------------------------------------
C Canon device driver support routine.  Converts an integer into
C the form used by the Canon Laser printer.
C
C- NUM          I   I   Integer to be converted.
C- CBUF         I/O C*  Buffer string
C- LBUF         I/O I   Number of characters used in CBUF.
C
C 26-Jun-86 - [AFT]
C-----------------------------------------------------------------------
      CHARACTER CBUF*(*)
      INTEGER   NUM, LBUF
      INTEGER   ITMP, IS, IC
      CHARACTER CTMP*5
C---
      ITMP=NUM
C     - Bit 4(=16) is set for positive numbers and clear for negative.
      IS=16
      IF(ITMP.LT.0) THEN
         IS=0
         ITMP=-ITMP
      END IF
C     - Bits 6+7(=64,128) clear and Bit 5(=32) set, flags that this
C     - is the last byte in the number.
      CTMP(5:5)=CHAR(32+IS+IAND(ITMP,15))
      ITMP=ITMP/16
      IC=1
      IF(ITMP.EQ.0) THEN
C     - Numbers in the range -15 to +15 can be sent in one byte.
         CBUF(LBUF+1:LBUF+1)=CTMP(5:5)
      ELSE
C     - Larger numbers require more bytes and are recorded 6 bits
C     - per byte with bit 7=(128) clear and bit 6(=64) set.
 150     CTMP(5-IC:5-IC)=CHAR(64+IAND(ITMP,63))
         IC=IC+1
         ITMP=ITMP/64
         IF(ITMP.NE.0) GOTO 150
         CBUF(LBUF+1:LBUF+IC)=CTMP(6-IC:5)
      END IF
      LBUF=LBUF+IC
      RETURN
      END
