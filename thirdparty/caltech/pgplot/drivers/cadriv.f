      SUBROUTINE CADRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      INTEGER   IFUNC, NBUF, LCHR
      REAL      RBUF(*)
      CHARACTER CHR*(*)
C
C PGPLOT driver for Canon Laser printer.
C
C Supported device:  Canon LBP-8/A2 Laser printer.
C   Conforms to ISO646,2022,2375 and 6429 specifications.
C   VDM (graphics) conforms to proposed American National
C   Standard VDM mode.
C
C Device type code:  /CAnon (landscape mode only).
C
C Default file name:  PGPLOT.CAN
C
C Default view surface dimensions:  24 cm by 19 cm.
C
C Resolution:  300 pixels per inch in both directions.
C
C Color capability:  Color indices 0 (erase) and 1 (black) are
C   supported.  Note, hardware polygon fill is used and colors
C   0-11 control the fill pattern.
C
C Input capability:  None.
C
C File format:  Variable length records with Carriage control
C   of LIST.
C
C Obtaining hardcopy:  If printer is connected to a terminal
C   line (RS-232 option) then printing the file on the corresponding
C   queue should suffice.  If the printer is connected using
C   the Centronics interface, which appears the to VAX as an
C   LP device, then it is important to ensure that (1) all 8 bit
C   characters are passed to the printer (2) lines longer than
C   132 bytes are not truncated, and (3) no extra formatting
C   commands (e.g. form-feeds) are sent to the printer.
C   This can be done with the VMS command:
C   $ SET PRINT/PASSALL/LOWER/CR <device>
C   Note, some interface boards have a option to append a carriage
C   return after a formfeed or LF character, it is suggested
C   that this be disabled.
C   The file should be printed with the /PASSALL qualifier i.e.,
C   $ PRINT/PASSALL <filename>
C   Note, SET PRINT/PASSALL and PRINT/PASSALL do not do the
C   same things and hence PASSALL is required in both locations.
C
C 27-Jan-1988 - Version can be sent over BITNET (I hope) [AFT].
C 27-Sep-1986 - Add color index 0 (erase) [AFT].
C  5-Aug-1986 - [AFT].
C 13-Nov-1991 - [MCS] Having corrected unit scale factor to be one dot
C               instead of 0.8mm in GRCA03, changed viewport dimensions
C               to appear the same as before.
C 14-Nov-1991 - [MCS] 11 colour indexes are already implemented as fill
C               patterns - however device info on this said there were
C               only 0 and 1 - corrected IFUNC 2 now reports 11 colours
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE
      PARAMETER (TYPE='CANON (Canon LBP-8/A2 Laser printer, landscape)')
      INTEGER    IS2,    IVESC
      PARAMETER (IS2=30, IVESC=125)
C- The maximum physical size of the plot in units of 1/300 inch.
      INTEGER    MXLEN,      MXWID
      PARAMETER (MXLEN=3366, MXWID=2362)
C- Default size of plot.
      INTEGER    IDEFL,      IDEFW
      PARAMETER (IDEFL=2835, IDEFW=2244)
C
      CHARACTER CBUF*256
      CHARACTER MSG*10
      CHARACTER CDASH(5),CFILL(0:11)
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
     :     '$','2','#','!','0',')'/
      DATA CDASH/'0','1','3','"','4'/
C---
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     :     110,120,130,140,150,160,900,180,190,200,
     :     210) IFUNC
 900  WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in CA device driver: '//MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC= 1, Return device name. -------------------------------------
10      CHR=TYPE
        LCHR=LEN(TYPE)
        RETURN
C
C--- IFUNC= 2, Return Physical min and max for plot device. ------------
20      RBUF(1)=0
        RBUF(2)=MXLEN
        RBUF(3)=0
        RBUF(4)=MXWID
        RBUF(5)=0
        RBUF(6)=11
        NBUF=6
        RETURN
C
C--- IFUNC= 3, Return device resolution. -------------------------------
30      RBUF(1)=300.0
        RBUF(2)=300.0
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
50      CHR='PGPLOT.CAN'
        LCHR=10
        RETURN
C
C--- IFUNC= 6, Return default physical size of plot. -------------------
60      RBUF(1)=0
        RBUF(2)=IDEFL
        RBUF(3)=0
        RBUF(4)=IDEFW
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
        OPEN (UNIT=LUN, FILE=CHR(:LCHR), STATUS='NEW',
     :        FORM='FORMATTED',
     :        RECL=512, IOSTAT=IER)
        IF (IER.EQ.0) THEN
          RBUF(2)=1
        ELSE
          RBUF(2) = IER
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
110     CALL GRCA03(LUN,1)
C- Use the origin transfer command to ensure that the picture is
C- centered on the page.
        I0=(MXLEN-NINT(RBUF(1)))/2
        J0=(MXWID-NINT(RBUF(2)))/2
        CBUF(1:2)=CHAR(IVESC)//'"'
        LBUF=2
        CALL GRCA04(J0,CBUF,LBUF)
        CALL GRCA04(I0,CBUF,LBUF)
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
        CALL GRCA01(LUN,I0,J0,I1,J1)
        RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
130     I0=NINT(RBUF(1))
        J0=NINT(RBUF(2))
        CALL GRCA01(LUN,I0,J0,I0,J0)
        RETURN
C
C--- IFUNC=14, End Picture. --------------------------------------------
140     CALL GRCA03(LUN,2)
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
C- every new line segment.  Note, if GRCA01 was modified to
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
            CALL GRCA04(J0-LASY,CBUF,LBUF)
            CALL GRCA04(I0-LASX,CBUF,LBUF)
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

        SUBROUTINE GRCA01 (LUN,I0,J0,I1,J1)
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
        CALL GRCA04(J0,CBUF,LBUF)
        CALL GRCA04(I0,CBUF,LBUF)
        IX=I1-I0
        IY=J1-J0
        CALL GRCA04(IY,CBUF,LBUF)
        CALL GRCA04(IX,CBUF,LBUF)
        LBUF=LBUF+1
        CBUF(LBUF:LBUF)=CHAR(IS2)
        WRITE(LUN,11) CBUF(1:LBUF)
11      FORMAT(A)
        RETURN
        END
C*********
        SUBROUTINE GRCA03(LUN,ICMD)
C-----------------------------------------------------------------------
C Canon device driver support routine.  Outputs to LUN the string
C that begins a new picture (ICMD=1) or ends the current picture (ICMD=2).
C
C- LUN          I  I    Logical unit of output file.
C- ICMD         I  I    =1 to begin plot, =2 to terminate plot.
C
C 26-Jun-1986 - [AFT]
C 18-Jan-1988 - Change close brace to CHAR(125) [AFT]
C 13-Nov-1991 - [MCS, Jodrell Bank, England] Noticed aliasing when
C               modified PGGRAY stipple pattern to be sinusoidal.
C               Error traced to scale factor set at 0.8mm
C               instead of 1 dot => 1/300 = 0.84667mm corrected by
C               specifying scale factor in integral dots.
C-----------------------------------------------------------------------
        INTEGER    IESC,    IS2
        PARAMETER (IESC=27, IS2=30)
        INTEGER   LUN,ICMD
        CHARACTER CBUF*32
C---
11      FORMAT(A)
C---
        IF(ICMD.EQ.1) THEN
C- Go to ISO mode (ignored if in ISO mode already), Hard reset,
C- and then go to ISO again (in case dip switches set to Diablo).
            CBUF( 1: 4)=CHAR(IESC)//';'//CHAR(IESC)//'c'
            CBUF( 5: 6)=CHAR(IESC)//';'
C- Enable full paint mode.
            CBUF( 7:10)=CHAR(155)//'2&z'
C- Go to vector mode.
            CBUF(11:13)=CHAR(155)//'&'//CHAR(125)
C- Begin picture
            CBUF(14:21)='#PGPLOT'//CHAR(IS2)
C- Scaling mode 1 pixel, Begin picture body.
            CBUF(22:28)='!0#1'//CHAR(IS2)//'$'//CHAR(IS2)
            WRITE(LUN,11) CBUF(1:28)
        ELSE IF(ICMD.EQ.2) THEN
C- End picture, Return to text (0,0)
            CBUF(1:7)='%'//CHAR(IS2)//CHAR(125)//'p00'//CHAR(IS2)
            WRITE(LUN,11) CBUF(1: 7)
        END IF
        RETURN
        END
C*********
        SUBROUTINE GRCA04(NUM,CBUF,LBUF)
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
C- Bit 4(=16) is set for positive numbers and clear for negative.
        IS=16
        IF(ITMP.LT.0) THEN
            IS=0
            ITMP=-ITMP
        END IF
C- Bits 6+7(=64,128) clear and Bit 5(=32) set, flags that this
C- is the last byte in the number.
        CTMP(5:5)=CHAR(32+IS+IAND(ITMP,15))
        ITMP=ITMP/16
        IC=1
        IF(ITMP.EQ.0) THEN
C- Numbers in the range -15 to +15 can be sent in one byte.
            CBUF(LBUF+1:LBUF+1)=CTMP(5:5)
        ELSE
C- Larger numbers require more bytes and are recorded 6 bits
C- per byte with bit 7=(128) clear and bit 6(=64) set.
150         CTMP(5-IC:5-IC)=CHAR(64+IAND(ITMP,63))
            IC=IC+1
            ITMP=ITMP/64
            IF(ITMP.NE.0) GOTO 150
            CBUF(LBUF+1:LBUF+IC)=CTMP(6-IC:5)
        END IF
        LBUF=LBUF+IC
        RETURN
        END
