	SUBROUTINE VBDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
	INTEGER   IFUNC, NBUF, LCHR
	REAL      RBUF(*)
	CHARACTER CHR*(*)
C
C PGPLOT driver for Canon Laser printer.  PORTRAIT MODE.
C
C Supported device:  Canon LBP-8/A2 Laser printer.
C   Conforms to ISO646,2022,2375 and 6429 specifications.
C   VDM (graphics) conforms to proposed American National
C   Standard VDM mode.
C
C Device type code:  /VBCanon (portrait mode only).
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
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE
      PARAMETER (TYPE=
     :     'VBCANON (Canon laser printer, bitmap mode, portrait)')
	INTEGER    IS2,    IVESC
	PARAMETER (IS2=30, IVESC=125)
C- The maximum physical size of the plot in units of .08mm.
	INTEGER    MXLEN,      MXWID
	PARAMETER (MXLEN=2470, MXWID=3580)
C- Default size of plot.
	INTEGER    IDEFL,      IDEFW
	PARAMETER (IDEFL=2375, IDEFW=3000)
C
	INTEGER   GRGE00
	CHARACTER CBUF*256
	CHARACTER MSG*10
	CHARACTER CDASH(5),CFILL(0:11)
	INTEGER   I0, J0, I1, J1
	INTEGER   LUN, ICOL, NPTS, LBUF, LASX, LASY, IMAXL, IMAXW
	SAVE      LUN, ICOL, NPTS, LBUF, LASX, LASY, IMAXL, IMAXW
C---
C- Patterns defined with 2 and " appear the same on our Canon
C- so only one is used.  Pattern 0 causes the polygon not to
C- be filled.  Pattern ) erases interior of polygon is the
C- last character in list as all colors > max are set to
C- this pattern.
	DATA CFILL/')','1','(','''','&','%',
     &		'$','2','#','!','0',')'/
	DATA CDASH/'0','1','3','"','4'/
C---
	GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     &       110,120,130,140,150,160,900,180,190,200,
     &       210) IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in CA device driver: '//MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC= 1, Return device name. -------------------------------------
10	CHR=TYPE
	LCHR=LEN(TYPE)
	RETURN
C
C--- IFUNC= 2, Return Physical min and max for plot device. ------------
20	RBUF(1)=0
	RBUF(2)=MXLEN
	RBUF(3)=0
	RBUF(4)=MXWID
	RBUF(5)=0
	RBUF(6)=1
	NBUF=6
	RETURN
C
C--- IFUNC= 3, Return device resolution. -------------------------------
30	RBUF(1)=300.0
	RBUF(2)=300.0
	RBUF(3)=1
	NBUF=3
	RETURN
C
C--- IFUNC= 4, Return misc device info. --------------------------------
40	CHR='HNNANNNNNN'
	LCHR=10
	RETURN
C
C--- IFUNC= 5, Return default file name. -------------------------------
50	CHR='PGPLOT.CAN'
	LCHR=10
	RETURN
C
C--- IFUNC= 6, Return default physical size of plot. -------------------
60	RBUF(1)=0
	RBUF(2)=IDEFL
	RBUF(3)=0
	RBUF(4)=IDEFW
	RETURN
C
C--- IFUNC= 7, Return misc defaults. -----------------------------------
70	RBUF(1)=1
	NBUF=1
	RETURN
C
C--- IFUNC= 8, Select plot. --------------------------------------------
80	RETURN
C
C--- IFUNC= 9, Open workstation. ---------------------------------------
90	RBUF(2)=GRGE00('FFL',LUN,CHR,LCHR)
	RBUF(1)=LUN
	RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
100	CLOSE(UNIT=LUN)
	CALL GRFLUN(LUN)
	RETURN
C
C--- IFUNC=11, Begin Picture. ------------------------------------------
110	CALL GRCA03(LUN,1)
C- Use the origin transfer command to ensure that the picture is
C- centered on the page.
	IMAXW=NINT(RBUF(2))
	I0=(MXWID-IMAXW)/2
	IMAXL=NINT(RBUF(1))
	J0=(MXLEN-IMAXL)/2
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
120	I0=IMAXW-NINT(RBUF(2))
	J0=NINT(RBUF(1))
	I1=IMAXW-NINT(RBUF(4))
	J1=NINT(RBUF(3))
	CALL GRCA01(LUN,I0,J0,I1,J1)
	RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
130	I0=IMAXW-NINT(RBUF(2))
	J0=NINT(RBUF(1))
	CALL GRCA01(LUN,I0,J0,I0,J0)
	RETURN
C
C--- IFUNC=14, End Picture. --------------------------------------------
140	CALL GRCA03(LUN,2)
	RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C- Save pen number (up to 11) for possible use in pattern interior.
150	ICOL=MAX(0,MIN(NINT(RBUF(1)),11))
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
160	RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
180	RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C- Currently turned off, since pattern is reset at beginning of
C- every new line segment.  Note, if GRCA01 was modified to
C- properly use polylines then dash pattern may work better.
190	CBUF(1:4)='E1'//CDASH(NINT(RBUF(1)))//CHAR(IS2)
	WRITE(LUN,11) CBUF(:4)
	RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
200	IF(NPTS.EQ.0) THEN
	    NPTS=RBUF(1)
	    CBUF(1:5)='I'//CFILL(ICOL)//'0'//CHAR(IS2)//'2'
	    LBUF=5
	    LASX=0
	    LASY=0
	ELSE
	    NPTS=NPTS-1
	    I0=IMAXW-NINT(RBUF(2))
	    J0=NINT(RBUF(1))
	    CALL GRCA04(J0-LASY,CBUF,LBUF)
	    CALL GRCA04(I0-LASX,CBUF,LBUF)
	    LASX=I0
	    LASY=J0
	    IF(NPTS.EQ.0) THEN
		LBUF=LBUF+1
		CBUF(LBUF:LBUF)=CHAR(IS2)
		WRITE(LUN,11) CBUF(:LBUF)
11		FORMAT(A)
		LBUF=0
	    END IF
	END IF
	RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C- (not possible but can be called).
210	RETURN
C-----------------------------------------------------------------------
	END
