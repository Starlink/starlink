C*LNDRIV   PGPLOT DRIVER FOR DIGITAL LN03 (LANDSCAPE)
	SUBROUTINE LNDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
	INTEGER IFUNC, NBUF, LCHR
	REAL    RBUF(*)
	CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for Digital LN03 Laser Printer (landscape)
C File : LNDRIVER.FOR
C-----------------------------------------------------------------------
C Version 1.0  - 1989 Nov. Sid Penstone, Queen's University
C Last Revision Dec.1,1989, added direct code for vertical lines,
C now do dots as a case of a zero length vector
C-----------------------------------------------------------------------
C	This routine has been written specifically for the LN03-PLUS
C	Laser Printer
C
C	Name: '/LN03'
C 	In all case, the initialization sequences are written out,
C	whether or not the plotter is connected as a terminal,
C	or driven from an intermediate file.
C
C	If there is more than one plot 
C	the page is ejected before the next one
C	
C	ref: Digital LN03 Programmer Reference Manual, P/N EK-OLN03-002,
C	  and Digital LN03-Plus "      "          "    P/N EK-LN03S-001
C
C	We end up with a 9" by 7" display area.
C
C-----------------------------------------------------------------------
	CHARACTER*(*) TYPE
	PARAMETER (TYPE='LN03 (Digital LN03 Laser Printer, landscape)')
C
	INTEGER MARGIN, NXPIX, NYPIX, NSIXROWS, NSIXCOLS
	PARAMETER(MARGIN=150)
	PARAMETER(NXPIX=3000)
	PARAMETER(NYPIX=2400)
	PARAMETER(NSIXROWS=(NYPIX/6)+2)
	PARAMETER(NSIXCOLS=NXPIX)
	CHARACTER*10 MSG
	INTEGER WIDTH,XLEFT,XRIGHT,YBOT,YTOP, INTENS, XMAX, YMAX, XMIN
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
C for debugging
	LOGICAL DEBUG
	DATA DEBUG/.FALSE./

C-----------------------------------------------------------------------
C
	GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
	GOTO 900
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
10	CHR = TYPE
	LCHR = LEN(TYPE)
	RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
20	RBUF(1) = 0
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
30	RBUF(1) = 300.0
	RBUF(2) = 300.0
	RBUF(3) = WIDTH
	NBUF = 3
	RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    No thick lines)
C
40	CHR = 'HNNNNNNNNN'
	LCHR = 10
	RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
50	CHR = 'PGPLOT.LN3'
	LCHR = 11
	RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
60	RBUF(1) = 0
	RBUF(2) = NXPIX-2*MARGIN
	RBUF(3) = 0
	RBUF(4) = NYPIX-2*MARGIN
	NBUF = 4
	RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
70	RBUF(1) = 10
	NBUF = 1
	RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
80	CONTINUE
	RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
90	CONTINUE
C Try to open the graphics device
	CALL GRGLUN(UNIT)
	OPEN (UNIT=UNIT,FILE=CHR(:LCHR),STATUS='NEW',
	1		FORM='FORMATTED', CARRIAGECONTROL='LIST',
	1		RECL=512,IOSTAT=IER)
	IF (IER.NE.0) THEN
	  CALL ERRSNS(IK1,IK2,IK3,IK4,IK5)
	  CALL GRWARN('Cannot open graphics device '
	1				//CHR(1:LCHR))
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
C Digital says that the allocated memory may not be zeroed:
C Clear the row flags (and the bit map)
	CALL LN03_CLEAR(%VAL(IPOINTS),BUFLEN,ACTIVE,NSIXROWS)
C
C always write the preamble
C this resets the plotter
	WRITE (UNIT, '(A)') ESC//'c'
C this sets it for landscape, origin at corner 
	WRITE (UNIT, '(A)') ESC//'[?21 J'
	PLOTNO = 0
	RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
  100	CONTINUE
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
  110	CONTINUE
C WE COULD GET THE VALUE OF XMAX AND YMAX HERE
	YMAX = YBOT - 2*MARGIN
	XMIN = XLEFT + MARGIN
	XMAX = XRIGHT - MARGIN
	PLOTNO = PLOTNO + 1
	RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120	CONTINUE
	I0 = XMIN + NINT(RBUF(1))
	J0 = YMAX - NINT(RBUF(2))
	I1 = XMIN + NINT(RBUF(3))
	J1 = YMAX - NINT(RBUF(4)) 
	CALL LN03_VECTOR(I0,J0,I1,J1,WIDTH,XLEFT,XRIGHT,
	1 YTOP,YBOT,%val(IPOINTS),ACTIVE,NSIXROWS,NSIXCOLS,INTENS)
	RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130	CONTINUE
	I0 = XMIN + NINT(RBUF(1))
	J0 = YMAX - NINT(RBUF(2))
	CALL LN03_VECTOR(I0,J0,I0,J0,WIDTH,XLEFT,XRIGHT,
	1 YTOP,YBOT,%VAL(IPOINTS),ACTIVE,NSIXROWS,NSIXCOLS,INTENS)
	RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140	CONTINUE
        CALL LN03_DUMP(UNIT,XLEFT,XMAX+WIDTH,YTOP+MARGIN,YMAX+WIDTH,
	1%val(IPOINTS),ACTIVE,NSIXROWS,NSIXCOLS)
C Clear the bitmap buffer
	IF (ALLOC) THEN
	    CALL LN03_CLEAR(%val(IPOINTS),BUFLEN,ACTIVE,NSIXROWS)
	ENDIF	    
C  Eject the paper with a form feed
C	WRITE (UNIT, '(A)') CHAR(12)
	RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150	INTENS = NINT(RBUF(1))
	IF (INTENS .GT.1) INTENS = 1
	if (debug) write(0,'(A,G13.7,I6)')'Intens= ',RBUF(1),INTENS
	RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C    (Null operation: buffering is not implemented.)
C
160	CONTINUE
	RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
170	GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Null operation: there is no alpha screen.)
C
180	CONTINUE
	RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called.)
C
190	GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C    (Not implemented: should not be called.)
C
200	GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C
210	RETURN
C	Other colors are not implemented
C
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called.)
C
220	GOTO 900
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
230	CONTINUE
	WRITE (UNIT, '(A)') CHR(:LCHR)
	RETURN
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900	WRITE (MSG,'(I10)') IFUNC
	CALL GRWARN('Unimplemented function in LN03 device driver: '//MSG)
	NBUF = -1
	RETURN
C-----------------------------------------------------------------------
	END

C------------------- PRIMITIVE LN03 FUNCTIONS -------------------
C
C----------------------------------------------------------------
C       CLEAR THE BITMAP IF IT WAS USED BEFORE
C
	SUBROUTINE LN03_CLEAR(BUFF,N,BUSY,NR)
	INTEGER*2 BUFF(0:*)
	LOGICAL BUSY(0:*)
	INTEGER N, I , NR
	DO 1 I = 0, N-1
1	BUFF(I) = 0
	DO 2 I = 0, NR-1
2	BUSY(I) = .FALSE.
	RETURN
	END


C---------------------------------------------------------------------
	CHARACTER*10 FUNCTION LN03_PACK(IARG,IP)
C-----------------------------------------------------------------------
C       (Internal routine, LN): Identical to the grgl00() routine
C       This subroutine translates the argument IARG into a character 
C	string and then returns the position of the first non-blank
C	character in the string
C Arguments:    IARG
C               IP (returned) 
C-----------------------------------------------------------------------
	INTEGER IARG, IP
C
	LN03_PACK = ' '
	IP = 10
	WRITE(LN03_PACK,'(I10)') IARG
	DO IP=1,10
	  IF (LN03_PACK(IP:IP) .NE. ' ') RETURN
	ENDDO
	END


C---------------------------- VECTOR DRAWING --------------------------
	SUBROUTINE LN03_VECTOR(X1,Y1,X2,Y2,WIDTH,XLEFT,XRIGHT,
	1 YTOP,YBOT,POINTS,ACTIVE,NSIXROWS,NSIXCOLS,INTENS)
C----------------------------------------------------------------------
C
C	Based on Bresenham's algorithm, and a C version written by
C	Paul Demone, Canadian Microelectronics Corporation
C
C	 We enter with the values x,y converted to internal values
C	That is, we have reflected the direction of Y
C

	INTEGER X1,Y1,X2,Y2,WIDTH,XLEFT,XRIGHT,YBOT,YTOP,NSIXCOLS,
	1 NSIXROWS, INTENS
	INTEGER X, Y, DX, DY, ADX, ADY, E, DA, DB, D1X, D1Y, D2X, D2Y
	INTEGER XX, YY, INDX, ITEMP
	INTEGER*2 SHIFT
	INTEGER*2 POINTS(0:*)
	LOGICAL ACTIVE(0:*)
	logical debug
	data debug /.false./

C 	if (debug) write(0,'(A,I6,I6)')'INTENS= ',INTENS
C Start at X1, Y1 
	if(debug)write(0, '(A,4(I6),A,I3,A,2(I10))')'LINE: ',
	1x1,y1,x2,y2,'INTENS=',INTENS,' ROW,COL: ',
	1 (Y1/6),((Y1/6)*NSIXCOLS+X1)
C Note that we always try to move the X index inside the loops, since they
C are adjacent in the bitmap array
C If this is a horizontal line, then we can do it faster
	IF (Y2 .EQ. Y1) THEN
	    IF(X2 .LT. X1) THEN
		ITEMP = X1
		X1 = X2
		X2 = ITEMP
	    ENDIF
	    YY = Y1
	    DO WHILE (YY .LT. Y1 + WIDTH .AND. YY .LT. YBOT)
		INDX = (YY/6)*NSIXCOLS
		SHIFT = JMOD(YY, 6)		    
		XX = X1
		DO WHILE ( XX .LT. X2 + WIDTH .AND. XX .LT. XRIGHT)
		    IF (INTENS .EQ. 0) THEN
			POINTS(INDX + XX) = IIBCLR(POINTS(INDX +XX), SHIFT)
		    ELSE
			POINTS(INDX + XX) = IIBSET(POINTS(INDX +XX), SHIFT)
		ENDIF
		    XX = XX + 1
		ENDDO
	        IF (INTENS .NE. 0) ACTIVE(YY/6) = .TRUE.
	        YY = YY + 1
	    ENDDO
	    RETURN
C		Might be a vertical line:
	ELSEIF (X2. EQ. X1) THEN
	    IF (Y2 .LT. Y1) THEN
		ITEMP = Y1
		Y1 = Y2
		Y2 = ITEMP
	    ENDIF
	    YY = Y1
	    DO WHILE (YY .LT. Y2 + WIDTH .AND. YY .LT. YBOT)
		INDX = (YY/6)*NSIXCOLS
		SHIFT = JMOD(YY, 6)		    
		XX = X1
		DO WHILE ( XX .LT. X1 + WIDTH .AND. XX .LT. XRIGHT)
		    IF (INTENS .EQ. 0) THEN
			POINTS(INDX + XX) = IIBCLR(POINTS(INDX +XX), SHIFT)
		    ELSE
			POINTS(INDX + XX) = IIBSET(POINTS(INDX +XX), SHIFT)
		ENDIF
		    XX = XX + 1
		ENDDO
	        IF (INTENS .NE. 0) ACTIVE(YY/6) = .TRUE.
	        YY = YY + 1
	    ENDDO
	    RETURN
	ENDIF
C		 It is a vector : Use the algorithm

	DX = X2 - X1
	DY = Y2 - Y1
	D2X = ISIGN(1,DX)
	D2Y = ISIGN(1,DY)
	ADX = IABS(DX)
	ADY = IABS(DY)
C Check for the maximum number of steps: X or Y ?
	IF (ADX .GT. ADY) THEN
		DA = ADX
		DB = ADY
		D1Y = 0
		D1X = ISIGN(1,DX)
	ELSE
		DA = ADY
		DB = ADX
		D1X = 0
		D1Y = ISIGN(1,DY)
	ENDIF
	DB = 2*DB
	E = DB - DA
	DA = 2*DA 
	X = X1
	Y = Y1
C Here we will be using some VAX Fortran extensions .......
  800   CONTINUE
C	DO WHILE (.TRUE.)
	    IF (X .GE. XLEFT  .AND. Y .GE. YTOP .AND.
	1       X .LT. XRIGHT .AND. Y .LT. YBOT) THEN
C Don't come in here if we are already off scale !
C If it is ok, then add a cluster of pixels of size width by width
C	if(debug)write(0, '(4(I6))')x,y
		XX = X
		DO WHILE (XX .LT. X+WIDTH .AND. XX .LT. XRIGHT)
		    YY = Y
		    DO WHILE(YY .LT. Y+WIDTH .AND. YY .LT. YBOT)
			INDX = (YY/6)*NSIXCOLS + XX
			SHIFT = JMOD(YY,6)
C	IF(DEBUG)WRITE(UNIT,'(2(I6),I10,6(I6))')
C	1   XX,YY,INDX,POINTS(INDX),INTENS,SHIFT
			IF (INTENS .EQ. 0) THEN 
			    POINTS(INDX) = IIBCLR(POINTS(INDX),SHIFT) 
		        ELSE
			    POINTS(INDX) = IIBSET(POINTS(INDX),SHIFT) 
			    ACTIVE(YY/6) = .TRUE.
			ENDIF
		        YY = YY + 1
		    ENDDO
		    XX = XX +1
		ENDDO
	    ENDIF	
C  Are we finished ?
	    IF (X .EQ. X2 .AND. Y .EQ. Y2) RETURN
C  Else move to the next point
	    IF ( E .GT. 0) THEN
	        X = X + D2X
	        Y = Y + D2Y
	        E = E + DB - DA
	    ELSE
	        X = X + D1X
	        Y = Y + D1Y
	        E = E + DB
	    ENDIF
	GOTO 800
C	ENDDO
	END


C ------------------------------------------------------------
	SUBROUTINE LN03_DUMP(UNIT,XLEFT,XRIGHT,YTOP,YBOT,
	1 POINTS,ACTIVE,NSIXROWS,NSIXCOLS)
C-------------------------------------------------------------
C Dump the bitmap to the printer
C Only write active sixel rows, and do run length encoding, too
C
C
C Parameters:
C	XLEFT: starting column in map, and initial x position
C	XRIGHT: last active column in map
C	YTOP: starting row in map, and initial y position
C	YBOT: last active row in the map

	INTEGER XLEFT, XRIGHT, YTOP, YBOT, NSIXROWS, NSIXCOLS, UNIT
	LOGICAL ACTIVE(0:*)
	INTEGER*2 POINTS(0:*)

	INTEGER*2 SXL
	INTEGER IROW, JCOL, K, PTR, RPT, INDX, N, MAXLEN
	CHARACTER*10 RUN, LN03_PACK
	CHARACTER*256 BUFFER
	CHARACTER*1 PAT, ESC
	DATA ESC /27/
	DATA MAXLEN /75/
	LOGICAL DEBUG
	DATA DEBUG /.false./
	INTEGER IOFFSET
	PARAMETER(IOFFSET = 34)

	CHARACTER*10 NEWX,NEWY
	INTEGER N1,N2

	NEWX = LN03_PACK(XLEFT,N1)
	NEWY = LN03_PACK(YTOP+IOFFSET,N2)
C Start at the top of the paper, down one line plus offset
C The pixels start 70 decipoints above the first line
C Set up the sixel modes
	WRITE(UNIT, '(A)') ESC//'[7 I'//ESC//'[11h'
	WRITE (UNIT, '(A)') 
	1 ESC//'['//NEWX(N1:)//'`'//ESC//'['//NEWY(N2:)//'d'
	1//ESC//'P0;0;1q"100;100'
C Now scan the bitmap
	PTR = 1
	DO 1000 IROW = 0, NSIXROWS-2
	IF (ACTIVE(IROW)) THEN
	if(debug)write(0,'(a,4(i6))')'row = ',irow
	    JCOL = XLEFT
	    DO WHILE (JCOL .LT. XRIGHT)
		INDX = IROW*NSIXCOLS
		SXL = POINTS(INDX + JCOL)
	        PAT = CHAR(IIAND(SXL,63) + 63)
		RPT = 0
C Look for repeated values on the rest of the line
		K = JCOL + 1
		DO WHILE( K .LT. XRIGHT .AND.
	1	    SXL .EQ. POINTS(INDX + K))
		    RPT = RPT +1
		    K = K + 1
		ENDDO
C	    IF (DEBUG) WRITE(1, '(2I10,2I6,1X,A,I5,I5)')
C	1 indx,indx+jcol, IROW, JCOL, PAT,ICHAR(PAT),SXL
C	Now check if there were any repeats
		IF (RPT .GT. 0) THEN
	            RUN = LN03_PACK(RPT +1, N)
	            BUFFER(PTR:) = '!'//RUN(N:)//PAT
		    PTR = PTR + LEN(RUN(N:)) + 2
		    JCOL = JCOL + RPT + 1
	        ELSE
	            BUFFER(PTR:PTR) = PAT
		    PTR = PTR + 1
		    JCOL = JCOL + 1
	        ENDIF
	        IF (PTR .GE. MAXLEN) THEN
		    WRITE (UNIT, '(A)') BUFFER(:PTR-1)
	            PTR = 1
	        ENDIF
	    ENDDO
	ENDIF
C Terminate each scan with a graphic newline character
        BUFFER(PTR:PTR) = '-'
	IF (PTR .GE. MAXLEN) THEN
	    WRITE (UNIT, '(A)') BUFFER(:PTR)
	    PTR = 1
	ELSE
	    PTR = PTR + 1
        ENDIF
1000	CONTINUE
	IF(PTR .GT. 1) WRITE (UNIT, '(A)') BUFFER(:PTR-1)
	WRITE(UNIT, '(A)') ESC//CHAR(92)
	RETURN
	END
