      SUBROUTINE SETUSV (VN,IV)
      CHARACTER*(*) VN
C
C This subroutine sets the values of various utility state variables.
C VN is the name of the variable and IV is its value.
C
C The labelled common block IUTLCM contains all of the utility state
C variables.
C
      COMMON /IUTLCM/ IU(100)
C
C Define an array in which to get the GKS aspect source flags.
C
      DIMENSION LF(13)
C
C Check for the linear-log scaling variable, which can take on these
C values:
C
C     1 = X linear, Y linear
C     2 = X linear, Y log
C     3 = X log   , Y linear
C     4 = X log   , Y log
C
      IF (VN(1:2).EQ.'LS') THEN
        IF (IV.LT.1.OR.IV.GT.4) THEN
          CALL SETER ('SETUSV - LOG SCALE VALUE OUT OF RANGE',2,2)
        END IF
        IU(1)=IV
C
C Check for the mirror-imaging variable, which can take on these
C values:
C
C     1 = X normal  , Y normal
C     2 = X normal  , Y reversed
C     3 = X reversed, Y normal
C     4 = X reversed, Y reversed
C
      ELSE IF (VN(1:2).EQ.'MI') THEN
        IF (IV.LT.1.OR.IV.GT.4) THEN
          CALL SETER ('SETUSV - MIRROR-IMAGING VALUE OUT OF RANGE',3,2)
        END IF
        IU(2)=IV
C
C Check for the scale factor setting the resolution of the plotter in
C the x direction.
C
      ELSE IF (VN(1:2).EQ.'XF') THEN
        IF (IV.LT.1.OR.IV.GT.15) THEN
          CALL SETER ('SETUSV - X RESOLUTION OUT OF RANGE',4,2)
        END IF
        IU(3)=IV
C
C Check for the scale factor setting the resolution of the plotter in
C the y direction.
C
      ELSE IF (VN(1:2).EQ.'YF') THEN
        IF (IV.LT.1.OR.IV.GT.15) THEN
          CALL SETER ('SETUSV - Y RESOLUTION OUT OF RANGE',5,2)
        END IF
        IU(4)=IV
C
C Check for the variable specifying the size of the buffer to be used
C by PLOTIT.
C
      ELSE IF (VN(1:2).EQ.'PB') THEN
        IF (IV.LT.2.OR.IV.GT.50) THEN
          CALL SETER ('SETUSV - PLOTIT BUFFER SIZE OUT OF RANGE',6,2)
        END IF
        CALL PLOTIT (0,0,2)
        IU(5)=IV
C
C Check for a metacode unit number.
C
      ELSE IF (VN(1:2).EQ.'MU') THEN
        IF (IV.LE.0) THEN
          CALL SETER ('SETUSV - METACODE UNIT NUMBER ILLEGAL',7,2)
        END IF
C
C For the moment (1/11/85), we have to deactivate and close the old
C workstation and open and activate a new one.  This does allow the
C user to break up his metacode output.  It does not necessarily allow
C for the resumption of output to a previously-written metacode file.
C
        CALL GDAWK (IU(6))
        CALL GCLWK (IU(6))
        IU(6)=IV
        CALL GOPWK (IU(6),2,1)
        CALL GACWK (IU(6))
C
C If, in the future, it becomes possible to have more than one metacode
C workstation open at once, the following code can be used instead.
C
C       CALL GDAWK (IU(6))
C       IU(6)=IV
C       CALL GQOPWK (0,IE,NO,ID)
C       IF (NO.NE.0) THEN
C         DO 101 I=1,NO
C           CALL GQOPWK (I,IE,NO,ID)
C           IF (ID.EQ.IU(6)) GO TO 102
C 101     CONTINUE
C       END IF
C       CALL GOPWK (IU(6),2,1)
C 102   CALL GAWK (IU(6))
C
C Check for one of the variables setting color and intensity.
C
      ELSE IF (VN(1:2).EQ.'IR') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF RED INTENSITY',8,2)
        END IF
        IU(7)=IV
C
      ELSE IF (VN(1:2).EQ.'IG') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF GREEN INTENSITY',9,2)
        END IF
        IU(8)=IV
C
      ELSE IF (VN(1:2).EQ.'IB') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF BLUE INTENSITY',10,2)
        END IF
        IU(9)=IV
C
      ELSE IF (VN(1:2).EQ.'IN') THEN
        IF (IV.LT.0.OR.IV.GT.10000) THEN
          CALL SETER ('SETUSV - ILLEGAL VALUE OF INTENSITY',11,2)
        END IF
        IU(10)=IV
C
C Assign the intensity-controlling variables to local variables with
C simple, meaningful names.
C
        IR=IU(7)
        IG=IU(8)
        IB=IU(9)
        IN=IU(10)
        II=IU(11)
        IM=IU(12)
C
C Compute the floating-point red, green, and blue intensities.
C
        FR=FLOAT(IR)/FLOAT(MAX0(IR,IG,IB,1))*FLOAT(IN)/10000.
        FG=FLOAT(IG)/FLOAT(MAX0(IR,IG,IB,1))*FLOAT(IN)/10000.
        FB=FLOAT(IB)/FLOAT(MAX0(IR,IG,IB,1))*FLOAT(IN)/10000.
C
C Dump PLOTIT's buffer before changing anything.
C
        CALL PLOTIT (0,0,2)
C
C Set the aspect source flags for all the color indices to "individual".
C
        CALL GQASF (IE,LF)
        LF( 3)=1
        LF( 6)=1
        LF(10)=1
        LF(13)=1
        CALL GSASF (LF)
C
C Pick a new color index and use it for polylines, polymarkers, text,
C and areas.
C
        II=MOD(II,IM)+1
        IU(11)=II
        CALL GSPLCI (II)
        CALL GSPMCI (II)
        CALL GSTXCI (II)
        CALL GSFACI (II)
C
C Now, redefine the color for that color index on each open workstation.
C
        CALL GQOPWK (0,IE,NO,ID)
C
        DO 103 I=1,NO
          CALL GQOPWK (I,IE,NO,ID)
          CALL GSCR (ID,II,FR,FG,FB)
  103   CONTINUE
C
C Check for variable resetting the color index.
C
      ELSE IF (VN(1:2).EQ.'II') THEN
        IF (IV.LT.1.OR.IV.GT.IU(12)) THEN
          CALL SETER ('SETUSV - ILLEGAL COLOR INDEX',12,2)
        END IF
        IU(11)=IV
C
        CALL PLOTIT (0,0,2)
C
        CALL GQASF (IE,LF)
        LF( 3)=1
        LF( 6)=1
        LF(10)=1
        LF(13)=1
        CALL GSASF (LF)
C
        CALL GSPLCI (IV)
        CALL GSPMCI (IV)
        CALL GSTXCI (IV)
        CALL GSFACI (IV)
C
C Check for the variable limiting the values of color index used.
C
      ELSE IF (VN(1:2).EQ.'IM') THEN
        IF (IV.LT.1) THEN
          CALL SETER ('SETUSV - ILLEGAL MAXIMUM COLOR INDEX',13,2)
        END IF
        IU(12)=IV
C
C Check for the variable setting the current line width scale factor.
C
      ELSE IF (VN(1:2).EQ.'LW') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL LINE WIDTH SCALE FACTOR',14,2)
        END IF
        IU(13)=IV
C
C Dump PLOTIT's buffer before changing anything.
C
        CALL PLOTIT (0,0,2)
C
C Set the aspect source flag for linewidth scale factor to "individual".
C
        CALL GQASF (IE,LF)
        LF(2)=1
        CALL GSASF (LF)
C
C Redefine the line width scale factor.
C
        CALL GSLWSC (FLOAT(IV)/1000.)
C
C Check for the variable setting the current marker size scale factor.
C
      ELSE IF (VN(1:2).EQ.'MS') THEN
        IF (IV.LT.0) THEN
          CALL SETER ('SETUSV - ILLEGAL MARKER SIZE SCALE FACTOR',15,2)
        END IF
        IU(14)=IV
C
C Set aspect source flag for marker size scale factor to "individual".
C
        CALL GQASF (IE,LF)
        LF(5)=1
        CALL GSASF (LF)
C
C Redefine the marker size scale factor.
C
        CALL GSMKSC (FLOAT(IV)/1000.)
C
C Otherwise, the variable name is unknown.
C
      ELSE
        CALL SETER ('SETUSV - UNKNOWN VARIABLE NAME IN CALL',1,2)
C
      ENDIF
      RETURN
      END
