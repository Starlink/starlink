C# IL>=a, OL>=0
      SUBROUTINE GKLCLP(N,X,Y,LTYPEF,PATLEN,XMIN,YMIN,XMAX,YMAX,LN)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     The polyline defined by the points in X,Y is clipped
*     to the rectangle XMIN,YMIN,XMAX,YMAX. Resulting polylines
*     will be output by LN.
*
*  MAINTENANCE LOG
*  ---------------
*     18/02/83  AS    Original version stabilized
*      5/04/83  AS    Change arguments
*     02/09/85  RMK   Changed so only calls GKLTYP for broken linestyles (S155).
*
*  ARGUMENTS
*  ---------
*     INP N     - number of points in the polyline
*     INP X,Y   - polyline
*     INP LTYPEF- linetype flag, TRUE if want simulation
*     INP PATLEN- pattern length of simulated linetype
*     INP XMIN  -
*     INP YMIN  - define clipping rectangle where
*     INP XMAX  - XMIN.LE.XMAX and YMIN.LE.YMAX.
*     INP YMAX  -
*     INP LN    - polyline routine to call
*
      INTEGER N
      LOGICAL LTYPEF
      REAL PATLEN, X(N), Y(N), XMIN, YMIN, XMAX, YMAX
      EXTERNAL LN
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     IXCOD - codes for position of X coordinates
*              1 - less than XMIN
*              0 - between XMIN and XMAX
*              2 - greater than XMAX
*     IYCOD - codes for position of Y coordinates
*              1 - less than YMIN
*              0 - between YMIN and YMAX
*              2 - greater than YMAX
*     NLN   - number of points in current clipped polyline
*     LLN   - index to first point in current clipped polyline
*     IXC1,IXC2,IYC1,IYC2 - temporary saves for IXCOD and IYCOD
*     XTEMP,YTEMP - temporary saves for X(I) and Y(I)
*
      INTEGER IXCOD(2),IYCOD(2),NLN,LLN,IXC1,IXC2,IYC1,IYC2,I,J
      REAL XTEMP,YTEMP
*
*---------------------------------------------------------------------


* Check if clipping rectangle valid

      IF (XMIN.GT.XMAX .OR. YMIN.GT.YMAX) GOTO 999

* Work out codes for first point

      IF (X(1).GE.XMIN .AND. X(1).LE.XMAX) THEN
        IXCOD(1) = 0
      ELSEIF (X(1).GT.XMAX) THEN
        IXCOD(1) = 2
      ELSE
        IXCOD(1) = 1
      ENDIF

      IF (Y(1).GE.YMIN .AND. Y(1).LE.YMAX) THEN
        IYCOD(1) = 0
      ELSEIF (Y(1).GT.YMAX)THEN
        IYCOD(1) = 2
      ELSE
        IYCOD(1) = 1
      ENDIF

      NLN = 1
      LLN = 1

      DO 30 I=2,N

* Work out codes for second point

        IF (X(I).GE.XMIN .AND. X(I).LE.XMAX) THEN
          IXCOD(2) = 0
        ELSEIF (X(I).GT.XMAX) THEN
          IXCOD(2) = 2
        ELSE
          IXCOD(2) = 1
        ENDIF

        IF (Y(I).GE.YMIN .AND. Y(I).LE.YMAX) THEN
          IYCOD(2) = 0
        ELSEIF (Y(I).GT.YMAX)THEN
          IYCOD(2) = 2
        ELSE
          IYCOD(2) = 1
        ENDIF


* If both points are within clipping rectangle then increase counter
* number of points in clipped polyline

        IF ( IXCOD(1).EQ.0 .AND. IXCOD(2).EQ.0 .AND.
     :       IYCOD(1).EQ.0 .AND. IYCOD(2).EQ.0 ) THEN
          NLN = NLN + 1
          GOTO 20
        ENDIF


* If both points are easily recognised as outside clipping
* rectangle then do nothing

        IF ( (IXCOD(1).EQ.IXCOD(2) .AND. IXCOD(1).NE.0) .OR.
     :       (IYCOD(1).EQ.IYCOD(2) .AND. IYCOD(1).NE.0) ) GOTO 20

        IXC1 = IXCOD(1)
        IXC2 = IXCOD(2)
        IYC1 = IYCOD(1)
        IYC2 = IYCOD(2)
        XTEMP  = X(I)
        YTEMP  = Y(I)

* Work out the intersection with the clipping rectangle.
* Only bother if point is outside clipping rectangle

        DO 10 J=1,2
          IF (IXCOD(J).NE.0 .OR. IYCOD(J).NE.0) THEN

* Check on X boundaries first and adjust Y

            IF (IXCOD(J).EQ.1) THEN
              Y(I-2+J)=Y(I-1)+(XMIN-X(I-1))*(Y(I)-Y(I-1))/(X(I)-X(I-1))
              X(I-2+J)=XMIN

* If Y is now within clipping rectangle then reset the code so
* that coordinate does not get changed when checking on
* Y boundaries

              IF (Y(I-2+J).GE.YMIN .AND. Y(I-2+J).LE.YMAX) IYCOD(J) = 0
            ELSEIF (IXCOD(J).EQ.2) THEN
              Y(I-2+J)=Y(I-1)+(XMAX-X(I-1))*(Y(I)-Y(I-1))/(X(I)-X(I-1))
              X(I-2+J)=XMAX
              IF (Y(I-2+J).GE.YMIN .AND. Y(I-2+J).LE.YMAX) IYCOD(J) = 0
            ENDIF

* Check on Y boundaries and adjust X

            IF (IYCOD(J).EQ.1) THEN
              X(I-2+J)=X(I-1)+(YMIN-Y(I-1))*(X(I)-X(I-1))/(Y(I)-Y(I-1))
              Y(I-2+J)=YMIN

* Check needed to determine if line is still outside

              IF (X(I-2+J).LT.XMIN .OR. X(I-2+J).GT.XMAX) GOTO 15
            ELSEIF (IYCOD(J).EQ.2) THEN
              X(I-2+J)=X(I-1)+(YMAX-Y(I-1))*(X(I)-X(I-1))/(Y(I)-Y(I-1))
              Y(I-2+J)=YMAX
              IF (X(I-2+J).LT.XMIN .OR. X(I-2+J).GT.XMAX) GOTO 15
            ENDIF
          ENDIF

* Here if line is partially in

   10   CONTINUE
        NLN = NLN + 1

* Here if beginning of line is outside

        IF ( IXC1.NE.0 .OR. IYC1.NE.0 ) THEN
          LLN = I-1
        ENDIF

* Here if end of line outside

        IF ( IXC2.NE.0 .OR. IYC2.NE.0 ) THEN
          IF (LTYPEF.AND.KWLNTY(KWKIX).NE.GLSOLI) THEN
            CALL GKLTYP(NLN,X(LLN),Y(LLN),PATLEN,LN)
          ELSE
            CALL LN(NLN,X(LLN),Y(LLN))
          ENDIF
            NLN  = 1
        ENDIF

* Reset things which might have changed but original values are needed
* for clipping next line.
   15   CONTINUE
        IYCOD(2) = IYC2
        X(I) = XTEMP
        Y(I) = YTEMP

   20   CONTINUE
        IXCOD(1) = IXCOD(2)
        IYCOD(1) = IYCOD(2)

   30 CONTINUE

      IF (NLN.GT.1) THEN
          IF (LTYPEF.AND.KWLNTY(KWKIX).NE.GLSOLI) THEN
            CALL GKLTYP(NLN,X(LLN),Y(LLN),PATLEN,LN)
          ELSE
            CALL LN(NLN,X(LLN),Y(LLN))
          ENDIF
      ENDIF


  999 CONTINUE
      END
