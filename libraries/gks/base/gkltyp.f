C# IL>=a, OL>=0
      SUBROUTINE GKLTYP(N,X,Y,PATLEN,LN)
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
*     The given polyline is divided into segments according to the
*     linetype and pattern length. Linetyping will be continuous
*     between polylines according to the flag LTCONT.
*
*  MAINTENANCE LOG
*  ---------------
*     18/02/83  AS    Original version stabilized
*      5/04/83  AS    Change arguments
*      3/08/83  AS    Put in code for linetyping over polylines
*     01/12/83  AS    Declare EXTERNAL LN so that PERQ will work!!
*     16/01/84  AS    Change definition of dotted line
*     06/04/84  NGB   Change definitions of all dots to zero length
*     03/04/86  DRJF  Tests for pattern at end of line not tight
*                     enough. Parts of dash dot pattern appearing
*                     incorrectly or not at all. Changed IF statements
*                     to have GE and AND logical operations in
*                     (bug fix S172).
*     04/07/86  RMK   Lengthened dots slightly in linestyle patterns to avoid
*                     problems with plotters, but still OK for other devices.
*     22/07/88  KEVP  Replaced IFIX with machine independent INT
*
*  ARGUMENTS
*  ---------
*     INP N      - number of points in polyline
*     INP X,Y    - arrays of points
*     INP PATLEN - pattern length
*     INP LN     - device dependent routine to draw a line.
*
      INTEGER N
      REAL PATLEN, X(N), Y(N)
      EXTERNAL LN
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     MAXPAT - maximum number of pattern segments for simulated
*              linetypes. Must be an even number.
*     MAXLT  - the highest linetype being simulated.
*     NPATLT - the number of pattern segments for each linetype.
*     NPAT   - current number of pattern segments
*     NCPAT  - current bit of pattern
*     PATLT  - array of patterns, fractions should add up to 1.0.
*     PAT    - array to hold currently used pattern in DC.
*     REMLEN - the remaining length of current pattern segment.
*     XA,YA  - arrays for points for calling LN.
*
      INTEGER MAXPAT, MAXLT
      PARAMETER (MAXPAT=8, MAXLT=5)
      INTEGER NPATLT(MAXLT), NCPAT, NPAT, I, J, K, L, NDO
      REAL REMLEN, PATLT(MAXPAT,MAXLT), PAT(MAXPAT),
     :     XA(2), YA(2), DIAG, DX, DY, ZERO
      PARAMETER (ZERO=0.0)

      DATA NPATLT/0, 2, 8, 4, 6/
      DATA PATLT /ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     :            0.45,0.55,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,
     :            0.03,0.30,0.03,0.30,0.03,0.31,ZERO,ZERO,
     :            0.39,0.29,0.03,0.29,ZERO,ZERO,ZERO,ZERO,
     :            0.30,0.23,0.03,0.18,0.03,0.23,ZERO,ZERO/
*
*  COMMENTS
*  --------
*     To add a new linetype -
*      increase MAXLT by 1.
*      if the number of pattern segments is greater than the current
*      number increase MAXPAT to that number. MAXPAT must be even.
*      put the number of pattern segments in data NPATLT.
*      add the pattern to data PATLT, and if MAXPAT has increased,
*      some 0.0's will have to be added to the other patterns to make
*      the numbers right.
*
*---------------------------------------------------------------------


* Check if line continuous with previous line.

      NPAT = NPATLT(KWLNTY(KWKIX))
      IF (ABS(X(1)-QWOLDX(KWKIX)).LE.QTOL .AND.
     :    ABS(Y(1)-QWOLDY(KWKIX)).LE.QTOL) THEN
        NCPAT  = KWCPAT(KWKIX)
        REMLEN = QWRMLG(KWKIX)
      ELSE
        NCPAT = NPAT
        REMLEN = 0.0
      ENDIF

* Load the current number of pattern segments and pattern.

      DO 10 I=1,NPAT
        PAT(I) = PATLT(I,KWLNTY(KWKIX)) * PATLEN
   10 CONTINUE

* Do over each line in polyline

      DO 50 I=1,N-1

* Work out some factors which are frequently used.

        DIAG = SQRT( (X(I+1)-X(I))**2 + (Y(I+1)-Y(I))**2 )
        IF (DIAG.EQ.0.0) GOTO 50
        DX = (X(I+1)-X(I))/DIAG
        DY = (Y(I+1)-Y(I))/DIAG
* Special case if LINE is less than current pattern segment length

        IF (REMLEN .GT. DIAG) THEN

* If current pat seg is even, ie filled part of pattern then draw,
* otherwise just update remaining length of pattern.

          IF (MOD(NCPAT,2) .EQ. 1) THEN
            CALL LN(2,X(I),Y(I))
          ENDIF
          REMLEN = REMLEN - DIAG

        ELSE

* Work out how many whole cycles through pattern so can have a loop
* with no checks in and it will go faster. But beginning and end of
* line are special cases.

          NDO = INT((DIAG-REMLEN)/PATLEN)

* Special case for beginning of line.
* If current pat seg is even, ie filled part of pattern then draw
* from beginning of line, otherwise project backwards for where
* beginning of gap would have been.

          IF (MOD(NCPAT,2) .EQ. 1) THEN
            XA(1) = X(I)
            YA(1) = Y(I)
            XA(2) = X(I) + REMLEN*DX
            YA(2) = Y(I) + REMLEN*DY
            CALL LN(2,XA,YA)
            NCPAT = MOD(NCPAT,NPAT) + 1
            REMLEN = 0.0
          ELSE
            XA(2) = X(I) - (PAT(NCPAT) - REMLEN)*DX
            YA(2) = Y(I) - (PAT(NCPAT) - REMLEN)*DY
          ENDIF

* Cycle through whole pattern.

          DO 20 J=1,NDO
            DO 20 K=1,NPAT,2
              XA(1) = XA(2) + PAT(NCPAT)*DX
              YA(1) = YA(2) + PAT(NCPAT)*DY
              NCPAT = MOD(NCPAT,NPAT) + 1
              XA(2) = XA(1) + PAT(NCPAT)*DX
              YA(2) = YA(1) + PAT(NCPAT)*DY
              NCPAT = MOD(NCPAT,NPAT) + 1
              CALL LN(2,XA,YA)
   20     CONTINUE

* Special case for end of line

          DO 30 L=1,NPAT
            XA(1) = XA(2) + PAT(NCPAT)*DX
            YA(1) = YA(2) + PAT(NCPAT)*DY
            IF (ABS(XA(1)-X(I)) .GE. ABS(X(I+1)-X(I))  .AND.
     :      ABS(YA(1)-Y(I)) .GE. ABS(Y(I+1)-Y(I)) ) THEN
              REMLEN = SQRT((XA(1)-X(I+1))**2 + (YA(1)-Y(I+1))**2)
              GOTO 50
            ENDIF
            NCPAT = MOD(NCPAT,NPAT) + 1
            XA(2) = XA(1) + PAT(NCPAT)*DX
            YA(2) = YA(1) + PAT(NCPAT)*DY
            IF (ABS(XA(2)-X(I)) .GE. ABS(X(I+1)-X(I))  .AND.
     :      ABS(YA(2)-Y(I)) .GE. ABS(Y(I+1)-Y(I)) ) THEN
              REMLEN = SQRT((XA(2)-X(I+1))**2 + (YA(2)-Y(I+1))**2)
              XA(2) = X(I+1)
              YA(2) = Y(I+1)
              CALL LN(2,XA,YA)
              GOTO 50
            ENDIF
            NCPAT = MOD(NCPAT,NPAT) + 1
            CALL LN(2,XA,YA)
   30     CONTINUE

        ENDIF

   50 CONTINUE

      QWRMLG(KWKIX) = REMLEN
      KWCPAT(KWKIX) = NCPAT
      QWOLDX(KWKIX) = X(N)
      QWOLDY(KWKIX) = Y(N)

      END
