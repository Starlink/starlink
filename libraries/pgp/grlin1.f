      SUBROUTINE GRLIN1 (X0,Y0,X1,Y1)
*+
*   - - - - - - - -
*     G R L I N 1      (GKS emulation of GRPCKG)
*   - - - - - - - -
*
*   Generate a visible dashed line between points (X0,Y0) and
*   (X1,Y1) according to the dash pattern stored in common.
*
*   Given
*      X0       r       Line start x coordinate
*      Y0       r       Line start y coordinate
*      X1       r       Line end x coordinate
*      Y1       r       Line end y coordinate
*
*   Read from COMMON
*      GRCIDE    i       Current device id
*      GRPOFF    i()     Pattern offset
*      GRIPAT    i()     Pattern segment number
*      GRSTYL    i()     Line style
*      GRWIDT    i()     Line width
*
*   Written to COMMON
*      GRPOFF    i()     Pattern offset
*      GRIPAT    i()     Pattern segment number
*
*   D.L.Terrett  Starlink  Aug 1987  (After T.J.Pearson)
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'


      REAL X0, Y0, X1, Y1
      REAL ADJUST, ARG1, ARG2, ALFARG
      REAL SCALE, SEGLEN, DS, DSOLD
      REAL ALPHA1, ALPHA2, XP, YP, XQ, YQ
      REAL PATN(8,2:5)
      INTEGER THICK

      DATA PATN/
     :          8*10.0,
     :          8.0, 6.0, 1.0, 6.0, 8.0, 6.0, 1.0, 6.0,
     :          1.0, 6.0, 1.0, 6.0, 1.0, 6.0, 1.0, 6.0,
     :          8.0, 6.0, 1.0, 6.0, 1.0, 6.0, 1.0, 6.0 /

      ADJUST(ARG1,ARG2,ALFARG) = ALFARG*(ARG2 - ARG1) + ARG1

      THICK = GRWIDT(GRCIDE)
      SCALE = SQRT(FLOAT(ABS(THICK)))
      SEGLEN = SQRT((X1-X0)**2 + (Y1-Y0)**2)
      IF (SEGLEN .EQ. 0.0) RETURN
      DS = 0.0
*
*      Repeat until (ALPHA2 .GE. 1.0)
*
*      Line segments matching the pattern segments are determined
*      by finding values (ALPHA1,ALPHA2) defining the start and end
*      of the segment in the parametric equation (1-ALPHA)*P1 + ALPHA*P2
*      defining the line.  DS measures the progress along the line
*      segment and defines the starting ALPHA1.  The ending ALPHA2
*      is computed from the end of the current pattern mark or space
*      or the segment end, whichever comes first.
*
   10 DSOLD = DS
      ALPHA1 = DS/SEGLEN
      ALPHA2 = AMIN1(1.0,(DS+SCALE*PATN(GRIPAT(GRCIDE),GRSTYL(GRCIDE))-
     :             GRPOFF(GRCIDE))/SEGLEN)
      IF (MOD(GRIPAT(GRCIDE),2) .NE. 0) THEN
          XP = ADJUST(X0,X1,ALPHA1)
          YP = ADJUST(Y0,Y1,ALPHA1)
          XQ = ADJUST(X0,X1,ALPHA2)
          YQ = ADJUST(Y0,Y1,ALPHA2)
          IF (THICK.GT.1) THEN
            CALL GRLIN3(XP,YP,XQ,YQ)
          ELSE
            CALL GRLIN2(XP,YP,XQ,YQ)
          END IF
      END IF
      DS = ALPHA2*SEGLEN
      IF (ALPHA2 .GE. 1.0) THEN
          GRPOFF(GRCIDE) = GRPOFF(GRCIDE) + DS - DSOLD
          GO TO 9999
      END IF
      GRIPAT(GRCIDE) = MOD(GRIPAT(GRCIDE),8) + 1
      GRPOFF(GRCIDE) = 0.0
      GO TO 10

 9999 CONTINUE
      END
