C*GRLIN1 -- draw a dashed line
C+
      SUBROUTINE GRLIN1 (X0,Y0,X1,Y1,RESET)
C
C GRPCKG : dashed line. Generate a visible dashed line between points
C (X0,Y0) and (X1,Y1) according to the dash pattern stored in common.
C If RESET = .TRUE., the pattern will start from the beginning.
C Otherwise, it will continue from its last position.
C     DASHED LINE PATTERN ARRAY CONTAINING LENGTHS OF
C          MARKS AND SPACES IN UNIT CUBE: GRPATN(*)
C     OFFSET IN CURRENT PATTERN SEGMENT: GRPOFF
C     CURRENT PATTERN SEGMENT NUMBER: GRIPAT
C     NUMBER OF PATTERN SEGMENTS: 8
C--
C (1-Feb-1983)
C  6-Sep-1989 - Changes for standard Fortran-77 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      REAL ADJUST, ARG1, ARG2, ALFARG
      REAL SCALE, SEGLEN, X1, X0, Y1, Y0, DS, DSOLD
      REAL ALPHA1, ALPHA2, XP, YP, XQ, YQ
      LOGICAL RESET
      INTEGER THICK
      INTRINSIC ABS, MIN, MOD, REAL, SQRT
C
      ADJUST(ARG1,ARG2,ALFARG) = ALFARG*(ARG2 - ARG1) + ARG1
C
      THICK = GRWIDT(GRCIDE)
      SCALE = SQRT(REAL(ABS(THICK)))
      IF (RESET) THEN
          GRPOFF(GRCIDE) = 0.0
          GRIPAT(GRCIDE) = 1
      END IF
      SEGLEN = SQRT((X1-X0)**2 + (Y1-Y0)**2)
      IF (SEGLEN .EQ. 0.0) RETURN
      DS = 0.0
C
C       Repeat until (ALPHA2 .GE. 1.0)
C
C       Line segments matching the pattern segments are determined
C       by finding values (ALPHA1,ALPHA2) defining the start and end
C       of the segment in the parametric equation (1-ALPHA)*P1 + ALPHA*P2
C       defining the line.  DS measures the progress along the line
C       segment and defines the starting ALPHA1.  The ending ALPHA2
C       is computed from the end of the current pattern mark or space
C       or the segment end, whichever comes first.
C
   10 DSOLD = DS
      ALPHA1 = DS/SEGLEN
      ALPHA2 = MIN(1.0,(DS+SCALE*GRPATN(GRCIDE,GRIPAT(GRCIDE))-
     1           GRPOFF(GRCIDE))/SEGLEN)
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
          RETURN
      END IF
      GRIPAT(GRCIDE) = MOD(GRIPAT(GRCIDE),8) + 1
      GRPOFF(GRCIDE) = 0.0
      GO TO 10
      END
