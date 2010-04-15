C-----------------------------------------------------------------------

      SUBROUTINE NTRANS (XSCALE, NBPTS, X1, X2, XFAC,
     &                   NLOW, NHIGH, IFAIL)

C   Routine to find an ordered point pair in X-scale array
C   corresponding to input X-values X1 and X2. Returns an error
C   code if no points in X-scale(NBPTS) are included in range, or
C   if range is less than one point interval

      REAL*4   XSCALE(*)
      LOGICAL  NO_LOW, NO_HIGH

      NO_LOW  = .FALSE.
      NO_HIGH = .FALSE.

      NLOW  = NINT ( XSNART (XSCALE, X1, NBPTS, IFAIL))
      IF (IFAIL.NE.0) NO_LOW  = .TRUE.
      NHIGH = NINT ( XSNART (XSCALE, X2, NBPTS, IFAIL))
      IF (IFAIL.NE.0) NO_HIGH = .TRUE.

      XMIN = MIN (XSCALE(1), XSCALE(NBPTS))
      XMAX = MAX (XSCALE(1), XSCALE(NBPTS))

C  Take care of the case where one or both points are outside the plot
C  limits (should NOT cause an error!)

C     First case: first point outside plot limits -- must figure out
C     whether to assume high or low limit from value of X.
      IF (NO_LOW) THEN
        IF (XFAC*(X1-XMIN).LT.0.0) THEN
          NLOW = 1
        ELSE
          NLOW = NBPTS
        END IF
      END IF

C    (Second) case: as for case 2, but for other point missing.
      IF (NO_HIGH) THEN
        IF (XFAC*(X2-XMIN).LT.0.0) THEN
          NHIGH = 1
        ELSE
          NHIGH = NBPTS
        END IF
      END IF

C  Test that we span at least one point

      IF (NLOW.EQ.NHIGH) THEN
        IFAIL = 26
        RETURN
      END IF

C  Assign result to NLOW and NHIGH in such a way that N1 is
C  always less than N2 (to facilitate direct use of routine with
C  range selection for plotting etc).

      IF (NLOW.GT.NHIGH) THEN
        NTEMP = NLOW
        NLOW  = NHIGH
        NHIGH = NTEMP
      END IF

C  Test again that both points are in range:

      IF (NLOW.LT.1 .OR. NHIGH.GT.NBPTS) THEN
        IFAIL = 19
      ELSE
        IFAIL = 0
      END IF

      RETURN
      END


