      SUBROUTINE ENDMSK (X, Y, NPTS, PROP, WMASK)
      IMPLICIT NONE
      INTEGER I, NPTS
      DOUBLE PRECISION X(*), Y(*)
      DOUBLE PRECISION WMASK(*)
      DOUBLE PRECISION PERIOD, PMASK, PROP
      DOUBLE PRECISION PI
      PARAMETER (PI = 3.1415926536D+00)
C
C  PMASK is the proportion of the datastream that is to be masked at EACH end.
C
      PERIOD = X(NPTS) - X(1)
      PMASK = PROP * PERIOD
      DO 100, I=1,NPTS
        IF (X(I).LT.(X(1)+PMASK)) THEN
          WMASK(I) = 0.5D+00 * (1.0D+00 - DCOS(PI*(X(I)-X(1))/PMASK))
        ENDIF
        IF ((X(I).GE.(X(1)+PMASK)).AND.(X(I).LE.(X(NPTS)-PMASK))) THEN
          WMASK(I) = 1.0D+00
        ENDIF
        IF (X(I).GT.(X(NPTS)-PMASK)) THEN
          WMASK(I) = 0.5D+00 * (1.0D+00 - DCOS(PI*(X(NPTS)-X(I))/PMASK))
        ENDIF
        Y(I) = Y(I) * WMASK(I)
  100 CONTINUE
      RETURN
      END
