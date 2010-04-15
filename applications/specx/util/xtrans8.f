*-----------------------------------------------------------------------

      DOUBLE PRECISION  FUNCTION XTRANS8 (X, P, NX, IFAIL)

C   Routine to return x-value in current units corresponding to
C   fractional points value XBAR. This routine recognizes as valid
C   input values between 0.5 channels and N+0.5 channels -- that is,
C   plus or minus half a channel on the nominal X array. This is then
C   consistent with the way in which histograms are plotted etc.

      IMPLICIT NONE

*     Formal parameters

      REAL*8   X(*)         ! current scaled x-axis of plot
      REAL*8   P            ! points value to be translated
      INTEGER  NX           ! length of xscale array
      INTEGER  IFAIL        ! error return

*     Local variables

      INTEGER  N1
      REAL*8   F
      REAL*8   X1, X2

C  Ok, go...

      IFAIL   = 0
      XTRANS8 = 0.0

      IF (NINT(P).LT.1 .OR. NINT(P).GT.NX)   THEN
        IFAIL = 26
        RETURN
      END IF

      N1 = P
      IF (N1.EQ.0) THEN
        X1 = X(1) - (X(2)-X(1))
        X2 = X(1)
      ELSE IF (N1.EQ.NX) THEN
        X1 = X(NX)
        X2 = X(NX) + (X(NX)-X(NX-1))
      ELSE
        X1 = X(N1)
        X2 = X(N1+1)
      END IF

      F       = P - N1
      XTRANS8 = X1*(1.-F) + X2*F

      RETURN
      END

*-----------------------------------------------------------------------
