*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C--------------------------------------------------------------------------

      SUBROUTINE RANGEXY2D (X, XL, XH, BUF, NQUAD, BLIM, ULIM, NINTS,
     &                      EXPAND, BADVAL, IERR)

C   Routine to set up reasonable default scales taking account of the data

      IMPLICIT  NONE

C     Formal parameters;

      REAL      X(*)
      REAL      XL, XH
      REAL      BUF(*)
      INTEGER   NQUAD
      REAL      BLIM,   ULIM
      INTEGER   NINTS
      REAL      EXPAND
      REAL      BADVAL
      INTEGER   IERR

C     Local variables:

      INTEGER   I
      INTEGER   NQ
      REAL      RANGE

C     Functions;

      INTEGER   NTOT
      LOGICAL   DOQUAD

C  Ok, go...

      IF (IERR.NE.0) RETURN

C     First find maximum and minimum values in unmasked data

      BLIM = +1.E20
      ULIM = -1.E20

      DO NQ = 1,NQUAD
        IF (DOQUAD(NQ)) THEN

          DO I = NTOT(NQ-1)+1, NTOT(NQ)
            IF (X(I).ge.XL  .and. X(I).le.XH) THEN
              IF (BUF(I).NE.BADVAL .AND. BUF(I).GT.ULIM) ULIM = BUF(I)
              IF (BUF(I).NE.BADVAL .AND. BUF(I).LT.BLIM) BLIM = BUF(I)
            END IF
          END DO

        END IF
      END DO

C  Expand scales more or less symmetrically if required

      RANGE = ULIM - BLIM

      IF (RANGE.GT.0.0) THEN
        IF (EXPAND.GT.1.) THEN
          BLIM = BLIM - 0.5*(EXPAND-1.)*RANGE
          ULIM = ULIM + 0.5*(EXPAND-1.)*RANGE
        END IF

C       ... and deduce more sensible scaling

        CALL AUTORANGE (BLIM,ULIM,NINTS)

      ELSE
        IERR = 35
        PRINT *,'Error in RANGEXY - Range = 0'
      END IF

      RETURN
      END


