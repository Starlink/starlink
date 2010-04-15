*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C--------------------------------------------------------------------------

      SUBROUTINE SETRANGE (NCH, BUF, BLIM, ULIM, NINTS,
     &                     EXPAND, BADVAL, IERR)

C   Routine to set up reasonable default scales taking account of the data.

      IMPLICIT  NONE

C     Formal parameters:

      INTEGER   NCH
      REAL      BUF(NCH)
      REAL      BLIM, ULIM
      INTEGER   NINTS
      REAL      EXPAND
      REAL      BADVAL
      INTEGER   IERR

C     Local variables:

      INTEGER   I
      REAL      RANGE

C  Ok, go...

      IF (IERR.NE.0) RETURN

C     First find maximum and minimum values in data array

      BLIM = BUF(1)
      ULIM = BUF(1)

      DO I = 1, NCH
        IF (BUF(I).NE.BADVAL .AND. BUF(I).GT.ULIM) ULIM = BUF(I)
        IF (BUF(I).NE.BADVAL .AND. BUF(I).LT.BLIM) BLIM = BUF(I)
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
        PRINT *,'Error in SETRANGE - Range = 0'
      END IF

      RETURN
      END


