*-----------------------------------------------------------------------

      SUBROUTINE TRUNC (NQ, ISHIFT)

C  Routine to truncate data in NQUAD quadrants
C  End NPTS of array are dropped symmetrically and centre channel reset

C  History:
C     6-JUN-2000 (AJC):
C       Replace 'Type *' with 'PRINT *'

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   NQ
      INTEGER   ISHIFT

*     Include files:

      INCLUDE   'STACKCOMM'
      INCLUDE   'STAKPAR'

*     Functions:

      LOGICAL   DOQUAD
      INTEGER   NTOT

*     Local variables:

      INTEGER   NPTSNEW(8)
      INTEGER   IFAIL
      INTEGER   I
      INTEGER   N1, N2
      INTEGER   NQ1, NQ2

*  Ok, go...

      IFAIL = 0

      CALL INITNPNEW (NPTSNEW)
      CALL QLIM      (NQ, NQ1, NQ2)

      CALL PUSH

      DO NQ = NQ1, NQ2
        IF(DOQUAD(NQ))   THEN
          NPTSNEW(NQ) = NPTS(NQ) - 2*ISHIFT
          IF (NPTSNEW(NQ).LE.0) THEN
            IFAIL = 5
          ELSE
            N1 = NTOT(NQ-1) + 1
            N2 = NTOT(NQ) - 2*ISHIFT
            DO I = N1, N2
              DATA(I) = DATA(I+ISHIFT)
            END DO
          END IF
        END IF
      END DO

      IF (IFAIL.EQ.0) THEN
        CALL COMPRESSQ (NPTSNEW)
        CALL XY
      ELSE
        PRINT *, ' -- trunc --'
        PRINT *, '    One or more quadrants will have zero points'
        PRINT *, '           Spectrum in stack not changed.'
      END IF

      CALL POP

      RETURN
      END

*-----------------------------------------------------------------------
