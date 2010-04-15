C-----------------------------------------------------------------------

      SUBROUTINE SLIDE (NQ, IFAIL)

C  Subroutine to average Kitt Peak data in the two polarizations and
C  reset to a single filterbank with half the number of channels.

C  History:
C     6-JUN-2000 (AJC):
C       Replace 'Type *' with 'PRINT *'

      IMPLICIT   NONE

C     Formal parameters

      INTEGER    NQ
      INTEGER    IFAIL

*     Common blocks

      INCLUDE   'FLAGCOMM'
      INCLUDE   'STACKCOMM'

*     Local variables

      INTEGER   J
      INTEGER   NP1
      INTEGER   NQ1,    NQ2
      INTEGER   NST
      INTEGER   NPTSNEW(8)

*     Functions

      INTEGER   ICHECK
      INTEGER   NTOT
      LOGICAL   DOQUAD

*  Ok, go...

      IFAIL = 0
      IF (ICHECK(1,IFAIL).NE.1)   RETURN

      CALL INITNPNEW (NPTSNEW)
      CALL QLIM      (NQ, NQ1, NQ2)
      DO NQ = NQ1, NQ2
        IF (DOQUAD(NQ))  THEN
          NP1 = NPTS(NQ)
          IF (MOD(NP1,2).NE.0) THEN
            IFAIL = 34
            PRINT *, 'Odd number of points in quadrant', NQ
          ELSE

C           Average two polarizations.

            NPTSNEW(NQ) = NP1/2
            NST         = NTOT(NQ-1)
            DO J = 1,NPTSNEW(NQ)
              IF (DATA(NST+J).EQ.BADPIX_VAL .AND.
     &            DATA(NST+NPTSNEW(NQ)).EQ.BADPIX_VAL) THEN
                DATA(NST+J) = BADPIX_VAL
              ELSE IF (DATA(NST+J).EQ.BADPIX_VAL) THEN
                DATA(NST+J) = DATA(NST+NPTSNEW(NQ)+J)
              ELSE IF (DATA(NST+NPTSNEW(NQ)).EQ.BADPIX_VAL) THEN
                DATA(NST+J) = DATA(NST+J)
              ELSE
                DATA(NST+J) = 0.5*(DATA(NST+J)+DATA(NST+NPTSNEW(NQ)+J))
              END IF
            END DO

C         Reset header parameters

            TSYS(NQ) = TSYS(NQ)/SQRT(2.)

          END IF
        END IF
      END DO

      CALL COMPRESSQ (NPTSNEW)

      RETURN
      END


