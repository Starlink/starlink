*-----------------------------------------------------------------------

      SUBROUTINE DIFFSP (NQ,BUF)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER   NQ
      REAL      BUF(*)

*     Common blocks

      INCLUDE   'FLAGCOMM'
      INCLUDE   'STACKCOMM'

*     Local variables

      INTEGER    J
      INTEGER    NQ1, NQ2
      INTEGER    NST

*     Functions

      INTEGER   NTOT
      LOGICAL   DOQUAD

*  Ok, go...

      CALL QLIM (NQ, NQ1, NQ2)

      DO NQ = NQ1, NQ2
        IF (DOQUAD(NQ))  THEN

          CALL COPYBF (NQ, BUF)

*         Middle channels
          NST = NTOT (NQ-1)
          DO J = 2, NPTS(NQ)-1
            IF (BUF(J+1).NE.BADPIX_VAL
     &          .AND. BUF(J-1).NE.BADPIX_VAL) THEN
              DATA(J+NST) = (BUF(J+1)-BUF(J-1)) * 0.5 / XFAC(NQ)
            ELSE
              DATA(J+NST) = BADPIX_VAL
            END IF
          END DO

*         First channel
          IF (BUF(1).NE.BADPIX_VAL .AND. BUF(2).NE.BADPIX_VAL) THEN
            DATA(NST+1) = (BUF(2)-BUF(1)) / XFAC(NQ)
          ELSE
            DATA(NST+1) = BADPIX_VAL
          END IF

*         Last channel
          IF (BUF(NPTS(NQ)).NE.BADPIX_VAL
     &        .AND. BUF(NPTS(NQ)-1).NE.BADPIX_VAL) THEN
            DATA(NST+NPTS(NQ)) = (BUF(NPTS(NQ))-BUF(NPTS(NQ)-1))
     &                           / XFAC(NQ)
          ELSE
            DATA(NST+NPTS(NQ)) = BADPIX_VAL
          END IF

        END IF
      END DO

      RETURN
      END

*-----------------------------------------------------------------------

