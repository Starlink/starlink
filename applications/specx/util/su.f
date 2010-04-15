*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      SUBROUTINE SU (NQD, IFAIL)

C   Routine to difference spectra in X and Y stack positions

      IMPLICIT  NONE

*     Formal parameters

      INTEGER   NQD
      INTEGER   IFAIL

*     Common blocks

      INCLUDE 'SPECX_PARS'
      INCLUDE 'STACKCOMM'
      INCLUDE 'STAKPAR'
      INCLUDE 'FLAGCOMM'

      REAL*4      STACK(1)
      EQUIVALENCE (STACK(1),SCAN_HEADER(1))

*     Local variables

      INTEGER   I
      INTEGER   NQ
      INTEGER   NSTX
      INTEGER   NSTY

*     Functions

      LOGICAL   DOQUAD
      INTEGER   ICHKQ2
      INTEGER   NTOT
      INTEGER   NTOT2
      INTEGER   NPTS2

C   Check that there are the same number of quadrants in both spectra

      IF (ICHKQ2(NQD).NE.0) THEN
        IFAIL = 27
        RETURN
      END IF

      DO NQ = 1, NQMAX
        IF (DOQUAD(NQ)) THEN

          IF (NPTS(NQ).EQ.NPTS2(NQ)) THEN
            NSTX = NTOT  (NQ-1)
            NSTY = NTOT2 (NQ-1)
            DO I = 1, NPTS(NQ)
              IF (      DATA(NSTX+I).NE.BADPIX_VAL
     &            .AND. STACK(IDAT2+NSTY+I).NE.BADPIX_VAL) THEN
                DATA(NSTX+I) = STACK(IDAT2+NSTY+I) - DATA(NSTX+I)
              ELSE
                DATA(NSTX+I) = BADPIX_VAL
              END IF
            END DO

          ELSE
            IFAIL=17
            PRINT *,'Problems in quadrant ',NQ
          END IF

        END IF
      END DO

      CALL XY
      CALL POP

      RETURN
      END


