C-----------------------------------------------------------------------

      SUBROUTINE QUOTSP (NQ,IERR)

C   Forms quotient of spectra in X and Y positions in stack

      IMPLICIT  NONE

C     Formal parameters

      INTEGER   NQ
      INTEGER   IERR

C     Common blocks

      INCLUDE   'FLAGCOMM'
      INCLUDE   'STACKCOMM'
      INCLUDE   'STAKPAR'

C     Local variables

      INTEGER    IZERO
      INTEGER    N1,    N2
      INTEGER    NQ1,   NQ2
      INTEGER    NP

      REAL       STACK(1)
      EQUIVALENCE (STACK(1),SCAN_HEADER(1))

C     Functions

      INTEGER    ICHKQ2
      INTEGER    NPTS2
      INTEGER    NTOT
      LOGICAL    DOQUAD

C  Ok, go...

C     Checks

      IF (ICHKQ2(NQ).NE.0) THEN
        IERR = 27
        RETURN
      END IF

      CALL QLIM (NQ, NQ1, NQ2)
      DO NQ = NQ1, NQ2
        IF (DOQUAD(NQ))   THEN
          IF (NPTS2(NQ).NE.NPTS(NQ))   THEN
            IERR = 17
            TYPE *,'Warning, # points different in quadrant', NQ
          ELSE
            IZERO = 0
            N1 = NTOT(NQ-1) + 1
            N2 = NTOT(NQ)
            DO NP = N1, N2
              IF (ABS(DATA(NP)).LE.1.E-10) THEN
                IZERO = IZERO+1
              END IF
            END DO

C           Check for zeros in denominator spectrum

            IF (IZERO.NE.0) THEN
              WRITE  (6,5000) IZERO
 5000         FORMAT (' Warning ,'I4
     &                ' points in denominator spectrum = 0.0')
            END IF

C           form quotient

            N1 = NTOT(NQ-1)+1
            N2 = NTOT(NQ)
            DO NP = N1, N2
              IF (STACK(IDAT2+NP).EQ.BADPIX_VAL
     &            .OR. DATA(NP).EQ.BADPIX_VAL) THEN
                STACK(IDAT2+NP) = BADPIX_VAL
              ELSE IF (ABS(DATA(NP)) .LE. 1.E-10) THEN
                STACK(IDAT2+NP) = BADPIX_VAL
              ELSE
                STACK(IDAT2+NP) = STACK(IDAT2+NP) / DATA(NP)
              END IF
            END DO

          END IF
        END IF
      END DO

      IF (IERR.EQ.0)   THEN
        CALL POP
      ELSE
        TYPE *,'Stack unchanged, X & Y may be damaged'
      END IF

      RETURN
      END

