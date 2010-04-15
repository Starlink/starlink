
C===========================================================================

      SUBROUTINE PERIOD_NOISEOLD(IPARRAY, NUMROWS, NUMCOLS, ISET, IDUM,
     :                           NOISE, DSDEV, NSIG, RANDOM, ERRORS,
     :                           ESDEV, ESIG, REGULAR, ISDEV, RSIG,
     :                           YERROR, OPARRAY, IFAIL)

C===========================================================================
C Applies noise to existing data, sampling and errors, for NOISE.
C Both arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, ISET, IDUM, IFAIL, I
      DOUBLE PRECISION IPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION OPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION RANDOM
      DOUBLE PRECISION NSIG, ESIG, RSIG
      DOUBLE PRECISION DSDEV, ESDEV, ISDEV
      DOUBLE PRECISION PERIOD_GASDEV
      LOGICAL YERROR
      CHARACTER*1 NOISE, ERRORS, REGULAR


C-----------------------------------------------------------------------------
C Apply noise to the existing data, sampling and errors.
C-----------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS

         OPARRAY(I, 1) = IPARRAY(I, 1)
         OPARRAY(I, 2) = IPARRAY(I, 2)
         OPARRAY(I, 3) = IPARRAY(I, 3)

         IF ( NOISE.EQ.'Y' ) THEN
            IF ( MOD(ISET,2).NE.0 ) THEN
               ISET = 2
            ELSE
               ISET = 1
            END IF
            RANDOM = PERIOD_GASDEV(ISET, IDUM)
            OPARRAY(I, 2) = OPARRAY(I, 2)
     :                         + (DSDEV*NSIG*RANDOM)
         END IF
         IF ( ERRORS.EQ.'Y' ) THEN
            IF ( ESIG.EQ.0.0D0 ) THEN
               IF ( OPARRAY(I, 2).LT.0.0D0 ) THEN
                  CALL PERIOD_WRITEBELL()
                  WRITE (*, *) '** ERROR: Square root of' //
     :                         ' negative number in PERIOD_NOISE.'
                  IFAIL = 1
                  GO TO 700
               END IF
               OPARRAY(I, 3) = DSQRT(OPARRAY(I, 2))
            ELSE
               IF ( MOD(ISET,2).NE.0 ) THEN
                  ISET = 2
               ELSE
                  ISET = 1
               END IF
               RANDOM = PERIOD_GASDEV(ISET, IDUM)
               IF ( YERROR ) THEN
                  OPARRAY(I, 3) = OPARRAY(I, 3)
     :                            + (ESDEV*ESIG*RANDOM)
               ELSE
                  OPARRAY(I, 3) = DSDEV*ESIG*RANDOM
               END IF
            END IF
         END IF
         IF ( REGULAR.EQ.'Y' ) THEN
            IF ( MOD(ISET,2).NE.0 ) THEN
               ISET = 2
            ELSE
               ISET = 1
            END IF
            RANDOM = PERIOD_GASDEV(ISET, IDUM)
            OPARRAY(I, 1) = OPARRAY(I, 1)
     :                      + (ISDEV*RSIG*RANDOM)
         END IF

  10  CONTINUE

 700  CONTINUE

      RETURN
      END
