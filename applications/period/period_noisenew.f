
C===========================================================================

      SUBROUTINE PERIOD_NOISENEW(IPARRAY, NUMROWS, NUMCOLS, ISET, IDUM,
     :                           DSDEV, DMEAN, RANDOM, ESDEV, EMEAN,
     :                           YERROR, OPARRAY)

C===========================================================================
C Generates a new dataset with the same mean and standard deviation as
C the old, for NOISE.
C Both arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, ISET, IDUM, I
      DOUBLE PRECISION IPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION OPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION RANDOM
      DOUBLE PRECISION DMEAN, EMEAN
      DOUBLE PRECISION DSDEV, ESDEV
      DOUBLE PRECISION PERIOD_GASDEV
      LOGICAL YERROR


C-----------------------------------------------------------------------------
C Generate a new dataset with the same mean and standard deviation as
C the old.
C-----------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         OPARRAY(I, 1) = IPARRAY(I, 1)
         IF ( MOD(ISET,2).NE.0 ) THEN
            ISET = 2
         ELSE
            ISET = 1
         END IF
         RANDOM = PERIOD_GASDEV(ISET, IDUM)
         OPARRAY(I, 2) = (RANDOM*DSDEV) + DMEAN
         IF ( YERROR ) THEN
            IF ( MOD(ISET,2).NE.0 ) THEN
               ISET = 2
            ELSE
               ISET = 1
            END IF
            RANDOM = PERIOD_GASDEV(ISET, IDUM)
            OPARRAY(I, 3) = (RANDOM*ESDEV) + EMEAN
         END IF
  10  CONTINUE

      RETURN
      END
