
C===========================================================================

      SUBROUTINE PERIOD_SIGINFOLT(IPARRAY, NUMROWS, NUMCOLS, NCOUNT,
     :                             STAT, NSTATS, SAMPLE, WORK2,
     :                             NUMWRK)

C===========================================================================
C Determines significance information for PERIOD; tests for LT/LE.
C All arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, NCOUNT, NSTATS, SAMPLE, NUMWRK, I
      DOUBLE PRECISION IPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION STAT(NSTATS), WORK2(NUMWRK)

C---------------------------------------------------------------------------
C Determine significance information.
C---------------------------------------------------------------------------

      DO 10 I = 1, NCOUNT
         IF ( WORK2(I).LT.STAT(SAMPLE) ) STAT(SAMPLE) = WORK2(I)
         IF ( SAMPLE.GT.1 ) THEN
            IF ( WORK2(I).LE.IPARRAY(I, 2) )
     :           IPARRAY(I, 3) = IPARRAY(I, 3) + 1.0D0
         END IF
   10 CONTINUE

      RETURN
      END
