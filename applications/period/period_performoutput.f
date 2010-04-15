
      SUBROUTINE PERIOD_PERFORMOUTPUT(YARRAY, NUMROWS, NUMCOLS,
     :                                YERROR, IUNIT)

C===========================================================================
C Output data to a disk file for OUTPUT.
C
C Essentially written by Vikram Singh Dhillon @LPO 27-January-1992,
C  as PERIOD_OUTPUT.
C
C Converted to Double Precision (KPD), August 2001
C Modified to incorporate dynamic memory allocation for major
C  data/work array(s) and/or use of such arrays (KPD), October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, IUNIT, I
      DOUBLE PRECISION YARRAY(NUMROWS,NUMCOLS)
      LOGICAL YERROR

C===========================================================================
C Output data to a disk file.
C===========================================================================

      DO 10 I = 1, NUMROWS
         IF ( YERROR ) THEN
            WRITE (IUNIT, *) YARRAY(I, 1), YARRAY(I, 2),
     :                       YARRAY(I, 3)
         ELSE
            WRITE (IUNIT, *) YARRAY(I, 1), YARRAY(I, 2)
         END IF
  10  CONTINUE

      RETURN
      END
