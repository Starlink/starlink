
      SUBROUTINE PERIOD_STRIP(STRING1,STRING2)

C=============================================================================
C
C Routine to remove the leading and following ' characters from a string
C returned by FITSIO.
C
C Written by Grant Privett 25th September 1995.
C
C=============================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C Declarations.
C-----------------------------------------------------------------------------
      CHARACTER*80 STRING1
      CHARACTER*40 STRING2
      INTEGER FOUND
      INTEGER FIRST
      INTEGER I
      INTEGER SECOND

*   Find the first and second ' character.
      FOUND=0
      STRING2=' '
      DO 10 I=1,40

*     Look at each character in turn.
        IF(STRING1(I:I).EQ.CHAR(39)) THEN
          FOUND=FOUND+1
          IF(FOUND.EQ.1) FIRST=I
          IF(FOUND.EQ.2) SECOND=I
        END IF

 10   CONTINUE

      IF(FOUND.GT.1) THEN
         DO 20 I=FIRST+1,SECOND-1
           STRING2(I-FIRST+1:I-FIRST+1)=STRING1(I:I)
 20      CONTINUE
         STRING2(SECOND-FIRST:SECOND-FIRST)=CHAR(0)
      END IF

      END
