C+
      CHARACTER*(*) FUNCTION GEN_NTH (N)
C
C     G E N _ N T H
C
C     Returns the two character abbreviation (st,nd,rd,th)
C     appropriate for use with a specific number.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) N     (Integer) The number in question.
C
C     Returns -
C
C     (<) GEN_NTH  (Character) The appropriate abbreviation.
C                  This will always be a two character string.
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                        KS / AAO 24th Sept 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N
C
C     Local variables
C
      INTEGER TENS, UNITS
      CHARACTER*2 TABLE(0:3)
      DATA TABLE/'th','st','nd','rd'/
C
      TENS=MOD(ABS(N),100)
      UNITS=MOD(TENS,10)
      TENS=TENS-UNITS
      IF ((TENS.EQ.10).OR.(UNITS.GT.3)) THEN
         GEN_NTH='th'
      ELSE
         GEN_NTH=TABLE(UNITS)
      END IF
C
      END
