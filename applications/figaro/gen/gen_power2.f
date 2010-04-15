C+
      INTEGER FUNCTION GEN_POWER2 (N,NP2)
C
C     G E N _ P O W E R 2
C
C     Given a number, returns that number or the next highest
C     number that is a power of 2, and returns the power itself.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) N    (Integer) The number in question.
C     (<) NP2  (Integer) The next number that is a power of 2.
C
C     Returns -  (if called as a function)
C
C     (<) GEN_POWER2   (Integer) The power of 2.  ie NP2=2**GEN_POWER2
C                      Note: If N<1, GEN_POWER2 is returned as -1, and
C                      NP2 is returned as 0.
C
C     Common variables - None
C
C     Subroutines / functions called - None
C
C                                               KS / CIT 22nd Sept 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N,NP2
C
C     Local variables
C
      INTEGER I,IP2
C
      IF (N.LT.1) THEN
         GEN_POWER2=-1
         NP2=0
      ELSE
         I=0
         IP2=1
         DO WHILE (IP2.LT.N)
            I=I+1
            IP2=IP2+IP2
         END DO
         GEN_POWER2=I
         NP2=IP2
      END IF
C
      END
