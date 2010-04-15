C+
      SUBROUTINE GEN_ICOR16(IN,N,OUT,OK)
C
C     G E N _ I C O R 1 6
C
C     Adds 65536 to each negative element of a real array.  This
C     odd operation corrects an array that was originally read as
C     signed 16 bit quantities but was intended to be unsigned
C     16 bit quantities.  It also checks if the array had values
C     such that it could have been such an array (ie all in the
C     range -32768..32767, and all integers).
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN     (Real array IN(N)) The input array
C                (Note that IN may be multiply dimensioned
C                in the calling program.  It is treated as
C                1D here for efficiency and generality.)
C     (>) N      (Integer) The number of elements of IN.
C     (<) OUT    (Real array OUT(N)) The result of the
C                addition. Note that IN and OUT may
C                be the same array.
C     (<) FLAG   (Logical) Set false if the array values were
C                appropriate for the operation being performed.
C
C     Subroutines / functions used - None
C
C                                         KS / AAO 6th Dec 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL OK
      INTEGER N
      REAL IN(N),OUT(N)
C
C     Local variable
C
      INTEGER I
      REAL INVAL
C
C     Perform operation
C
      OK=.TRUE.
      DO I=1,N
         INVAL=IN(I)
         IF (INVAL.LT.0.) THEN
            IF (INVAL.LT.-32768.) OK=.FALSE.
            OUT(I)=INVAL+65536.
         ELSE
            IF (INVAL.GT.32767.) OK=.FALSE.
            OUT(I)=INVAL
         END IF
         IF (OK) OK=INT(INVAL).EQ.INVAL
      END DO
C
      END
