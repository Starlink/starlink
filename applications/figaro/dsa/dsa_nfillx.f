C+
C                          D S A _ N F I L L x
C
C  Routine name:
C     DSA_NFILLx
C
C  Function:
C     Fills an array of a specific type with the numbers 1..N
C
C  Description:
C     DSA_NFILLx, where x is F,D,I,B,U or S is a set of routines each
C     of which fills an array of the appropriate type with the numbers
C     1..N
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_NFILLx (NELM,ARRAY)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM      (Integer,ref) The number of elements in the array.
C     (<) ARRAY     (Array of appropriate type,ref) The array to be filled.
C
C  External subroutines / functions used:  None.
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  History:
C     7th July 1987  Original version.  KS / AAO.
C     25th Apr 1989  Support for USHORT type added.  KS / AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_NFILLF (NELM,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL ARRAY(NELM)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=I
      END DO
C
      END
C
      SUBROUTINE DSA_NFILLD (NELM,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      DOUBLE PRECISION ARRAY(NELM)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=I
      END DO
C
      END
C
      SUBROUTINE DSA_NFILLI (NELM,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER ARRAY(NELM)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=I
      END DO
C
      END
C
      SUBROUTINE DSA_NFILLS (NELM,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER*2 ARRAY(NELM)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=I
      END DO
C
      END
C
      SUBROUTINE DSA_NFILLB (NELM,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      BYTE ARRAY(NELM)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=I
      END DO
C
      END
C
      SUBROUTINE DSA_NFILLU (NELM,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER*2 ARRAY(NELM)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=I
      END DO
C
      END
