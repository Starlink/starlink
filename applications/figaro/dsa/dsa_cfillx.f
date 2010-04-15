C+
C                          D S A _ C F I L L x
C
C  Routine name:
C     DSA_CFILLx
C
C  Function:
C     Fills an array of a specific type with a constant value
C
C  Description:
C     DSA_CFILLx, where x is F,D,I,B,U or S is a set of routines each
C     of which fills an array of the appropriate type with a constant value.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CFILLx (NELM,VALUE,ARRAY)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM      (Integer,ref) The number of elements in the array.
C     (>) VALUE     (Scalar of appropriate type,ref) The constant value to
C                   be used.
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
C     26th Aug 1988  Original version.  KS / AAO.
C     24th Apr 1989  DSA_CFILLU added.  KS / AAO.
C     21st Aug 1992  Automatic portability modifications
C                    ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992  "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_CFILLF (NELM,VALUE,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL ARRAY(NELM), VALUE
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_CFILLD (NELM,VALUE,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      DOUBLE PRECISION ARRAY(NELM), VALUE
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_CFILLI (NELM,VALUE,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER ARRAY(NELM), VALUE
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_CFILLS (NELM,VALUE,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER*2 ARRAY(NELM), VALUE
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_CFILLB (NELM,VALUE,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      BYTE ARRAY(NELM), VALUE
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=VALUE
      END DO
C
      END
C
      SUBROUTINE DSA_CFILLU (NELM,VALUE,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      INTEGER*2 ARRAY(NELM), VALUE
C
C     Local variables
C
      INTEGER I
C
      DO I=1,NELM
         ARRAY(I)=VALUE
      END DO
C
      END
