C+
C                           D S A _ S Q R T _ x
C
C  Routine name:
C     DSA_SQRT_{x}
C
C  Function:
C     Takes the square root of all the elements of an array.
C
C  Description:
C     DSA_SQRT_{x} covers the routines DSA_SQRT_F, DSA_SQRT_I,
C     DSA_SQRT_B, DSA_SQRT_D, DSA_SQRT_U and DSA_SQRT_S, which take the
C     square root of all the elements of an array of type float,
C     integer, byte, double, unsigned short and short respectively.  In
C     each case the number of artihmetic errors that result from the
C     squaring is returned.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL DSA_SQRT_{x} (NELM,INPUT,OUTPUT,ERRORS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM          (Integer,ref) The number of elements in the
C                       array.
C     (>) INPUT         (Array of appropriate type, ref) The array to
C                       be rooted.  The type varies with the routine.
C     (<) OUTPUT        (Array of appropriate type, ref) The resulting
C                       array.  The type varies with the routine.
C     (<) ERRORS        (Integer,ref) The number of errors (mostly
C                       overflows) resulting from the application of the
C                       scale and zero values.
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C-
C  History:
C     27th June 1989  Original version.  KS / AAO.
C     31st Aug  1992  Call VEC_ and avoid LIB$.  HME / UoE, Starlink.
C+
      SUBROUTINE DSA_SQRT_F (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM, ERRORS
      REAL INPUT(NELM), OUTPUT(NELM)
C
C     Local variables
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_SQRTR( .FALSE., NELM, INPUT, OUTPUT, I, ERRORS, STATUS )
C
      END
C
      SUBROUTINE DSA_SQRT_D (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
      INTEGER NELM, ERRORS
      DOUBLE PRECISION INPUT(NELM), OUTPUT(NELM)
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_SQRTD( .FALSE., NELM, INPUT, OUTPUT, I, ERRORS, STATUS )
C
      END
C
      SUBROUTINE DSA_SQRT_S (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
      INTEGER NELM, ERRORS
      INTEGER*2 INPUT(NELM), OUTPUT(NELM)
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_SQRTW( .FALSE., NELM, INPUT, OUTPUT, I, ERRORS, STATUS )
C
      END
C
      SUBROUTINE DSA_SQRT_I (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
      INTEGER NELM, ERRORS
      INTEGER INPUT(NELM), OUTPUT(NELM)
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_SQRTI( .FALSE., NELM, INPUT, OUTPUT, I, ERRORS, STATUS )
C
      END
C
      SUBROUTINE DSA_SQRT_B (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
      INTEGER NELM, ERRORS
      BYTE INPUT(NELM), OUTPUT(NELM)
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_SQRTB( .FALSE., NELM, INPUT, OUTPUT, I, ERRORS, STATUS )
C
      END
C
      SUBROUTINE DSA_SQRT_U (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
      INTEGER NELM, ERRORS
      INTEGER*2 INPUT(NELM), OUTPUT(NELM)
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_SQRTUW( .FALSE., NELM, INPUT, OUTPUT, I, ERRORS, STATUS )
C
      END
