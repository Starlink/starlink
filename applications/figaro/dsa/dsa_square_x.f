C+
C                           D S A _ S Q U A R E _ x
C
C  Routine name:
C     DSA_SQUARE_{x}
C
C  Function:
C     Squares all the elements of an array.
C
C  Description:
C     DSA_SQUARE_{x} covers the routines DSA_SQUARE_F, DSA_SQUARE_I,
C     DSA_SQUARE_B, DSA_SQUARE_D, DSA_SQUARE_U and DSA_SQUARE_S, which
C     square all the elements of an array of type float, integer, byte,
C     double, unsigned short and short respectively.  In each case the
C     number of artihmetic errors that result from the squaring is
C     returned.
C
C  Language:
C     Starlink Fortran 77
C
C  Call:
C     CALL DSA_SQUARE_{x} (NELM,INPUT,OUTPUT,ERRORS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM          (Integer,ref) The number of elements in the
C                       array.
C     (>) INPUT         (Array of appropriate type, ref) The array to
C                       be squared.  The type varies with the routine.
C     (<) OUTPUT        (Array of appropriate type, ref) The resulting
C                       squared array.  The type varies with the
C                       routine.
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
      SUBROUTINE DSA_SQUARE_F (NELM,INPUT,OUTPUT,ERRORS)
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
      CALL VEC_MULR( .FALSE., NELM, INPUT, INPUT, OUTPUT,
     :   I, ERRORS, STATUS )
C
      END
C
      SUBROUTINE DSA_SQUARE_D (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
      INTEGER NELM, ERRORS
      DOUBLE PRECISION INPUT(NELM), OUTPUT(NELM)
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_MULD( .FALSE., NELM, INPUT, INPUT, OUTPUT,
     :   I, ERRORS, STATUS )
C
      END
C
      SUBROUTINE DSA_SQUARE_S (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
      INTEGER NELM, ERRORS
      INTEGER*2 INPUT(NELM), OUTPUT(NELM)
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_MULW( .FALSE., NELM, INPUT, INPUT, OUTPUT,
     :   I, ERRORS, STATUS )
C
      END
C
      SUBROUTINE DSA_SQUARE_I (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
      INTEGER NELM, ERRORS
      INTEGER INPUT(NELM), OUTPUT(NELM)
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_MULI( .FALSE., NELM, INPUT, INPUT, OUTPUT,
     :   I, ERRORS, STATUS )
C
      END
C
      SUBROUTINE DSA_SQUARE_B (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
      INTEGER NELM, ERRORS
      BYTE INPUT(NELM), OUTPUT(NELM)
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_MULB( .FALSE., NELM, INPUT, INPUT, OUTPUT,
     :   I, ERRORS, STATUS )
C
      END
C
      SUBROUTINE DSA_SQUARE_U (NELM,INPUT,OUTPUT,ERRORS)
C
      IMPLICIT NONE
C
      INTEGER NELM, ERRORS
      INTEGER*2 INPUT(NELM), OUTPUT(NELM)
C
      INTEGER I, STATUS
C
      STATUS = 0
      CALL VEC_MULUW( .FALSE., NELM, INPUT, INPUT, OUTPUT,
     :   I, ERRORS, STATUS )
C
      END
