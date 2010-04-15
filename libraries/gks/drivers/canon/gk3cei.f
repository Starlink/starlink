      SUBROUTINE GK3CEI(I,C,NC)
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Encodes an integer
*
*  ARGUMENTS
*  ---------
*     INP I - Integer to be encoded
*     OUT C - Encoded integer
*     OUT NC - Number of characters in encoding
*
      INTEGER I, NC
      CHARACTER*(*) C
*
*
*--------------------------------------------------------------------
      INTEGER I1,I2

      NC = 1
      I1 = ABS(I)
      I2 = I1 / 268435456
      IF (I2.NE.0) THEN
        C(NC:NC) = CHAR(I2+64)
        NC = NC + 1
        I1 = I1 - I2 * 268435456
      END IF
      I2 = I1 / 4194304
      IF ( I2.NE.0 .OR. NC.NE.1 ) THEN
        C(NC:NC) = CHAR(I2+64)
        NC = NC + 1
        I1 = I1 - I2 * 4194304
      END IF
      I2 = I1 / 65536
      IF ( I2.NE.0 .OR. NC.NE.1 ) THEN
        C(NC:NC) = CHAR(I2+64)
        NC = NC + 1
        I1 = I1 - I2 * 65536
      END IF
      I2 = I1 / 1024
      IF ( I2.NE.0 .OR. NC.NE.1 ) THEN
        C(NC:NC) = CHAR(I2+64)
        NC = NC + 1
        I1 = I1 - I2 * 1024
      END IF
      I2 = I1 / 16
      IF ( I2.NE.0 .OR. NC.NE.1 ) THEN
        C(NC:NC) = CHAR(I2+64)
        NC = NC + 1
        I1 = I1 - I2 * 16
      END IF
      IF (I.GE.0) THEN
        C(NC:NC) = CHAR(I1+48)
      ELSE
        C(NC:NC) = CHAR(I1+32)
      END IF
      END
