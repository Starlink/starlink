      BYTE FUNCTION str_LOWER(C)

*+
*
*   Name:
*      BYTE FUNCTION str_LOWER
*
*   Description:
*      Give lower case equivalent of character.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      03-JAN-82
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      Assume SWT character system and ASCII collating sequence.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE BIGA   ! ASCII "A"
      BYTE BIGZ   ! ASCII "Z"

      PARAMETER (BIGA=65, BIGZ=90)

*   Local constants:
      BYTE K   ! offset

      PARAMETER (K=32)

*   Import:
      BYTE C         ! character to be converted

      IF (C.GE.BIGA .AND. C.LE.BIGZ) THEN
         str_LOWER = C + K
      ELSE
         str_LOWER = C
      END IF

      END
