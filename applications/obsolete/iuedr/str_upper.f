      BYTE FUNCTION str_UPPER( C )

*+
*
*   Name:
*      BYTE FUNCTION str_UPPER
*
*   Description:
*      Give upper case equivalent of character.
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
*      Martin Clayton     18-OCT-94     IUEDR Vn. 3.1-7
*
*   Method:
*      Assume SWT character system and ASCII collating sequence.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      BYTE LETA   ! ASCII "a"
      BYTE LETZ   ! ASCII "z"
      PARAMETER (LETA=97, LETZ=122)

      BYTE K      ! offset
      PARAMETER (K=-32)

*   Import:
      BYTE C     ! character to be converted

      IF ( C.GE.LETA .AND. C.LE.LETZ ) THEN
         str_UPPER = C + K

      ELSE
         str_UPPER = C
      END IF
      END
