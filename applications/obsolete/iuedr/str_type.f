      INTEGER FUNCTION str_TYPE(C)

*+
*
*   Name:
*      INTEGER FUNCTION str_TYPE
*
*   Description:
*      Determine type of SWT character.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      25-AUG-81
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      Check against Alphabetic and Numeric character sets.
*      If it is none of these return character index itself.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER DIGIT         ! DIGIT index
      INTEGER LETTER        ! LETTER index

      PARAMETER (DIGIT=2, LETTER=1)

*   Import:
      BYTE C                ! character to be typed

*   External references:
      INTEGER str_INDEX     ! index of character in string

      IF (str_INDEX('abcdefghijklmnopqrstuvwxyz\\', C).GT.0) THEN
         str_TYPE = LETTER
      ELSE IF (str_INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ\\', C).GT.0) THEN
         str_TYPE = LETTER
      ELSE IF (STR_INDEX('0123456789\\', C).GT.0) THEN
         str_TYPE = DIGIT
      ELSE
         str_TYPE = C
      END IF

      END
