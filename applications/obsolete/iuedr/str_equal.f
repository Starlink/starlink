      LOGICAL FUNCTION str_EQUAL( STR1, STR2 )
*+
*  Name:
*     LOGICAL FUNCTION str_EQUAL
*
*  Description:
*     SWT string equality test.
*
*  Authors:
*     Jack Giddings
*
*  History:
*     Jack Giddings      25-AUG-81
*        AT4 version.
*     Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*        Conversion to FORTRAN.
*     Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*        Final conversion to SGP/16 style.
*     Martin Clayton     18-OCT-94     IUEDR Vn. 3.1-7
*
*  Method:
*     Compare two strings until they differ.
*
*-

*  Implicit:
      IMPLICIT NONE

*  Global constants:
      INTEGER ARB            ! arbitrary string length
      PARAMETER (ARB=100)

*  Import:
      BYTE STR1(ARB)         ! first string
      BYTE STR2(ARB)         ! second string

*  External references:
      INTEGER str_LEN        ! string length

*  Local variables:
      INTEGER I              ! loop index
      INTEGER NCHAR          ! length of str1

      NCHAR = str_LEN(STR1)

      IF ( str_LEN(STR2) .NE. NCHAR ) THEN
         str_EQUAL = .FALSE.

      ELSE
         str_EQUAL = .TRUE.

         DO I = 1, NCHAR
            IF ( STR1(I) .NE. STR2(I) ) THEN
               str_EQUAL = .FALSE.
               GO TO 100
            END IF
         END DO
 100     CONTINUE
      END IF
      END
