      SUBROUTINE str_UCASE( STR )

*+
*
*   Name:
*      SUBROUTINE str_UCASE
*
*   Description:
*      Convert a string to upper case.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      10-SEP-81
*         AT4 version.
*      Paul Rees          31-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          14-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*      Martin Clayton     18-OCT-94     IUEDR Vn. 3.1-7
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global constants:
      INTEGER ARB         ! arbitrary string length
      PARAMETER (ARB=100)

*   Import/Export:
      BYTE STR(ARB)       ! string to be converted

*   External references:
      BYTE str_UPPER      ! convert SWT character to upper case

      INTEGER str_LEN     ! string length

*   Local variables:
      INTEGER I           ! loop index
      INTEGER NCHAR       ! character count

      NCHAR = str_LEN(STR)

      IF ( NCHAR .GT. 0 ) THEN
         DO I = 1, NCHAR
            STR(I) = str_UPPER(STR(I))
         END DO
      END IF
      END
