      LOGICAL FUNCTION STR_SIMLR2( STR1, STR2 )
*+
*  Name:
*     LOGICAL FUNCTION STR_SIMLR2

*  Purpose:
*     Test SWT string equality without case distinction.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL STR_SIMLR2( STR1, STR2 )

*  Arguments:
*     STR1 = BYTE( * ) (Given)
*        First string.
*     STR2 = BYTE( * ) (Given)
*        Second string.

*  Method:
*     Compare two strings until they differ. Ignore case distinctions.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     25-AUG-81 (JRG):
*       AT4 version.
*     31-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     14-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Final conversion to SGP/16 style.
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*       Moved function to own source file.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INTEGER ARB      ! Arbitrary string length.
      PARAMETER ( ARB = 100 )

*  Arguments Given:
      BYTE STR1( ARB ) ! First string.
      BYTE STR2( ARB ) ! Second string.

*  External References:
      BYTE str_LOWER   ! Convert alphabetic to lower case.

      INTEGER str_LEN  ! String length.

*  Local Variables:
      INTEGER I        ! Loop index.
      INTEGER NCHAR    ! Length of str1.
*.

      NCHAR = STR_LEN( STR1 )

      IF ( STR_LEN( STR2 ) .LT. NCHAR ) THEN
         NCHAR = STR_LEN( STR2 )
      END IF

      STR_SIMLR2 = .TRUE.

      DO I = 1, NCHAR
         IF ( STR_LOWER( STR1( I ) ) .NE. STR_LOWER( STR2( I ) ) ) THEN
            STR_SIMLR2 = .FALSE.
            GO TO 100
         END IF
      END DO

 100  CONTINUE

      END
