      SUBROUTINE ERROUT( STR, STATUS )
*+
*  Name:
*     SUBROUTINE ERROUT

*  Description:
*     Add a string to the current error message and finish it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ERROUT( STR, STATUS )

*  Arguments:
*     STR = BYTE( * ) (Given)
*        The message string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-81 (JRG):
*       AT4 version.
*     20-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     23-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Conversion to SGP/16 style.
*     24-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMERR'

*  Local Constants:
      INTEGER ARB        ! Arbitrary string length.
      INTEGER ERR        ! Error status.
      INTEGER MAXLINE    ! Maximum length of text string.
      PARAMETER ( ARB = 100, ERR = -3, MAXLINE = 400 )

*  Arguments Given:
      BYTE STR( ARB )    ! String to be written.

*  Status:
      INTEGER STATUS     ! Global status.

*  External References:
      INTEGER STR_LEN    ! String length.

*  Local Variables:
      INTEGER ISTAT      ! Status from PRTLIN.
*.

      IF ( STR_LEN( STR ) .GT. 0 ) THEN
         CALL STR_WRITS( '%s.\\', STR, MAXLINE, EMESS, EPOS )
         ISTAT = SAI__OK
         CALL PRTLIN( EMESS, ISTAT )

         IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = ERR
         END IF

         EPOS = 0

         CALL STR_WCONT( '%p!  \\', MAXLINE, EMESS, EPOS )

      ELSE
         IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = ERR
         END IF
      END IF

      END
