      SUBROUTINE USR_SGS( STATUS )
*+
*  Name:
*     SUBROUTINE USR_SGS

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_SGS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Purpose:
*     Write a list of available SGS device names to output.

*  Method:
*     Use SGS_WLIST to perform function.

*  Authors:
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*   History:
*     20-DEC-88 (PCTR):
*       IUEDR Vn. 2.0
*     13-SEP-94 (MJC):
*       IUEDR Vn. 3.1-3
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Constants:
      INTEGER STDCMD   ! Command logical unit.
      PARAMETER ( STDCMD = 6 )

*  Status:
      INTEGER STATUS   ! Global status.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Display a heading.
      CALL line_WCONT( '%p Available Display devices are:\\' )
      CALL PRTBUF( STATUS )

*   List available devices.
      CALL SGS_WLIST( STDCMD )

      END
