      SUBROUTINE CNPAR( NAME, STATUS )
*+
*  Name:
*     SUBROUTINE CNPAR

*  Purpose:
*     Cancel parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CNPAR( NAME, STATUS )

*  Arguments:
*     NAME = BYTE( 16 ) (Given)
*        Name of the parameter to be cancelled.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     24-MAR-81 (JRG):
*       AT4 version.
*     29-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     01-JUN-89 (PCTR):
*       IUEDR Vn. 2.1
*       Conversion to SGP/16 style.
*     01-OCT-92 (DMILLS)
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     20-JAN-95 (MJC):
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
      INTEGER MAXNAME       ! Maximum length of name string.
      PARAMETER ( MAXNAME = 16 )

*  Arguments Given:
      BYTE NAME( MAXNAME )  ! Parameter name.

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      INTEGER NCHAR           ! Character count.

      CHARACTER*( MAXNAME ) LNAME  ! CHARACTER version of parameter name.
*.

*   Check inherited global status.
*      IF ( STATUS .NE. SAI__OK ) RETURN
      STATUS = SAI__OK

*   Generate CHARACTER version of parameter name.
      CALL GEN_STOC( NAME, MAXNAME, LNAME, NCHAR )

*   Cancel the parameter.
      CALL PAR_CANCL( LNAME, STATUS )

      END
