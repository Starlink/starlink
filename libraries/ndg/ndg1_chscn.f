      SUBROUTINE NDG1_CHSCN( NAME, STATUS )
*+
*  Name:
*     NDG1_CHSCN

*  Purpose:
*     Check an HDS component name for standard form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_CHSCN( NAME, STATUS )

*  Description:
*     The routine checks that the name of an HDS component has a
*     standard form and reports an error if it does not. A standard
*     name must be no more than DAT__SZNAM characters long, must begin
*     with an alphabetic character and continue with alphanumeric
*     characters (including underscore) only.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name to be checked.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Test if the name is non-standard.
*     -  If so, then set a STATUS value and report an error.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-NOV-1989 (RFWS):
*        Original version.
*     15-FEB-1998 (DSB):
*        Brought into NDG from NDF.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) NAME

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_ISNAM          ! Whether a string is a standard name
      INTEGER CHR_LEN            ! Significant length of a string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the name has the standard form. Report an error if it does
*  not.
      IF ( ( CHR_LEN( NAME ) .GT. DAT__SZNAM ) .OR.
     :     ( .NOT. CHR_ISNAM( NAME ) ) ) THEN
         STATUS = NDF__NSHNM
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'NDG1_CHSCN_NS',
     :                 'Non-standard HDS component name ''^NAME'' ' //
     :                 'specified.', STATUS )
      END IF
       
      END
