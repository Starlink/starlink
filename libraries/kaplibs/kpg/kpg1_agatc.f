      SUBROUTINE KPG1_AGATC( STATUS )
*+
*  Name:
*     KPG1_AGATC

*  Purpose:
*     Reports the character attributes of the current picture in the
*     graphics database.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AGATC( STATUS )

*  Description:
*     This routine inquires the name, comment, and label (if one is
*     present) of the current picture in the AGI graphics database
*     and reports their values.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 7 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER
     :  PICID                    ! Current-picture identifier

      LOGICAL                    ! True if:
     :  HASLAB                   ! The picture has a label

      CHARACTER
     :  LABEL * ( DAT__SZNAM ),  ! Picture label
     :  PNAME * ( DAT__SZNAM ),  ! Picture name
     :  PICCOM * 256             ! Picture comment

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Inquire the current picture.

      CALL AGI_ICURP( PICID, STATUS )

*    Get the name of the picture.

      CALL AGI_INAME( PNAME, STATUS )
      CALL MSG_SETC( 'PNAME', PNAME )

*    Get the comment of the picture.

      CALL AGI_ICOM( PICCOM, STATUS )
      CALL MSG_SETC( 'PICCOM', PICCOM )

*    Obtain the label associated with the picture, if a label exists.

      HASLAB = .FALSE.
      CALL AGI_ILAB( PICID, LABEL, STATUS )
      IF ( LABEL( 1:1 ) .NE. ' ' ) THEN
         CALL MSG_SETC( 'LABEL', LABEL )
         HASLAB = .TRUE.
      END IF

*    Report it to the user.

      IF ( HASLAB ) THEN
         CALL MSG_OUT( 'CURPIC', 'Current picture has name: ^PNAME, '/
     :     /'comment: ^PICCOM, label: ^LABEL.', STATUS )
      ELSE
         CALL MSG_OUT( 'CURPIC', 'Current picture has name: ^PNAME, '/
     :     /'comment: ^PICCOM.', STATUS )
      END IF

      END
