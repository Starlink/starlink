      SUBROUTINE CCD1_CAT( FILE, STATUS )
*+
*  Name:
*     CCD1_CAT

*  Purpose:
*     Lists the contents of a file to the user.

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL CCD1_CAT( FILE, STATUS )

*  Description:
*     This routine outputs the contents of a text file using the
*     message system to deliver it.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the file to be listed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     4-JUN-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MSG_PAR'         ! Message system constants
      INCLUDE 'FIO_ERR'         ! FIO errors codes
      
*  Arguments Given:
      CHARACTER * ( * ) FILE
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( MSG__SZMSG) LINE ! Line buffer for reads
      INTEGER FD                ! File descriptor
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      

*  Now open the file.
      CALL FIO_OPEN( FILE, 'READ', 'LIST', 0, FD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN 

*  Write out its name.
         CALL MSG_SETC( 'FILE', FILE )
         CALL MSG_OUT( ' ', 'Contents of file: ^FILE', STATUS )
         CALL MSG_BLANK( STATUS )

*  Now loop until file is all read.
 1       CONTINUE
         IF ( STATUS .EQ. SAI__OK ) THEN 
            CALL FIO_READF( FD, LINE, STATUS )
            CALL MSG_OUT( ' ', LINE, STATUS )
            GO TO 1
         END IF
         IF ( STATUS .EQ. FIO__EOF ) THEN 
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL FIO_CLOSE( FD, STATUS )
      END IF
      END
