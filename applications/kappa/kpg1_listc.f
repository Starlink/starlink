      SUBROUTINE KPG1_LISTC( FD, EL, ARRAY, FILE, STATUS )
*+
*  Name:
*     KPG1_LISTC

*  Purpose:
*     Lists a character array to an ASCII file or reports it to the
*     user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_LISTC( FD, EL, ARRAY, FILE, STATUS )

*  Description:
*     This takes a character array and either writes the array to an
*     open ASCII file or uses the message system to report it to the
*     user.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor for the log file.  It is ignored if
*        FILE is false.
*     EL = INTEGER (Given)
*        The number of character strings in the array.
*     ARRAY( EL ) = CHARACTER * ( * ) (Given)
*        The array of character strings.
*     FILE = LOGICAL (Given)
*        If true the array is listed to the ASCII file otherwise the
*        array is reported to the user.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The ASCII file must be opened.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 28 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  FD,
     :  EL

      LOGICAL
     :  FILE

      CHARACTER * ( * )
     :  ARRAY( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  I                      ! Loop counter

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( FILE ) THEN

*       Write the character array to the ASCII file.

         DO  I = 1, EL
            CALL FIO_WRITE( FD, ARRAY( I ), STATUS )
         END DO

      ELSE

*       Report the character array to the user.

         DO  I = 1, EL
            CALL MSG_OUT( 'ARRAY', ARRAY( I ), STATUS )
         END DO
      END IF

      END
