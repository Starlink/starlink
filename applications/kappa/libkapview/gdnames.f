      SUBROUTINE GDNAMES( STATUS )
*+
*  Name:
*     GDNAMES

*  Purpose:
*     Shows which graphics devices are available.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GDNAMES( STATUS )

*  Usage:
*     gdnames

*  Arguments :
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine displays a list of the graphics devices available and
*     the names which identify them.  Each name is accompanied by a
*     brief descriptive comment.
     
*  Algorithm:
*     A call is made to the PGPLOT routine PGLDEV and a suitable
*     service routine is provided.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TDCA: Tim Ash (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-MAY-1989 (RFWS):
*        Original version (RFWS).
*     1990 Mar 31 (MJC):
*        Renamed from SHODEV to GDNAMES for consistency (MJC).
*     1999 Jul 19 (TDCA):
*        Replaced call to SGS_WNAME with call to PGLDEV.

*  Bugs:
*     {note_any_bugs_here}

*-
*    Type definitions:
      IMPLICIT NONE              ! No implicit typing

*    Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*    Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Display a header for the list of devices.
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_OUT( 'HEADING', 'Graphics devices available:', STATUS )
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
   
*  Call PGLDEV to display the list of devices.
      CALL PGLDEV

*  Put a blank line at the end of the list.
      CALL MSG_OUT( 'BLANK', ' ', STATUS )

      END
