************************************************************************

      SUBROUTINE OUTMEN( IMGDIS )

*+
*  Name :
*     OUTMEN
*
*  Purpose :
*     This informs which buttons correspond to the screen menu.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL OUTMEN
*
*  Description :
*     This informs which buttons correspond to the screen menu.
*
*  Arguments:
*     IMGDIS = LOGICAL (Given)
*        Whether display is an image display with a mouse.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-AUG-1990 (NE):
*        Original version.
*     5-FEB-1992 (NE):
*        Initialise local status value
*     8-NOV-1996 (PWD):
*        Added IMGDIS argument.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      LOGICAL IMGDIS

*  Local Variables :
      INTEGER STATUS
*.

*   Initialise status
      STATUS = SAI__OK

*   Print out general heading
      CALL MSG_OUT( ' ', 'Select operation according to screen menu',
     :              STATUS )

*   If IMGDIS then assume the device has a mouse
      IF ( IMGDIS ) THEN
         CALL MSG_OUT( ' ',
     :      'Left hand box  - Press left hand mouse button', STATUS )
         CALL MSG_OUT( ' ',
     :      'Centre box     - Press centre mouse button', STATUS )
         CALL MSG_OUT( ' ',
     :      'Right hand box - Press right hand mouse button', STATUS )

*   Otherwise assume it is running from on a terminal
      ELSE
         CALL MSG_OUT( ' ',
     :      'Left hand box  - Press number 1 on keyboard', STATUS )
         CALL MSG_OUT( ' ',
     :      'Centre box     - Press number 2 on keyboard', STATUS )
         CALL MSG_OUT( ' ',
     :      'Right hand box - Press number 0 on keyboard', STATUS )
      ENDIF

      END

* $Id$
