      SUBROUTINE PICINFO( STATUS )
*+
*  Name:
*     PICINFO

*  Purpose:
*     Reports information about the current AGI DATA picture.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICINFO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine reports information about the current AGI DATA
*     picture. Specifically it reports the size in device pixels and
*     world coordinates. This information is of particular use with GWM
*     canvas widgets where native interactions are of at the device
*     pixel level and conversion to/from world coordinates is needed.

*  Usage:
*     PICINFO DEVICE

*  ADAM Parameters:
*     DEVICE = DEVICE (Write)
*        The name of the device.
*        [Current display device]

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1995 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER PICID             ! Database picture id
      REAL XP1, XP2, YP1, YP2   ! Pixel coordinates of DATA picture
      REAL XW1, XW2, YW1, YW2   ! World coordinates of DATA picture

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the device.
      CALL AGP_ASSOC( 'DEVICE', 'UPDATE', 'DATA', .FALSE., PICID,
     :                STATUS )

*  Get the size of the viewport in pixels.
      CALL PGQVP( 3, XP1, XP2, YP1, YP2 )

*  And the size in world coordinates.
      CALL PGQWIN( XW1, XW2, YW1, YW2 )

*  And write out the values.
      CALL MSG_OUT( ' ', ' ', STATUS )
      CALL MSG_OUT( ' ', '    Picture information:', STATUS )
      CALL MSG_OUT( ' ', '    ====================', STATUS )
      CALL MSG_OUT( ' ', ' ', STATUS )
      CALL MSG_OUT( ' ', '  Device pixels:', STATUS )
      CALL MSG_SETR( 'XP1', XP1 )
      CALL MSG_SETR( 'XP2', XP2 )
      CALL MSG_OUT( ' ', '    X: ^XP1 to ^XP2' , STATUS )
      CALL MSG_SETR( 'YP1', YP1 )
      CALL MSG_SETR( 'YP2', YP2 )
      CALL MSG_OUT( ' ', '    Y: ^YP1 to ^YP2' , STATUS )
      CALL MSG_OUT( ' ', ' ', STATUS )
      CALL MSG_OUT( ' ', '  World coordinates:', STATUS )
      CALL MSG_SETR( 'XW1', XW1 )
      CALL MSG_SETR( 'XW2', XW2 )
      CALL MSG_OUT( ' ', '    X: ^XW1 to ^XW2' , STATUS )
      CALL MSG_SETR( 'YW1', YW1 )
      CALL MSG_SETR( 'YW2', YW2 )
      CALL MSG_OUT( ' ', '    Y: ^YW1 to ^YW2' , STATUS )
      CALL MSG_OUT( ' ', ' ', STATUS )

*  Close the graphics device.
      CALL AGP_DEASS( 'DEVICE', .TRUE., STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PICINFO_ERR',
     :   'PICINFO: Error reading device information.',
     :   STATUS )
      END IF

      END
* $Id$
