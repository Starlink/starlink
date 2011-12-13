      SUBROUTINE GNS_1GWMOV ( WNAME, NAMLEN, STATUS )

*+
*  Name:
*     GNS_1GWMOV

*  Purpose:
*     Open a GWM window with an overlay plane

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GNS_1GWMOV( WNAME, NAMLEN, STATUS )

*  Description:
*     Open a GWM window with an overlay plane. If a GWM window of the
*     given name does not exist then a new window is created with this
*     name with an overlay plane. The other window characteristics are
*     taken as the defualt.

*  Arguments:
*     WNAME = CHARACTER*(*) (Given)
*        Window name.
*     NAMLEN = INTEGER (Given)
*        Length of window name string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}

*  History:
*     14-AUG-1992 (NE):
*        Original version.
*     {enter_changes_here}
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER * ( * )  WNAME
      INTEGER NAMLEN

*  Status:
      INTEGER STATUS

*  Local Variables:
      LOGICAL THERE
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Establish the default client server connection
      CALL GWM_OPEN( ' ', .TRUE., STATUS )

*   See if a window of that name already exists
      CALL GWM_EXIST( WNAME(:NAMLEN), THERE, STATUS )

*   If it doesn't exist then create a window with an overlay
      IF ( .NOT. THERE ) THEN
         CALL GWM_WSETL( 'OVERLAY', .TRUE., STATUS )
         CALL GWM_CRWIN( WNAME(:NAMLEN), STATUS )
      ENDIF

*   Close the client server connection
      CALL GWM_CLOSE( STATUS )

      END

