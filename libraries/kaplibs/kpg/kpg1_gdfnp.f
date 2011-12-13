      SUBROUTINE KPG1_GDFNP( NAME, X, Y, IPIC, STATUS )
*+
*  Name:
*     KPG1_GDFNP

*  Purpose:
*     Selects the highest picture of a given name that embraces a
*     position and has WCS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GDFNP( NAME, X, Y, IPIC, STATUS )

*  Description:
*     This routine returns an AGI identifier for the youngest picture
*     that has the specified name, embraces the given position and
*     lies within the bounds of the current picture. In this sense it is
*     like AGI_RCLP. However, it also applies an extra requirement that
*     the returned picture should have associated WCS. If no picture with
*     associated WCS can be found, then it abandons this extra requirement
*     and just returns the youngest picture encompassing the supplied
*     position.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the picture to be searched for in the graphics
*        database.
*     X = REAL (Given)
*        X position of test point
*     Y = REAL (Given)
*        Y position of test point
*     IPIC = INTEGER (Returned)
*        The picture identifier of the most-recent picture named NAME.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     17-SEP-2007 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! Data-system constants
      INCLUDE 'AGI_ERR'         ! AGI error constants

*  Arguments Given:
      CHARACTER * ( * ) NAME
      REAL X
      REAL Y

*  Arguments Returned:
      INTEGER IPIC              ! Returned picture identifier

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER MORLOC*(DAT__SZLOC)! HDS locator for MORE structure
      INTEGER IPICC             ! Current picture identifier
      INTEGER IPICT             ! Work picture identifier
      LOGICAL HASMOR            ! Does picture have a MORE component?
      LOGICAL HASWCS            ! Does picture have a WCS Plot?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get an identifier for the original current picture.
      CALL AGI_ICURP( IPICC, STATUS )

*  Get the last picture of the chosen name which encompasses the cursor
*  position. If found it becomes the current AGI picture.
      CALL AGI_RCLP( NAME, X, Y, IPIC, STATUS )

*  If a picture with the required name was found, see if it has a WCS Plot
*  in its MORE component.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL AGI_IMORE( IPIC, HASMOR, STATUS )
         IF( HASMOR ) THEN
            CALL AGI_MORE( IPIC, 'READ', MORLOC, STATUS )
            CALL DAT_THERE( MORLOC, 'AST_PLOT', HASWCS, STATUS )
            CALL DAT_ANNUL( MORLOC, STATUS )
         ELSE
            HASWCS = .FALSE.
         END IF

*  If it does not have a WCS component, we need to continue searching.
         DO WHILE( .NOT. HASWCS .AND. STATUS .EQ. SAI__OK )

*  Re-instate the original current picture.
            CALL AGI_SELP( IPICC, STATUS )

*  Recall the picture preceding the one found above that has the specified
*  name, embraces the given position and lies within the bounds of the
*  original current picture. This picture becomes the current picture.
            CALL AGI_RCPP( NAME, IPIC, X, Y, IPICT, STATUS )

*  Free the old picture identifier, and use the new one instead.
            CALL AGI_ANNUL( IPIC, STATUS )
            IPIC = IPICT

*  See if it has WCS information.
            CALL AGI_IMORE( IPICT, HASMOR, STATUS )
            IF( HASMOR ) THEN
               CALL AGI_MORE( IPICT, 'READ', MORLOC, STATUS )
               CALL DAT_THERE( MORLOC, 'AST_PLOT', HASWCS, STATUS )
               CALL DAT_ANNUL( MORLOC, STATUS )
            ELSE
               HASWCS = .FALSE.
            END IF
         END DO

*  If no picture with WCS could be found, annul the error and return the
*  youngest picture which meets the other requirements.
         IF( STATUS .EQ. AGI__NONAM ) THEN
            CALL ERR_ANNUL( STATUS )
            CALL AGI_SELP( IPICC, STATUS )
            CALL AGI_RCLP( NAME, X, Y, IPIC, STATUS )
         END IF
      END IF

      END
