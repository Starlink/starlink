      SUBROUTINE KPG1_SOLIN( STATUS )
*+
*  Name:
*     KPG1_SOLIN

*  Purpose:
*     Sets the GKS line type to solid for all polylines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SOLIN( STATUS )

*  Description:
*     The routine sets the linetype of the current workstation to
*     solid for all polylines by setting the aspect source flag
*     to individual.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     A GKS workstation should be open and active.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 12 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER GSTAT              ! Graphics status
      INTEGER LASF( 13 )         ! GKS list of aspect source flags

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Inquire the current aspect source flag setting.

      CALL GQASF( GSTAT, LASF )

*    Set the linetype aspect source flags to individual.

      LASF( 1 ) = 1
      CALL GSASF( LASF )

*    Want solid lines.

      CALL GSLN( 1 )

*    Determine whether or not there was an error in GKS.

      CALL GKS_GSTAT( STATUS )

      END
