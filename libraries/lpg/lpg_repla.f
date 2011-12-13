      SUBROUTINE LPG_REPLA( REPLAC, STATUS )
*+
*  Name:
*     LPG_REPLA

*  Purpose:
*     Indicate if input NDFs can be replaced.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LPG_REPLA( REPLAC, STATUS )

*  Description:
*     Sets a flag indicating if LPG applications can use a single NDF as
*     both input and output. If so, a temporary NDF is used to store
*     the output. This NDF is then used to replace the existing input NDF once
*     the application has completed. If REPLAC is .FALSE. (the default), an
*     error is reported if an attempt is made to use a single NDF as both input
*     and output.

*  Arguments:
*     REPLAC = LOGICAL (Given)
*        If .TRUE., a single NDF can be used as both input and output
*        from an application. If .FALSE., an error will be reported if this
*        is attempted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAR-2004 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants.
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'LPG_CONST'        ! LPG private constants

*  Global Variables:
      INCLUDE 'LPG_COM'          ! LPG global variables
*        REPLACE = LOGICAL (Read)
*           Should the user be allowed to use the same input as both
*           input and output? If so, a temporary NDF will be used to
*           store the output while the application is running. Once the
*           application has finsished, the existing input NDF will be
*           replaced by a copy of the temporary NDF. If REPLACE is false
*           an error will be reported if an attempt is amde to use a
*           single NDF as both input and output.

*  Arguments Given:
      LOGICAL REPLAC

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      REPLACE = REPLAC

      END
