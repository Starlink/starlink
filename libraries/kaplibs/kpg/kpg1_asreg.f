      SUBROUTINE KPG1_ASREG( STATUS )
*+
*  Name:
*     KPG1_ASREG

*  Purpose:
*     Registers all AST IntraMaps known by KAPPA.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASREG( STATUS )

*  Description:
*     This routine registers all AST IntraMaps known to KAPPA. It should
*     be called before any AST routine which may use an IntraMap (such
*     as a transformation routine, plotting routine, read/write routine,
*     etc.).

*  Arguments:
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1998 (DSB):
*        Original version.
*     12-SEP-2005 (TIMJ):
*        - CONTACT and AUTHOR now in KPG_PAR.
*        - Factor out into graphical and non-graphical mappings.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Non-graphical mappings
      CALL KPG1_ASRGN( STATUS )

*  Graphical mappings
      CALL KPG1_ASRGG( STATUS )

      END
