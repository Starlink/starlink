      SUBROUTINE GRP1_TRUNC( SLOT, STATUS )
*+
*  Name:
*     GRP1_TRUNC

*  Purpose:
*     Truncate group arrays to remove unused space at the ends.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_TRUNC( SLOT, STATUS )

*  Description:
*     Each of the five arrays created by routine GRP1_GTSLT which hold
*     group information are truncated to the current used size of the
*     group. If the group currently has zero size, the arrays are
*     truncated to a size of 1.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The GRP slot number for the group to be truncated.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_NMPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped NAMES array of each group.
*        CMN_MGPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_GROUP array of each group.
*        CMN_MIPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_INDEX array of each group.
*        CMN_LVPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped LEVEL array of each group.
*        CMN_INPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped FILE_INDEX array of each group.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read)
*           The index of the last used element in each group.
*        CMN_SIZE( GRP__MAXG ) = INTEGER (Write)
*           The total size of the arrays associated with each group.

*  Arguments Given:
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER SIZE               ! Size to which the arrays will be
                                 ! truncated.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Limit the minimum size to 1.
      SIZE = MAX( 1, CMN_GSIZE( SLOT ) )

*  NAMES array...
      CALL PSX_REALLOC( SIZE*GRP__NBC*GRP__SZNAM,
     :                  CMN_NMPNT( SLOT ), STATUS )

*  MOD_GROUP array...
      CALL PSX_REALLOC( SIZE*GRP__NBI, CMN_MGPNT( SLOT ),
     :                  STATUS )

*  MOD_INDEX array...
      CALL PSX_REALLOC( SIZE*GRP__NBI, CMN_MIPNT( SLOT ),
     :                  STATUS )

*  LEVEL array...
      CALL PSX_REALLOC( SIZE*GRP__NBI, CMN_LVPNT( SLOT ),
     :                  STATUS )

*  FILE_INDEX array...
      CALL PSX_REALLOC( SIZE*GRP__NBI, CMN_INPNT( SLOT ),
     :                  STATUS )

*  Store the new size in common.
      CMN_SIZE( SLOT ) = SIZE

      END
