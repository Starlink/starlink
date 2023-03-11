      SUBROUTINE GRP1_IDELE( SLOT, STATUS )
*+
*  Name:
*     GRP1_IDELE

*  Purpose:
*     Delete a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_IDELE( SLOT, STATUS )

*  Description:
*     It is assumed that the supplied GRP slot is currently in
*     use. All mapped array associated with the group are unmapped. The
*     slot number is flagged as no longer being in use.
*
*     Note, this routine executes even if STATUS is bad on entry.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The GRP slot number of the group to be deleted.
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
*        CMN_CHK( GRP__MAXG ) = INTEGER (Read)
*           The GRP identifier issued for each slot.
*        CMN_FLPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped FILES array of each group.
*        CMN_FLSIZ( GRP__MAXG ) = INTEGER (Read and Write)
*           The current size of the FILES array.
*        CMN_GSIZE( GRP__MAXG ) = INTEGER (Read)
*           The index of the last entry in each group.
*        CMN_INPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped FILE_INDEX array of each group.
*        CMN_LVPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped LEVEL array of each group.
*        CMN_MGPNT( GRP__MAXG ) = INTEGER (Read and Write)
*           Pointers to the mapped MOD_GROUP array of each group.
*        CMN_MIPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped MOD_INDEX array of each group.
*        CMN_NMPNT( GRP__MAXG ) = INTEGER (Write)
*           Pointers to the mapped NAMES array of each group.
*        CMN_USED( GRP__MAXG ) = LOGICAL (Read and Write)
*           True if the corresponding group slot is in use.

*  Arguments Given and Returned:
      INTEGER SLOT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*.

*  Start a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  If the FILES array was created (see routine GRP1_PTIND)...
      IF( CMN_FLSIZ( SLOT ) .GT. 0 ) THEN

*  Unmap the array.
         CALL PSX_FREE( CMN_FLPNT( SLOT ), STATUS )

*  Set the current size to zero.
         CMN_FLSIZ( SLOT ) = 0

      END IF

*  Unmap the five remaining temporary arrays.
      CALL PSX_FREE( CMN_NMPNT( SLOT ), STATUS )
      CALL PSX_FREE( CMN_MGPNT( SLOT ), STATUS )
      CALL PSX_FREE( CMN_MIPNT( SLOT ), STATUS )
      CALL PSX_FREE( CMN_LVPNT( SLOT ), STATUS )
      CALL PSX_FREE( CMN_INPNT( SLOT ), STATUS )

*  Indicate that the group is no longer in use.
      CMN_USED( SLOT ) = .FALSE.

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
