      SUBROUTINE GRP_DELET( IGRP, STATUS )
*+
*  Name:
*     GRP_DELET

*  Purpose:
*     Delete a group from the GRP system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_DELET( IGRP, STATUS )

*  Description:
*     This routine releases the identifier and internal resources used
*     by the specified group so that they can be used for another
*     group.  There is a limited number of groups available for use
*     within an application, so each group should be deleted when it is
*     no longer needed to avoid the possibility of reaching the limit.
*
*     Note, any parameter association used to establish the group is NOT
*     cancelled.
*
*     This routine also deletes any groups which are related to the
*     supplied group by means of "owner-slave" relationships
*     established by calls to GRP_SOWN. All groups in the same
*     "owner-slave" chain are deleted whether higher up or lower down
*     than the supplied group.
*
*     This routine attempts to execute even if STATUS is bad on entry,
*     although no further error report will be made if it subsequently
*     fails under these circumstances.

*  Arguments:
*     IGRP = INTEGER (Given and Returned)
*        A GRP identifier for the group. Returned equal to GRP__NOID.
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
*        CMN_USED( GRP__MAXG ) = LOGICAL (Read)
*           True if the corresponding slot in the common arrays is in
*           use.

*  Arguments Given:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER I                  ! Loop count.
      INTEGER ISLOT              ! The next group to be checked for
                                 ! subjugation to the group at the top
                                 ! of the owner-slave chain.
      INTEGER NDEL               ! No. of groups to be deleted.
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
      INTEGER SLOTS( GRP__MAXG ) ! Slots for the groups which are
                                 ! related to the supplied group.

*.

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  All related groups are deleted. Initialise the list of group to
*  be deleted to contain just the supplied group.
      NDEL = 1
      SLOTS( 1 ) = SLOT

*  Starting at the supplied group, follow the chain of owners adding
*  each owner to the list of groups to be deleted, until a free group
*  is found.
      CALL GRP1_ID2SL( CMN_OWNER( SLOT ), ISLOT )
      DO WHILE( ISLOT .NE. 0 )
         NDEL = NDEL + 1
         SLOTS( NDEL ) = ISLOT
         CALL GRP1_ID2SL( CMN_OWNER( ISLOT ), ISLOT )
      END DO

*  Starting at the supplied group, follow the chain of slaves adding
*  each slave to the list of groups to be deleted, until a group is
*  found which has no slaves.
      CALL GRP1_ID2SL( CMN_SLAVE( SLOT ), ISLOT )
      DO WHILE( ISLOT .NE. 0 )
         NDEL = NDEL + 1
         SLOTS( NDEL ) = ISLOT
         CALL GRP1_ID2SL( CMN_SLAVE( ISLOT ), ISLOT )
      END DO

*  Delete the groups.
      DO I = 1, NDEL
         CALL GRP1_IDELE( SLOTS( I ), STATUS )
      END DO

*  If an error occurred, add a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_DELET_ERR1',
     :            'GRP_DELET: Unable to delete a group', STATUS )
      END IF

*  Return an invalid identifier.
      IGRP = GRP__NOID

*  End the current error reporting environment.
      CALL ERR_END( STATUS )

      END
