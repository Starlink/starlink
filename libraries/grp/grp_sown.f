      SUBROUTINE GRP_SOWN( IGRP1, IGRP2, STATUS )
*+
*  Name:
*     GRP_SOWN

*  Purpose:
*     Establish one group as the owner of another group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_SOWN( IGRP1, IGRP2, STATUS )

*  Description:
*     This routine establishes one specified group as "owner" of
*     another specified group. An error is reported if the group is
*     already owned by another group. An error is also reported if the
*     "owner" group already owns a slave.
*
*     This routine may also be used to cancel an "owner-slave"
*     relationship, by specifying IGRP2 as GRP__NOID. The group
*     identified by IGRP1 then becomes a "free" group (i.e. has no
*     owner). An error is reported if IGRP1 identifies a group which is
*     already free.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        The identifier of the "slave" group which is to have an owner
*        established for it. An error is reported if an invalid
*        identifier is given.
*     IGRP2 = INTEGER (Given)
*        The identifier of the group which is to be established as
*        the owner of the group identified by IGRP1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*
*     -  There is a restriction on the use of owner-slave
*     relationships, namely that a slave cannot own its own owner,
*     either directly or indirectly. That is, if group A is owned by
*     group B, and group B is owned by group C, then group C cannot be
*     owned by either group B, or group A. An error is reported, if an
*     attempt is made to set up such a relationship.
*
*     -  When a group is deleted using GRP_DELET, all other groups in
*     the same owner/slave chain, whether higher up or lower down, are
*     also deleted. If a group is to be deleted without deleting all
*     other related groups, then the group must be established as a
*     "free" group (i.e. no owner) by calling this routine with IGRP2
*     set to GRP__NOID, before calling GRP_DELET.

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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_ERR'          ! GRP error constants.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_OWNER( GRP__MAXG ) = INTEGER (Read and Write)
*           The identifers of the owner of each group.
*        CMN_SLAVE( GRP__MAXG ) = INTEGER (Read and Write)
*           The identifers of the slave of each group.

*  Arguments Given:
      INTEGER IGRP1
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER OSLOT              ! Slot no. for current owner group.
      LOGICAL SLAVE              ! True if the owner group is already a
                                 ! slave of the specified group.
      INTEGER SLOT1              ! Index within common arrays at which
                                 ! properties of group IGRP1 are stored.
      INTEGER SLOT2              ! Index within common arrays at which
                                 ! properties of group IGRP2 are stored.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP1, SLOT1, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  See if IGRP2 has the value GRP__NOID. If so the slave group is to be
*  made free.
      IF( IGRP2 .EQ. GRP__NOID ) THEN

*  Report an error if group IGRP1 is already free.
         IF( CMN_OWNER( SLOT1 ) .EQ. GRP__NOID ) THEN
            STATUS = GRP__FREE
            CALL ERR_REP( 'GRP_SOWN_ERR1',
     :               'GRP_SOWN: Group to be made free is already free.',
     :                    STATUS )
            GO TO 999

*  Otherwise, cancel the ownership.
         ELSE
            CALL GRP1_ID2SL( CMN_OWNER( SLOT1 ), OSLOT )
            CMN_SLAVE( OSLOT ) = GRP__NOID
            CMN_OWNER( SLOT1 ) = GRP__NOID
         END IF

*  If IGRP2 does not have the value GRP__NOID, check that it is valid,
*  and find the index within the common arrays at which information
*  describing the group is stored.
      ELSE
         CALL GRP1_IMPID( IGRP2, SLOT2, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check that the owner group does not already have a slave. If it
*  does, report an error.
         IF( CMN_SLAVE( SLOT2 ) .NE. GRP__NOID ) THEN
            STATUS = GRP__OWNER
            CALL ERR_REP( 'GRP_SOWN_ERR2',
     :           'GRP_SOWN: Specified owner group already has a '//
     :           'slave group.', STATUS )
            GO TO 999
         END IF

*  Check that the slave group is not already owned. If it is, report an
*  error.
         IF( CMN_OWNER( SLOT1 ) .NE. GRP__NOID ) THEN
            STATUS = GRP__OWNED
            CALL ERR_REP( 'GRP_SOWN_ERR3',
     :           'GRP_SOWN: Group to be enslaved is already owned by '//
     :           'another group.', STATUS )
            GO TO 999

         END IF

*  Check that the specified owner group is not already a slave (either
*  directly or indirectly) of the group specified by IGRP1.
         CALL GRP1_SLAVE( SLOT2, IGRP1, SLAVE, STATUS )

* If it is, report an error.
         IF( SLAVE ) THEN
            STATUS = GRP__SLAVE
            CALL ERR_REP( 'GRP_SOWN_ERR4',
     :            'GRP_SOWN: The group specified by IGRP2 is already'//
     :            ' a slave of the group specified by IGRP1', STATUS )

*  Otherwise, establish the ownership of the slave group.
         ELSE
            CMN_OWNER( SLOT1 ) = IGRP2
            CMN_SLAVE( SLOT2 ) = IGRP1

         END IF

      END IF

*  If an error has occurred, give a context message.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_SOWN_ERR5',
     :               'GRP_SOWN: Unable to set up an owner for a group.',
     :                 STATUS )
      END IF

      END
