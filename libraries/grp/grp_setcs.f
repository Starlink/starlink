      SUBROUTINE GRP_SETCS( IGRP, SENSIT, STATUS )
*+
*  Name:
*     GRP_SETCS

*  Purpose:
*     Establish the case sensitivity of a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_SETCS( IGRP, SENSIT, STATUS )

*  Description:
*     When a group is created using GRP_NEW, it is initially case
*     sensitive. This routine can be called to make it case
*     insensitive, or to make it case sensitive again.
*
*     All names stored within a case sensitive group are used in the
*     same form as they were supplied by the environment or
*     application. If the group is case insensitive the upper case
*     equivalent is used when any reference is made to a name stored
*     within the group, and all comparisons between strings (such as
*     performed within routines GRP_INDEX and GRP_PURGE for instance)
*     related to that group are performed without reference to case.
*
*     Note, names are always stored in the form they are given.  Any
*     case conversion takes place when the names are read out of the
*     group, not when they are put into the group. This means that
*     groups can be changed at any time from being case insensitive to
*     being case sensitive (or vice-versa).

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group.
*     SENSIT = LOGICAL (Given)
*        If .TRUE. then the group is made case sensitive. If .FALSE.,
*        then the group is made case insensitive.
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

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_UPPER( GRP__MAXG ) = LOGICAL (Write)
*           If true, then all names in the group should be converted
*           to upper case before being used. Otherwise, they are left
*           as they are.

*  Arguments Given:
      INTEGER IGRP
      LOGICAL SENSIT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied GRP identifier is valid, and find the index
*  within the common arrays at which information describing the group is
*  stored.
      CALL GRP1_IMPID( IGRP, SLOT, STATUS )

*  If OK, store the supplied flag
      IF ( STATUS .EQ. SAI__OK ) CMN_UPPER( SLOT ) = .NOT. SENSIT

*  If an error has occurred, give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GRP_SETCS_ERR1',
     :    'GRP_SETCS: Error trying to set the case sensitivity flag '//
     :    'for a group', STATUS )
      END IF

      END
