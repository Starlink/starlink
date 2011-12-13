      SUBROUTINE ARD_GROUP( PARAM, IGRP1, IGRP2, STATUS )
*+
*  Name:
*     ARD_GROUP

*  Purpose:
*     Obtain an ARD description from the environment

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_GROUP( PARAM, IGRP1, IGRP2, STATUS )

*  Description:
*     An ARD description is obtained from the environment using the
*     supplied parameter name and stored in a group identified by the
*     returned value of IGRP2. If the last character in the ARD
*     description is a minus sign ("-") then the parameter value is
*     then cancelled and further ARD descriptions are obtained and
*     appended to the returned group. This process continues until an
*     ARD description is supplied which does not end with a minus
*     sign, or a null value is supplied.
*
*     If a GRP identifier for an existing group is supplied for IGRP1
*     then the group will be used as the basis for any modification
*     elements contained within the ARD descriptions obtained from the
*     environment. No checks are made for modification elements if the
*     symbolic constant GRP__NOID is supplied for IGRP1.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     IGRP1 = INTEGER (Given)
*        GRP identifier for a group to be used as a basis for
*        modification elements.
*     IGRP2 = INTEGER (Returned)
*        GRP identifier for the created group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  No checks are made on the syntax of the ARD description.
*     -  The returned GRP identifier (IGRP2) should be deleted using
*     GRP_DELET when it is no longer needed.
*     -  If an error occurs either before or during this routine then
*     IGRP2 will be returned holding the symbolic value GRP__NOID
*     (defined in include file GRP_PAR).
*     -  An error is returned if the first value obtained for the
*     parameter is a null value.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     29-APR-1994 (DSB):
*        Original version.
*     7-SEP-1995 (DSB):
*        Local calculation of group size included to avoid a null value
*        on the first prompt not resulting in STATUS=PAR__NULL being returned.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IGRP1

*  Arguments Returned:
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  ADDED,                   ! No. of elements added to group.
     :  LSIZE,                   ! Local version of group size
     :  SIZE,                    ! Current size of group.
     :  TSTAT                    ! Temporary status value

      LOGICAL
     :  FLAG                     ! Was the group expression flagged?
*.

*  Ensure a null group is returned if an error condition exists on
*  entry.
      IGRP2 = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark the error stack, so that flushing error messages doesn't
*  disturb any pre-existing error stack contents.
      CALL ERR_MARK

*  Create a new empty group.
      CALL GRP_NEW( 'ARD DESCRIPTION', IGRP2, STATUS )

*  Modify the groups control characters so that ";" is used to delimit
*  elements, and no parenthesised "nests" are allowed.
      CALL GRP_SETCC( IGRP2, 'NUL,DEL,OPEN_N,CLOSE_N', '%;%%', STATUS )

*  Initialise the current size of the group to zero. The size of the
*  group is calculated locally rather than using the value returned by
*  GRP_GROUP because the value returned by GRP_GROUP is set to 1 if
*  an error occurs (such as a null value being supplied at the first
*  prompt) which causes the trap for null groups to fail in this module.
      LSIZE = 0

*  Loop round appending further elements to the group until a group
*  expression is obtained which is not flagged, or a null value is
*  obtained.
      FLAG = .TRUE.
      DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )
         CALL GRP_GROUP( PARAM, IGRP1, IGRP2, SIZE, ADDED, FLAG,
     :                   STATUS )

*  Update the group size.
         LSIZE = LSIZE + ADDED

*  If a null value was given, annull the error and set flag to .FALSE.
*  unless the group is currently empty.
         IF( STATUS .EQ. PAR__NULL .AND. LSIZE .GT. 0 ) THEN
            CALL ERR_ANNUL( STATUS )
            FLAG = .FALSE.
         END IF

*  If a new parameter value is to be obtained, cancel the current value.
         IF( FLAG ) CALL PAR_CANCL( PARAM, STATUS )

      END DO

*  If an error has occurred, delete the returned group.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP2, STATUS )

*  If an "abort" was requested, then annul any error messages and issue
*  an appropriate new one.
         IF ( STATUS .EQ. PAR__ABORT ) THEN
            TSTAT = STATUS
            CALL ERR_ANNUL( TSTAT )
            CALL MSG_SETC( 'PARAM', PARAM )
            CALL ERR_REP( 'ARD_GROUP_ERR1', 'Aborted attempt to '//
     :                    'associate an ARD description with the ' //
     :                    '''%^PARAM'' parameter.', STATUS )

*  If a "null" VALUE was specified, then annul any error messages and
*  issue an appropriate new one.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            TSTAT = STATUS
            CALL ERR_ANNUL( TSTAT )
            CALL MSG_SETC( 'PARAM', PARAM )
            CALL ERR_REP( 'ARD_GROUP_ERR2', 'Null ARD description '//
     :                    'specified for the ''%^PARAM'' parameter.',
     :                    STATUS )

*  For other errors, add context information.
         ELSE
            CALL MSG_SETC( 'PARAM', PARAM )
            CALL ERR_REP( 'ARD_GROUP_ERR3', 'ARD_GROUP: Error '//
     :                    'associating an ARD description with the '//
     :                    '''%^PARAM'' parameter.', STATUS )
         END IF

      END IF

*  Relase the error stack.
      CALL ERR_RLSE

      END
