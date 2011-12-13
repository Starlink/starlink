      SUBROUTINE CTG1_ASEXP( GRPEXP, VERB, IGRP1, IGRP2, SIZE, FLAG,
     :                       STATUS )
*+
*  Name:
*     CTG1_ASEXP

*  Purpose:
*     Store names of existing catalogues supplied as a group expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTG1_ASEXP( GRPEXP, VERB, IGRP1, IGRP2, SIZE, FLAG, STATUS )

*  Description:
*     The supplied group expression is parsed (using the facilities of
*     the GRP routine GRP_GROUP, see SUN/150) to produce a list of
*     explicit names for existing catalogues which are appended to the
*     end of the supplied group (a new group is created if none is
*     supplied). catalogue identifiers for particular members of the
*     group can be obtained using CTG_CATAS.
*
*     If any of the catalogues specified by the group expression cannot
*     be accessed, an error is reported and STATUS is returned equal to
*     CTG__NOFIL. If this happens strings holding the name of each bad
*     catalogue are appended to the group identified by IGRP1 (so long as
*     IGRP1 is not equal to GRP__NOID).

*  Arguments:
*     GRPEXP = CHARACTER * ( * ) (Given)
*        The group expression specifying the catalogue names to be stored
*        in the group.
*     VERB = LOGICAL (Given)
*        If TRUE then errors which occur whilst accessing supplied catalogues
*        are flushed so that the user can see them before re-prompting for
*        a new catalogue ("verbose" mode). Otherwise, they are annulled and
*        a general "Cannot access file xyz" message is displayed before
*        re-prompting.
*     IGRP1 = INTEGER (Given)
*        The identifier of a group to which the names of any
*        inaccessable catalogues will be appended. The group should already
*        have been created by a call to GRP_NEW, and should be deleted
*        when no longer needed by a call to GRP_DELET. If IGRP1 is
*        supplied equal to symbolic constant GRP__NOID, then no
*        information is stored describing the bad catalogues.
*     IGRP2 = INTEGER (Given and Returned)
*        The identifier of the group in which the catalogue names are to be
*        stored. A new group is created if the supplied value is GRP__NOID.
*        It should be deleted when no longer needed using GRP_DELET.
*     SIZE = INTEGER (Returned)
*        The total number of catalogue names in the returned group IGRP2.
*     FLAG = LOGICAL (Returned)
*        If the group expression was terminated by the GRP "flag
*        character", then FLAG is returned .TRUE. Otherwise it is
*        returned .FALSE. Returned .FALSE. if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2000 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1999 (DSB):
*        Original version.
*     10-APR-2000 (DSB):
*        Added argument VERB.
*     15-APR-2005 (PWD):
*        Parameterize backslash use for improved portability.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CTG_CONST'        ! CTG constants.
      INCLUDE 'CTG_ERR'          ! CTG error constants.
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Local Constants:
      CHARACTER ESC*1            ! Single backslash
*  Some compilers need '\\' to get '\', which isn't a problem as Fortran
*  will truncate the string '\\' to '\' on the occasions when that isn't
*  needed.
      PARAMETER( ESC = '\\' )

*  Arguments Given:
      CHARACTER GRPEXP*(*)
      INTEGER VERB
      INTEGER IGRP1

*  Arguments Given and Returned:
      INTEGER IGRP2

*  Arguments Returned:
      INTEGER SIZE
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ADDED              ! No. of names added to group
      INTEGER SIZE0              ! Size of group on entry to this routine
*.

*  Ensure a .FALSE. value for FLAG is returned if an error has already
*  occured.
      FLAG = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  If the supplied value of IGRP2 is GRP__NOID, create a new group to
*  hold the names of the catalogues. Set the group case insensitive if the
*  host file system is case insensitive.
      IF( IGRP2 .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'A list of existing catalogues', IGRP2, STATUS )
         SIZE0 = 0

*  If a group identifier was supplied, get the original size of the
*  group.
      ELSE
         CALL GRP_GRPSZ( IGRP2, SIZE0, STATUS )

      END IF

*  Set the group case insensitive if the host file system is case
*  insensitive.
      IF( CTG__UCASE ) CALL GRP_SETCS( IGRP2, .FALSE., STATUS )

*  Ensure the group uses "\" as its escape character.
      CALL GRP_SETCC( IGRP2, 'ESC', ESC, STATUS )

*  Append the names to the end of the group.
      IF( GRPEXP .NE. ' ' ) THEN
         CALL GRP_GRPEX( GRPEXP, GRP__NOID, IGRP2, SIZE, ADDED, FLAG,
     :                   STATUS )
      ELSE
         SIZE = SIZE0
         ADDED = 0
         FLAG = .FALSE.
      END IF

*  Check the names added to the group as a result of the above call.
*  Each name may potentially be expanded into a list of names (eg because
*  of wild-cards, etc). These are appended to the end of the group and the
*  original name deleted. An error is reported if no accessable catalogues can
*  be found matching any one of the supplied names.
      CALL CTG1_CATCH( VERB, IGRP2, SIZE0 + 1, IGRP1, STATUS )

*  Update the SIZE argument to take account of the new group
*  members produced as a result of the expansion of any wild cards. This
*  needs to happen even if an error has been reported, so do it in a new
*  error reporting context.
      CALL ERR_BEGIN( STATUS )
      CALL GRP_GRPSZ( IGRP2, SIZE, STATUS )
      CALL ERR_END( STATUS )

*  If an error has been reported (other than "some catalogues not accessible")
*  set the group back to its original size.
      IF( STATUS .NE. SAI__OK .AND. STATUS .NE. CTG__NOFIL ) THEN
         CALL ERR_BEGIN( STATUS )

         SIZE = SIZE0
         CALL CTG_SETSZ( IGRP2, SIZE0, STATUS )

         CALL ERR_END( STATUS )
      END IF

*  If an error occured give a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', GRPEXP )
         CALL ERR_REP( 'CTG1_ASEXP_ERR2', 'Error obtaining a group of'//
     :                 ' existing catalogues using group expression '//
     :                 '"^P"', STATUS )
      END IF

*  Release the current error context.
      CALL ERR_RLSE

      END
