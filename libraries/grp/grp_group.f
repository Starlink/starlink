      SUBROUTINE GRP_GROUP( PARAM, IGRP1, IGRP2, SIZE, ADDED, FLAG,
     :                      STATUS )
*+
*  Name:
*     GRP_GROUP

*  Purpose:
*     Append a list of names obtained from the environment
*     to a previously created group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP_GROUP( PARAM, IGRP1, IGRP2, SIZE, ADDED, FLAG, STATUS )

*  Description:
*     A group expression is obtained from the environment
*     using the supplied parameter name. The expression is parsed to
*     produce a list of names which are appended to the end of the
*     group identified by IGRP2. Note, no permanent association between
*     the parameter and the group exists. The parameter value can be
*     cancelled without effecting the contents of the group. If an error
*     occurs while parsing the group expression, the user is re-prompted
*     for a new group expression.
*
*     If the group expression contains any modification elements, then
*     the list of names added to the output group is based on the group
*     identified by IGRP1. If IGRP1 is invalid (equal to the symbolic
*     constant GRP__NOID for instance), then any elements with the
*     syntax of a modification element are stored in the output group
*     as a single literal name.
*
*     If the last character read from the group expression (or from a
*     text file if the last element of the group expression is an
*     indirection element) is equal to the current "flag" character for
*     the group IGRP2 (see routine GRP_SETCC), then argument FLAG is
*     returned set to .TRUE. Otherwise, it is returned set to .FALSE.
*     The calling application can use this flag for any purpose (eg it
*     may use it to indicate that the user wants to give more names).
*     Note, the flag character itself is not included in the returned
*     group.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The ADAM parameter with which to associate the group
*        expression. This may be of any type.
*     IGRP1 = INTEGER (Given)
*        A GRP identifier for the group to be used as the basis for any
*        modification elements which may be contained within the group
*        expression obtained from the environment. This can be set to
*        the symbolic constant GRP__NOID if modification elements are
*        to be treated as literal names.
*     IGRP2 = INTEGER (Given)
*        A GRP identifier for the group to which the new names are to
*        be appended.
*     SIZE = INTEGER (Returned)
*        The number of names in the returned group. It is returned equal
*        to 1 if an error status exists on entry. If an error occurs
*        during execution of this routine, then SIZE is returned equal
*        to the size of the group on entry (unless the group has zero
*        size on entry, in which case it is returned equal to 1).
*     ADDED = INTEGER (Returned)
*        The number of names added to the group as a result of the
*        current call to this routine.
*     FLAG = LOGICAL (Returned)
*         .TRUE. if the last character in the group expression is equal
*         to the current flag character for group IGRP2. Note, if this
*         is the case, then the flag character itself is not included
*         in the returned group. FLAG is returned .FALSE. if the last
*         character is not a flag character. Returned .FALSE. if an
*         error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*        -  A null value (!) can be given for the parameter to indicate
*        that no more names are to be specified. The corresponding error
*        is annulled before returning unless no names have been added to
*        the group.

*  Copyright:
*     Copyright (C) 1992, 1993, 1994 Science & Engineering Research Council.
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
*     10-SEP-1992 (DSB):
*        FLAG set to .FALSE. before checking inherited status. PAR__NULL
*        only re-reported if no names have been added to the group.
*     19-FEB-1993 (DSB):
*        SIZE set to 1 before checking inherited status. SIZE returned
*        equal to the size of the group on entry if any error occurs,
*        unless the group had zero size on entry, in which case it is
*        returned equal to 1.
*     31-AUG-1994 (DSB):
*        SIZE set to zero after the initial status check so that a null
*        parameter with an empty input group does not result in
*        SIZE > SIZE0 at the check at the end, and so causes the null
*        parameter status to be re-reported.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IGRP1
      INTEGER IGRP2

*  Arguments Returned:
      INTEGER SIZE
      INTEGER ADDED
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER GRPEXP*(GRP__SZGEX)! Group expression obtained from the
                                   ! environment.
      INTEGER IPAR               ! SUBPAR pointer to the parameter.
      INTEGER SIZE0              ! The original size of the group.
      INTEGER SLOT               ! Index within common arrays at which
                                 ! the group properties are stored.
*.

*  Ensure FLAG is returned .FALSE. and SIZE returned equal to 1 if an
*  error has already occured.
      FLAG = .FALSE.
      SIZE = 1

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the value zero for the current group size.
      SIZE = 0

*  Ensure that error reports are defferred.
      CALL ERR_MARK

*  Check that the supplied GRP identifier is valid.
      CALL GRP1_IMPID( IGRP2, SLOT, STATUS )

*  Save the current size of the group. Note, this routine can't directly
*  access the common blocks because it is an ADAM routine and may not be
*  part of the sharable library in which the common blocks are defined.
      SIZE0 = 1
      CALL GRP_GRPSZ( IGRP2, SIZE0 , STATUS )

*  Get a group expression from the environment using the supplied
*  parameter.
 10   CONTINUE
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, GRPEXP, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Expand the group expression into a list of literal names and append
*  them to the end of the specified group.
      CALL GRP_GRPEX( GRPEXP, IGRP1, IGRP2, SIZE, ADDED, FLAG, STATUS )

*  If an error has occurred while expanding the group expression...
      IF( STATUS .NE. SAI__OK ) THEN

*  ... add contextual information.
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'GRP_GROUP_ERR1',
     : 'GRP_GROUP: Unable to associate a group of names with'//
     : ' parameter %^P', STATUS )

*  Flush the error stack.
         CALL ERR_FLUSH( STATUS )

*  Cancel the parameter value.
         CALL SUBPAR_CANCL( IPAR, STATUS )

*  Annul any errors produced by the previous line.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*  Go back for a re-prompt.
         GO TO 10

      END IF

*  If a null parameter value was given, annul the error. Re-report it
*  with a more friendly report if no names were added to the group.
 999  CONTINUE

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         IF( SIZE .LE. SIZE0 ) THEN
            STATUS = PAR__NULL
            CALL MSG_SETC( 'P', PARAM )
            CALL ERR_REP( 'GRP_GROUP_ERR2',
     :          'GRP_GROUP: Null group expression given for the %^'//
     :          'P parameter.', STATUS )
         END IF

*  If the parameter request was aborted, annul the error and re-report
*  it with a more friendly message.
      ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'GRP_GROUP_ERR3',
     :    'GRP_GROUP: Aborted attempt to associate a group expression'//
     :    ' with the %^P parameter.', STATUS )

*  If any other error occurred, add a context message.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'GRP_GROUP_ERR4',
     : 'GRP_GROUP: Unable to associate a group of names with'//
     : ' parameter %^P', STATUS )
      END IF

*  If an error has occurred, set SIZE to the initial group size. If this
*  was zero, return 1 to avoid possible adjustable array dimension
*  errors.
      IF( STATUS .NE. SAI__OK ) SIZE = MAX( 1, SIZE0 )

*  Release the error stack.
      CALL ERR_RLSE

      END
