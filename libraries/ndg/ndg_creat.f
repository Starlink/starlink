      SUBROUTINE NDG_CREAT( PARAM, IGRP0, IGRP, SIZE, FLAG, STATUS )
*+
*  Name:
*     NDG_CREAT

*  Purpose:
*     Obtain the names of a group of NDF to be created from the
*     environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_CREAT( PARAM, IGRP0, IGRP, SIZE, FLAG, STATUS )

*  Description:
*     A group expression is obtained from the environment using the
*     supplied parameter. The expression is parsed (using the
*     facilities of the GRP routine GRP_GROUP, see SUN/150) to produce
*     a list of explicit NDF names. These names are appended
*     to the group identified by IGRP. The user is re-prompted if an
*     error occurs while parsing the group expression. If IGRP has the
*     value GRP__NOID on entry, then a new group is created and IGRP is
*     returned holding the new group identifier.
*
*     If IGRP0 holds a valid group identifier on entry, then the group
*     identified by IGRP0 is used as the basis for any modification
*     element contained in the group expression obtained from the
*     environment. If IGRP0 holds an invalid identifier (such as
*     GRP__NOID) on entry then modification elements are included
*     literally in the output group.
*
*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The parameter with which to associate the group.
*     IGRP0 = INTEGER (Given)
*        The GRP identifier for the group to be used as the basis for
*        any modification elements. If a valid GRP identifier is
*        supplied, and if the supplied group expression contains a
*        modification element, then:
*
*        - the basis token (an asterisk) is replaced by the file basename
*        associated with the corresponding element of the basis group (the
*        "basis NDF").
*
*        - if no directory specification is included in the group expression,
*        the directory specification associated with the basis NDF is used.
*
*        - if no HDS component path is included in the group expression,
*        the HDS component path associated with the basis NDF (if any) is
*        used. Any required higher level HDS objects are created in the
*        output HDS file by copying the structure of the HDS file containing
*        the basis NDF. Thus if, the basis NDF is fred.a.b(2).c, and the
*        group expression is "*_a", then an HDS container file called
*        "fred_a.sdf" is created by copying fred.sdf and then deleting all
*        NDFs from fred_a.sdf (unless this has already been done while
*        creating a previous member of the returned group). Other non-NDF
*        components in fred_a.sdf are retained. This ensures that all necessary
*        structure exists in fred_a.sdf, so that the NDF fred_a.a.b(2).c
*        can be created when necessary.
*
*        The supplied group will often be created by NDG_ASSOC, but
*        groups created "by hand" using GRP directly can also be used
*        (i.e. without the supplemental groups created by NDG). In
*        this case, there are no defaults for directory path, file type,
*        or HDS component path, and the basis token ("*") in the group
*        expression represents the full basis file specification supplied
*        in IGRP0, not just the file basename.
*     IGRP = INTEGER (Given and Returned)
*        The GRP identifier for the group to which the supplied .sdf
*        files are to be appended.
*     SIZE = INTEGER (Returned)
*        The total number of file names in the returned group.
*     FLAG = LOGICAL (Returned)
*        If the group expression was terminated by the GRP "flag"
*        character, then FLAG is returned .TRUE. Otherwise it is
*        returned .FALSE. Returned .FALSE. if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If an error is reported the group is returned unaltered.
*     -  A null value (!) can be given for the parameter to indicate
*     that no more NDFs are to be specified. The corresponding error
*     is annulled before returning unless no NDFs have been added to
*     the group.
*     -  Explicit file types are included in all the elements of the returned
*     group. This is done because the name may be passed out to a script (eg
*     POLPACK:POLKA) which may change the value of NDF_FORMATS_OUT before
*     using the NDF name. If no file type is supplied in the group
*     expression, then the first file type listed in the current value of
*     the NDF_FORMATS_OUT environment variable (see SSN/20) is used. If
*     this is "*" then the file type is copied from the corresponding input
*     file if a modification element was used to specify the output file
*     name (if the NDF was not specified by a modification element, the
*     second file type in NDF_FORMATS_OUT is used).
*     -  If the last character in the supplied group expression is
*     a colon (:), a list of the NDFs represented by the group
*     expression (minus the colon) is displayed, but none are
*     actually added to the group. The user is then re-prompted for
*     a new group expression.
*     -  The returned group has no associated groups holding supplemental
*     information (unlike the group returned by NDG_ASSOC).

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     12-AUG-1992 (DSB):
*        Original version.
*     25-AUG-1999 (DSB):
*        Added facility to list names by terminating a group expression
*        with a colon (like NDG_ASSOC does).
*     9-SEP-1999 (DSB):
*        Tidied up the prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'PAR_ERR'          ! Parameter system error constants.

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER   IGRP0

*  Arguments Given and Returned:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER SIZE
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER GRPEXP*(GRP__SZGEX)! Group expression.
      CHARACTER NAME*(GRP__SZNAM)  ! Good NDF file name.
      INTEGER FIRST                ! Index of first non-blank character.
      INTEGER I                    ! Loop count.
      INTEGER IPAR                 ! SUBPAR parameter identifier.
      INTEGER ISTAT                ! Temporary status.
      INTEGER LAST                 ! Index of last non-blank character.
      INTEGER SIZE0                ! Initial size of the group.
      LOGICAL AGAIN                ! True if the user is to be re-prompted.
      LOGICAL LIST                 ! True if a listing of files is required.
*.

*  Ensure that a .FALSE. value is returned for FLAG if an error
*  has already occured.
      FLAG = .FALSE.
      SIZE = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that error reports are defferred.
      CALL ERR_MARK

*  Get the initial size of the group. If an invalid group identifier was
*  supplied, use a size of zero.
      CALL GRP_GRPSZ( IGRP, SIZE0, STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         SIZE0 = 0
      END IF

*  Get a group expression from the environment using the supplied
*  parameter.
 10   CONTINUE
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, GRPEXP, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If the first and last characters are single quotes, remove them.
      CALL CHR_FANDL( GRPEXP, FIRST, LAST )
      IF( GRPEXP( FIRST : FIRST ) .EQ. '''' .AND.
     :    GRPEXP( LAST : LAST ) .EQ. '''' ) THEN
         GRPEXP( FIRST : FIRST ) = ' '
         GRPEXP( LAST : LAST ) = ' '
      END IF

*  If the last character is a colon remove it and set a flag
*  indicating that the names are to be listed but not included in the
*  returned group.
      CALL CHR_FANDL( GRPEXP, FIRST, LAST )
      IF( GRPEXP( LAST : LAST ) .EQ. ':' ) THEN
         LIST = .TRUE.
         GRPEXP( LAST : LAST ) = ' '
      ELSE
         LIST = .FALSE.
      END IF

*  Remove leading spaces.
      CALL CHR_LDBLK( GRPEXP )

*  Expand the group expression into a list of NDF names and append
*  them to the end of the specified group.
      CALL NDG_CREXP( GRPEXP, IGRP0, IGRP, SIZE, FLAG, STATUS )

*  If an error has occurred while expanding the group expression, ask
*  the user to try again.
      IF( STATUS .NE. SAI__OK ) THEN
         AGAIN = .TRUE.

*  If all went well, but the group expression ended in a colon,
*  list the new names added to the group, and indicate that a new
*  group is required. Flush each report individually to avoid the
*  possibilioty of the EMS stack overflowing if many NDFs have
*  been specified.
      ELSE IF( LIST ) THEN

         ISTAT = SAI__ERROR
         CALL ERR_REP( ' ', ' ', ISTAT )
         CALL ERR_FLUSH( ISTAT )

         IF( SIZE0 .LT. SIZE ) THEN
            DO I = SIZE0 + 1, SIZE
               CALL GRP_GET( IGRP, I, 1, NAME, STATUS )
               CALL MSG_SETC( 'NAME', NAME )
               ISTAT = SAI__ERROR
               CALL ERR_REP( ' ','    ^NAME', ISTAT )
               CALL ERR_FLUSH( ISTAT )
            END DO
         ELSE
            ISTAT = SAI__ERROR
            CALL ERR_REP( ' ', 'No NDFs given.', ISTAT )
            CALL ERR_FLUSH( ISTAT )
         END IF

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', ' ', STATUS )
         AGAIN = .TRUE.

      ELSE
         AGAIN = .FALSE.

      END IF

*  If the user is to be re-prompted...
      IF( AGAIN ) THEN

*  Ask the user to give a new parameter value.
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_CREAT_ERR1',
     :                 '  Please give a new value for parameter %^P',
     :                 STATUS )
         CALL ERR_REP( ' ', ' ', STATUS )

*  Flush the errors so that the user sees them.
         CALL ERR_FLUSH( STATUS )

*  Cancel the parameter value.
         CALL SUBPAR_CANCL( IPAR, STATUS )

*  Annul any errors produced by the previous line.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*  Set the group back to its previous size.
         CALL NDG_SETSZ( IGRP, SIZE0, STATUS )
         SIZE = SIZE0

*  Go back for a re-prompt.
         GO TO 10

      END IF

*  Register the returned group with NDG so that its contents will be
*  appended to the end of any default history records written out by the
*  NDF library.
      IF( SIZE .GT. 0 ) CALL NDG_ADDGH( PARAM, IGRP, STATUS )

*  If a null parameter value was given, annul the error. If no NDFs
*  were added to the group re-report it with a more friendly report.
 999  CONTINUE

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         IF( SIZE .LE. SIZE0 ) THEN
            STATUS = PAR__NULL
            CALL MSG_SETC( 'P', PARAM )
            CALL ERR_REP( 'NDG_CREAT_ERR2', 'Null value given for '//
     :                    'parameter %^P.', STATUS )
         END IF

*  If the parameter request was aborted, annul the error and re-report
*  it with a more friendly message.
      ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_CREAT_ERR3', 'Aborted attempt to '//
     :                 'associate a group of data sets with '//
     :                 'parameter %^P.', STATUS )

*  If any other error occurred, add a context message.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_CREAT_ERR4', 'Unable to associate a '//
     :                 'group of data sets with parameter %^P', STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
