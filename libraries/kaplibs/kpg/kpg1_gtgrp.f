      SUBROUTINE KPG1_GTGRP( PARAM, IGRP, SIZE, STATUS )
*+
*  Name:
*     KPG1_GTGRP

*  Purpose:
*     Obtains a group of strings from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTGRP( PARAM, IGRP, SIZE, STATUS )

*  Description:
*     This routine obtains  group of strings using the specified
*     parameter, and returns them in a GRP group (see SUN/150).
*
*     The user specifies the strings by supplying a GRP group expression
*     for the parameter value. If the final character in the supplied
*     string is the group "flag character" (a minus sign by default), then
*     the user is re-prompted for another group expression, and the strings
*     specified by the second group expression are appended to the
*     returned group. This continues until a group expression is supplied
*     which does not end with the continuation character, or a null value is
*     supplied (which is annulled).
*
*     Normally, the "current value" for the parameter on exit would be
*     the final group expression. If more than one group expression was
*     supplied, then this will not represent the entire group. For this
*     reason, this routine concatenates all the group expressions supplied,
*     and stores the total group expression as the parameter value before
*     exiting. Since a new value is stored for the parameter, the parameter
*     should ne be given an access mode of READ in the interface file.
*
*     If a null value is supplied the first time the parameter value is
*     obtained, then the PAR__NULL status is returned, and SIZE is returned
*     as zero (a valid GRP identifier is still returned however). If a null
*     value is supplied on a subsequent access to the parameter, then the
*     PAR__NULL status is annulled, and the group is returned containing
*     any values obtained earlier.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     IGRP = INTEGER (Given and Returned)
*        The group to use. If this is GRP__NOID then a new group is
*        created with default control characters and its identifier is
*        returned. If the group already exists, its contents are discarded
*        before adding new strings.
*     SIZE = INTEGER (Returned)
*        The total size of the returned group. Returned equal to zero if an
*        error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     18-MAR-1998 (DSB):
*        Original version.
*     6-OCT-2009 (DSB):
*        Use SUBPAR_CURSAV to save the concatenated group expression rather
*        than SUBPAR_PUTNAME.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IGRP

*  Arguments Returned:
      INTEGER SIZE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXGEX              ! Max. length of total group expression
      PARAMETER( MXGEX = 1024 )

*  Local Variables:
      CHARACTER DC*1             ! Delimiter character for group expressions
      CHARACTER FC*1             ! Flag character for group expressions
      CHARACTER GRPEXP*(MXGEX)   ! The total group expression
      CHARACTER TEXT*(GRP__SZGEX)! A single group expression
      INTEGER ADDED              ! No. of strings added to group
      INTEGER IAT                ! No. of characters in GRPEXP so far
      INTEGER IPAR               ! SUBPAR pointer to the parameter
      INTEGER NEXP               ! No. of group expressions obtained
      INTEGER TLEN               ! Length of supplied group expression
      LOGICAL CFLAG              ! More strings remain to be given?
      LOGICAL VERB               ! Run in verbose mode?
*.

*  Initialise the pointer to the returned group size
      SIZE = 0

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a null identifier was supplied, create a new group with default
*  control characters.
      IF( IGRP .EQ. GRP__NOID) THEN
         CALL GRP_NEW( 'kAPPA group', IGRP, STATUS )

*  Otherwise, ensure the supplied group is empty.
      ELSE
         CALL GRP_SETSZ( IGRP, 0, STATUS )
      END IF

*  Get the demimiter and flag control characters for the group.
      CALL GRP_GETCC( IGRP, 'DELIMITER', DC, STATUS )
      CALL GRP_GETCC( IGRP, 'FLAG', FC, STATUS )

*  Initialise the total group expression given so far.
      GRPEXP = ' '
      IAT = 0

*  Get a SUBPAR pointer for the parameter.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )

*  Initialise the number of group expression obtained.
      NEXP = 0

*  Allow for continuation lines.
      CFLAG = .TRUE.
      DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the group of strings from the environment.
         CALL GRP_GROUP( PARAM, GRP__NOID, IGRP, SIZE, ADDED,
     :                   CFLAG, STATUS )

*  If succesful, increment the number of group expressions obtained, and get
*  the group expression supplied for the parameter as an uninterpreted text
*  string.
         IF( STATUS .EQ. SAI__OK ) THEN
            NEXP = NEXP + 1
            CALL SUBPAR_GETNAME( IPAR, TEXT, STATUS )

*  If not succesful, annul the error.
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )

*  Otherwise, ignore blank strings
            ELSE IF( TEXT .NE. ' ' ) THEN

*  Get the used length of the string.
               TLEN = CHR_LEN( TEXT )

*  If the group expression was flagged, replace the last non-blank character
*  with a delimiter character if it is a flag character.
               IF( CFLAG .AND. TEXT( TLEN : TLEN ) .EQ. FC ) THEN
                  TEXT( TLEN : TLEN ) = DC
               END IF

*  Find out how much of the text can be added to the total group
*  expression.
               TLEN = MIN( TLEN, MXGEX - IAT )

*  Append this part of the string to the total group expression given so far.
               IF( TLEN .GT. 0 ) THEN
                  CALL CHR_APPND( TEXT( : TLEN ), GRPEXP, IAT )
               END IF

            END IF

         END IF

*  Cancel the parameter association in order to get more strings
*  through the parameter, unless there are no more to obtain.
         IF ( CFLAG ) THEN
            CALL PAR_CANCL( PARAM, STATUS )
         END IF
      END DO

*  If a null parameter was supplied, annull the error unless only 1 group
*  expression was supplied.
      IF( STATUS .EQ. PAR__NULL .AND. NEXP .GT. 1 ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Report the total group expression if in verbose mode.
      CALL KPG1_VERB( VERB, 'KAPPA', STATUS )
      IF( VERB ) THEN
         CALL MSG_SETC( 'P', PARAM )

         IF( IAT .GT. 0 ) THEN
            CALL MSG_SETC( 'G', GRPEXP( : IAT ) )
            CALL MSG_OUT( 'KPG1_GTGRP_MSG1', 'The following group '//
     :                 'expression was obtained for parameter %^P: '//
     :                 '^G', STATUS )
         ELSE
            CALL MSG_OUT( 'KPG1_GTGRP_MSG2', 'A blank group '//
     :                    'expression was obtained for parameter %^P.',
     :                    STATUS )
         END IF

      END IF

*  If more than one group expression was supplied, store the total group
*  expression as the current value for the parameter. Do not do this is
*  the total group expression consists of a single flag character.
      IF( NEXP .GT. 1 .AND. IAT .GT. 0 ) THEN
         IF( GRPEXP( : IAT ) .NE. FC ) THEN
            CALL SUBPAR_CURSAV( IPAR, GRPEXP( : IAT ), STATUS )
         END IF
      END IF

*  Get the group size.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Register the returned group with NDG so that its contents will be
*  appended to the end of any default history records written out by the
*  NDF library.
      CALL NDG_ADDGH( PARAM, IGRP, STATUS )

*  If an error occurred, return a group size of zero.
      IF( STATUS .NE. SAI__OK ) SIZE = 0

      END
