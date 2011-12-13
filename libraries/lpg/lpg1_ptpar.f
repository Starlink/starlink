      SUBROUTINE LPG1_PTPAR( PARAM, IGRP, STATUS )
*+
*  Name:
*     LPG1_PTPAR

*  Purpose:
*     Set the current value of a parameter equal to the contents of a
*     group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LPG1_PTPAR( PARAM, IGRP, STATUS )

*  Description:
*     This routine sets the current value of a specified parameter equal
*     to a group expression which equates to the contents of a speified
*     group. Since a new value is stored for the parameter, the parameter
*     should not be given an access mode of READ in the interface file.
*
*     The group expresion is, either:
*        1) A list of group elements, delimited by the groups current
*        delimiter character
*
*     or, if the above string would be too long to use:
*
*        2) An indirection element refering to a new file created in the
*        users ADAM directory with a name of the form <param>_values.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     IGRP = INTEGER (Given)
*        The group to use.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2004 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
*        Original version.
*     12-OCT-2004 (TIMJ):
*        Use PSX_REMOVE rather than CTG1_RM
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MXEXP              ! Longest list of explicit file names
      PARAMETER ( MXEXP = 60 )

*  Local Variables:
      CHARACTER COMM*50          ! Comment for indirection file
      CHARACTER DC*1             ! Delimiter character for group expressions
      CHARACTER GRPEXP*(MXEXP)   ! The total group expression
      CHARACTER LPAR*(DAT__SZNAM)! Lower case parameter name
      CHARACTER PATH*(GRP__SZFNM)! Indirection file path
      CHARACTER TEXT*(GRP__SZNAM)! A single group element
      INTEGER I                  ! Element index
      INTEGER IAT                ! No. of characters in GRPEXP so far
      INTEGER IPAR               ! SUBPAR pointer to the parameter
      INTEGER NC                 ! No. of used characters
      INTEGER SIZE               ! Size of group
      INTEGER TLEN               ! Length of supplied group expression
      LOGICAL FULL               ! Is GRPEXP too long?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the demimiter control character for the group.
      CALL GRP_GETCC( IGRP, 'DELIMITER', DC, STATUS )

*  Initialise the total group expression given so far.
      GRPEXP = ' '
      IAT = 0
      FULL = .FALSE.

*  Loop round the contents of the group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      DO I = 1, SIZE
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )

*  Get its length.
         TLEN = CHR_LEN( TEXT )

*  Is there room for this element in the group expression?
         IF( IAT + TLEN + 1 .LE. MXEXP ) THEN

*  If so, append a delimiter to the current group expression (unless this
*  is the first element), followed by the element text.
            IF( I .GT. 1 ) CALL CHR_APPND( DC, GRPEXP, IAT )
            CALL CHR_APPND( TEXT( : TLEN ), GRPEXP, IAT )

*  If the group expression is too long, set a flag and exit.
         ELSE
            FULL = .TRUE.
            GO TO 10
         END IF

      END DO

 10   CONTINUE

*  If the contents of the group are too large to fit in a single group
*  expression, store them in a file in the user's adam directory.
      IF( FULL .AND. STATUS .EQ. SAI__OK ) THEN

*  First find the path to the ADAM user directory. Translate the environment
*  variable/logical name for ADAM_USER.
         CALL PSX_GETENV( 'ADAM_USER', PATH, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  ADAM_USER may not be defined so annul the error and try a different
*  route to the file.
            CALL ERR_ANNUL( STATUS )

*  Obtain the home directory.
            CALL PSX_GETENV( 'HOME', PATH, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'LPG1_PTPAR_1', '$HOME not defined.',
     :                        STATUS )
               GO TO 999
            END IF

*  Generate the path of the ADAM_USER.
            NC = CHR_LEN( PATH )
            CALL CHR_APPND( '/adam', PATH, NC )

         ELSE

*  Find the length of the path for ADAM_USER.
            NC = CHR_LEN( PATH )

         END IF

*  Add the lower case version of the parameter name to the adam path.
         LPAR = PARAM
         CALL CHR_LCASE( LPAR )

         CALL CHR_APPND( '/', PATH, NC )
         CALL CHR_APPND( LPAR, PATH, NC )

*  Now add "_values".
         CALL CHR_APPND( '_values', PATH, NC )

*  Attempt to delete any existing file with this path.
*  Do not worry if there is no file to delete
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PSX_REMOVE( PATH( : NC ), STATUS )
            IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL( STATUS )
         END IF

*  Create a comment for the file.
         COMM = ' '
         NC = 0
         CALL CHR_APPND( 'Values associated with parameter',
     :                   COMM, NC )
         NC = NC + 1
         CALL CHR_APPND( PARAM, COMM, NC )

*  List the contents of the group to a new file with this name.
         CALL GRP_LISTF( PATH, 0, 0, COMM( : NC ), IGRP, STATUS )

*  Create a group expression to read this file.
         GRPEXP = '^'
         IAT = 1
         CALL CHR_APPND( PATH, GRPEXP, IAT )

      END IF

*  Get a SUBPAR pointer for the parameter.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )

*  Store the group expression as the current value for the parameter.
      CALL SUBPAR_PUTNAME( IPAR, GRPEXP( : IAT ), STATUS )

*  For some reason it seems to be necesary to read the new parameter value
*  before it takes effect.
      CALL SUBPAR_GETNAME( IPAR, TEXT, STATUS )

 999  CONTINUE

      END
