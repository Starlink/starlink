      PROGRAM GRP_TEST
*+
*  Name:
*     GRP_TEST

*  Purpose:
*     Test installation of the stand-alone GRP package.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     PROGRAM

*  Invocation:
*     RUN GRP_TEST

*  Description:
*     This program tests the installation of the stand-alone GRP
*     package. Note, it is not an exhaustive test of the GRP_ system
*     itself. The text file grp_test.dat should be available and
*     contain the following names "ONE, TWO, HELLO, GOODBYE"

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     PWD: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     31-AUG-1994 (DSB):
*        Modified to include tests of the kernel facilities introduced in
*        V2.0
*     27-AUG-1999 (DSB):
*        Corrected calls from MSG_SETC to MSG_SETI. Added testing of
*        escape characters.
*     15-APR-2005 (PWD):
*        Parameterize backslash uses to improve portability
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants.

*  Local Constants:
      CHARACTER * ( 1 ) ESC
      PARAMETER ( ESC = '\\' )   ! To keep UNIX compilers happy, will be
                                 ! '/' regardless

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ADDED              ! No. of names added to group.
      INTEGER DEPTH              ! No. of levels of indirection.
      CHARACTER FILE*(GRP__SZFNM)! Indirection file name.
      LOGICAL FLAG               ! True if the group was flagged by the
                                 ! user.
      INTEGER IGRP               ! Group identifier.
      INTEGER INDX               ! Index of name "HELLO".
      CHARACTER NAME*(GRP__SZNAM)! Second name in group.
      INTEGER SIZE               ! Total size of group.

*.

*  Initialise inherited global status.
      STATUS = SAI__OK

*  Create a new, empty group.
      CALL GRP_NEW( 'A TEST GROUP', IGRP, STATUS )

*  Change the control character used to indicate indirection, to a "+"
*  character.
      CALL GRP_SETCC( IGRP, 'IND', '+', STATUS )

*  Set the escape character to "\".
      CALL GRP_SETCC( IGRP, 'ESC', ESC, STATUS )

*  Obtain a list of names and put them in the group just created. Append
*  the letter B to them using a kernel.
      CALL GRP_GRPEX( '{+$PWD/grp_test.dat}B', GRP__NOID, IGRP, SIZE,
     :                ADDED, FLAG, STATUS )

*  Report an error if the group does not contain 4 names.
      IF( STATUS .EQ. SAI__OK .AND. SIZE .NE. 4 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'SIZE', SIZE )
         CALL ERR_REP( 'GRP_TEST_ERR1',
     : 'GRP_TEST: No. of names in test group (^SIZE) should be 4.',
     :                 STATUS )
      END IF

*  Get the second name in the group.
      CALL GRP_GET( IGRP, 2, 1, NAME, STATUS )

*  Report an error if the name is not "TWO\|B".
      IF( STATUS .EQ. SAI__OK .AND. NAME .NE. 'TWO'//ESC//'|B' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'GRP_TEST_ERR2',
     : 'GRP_TEST: Second test name (^NAME) should be "TWO\\|B"',
     :                 STATUS )
      END IF

*  Find the index of the name "HELLOB".
      CALL GRP_INDEX( 'HELLOB', IGRP, 1, INDX, STATUS )

*  Report an error if the index is not 3.
      IF( STATUS .EQ. SAI__OK .AND. INDX .NE. 3 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'INDEX', INDX )
         CALL ERR_REP( 'GRP_TEST_ERR3',
     : 'GRP_TEST: Index of name "HELLOB" (^INDEX) should be 3. ',
     :                 STATUS )
      END IF

*  Display it
      CALL GRP_MSG( 'N', IGRP, INDX )
      CALL MSG_OUT( ' ', 'Element 3 is "^N" ("HELLOB").', status )

*  Find the depth of indirection and the file name of the first name.
      CALL GRP_INFOI( IGRP, 1, 'DEPTH', DEPTH, STATUS )
      IF( STATUS .EQ. SAI__OK .AND. DEPTH .NE. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'DEPTH', DEPTH )
         CALL ERR_REP( 'GRP_TEST_ERR4',
     :           'GRP_TEST: Depth of first name (^DEPTH) should be 1. ',
     :                 STATUS )
      END IF

      CALL GRP_INFOC( IGRP, 1, 'FILE', FILE, STATUS )
      CALL CHR_UCASE( FILE )

      IF( STATUS .EQ. SAI__OK .AND.
     :    INDEX( FILE, 'GRP_TEST.DAT' ) .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FILE', FILE )
         CALL ERR_REP( 'GRP_TEST_ERR5',
     : 'GRP_TEST: Indirection file (^FILE) should be GRP_TEST.DAT. ',
     :                 STATUS )
      END IF

*  Delete the group.
      CALL GRP_DELET( IGRP, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

         CALL ERR_REP( 'GRP_TEST_ERR6',
     :   'GRP_TEST: GRP_ installation test failed.', STATUS )

      ELSE
         CALL MSG_OUT( ' ', 'GRP_ installation test passed.', STATUS )

      END IF

      END
