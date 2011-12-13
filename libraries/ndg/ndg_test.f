      PROGRAM NDG_TEST
*+
*  Name:
*     NDG_TEST

*  Purpose:
*     Test installation of the  NDG package.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Program

*  Invocation:
*     ndg_test

*  Description:
*     This program tests the installation of the  NDG
*     package. Note, it is not an exhaustive test of the
*     NDG_ system itself.

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
*     14-AUG-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL FLAG               ! True if the group expression was
                                 ! "flagged".
      INTEGER I                  ! Loop count.
      INTEGER IGRP                ! First group identifier.
      INTEGER IGRP2               ! Second group identifier.
      INTEGER INDF               ! NDF identifier.
      INTEGER IP                 ! Pointer to mapped DATA array.
      CHARACTER NAME(3)*(GRP__SZFNM)! The name of each NDF.
      INTEGER NEL                ! No. of mapped elements.
      INTEGER SIZE               ! Total size of first group.
      CHARACTER TITLE*(GRP__SZFNM) ! NDF title.

*.

*  Initialise inherited global status.
      STATUS = SAI__OK

*  Start up HDS.
      CALL HDS_START( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Store a group of NDFs to be created.
      IGRP = GRP__NOID
      CALL NDG_CREXP( 'TOM,DICK,HARRY-', GRP__NOID, IGRP, SIZE, FLAG,
     :                 STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check returned values.
      IF( SIZE .NE. 3 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'I', SIZE )
         CALL ERR_REP( 'NDG_TEST_ERR1',
     :       'NDG_TEST: NDG_CREXP argument SIZE (^I) should be 3',
     :                 STATUS )

      ELSE IF( .NOT. FLAG ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETL( 'I', FLAG )
         CALL ERR_REP( 'NDG_TEST_ERR2',
     :       'NDG_TEST: NDG_CREXP argument FLAG (^I) should be TRUE',
     :                 STATUS )

      END IF

*  Create each of the 3 NDFs, setting the data array to all zeros, and
*  storing the file name as the title.
      DO I = 1, 3
         CALL NDG_NDFCR( IGRP, I, '_REAL', 1, -5, 5, INDF, STATUS )

         CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE/ZERO', IP, NEL,
     :                 STATUS )
         CALL GRP_GET( IGRP, I, 1, NAME( I ), STATUS )
         CALL NDF_CPUT( NAME( I ), INDF, 'TITLE', STATUS )

         CALL NDF_ANNUL( INDF, STATUS )

      END DO

*  Get a group containing the three sdf names.
      IGRP2 = GRP__NOID
      CALL NDG_ASEXP( 'TOM,DICK,HARRY', .TRUE., GRP__NOID, IGRP2,
     :                 SIZE, FLAG, STATUS )

*  Delete the original group.
      CALL GRP_DELET( IGRP, STATUS )

*  Attempt to access each of the three NDFs, checking that the
*  title equals the file name.
      DO I = 1, 3
         CALL NDG_NDFAS( IGRP2, I, 'UPDATE', INDF, STATUS )
         CALL NDF_CGET( INDF, 'TITLE', TITLE, STATUS )

         IF( TITLE .NE. NAME( I ) .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T',  TITLE )
            CALL MSG_SETC( 'NAME', NAME( I ) )
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'NDG_TEST_ERR3',
     :               'NDG_TEST: Title of NDF ^NDF (^T) should be ^NAME',
     :                    STATUS )
         END IF

         CALL NDF_ANNUL( INDF, STATUS )

      END DO

*  Delete the second group.
      CALL GRP_DELET( IGRP2, STATUS )

*  End the NDF context.
 999  CONTINUE
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

         CALL ERR_REP( 'NDG_TEST_ERR4',
     :   'NDG_TEST: NDG_ installation test failed.', STATUS )

      ELSE
         CALL MSG_OUT( ' ', 'NDG_ installation test passed.', STATUS )

      END IF

*  Close down HDS.
      CALL HDS_STOP( STATUS )

      END
