      PROGRAM CTG_TEST
*+
*  Name:
*     CTG_TEST

*  Purpose:
*     Test installation of the  CTG package.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This program tests the installation of the  CTG package. Note, 
*     it is not an exhaustive test of the CTG_ system itself.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-1999 (DSB):
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
      INCLUDE 'CAT_PAR'          ! CAT_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME(3)*(GRP__SZFNM)! The name of each catalogue.
      CHARACTER TITLE*(GRP__SZFNM)! The catalogue title
      INTEGER CI                 ! Catalogue identifier.
      INTEGER FII                ! Column identifier.
      INTEGER I                  ! Loop count.
      INTEGER IGRP               ! First group identifier.
      INTEGER IGRP2              ! Second group identifier.
      INTEGER QI                 ! Parameter identifier.
      INTEGER SIZE               ! Total size of first group.
      LOGICAL FLAG               ! Was the group expression "flagged"?
      LOGICAL NULL               ! Null parameter?

*.

*  Initialise inherited global status.
      STATUS = SAI__OK 

*  Store a group of catalogues to be created.
      IGRP = GRP__NOID
      CALL CTG1_CREXP( 'TOM,DICK,HARRY-', GRP__NOID, IGRP, SIZE, FLAG, 
     :                 STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check returned values.
      IF( SIZE .NE. 3 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'I', SIZE )
         CALL ERR_REP( 'CTG_TEST_ERR1',
     :       'CTG_TEST: CTG_CREXP argument SIZE (^I) should be 3',
     :                 STATUS )

      ELSE IF( .NOT. FLAG ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETL( 'I', FLAG )
         CALL ERR_REP( 'CTG_TEST_ERR2',
     :       'CTG_TEST: CTG_CREXP argument FLAG (^I) should be TRUE',
     :                 STATUS )

      END IF

*  Create each of the 3 catalogues, storing the file name as the title.
      DO I = 1, 3
         CALL CTG_CATCR( IGRP, I, CI, STATUS )

         CALL GRP_GET( IGRP, I, 1, NAME( I ), STATUS )

         CALL CAT_PPTSC( CI, 'TITLE', NAME( I ), ' ', QI, STATUS)


         CALL CAT_PNEW0( CI, CAT__FITYP, 'COLI', CAT__TYPEI, FII,
     :                   STATUS )
         CALL CAT_PUT0I( FII, 1, .FALSE., STATUS )
         CALL CAT_RAPND( CI, STATUS )

         CALL CAT_TRLSE( CI, STATUS )

      END DO

*  Get a group containing the three sdf names.
      IGRP2 = GRP__NOID
      CALL CTG1_ASEXP( 'TOM,DICK,HARRY', GRP__NOID, IGRP2, SIZE, FLAG, 
     :                 STATUS )

*  Delete the original group.
      CALL GRP_DELET( IGRP, STATUS )

*  Attempt to access each of the three catalogues, checking that the
*  title equals the file name.
      DO I = 1, 3
         CALL CTG_CATAS( IGRP2, I, 'READ', CI, STATUS )

         CALL CAT_TIDNT( CI, 'TITLE', QI, STATUS ) 
         CALL CAT_EGT0C( QI, TITLE, NULL, STATUS )

         IF( TITLE .NE. NAME( I ) .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T',  TITLE )
            CALL MSG_SETC( 'NAME', NAME( I ) )
            CALL ERR_REP( 'CTG_TEST_ERR3', 'CTG_TEST: Title of '//
     :                    'catalogue (^T) should be ^NAME', STATUS )
         END IF

         CALL CAT_TRLSE( CI, STATUS )

      END DO

*  Delete the second group.
      CALL GRP_DELET( IGRP2, STATUS )

 999  CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

         CALL ERR_REP( 'CTG_TEST_ERR4',
     :   'CTG_TEST: CTG_ installation test failed.', STATUS )

      ELSE
         CALL MSG_OUT( ' ', 'CTG_ installation test passed.', STATUS )

      END IF

      END
