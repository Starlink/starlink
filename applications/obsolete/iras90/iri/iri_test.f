	PROGRAM IRI_TEST
*+
*  Name:
*     IRI_TEST

*  Purpose:
*     Test installation of the IRAS90 image handling package, IRI.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Stand-alone program

*  Invocation:
*     RUN IRI_TEST

*  Description:
*     This program tests the installation of the IRAS90 image handling
*     package (IRI_). Note, it is not an exhaustive test of the
*     IRI_ system itself. The default diretory at the time this test
*     program is run must contain the file IRI_TEST.SDF.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUN-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BAND               ! IRAS waveband index.
      INTEGER IDA                ! IRA identifier for astrometry info.
      INTEGER INDF               ! NDF identifier for temporary image.
      CHARACTER INSTRM*(IRI__SZINS)! IRAS instrument.
      INTEGER LBND( 2 )          ! Lower bounds of NDF axes.
      CHARACTER LOC*(DAT__SZLOC) ! HDS locator to IRAS extension.
      DOUBLE PRECISION P(8)      ! Astrometry parameters.
      INTEGER PLACE              ! Place holder for temporary NDF.
      INTEGER SKYPLA             ! Retrieved SKYFLUX plate number.
      CHARACTER TYPE*(IRI__SZTYP)! Image type.
      INTEGER UBND( 2 )          ! Upper bounds of NDF axes.
      CHARACTER UNITS*(IRI__SZUNI)! DATA units.

*.

*  Initialise the inherited global status.
      STATUS = SAI__OK

*  Start up HDS.
      CALL HDS_START( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get an identifier for a temporary NDF.
      CALL NDF_TEMP( PLACE, STATUS )
      LBND( 1 ) = -10
      LBND( 2 ) = -10
      UBND( 1 ) = 10
      UBND( 2 ) = 10
      CALL NDF_NEW( '_REAL', 2, LBND, UBND, PLACE, INDF, STATUS )

*  Create an IRAS extension, specifying the image as a SKYFLUX map.
      CALL IRI_NEW( INDF, 'SURVEY', 3, IRI__SKYFL, IRI__FPP, LOC,
     :              STATUS )

*  Add an astrometry structure.
      P( 1 ) = 0.0
      P( 2 ) = 0.0
      P( 3 ) = 0.0
      P( 4 ) = 0.0
      P( 5 ) = 1.0D-3
      P( 6 ) = 1.0D-3
      P( 7 ) = 0.0
      P( 8 ) = 0.0

      CALL IRA_INIT( STATUS )
      CALL IRA_CREAT( 'GNOMONIC', 8, P, 'EQUAT', IRA__IRJEP, INDF, IDA,
     :                STATUS )
      CALL IRA_CLOSE( STATUS )

*  Add some SKYFLUX optional items.
      CALL DAT_NEW0D( LOC, 'FIELDLON', STATUS )
      CALL CMP_PUT0D( LOC, 'FIELDLON', P( 1 ), STATUS )

      CALL DAT_NEW0D( LOC, 'FIELDLAT', STATUS )
      CALL CMP_PUT0D( LOC, 'FIELDLAT', P( 2 ), STATUS )

      CALL DAT_NEW0C( LOC, 'FIELDSCS', IRA__SZSCS, STATUS )
      CALL CMP_PUT0C( LOC, 'FIELDSCS', 'EQUAT', STATUS )

      CALL DAT_NEW0I( LOC, 'SKYFLUX', STATUS )
      CALL CMP_PUT0I( LOC, 'SKYFLUX', 111, STATUS )

      CALL DAT_NEW0L( LOC, 'SKYWEIGHT', STATUS )
      CALL CMP_PUT0L( LOC, 'SKYWEIGHT', .TRUE., STATUS )

*  Annul the locator to the IRAS extension.
      CALL DAT_ANNUL( LOC, STATUS )

*  Now try to access the IRAS extension using IRI_OLD.
      CALL IRI_OLD( INDF, INSTRM, BAND, TYPE, UNITS, LOC, STATUS )

*  Check UNITS, and items INSTRM, BAND and TYPE have the right values.
      IF( STATUS .EQ. SAI__OK .AND. INSTRM .NE. 'SURVEY' ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'I', INSTRM )
         CALL ERR_REP( 'IRI_TEST_ERR1',
     :  'IRI_TEST: INSTRM ("^I") should be "SURVEY".', STATUS )
      END IF

      IF( STATUS .EQ. SAI__OK .AND. BAND .NE. 3 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'I', BAND )
         CALL ERR_REP( 'IRI_TEST_ERR2',
     :  'IRI_TEST: BAND ("^I") should be "3".', STATUS )
      END IF

      IF( STATUS .EQ. SAI__OK .AND. TYPE .NE. IRI__SKYFL ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T1', TYPE )
         CALL MSG_SETC( 'T2', IRI__SKYFL )
         CALL ERR_REP( 'IRI_TEST_ERR3',
     :  'IRI_TEST: TYPE ("^T1") should be "^T2".', STATUS )
      END IF

      IF( STATUS .EQ. SAI__OK .AND. UNITS .NE. IRI__FPP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'I', UNITS )
         CALL MSG_SETC( 'J', IRI__FPP )
         CALL ERR_REP( 'IRI_TEST_ERR4',
     :  'IRI_TEST: UNITS ("^I") should be "^J".', STATUS )
      END IF

*  Get value of SKYFLUX and check its value.
      CALL CMP_GET0I( LOC, 'SKYFLUX', SKYPLA, STATUS )
      IF( STATUS .EQ. SAI__OK .AND. SKYPLA .NE. 111 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'I', SKYPLA )
         CALL ERR_REP( 'IRI_TEST_ERR5',
     :  'IRI_TEST: SKYFLUX ("^I") should be "111".', STATUS )
      END IF

*  Annul the locator to the IRAS extension.
      CALL DAT_ANNUL( LOC, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Stop HDS.
      CALL HDS_STOP( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRI_TEST_ERR6',
     :   'IRI_TEST: IRI_ installation test failed.', STATUS )

      ELSE
         CALL MSG_OUT( ' ', 'IRI_ installation test passed.', STATUS )

      END IF

      END
