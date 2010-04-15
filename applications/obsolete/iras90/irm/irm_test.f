	SUBROUTINE IRM_TEST( STATUS )
*+
*  Name:
*     IRM_TEST

*  Purpose:
*     Test installation of the IRAS90 miscellaneous utilities
*     package, IRM.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task module

*  Invocation:
*     IRM_TEST

*  Description:
*     This routine tests the installation of the IRAS90 IRM package.
*     Note, it is not an exhaustive test of the IRM_ system itself.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-MAY-1992 (DSB):
*        Original version.
*     22-APR-1993 (DSB):
*        Changed from a stand-alone program to an ADAM A-task.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'I90_DAT'          ! IRAS satellite and mission data.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER LBND,UBND          ! Lower and upper pixel bounds of
                                 ! the temporary NDF.

      PARAMETER ( LBND = -4,
     :            UBND = 10 )

*  Local Variables:
      INTEGER DETS( I90__MAXDT ) ! Returned list of detectors.
      INTEGER INDF               ! Temporary NDF identifier.
      INTEGER IP                 ! pointer to mapped DATA array.
      CHARACTER LOCS( 5 )*(DAT__SZLOC)! Locators to quality name
                                 ! information.
      INTEGER NDETS2             ! No. of detectors in returned list.
      INTEGER NEL                ! No. of elements in mapped array.
      INTEGER NSET               ! No. of pixels with given quality.
      INTEGER ONDF               ! Temporary NDF identifier.
      INTEGER PLACE              ! Place holder for temporary NDF.
      CHARACTER QEXP*254         ! Quality expression.

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain an palceholder for a temporary NDF.
      CALL NDF_TEMP( PLACE, STATUS )

*  Get an NDF identifier for the temporary NDF.
      CALL NDF_NEW( '_REAL', 1, LBND, UBND, PLACE, INDF, STATUS )

*  Fill the NDF DATA component with zeros.
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'WRITE/ZERO', IP, NEL,
     :              STATUS )
      CALL NDF_UNMAP( INDF, 'DATA', STATUS )

*  Create an extension called TEST.
      CALL NDF_XNEW( INDF, 'TEST', 'TEST', 0, 0, LOCS( 1 ), STATUS )
      CALL DAT_ANNUL( LOCS( 1 ), STATUS )

*  Create a structure to hold quality names within the NDF.
      CALL IRQ_NEW( INDF, 'TEST', LOCS, STATUS )

*  Add a new quality name FRED.
      CALL IRQ_ADDQN( LOCS, 'FRED', .FALSE., 'A TEST', STATUS )

*  Assign the quality FRED to the mid pixel of the NDF.
      CALL IRQ_SETQL( LOCS, .TRUE., 'FRED', 1, 1, ( LBND + UBND )/2,
     :                NSET, STATUS )

*  Release access to the NDF quality names information.
      CALL IRQ_RLSE( LOCS, STATUS )

*  Store a quality expression
      QEXP = '(FRED.AND.FRED).OR..NOT.(.NOT.FRED)'

*  Set pixels of the DATA component of the NDF bad if they satisfy the
*  quality expression.
      CALL IRM_QNDF( INDF, QEXP, .FALSE., .TRUE., ONDF, STATUS )

*  Map the data array of the output NDF.
      CALL NDF_MAP( ONDF, 'DATA', '_REAL', 'READ', IP, NEL, STATUS )

*  Check that the array VEC holds the correct values.
      IF ( STATUS .EQ. SAI__OK ) CALL CHECK( %VAL( IP ), UBND, LBND,
     :                                       STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Get a list of all the detectors in band 1 excluding the first
*  detector.
      CALL IRM_CVDET( I90__NDETS( 1 ), I90__BDETS( 1, 1 ), 1,
     :                I90__BDETS( 1, 1 ), DETS, NDETS2, STATUS )

*  Check that the number of detectors in the returned list is one less
*  than the number in the supplied list.
      IF( NDETS2 .NE. I90__NDETS( 1 ) - 1 .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N1', I90__NDETS( 1 ) )
         CALL MSG_SETI( 'N2', NDETS2 )
         CALL ERR_REP( 'IRM_TEST_ERR1',
     :                 'IRM_TEST: ^N2 detectors listed. Should be ^N1.',
     :                  STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_TEST_ERR2',
     :   'IRM_TEST: IRM_ installation test failed.', STATUS )

      ELSE
         CALL MSG_OUT( ' ', 'IRM_ installation test passed.', STATUS )

      END IF

      END


*--------------------------------------------------------------
      SUBROUTINE CHECK( VEC, UBND, LBND, STATUS )

*  Check that the array VEC holds the correct values.

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INTEGER UBND, LBND, STATUS, I
      REAL VEC( LBND : UBND )

      IF( STATUS .NE. SAI__OK ) RETURN

      DO I = LBND, UBND

         IF( I .EQ. ( UBND + LBND )/2 ) THEN

            IF( VEC( I ) .NE. VAL__BADR ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETR( 'V', VEC( I ) )
               CALL MSG_SETI( 'P', ( UBND + LBND )/2 )
               CALL ERR_REP( 'IRM_TEST_ERR3',
     :       'IRM_TEST: Pixel ^P has value ^V. It should be VAL__BADR.',
     :                       STATUS )
               GO TO 999
            END IF

         ELSE

            IF( VEC( I ) .NE. 0.0  ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETR( 'V', VEC( I ) )
               CALL MSG_SETI( 'P', ( UBND + LBND )/2 )
               CALL ERR_REP( 'IRM_TEST_ERR4',
     :    'IRM_TEST: Pixel ^P has value ^V. It should be zero.',
     :                       STATUS )
               GO TO 999
            END IF

         END IF

      END DO

  999 CONTINUE

      END
