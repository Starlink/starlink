	PROGRAM IRQ_TEST
*+
*  Name:
*     IRQ_TEST

*  Purpose:
*     Test installation of the IRAS90 Quality handling package, IRQ.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Stand-alone program

*  Invocation:
*     RUN IRQ_TEST

*  Description:
*     This program tests the installation of the IRAS90 quality handling
*     package (IRQ_). Note, it is not an exhaustive test of the
*     IRQ_ system itself.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAY-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants
      INCLUDE 'IRQ_PAR'          ! IRQ_ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ_ error constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER LBND,UBND          ! Lower and upper pixel bounds of
                                 ! the temporary NDF.
      REAL    BASEV              ! The value to fill VEC with.

      PARAMETER ( LBND = -4,
     :            UBND = 10,
     :            BASEV = 1.0 )

*  Local Variables:
      LOGICAL ALLBAD             ! True if all pixels in VEC are bad.
      INTEGER ERRPNT             ! POinter to position of first error in
                                 ! quality expression.
      INTEGER I                  ! Loop count.
      INTEGER IDQ                ! Identifier for compiled quality
                                 ! expression.
      INTEGER INDF               ! Temporary NDF identifier.
      CHARACTER LOCS( 5 )*(DAT__SZLOC)! Locators to quality name
                                 ! information.
      LOGICAL NOBAD              ! True if no pixels in VEC are bad.
      INTEGER NSET               ! No. of pixels with given quality.
      INTEGER NUNDEF             ! No. of undefined quality names.
      INTEGER PLACE              ! Place holder for temporary NDF.
      CHARACTER QEXP*(IRQ__SZQNM)! Quality expression.
      CHARACTER UNDEF*(IRQ__SZQNM)! Any undefined quality name.
      REAL    VEC( LBND : UBND ) ! Vector to store bad values in.

*.

*  Initialise the inherited global status.
      STATUS = SAI__OK

*  Start up HDS.
      CALL HDS_START( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain an palceholder for a temporary NDF.
      CALL NDF_TEMP( PLACE, STATUS )

*  Get an NDF identifier for the temporary NDF.
      CALL NDF_NEW( '_REAL', 1, LBND, UBND, PLACE, INDF, STATUS )

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

*  Compile a quality expression
      QEXP = '(FRED.AND.FRED).OR..NOT.(.NOT.FRED)'
      CALL IRQ_COMP( LOCS, 1, .TRUE., QEXP, UNDEF, NUNDEF, ERRPNT, IDQ,
     :               STATUS )

*  Initialise the contents of VEC.
      DO I = LBND, UBND
         VEC( I ) = BASEV
      END DO

*  Set pixels bad in VEC which hold quality satisfying the compiled
*  quality expresion (the mid pixel only).
      CALL IRQ_SBAD( IDQ, .TRUE., UBND - LBND + 1, VEC, ALLBAD, NOBAD,
     :               STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check that the array VEC holds the correct values.
      DO I = LBND, UBND

         IF( I .EQ. ( UBND + LBND )/2 ) THEN

            IF( VEC( I ) .NE. VAL__BADR ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETR( 'V', VEC( I ) )
               CALL MSG_SETI( 'P', ( UBND + LBND )/2 )
               CALL ERR_REP( 'IRQ_TEST_ERR1',
     :       'IRQ_TEST: Pixel ^P has value ^V. It should be VAL__BADR.',
     :                       STATUS )
               GO TO 999
            END IF

         ELSE

            IF( VEC( I ) .NE. BASEV ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETR( 'S', BASEV )
               CALL MSG_SETR( 'V', VEC( I ) )
               CALL MSG_SETI( 'P', ( UBND + LBND )/2 )
               CALL ERR_REP( 'IRQ_TEST_ERR2',
     :    'IRQ_TEST: Pixel ^P has value ^V. It should be ^S.',
     :                       STATUS )
               GO TO 999
            END IF

         END IF

      END DO

*  Annul the compiled quality expression identifier (not strictly
*  necessary as the identifier would be annuled by the following call to
*  IRQ_CLOSE).
      CALL IRQ_ANNUL( IDQ, STATUS )

*  Close down the IRQ package.
 999  CONTINUE
      CALL IRQ_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Stop HDS.
      CALL HDS_STOP( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRQ_TEST_ERR3',
     :   'IRQ_TEST: IRQ_ installation test failed.', STATUS )

      ELSE
         CALL MSG_OUT( ' ', 'IRQ_ installation test passed.', STATUS )

      END IF

      END
