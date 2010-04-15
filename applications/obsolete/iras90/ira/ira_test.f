      PROGRAM IRA_TEST
*+
*  Name:
*     IRA_TEST

*  Purpose:
*     Test installation of the IRAS90 astrometry package.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     PROGRAM

*  Invocation:
*     RUN IRA_TEST

*  Description:
*     This program tests the installation of the iras90 astrometry
*     package (IRA_). Note, it is not an exhaustive test of the
*     IRA_ system itself.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'NDF_PAR'          ! NDF_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A( 2 )    ! Sky longitude coordinates.
      DOUBLE PRECISION B( 2 )    ! Sky latitude coordinates.
      DOUBLE PRECISION ENDANG    ! Position angle of great circle at end
                                 ! of shift.
      INTEGER IDA                ! Astrometry identifier.
      DOUBLE PRECISION P( 8 )    ! Projection parameters.
      DOUBLE PRECISION PAR       ! Displacement parallel to great
                                 ! circle.
      DOUBLE PRECISION PRP       ! Displacement perpendicular to great
                                 ! circle.
      CHARACTER SCS*(IRA__SZSCS)! Sky coordinate system.
      DOUBLE PRECISION X( 2 )    ! Image X coordinates.
      DOUBLE PRECISION Y( 2 )    ! Image Ycoordinates.

*.

*  Initialise the inherited global status.
      STATUS = SAI__OK

*  Initialise the IRA_ system.
      CALL IRA_INIT( STATUS)

*  Generate a string holding a Sky Coordinate System name with
*  equinox given by the Besselian epoch 1992.0.
      SCS = 'EQU'
      CALL IRA_SETEQ( 1992.0D0, 'B', SCS, STATUS )

*  Create a temporary astrometry structure describing an orthographic
*  projection centred on RA=6h DEC=45D with pixel dimensions of 1x2
*  arcsecs, with the Y axis inclined at an angle of 30 degrees to north.
*  The image centre has pixel coordinates (0,0).
      P( 1 ) = 6.0D0*IRA__TH2R
      P( 2 ) = 45.0D0*IRA__DTOR
      P( 3 ) = 0.0D0
      P( 4 ) = 0.0D0
      P( 5 ) = 1.0D0*IRA__AS2R
      P( 6 ) = 2.0D0*IRA__AS2R
      P( 7 ) = 30.0D0*IRA__DTOR
      P( 8 ) = 0.0D0

      CALL IRA_CREAT( 'ORTH', 8, P, SCS, IRA__IRJEP, NDF__NOID, IDA,
     :                STATUS)

*  Transform the image coordinates (-100,100) and (+100,200) to sky
*  coordinates.
      X( 1 ) = -100.0D0
      X( 2 ) = 100.0D0
      Y( 1 ) = 100.0D0
      Y( 2 ) = 200.0D0
      CALL IRA_TRANS( 2, X, Y, .TRUE., SCS, IDA, A, B, STATUS )

*  Resolve the displacement between the two points into components
*  parallel and perpendicaular to the Y axis.
      CALL IRA_DIST2( A( 1 ), B( 1 ), P( 7 ), A( 2 ), B( 2 ), PAR, PRP,
     :                STATUS )

*  Apply two shifts corresponding to the parallel and perpendicular
*  displacements just found.
      CALL IRA_SHIFT( A( 1 ), B( 1 ), P( 7 ), PAR, A( 1 ), B( 1 ),
     :                ENDANG, STATUS )
      CALL IRA_SHIFT( A( 1 ), B( 1 ), ENDANG + IRA__PIBY2, PRP, A( 1 ),
     :                B( 1 ), ENDANG, STATUS )

*  Convert the final position and reference position into galactic
*  coordinates.
      CALL IRA_PACON( 1, A, B, ENDANG, SCS, 'GAL', IRA__IRJEP, A, B,
     :                ENDANG, STATUS )

*  Convert the galactic coordinates into image coordinates.
      CALL IRA_TRANS( 1, A, B, .FALSE., 'GAL', IDA, X, Y, STATUS )

*  Annul the astrometry identifier, and close down IRA.
      CALL IRA_ANNUL( IDA, STATUS )
      CALL IRA_CLOSE( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK .OR.
     :     ABS( X( 1 ) - 100.0D0 ) .GT. 1.0D-3 .OR.
     :     ABS( Y( 1 ) - 200.0D0 ) .GT. 1.0D-3 ) THEN

         CALL MSG_SETD( 'X', X( 1 ) )
         CALL MSG_SETD( 'Y', Y( 1 ) )
         CALL MSG_OUT( ' ',
     : ' Test results (^X,^Y) should be (100.0,200.0)', STATUS )

         IF( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL ERR_REP( 'IRA_TEST_ERR',
     :   'IRA_TEST: IRA_ installation test failed.', STATUS )

      ELSE
         CALL MSG_OUT( ' ', 'IRA_ installation test passed.', STATUS )

      END IF

      END
