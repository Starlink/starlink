      SUBROUTINE IRM_UNTIM( INDF, UNITS, SCALE, BAND, STATUS )
*+
*  Name:
*     IRM_UNTIM

*  Purpose:
*     Find a factor to convert NDF image data values to specified units.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_UNTIM( INDF, UNITS, SCALE, BAND, STATUS )

*  Description:
*     A scaling factor is returned which converts data values from the
*     specified NDF into values in the specified units. An error may be
*     reported if the NDF contains CPC data, depending on the specific
*     conversion required (some can be performed for CPC data, and some
*     cannot). An error is also reported if the input data is in colour
*     corrected flux density based system, and the requested units are
*     a flux based system.

*  Arguments:
*     INDF = INTEGER (Given)
*        An NDF identifier for an IRAS90 image.
*     UNITS = CHARACTER * ( * ) (Given)
*        The required units (see IRI document ID1).
*     SCALE = REAL (Returned)
*        The scale factor for converting the NDF data values to the
*        required units.
*     BAND = INTEGER (Returned)
*        The survey waveband index. Negative values are returned for
*        CPC waveband indices (eg -1 or -2 ).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-APR-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.

*  Arguments Given:
      INTEGER INDF
      CHARACTER UNITS*(*)

*  Arguments Returned:
      REAL SCALE
      INTEGER BAND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :      INSTRM*(IRI__SZINS), ! IRAS instrument from which the data
                                 ! was derived.
     :      LOC*(DAT__SZLOC),    ! Locator to IMAGE_INFO structure.
     :      OUNITS*(IRI__SZUNI), ! Original units.
     :      TYPE*(IRI__SZTYP)    ! Image type.

      DOUBLE PRECISION
     :      PIXSIZ( 2 ),         ! Nominal pixel dimensions.
     :      PIXSOL               ! Nominal pixel solid angle.

      INTEGER
     :      IDA                  ! IRA identifier for astrometry information.

      LOGICAL
     :      ACTIVE,              ! True if IRA package is active.
     :      THERE                ! True if NDF is colour corrected.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the supplied NDF is a valid IRAS90 image.
      CALL IRI_OLD( INDF, INSTRM, BAND, TYPE, OUNITS, LOC, STATUS )

*  If the NDF contains CPC data, use a negative waveband index.
      IF( INSTRM .EQ. 'CPC' ) BAND = -ABS( BAND )

*  Colour correction is significant if converting from a flux density
*  based system to a flux based system.
      IF( ( OUNITS .EQ. IRI__JPS .OR. OUNITS .EQ. IRI__MJPS .OR.
     :      OUNITS .EQ. IRI__JPP ) .AND.
     :    ( UNITS .EQ. IRI__FPS .OR. UNITS .EQ. IRI__FPP ) ) THEN

*  See if the image values have been colour corrected.
         CALL DAT_THERE( LOC, 'COLCOR', THERE, STATUS )

*  If so, report an error.
         IF( THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRM_UNTIM_ERR2',
     :              'IRM_UNTIM: Supplied NDF has been colour corrected',
     :                 STATUS )
            GO TO 999
         END IF

      END IF

*  Get an IRA identifier to the astrometry information.
      CALL IRA_ACTIV( ACTIVE )
      IF( .NOT. ACTIVE ) CALL IRA_INIT( STATUS )

      CALL IRA_IMPRT( INDF, IDA, STATUS )
      CALL IRA_PIXSZ( IDA, PIXSIZ, STATUS )
      CALL IRA_ANNUL( IDA, STATUS )

      IF( .NOT. ACTIVE ) CALL IRA_CLOSE( STATUS )

*  Calculate the scaling factor.
      PIXSOL = PIXSIZ( 1 )*PIXSIZ( 2 )
      CALL IRM_UNTIV( OUNITS, UNITS, BAND, PIXSOL, SCALE, STATUS )

*  Annul the locator to the IMAGE_INFO structure.
 999  CONTINUE
      CALL DAT_ANNUL( LOC, STATUS )

*  Add a contextual report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'N', INDF )
         CALL MSG_SETC( 'U', UNITS )
         CALL ERR_REP( 'IRM_UNTIM_ERR3',
     :  'IRM_UNTIM: Unable to convert data values from ^N into units '//
     :  'of ^U', STATUS )
      END IF

      END
