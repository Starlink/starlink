      SUBROUTINE IRM_UNTIV( UTS1, UTS2, BAND, PIXSIZ, SCALE, STATUS )
*+
*  Name:
*     IRM_UNTIV

*  Purpose:
*     Calculate scale factor for valid image units conversions

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_UNTIV( UTS1, UTS2, BAND, PIXSIZ, SCALE, STATUS )

*  Description:
*     If the input and output units are recognised, then SCALE is
*     returned holding the factor which converts values in the input
*     units to the output units. If either of the two units systems are
*     unrecognised, an error is reported.

*  Arguments:
*     UTS1 = CHARACTER * ( * )  (Given)
*        The units of the input image.
*     UTS2 = CHARACTER * ( * ) (Given)
*        The units required for the output image.
*     BAND = INTEGER (Given)
*        The survey waveband index. If CPC data is being used an
*        illegal survey waveband index should be supplied. An error
*        will only be reported if the scale factor explicitly depends
*        on the waveband.
*     PIXSIZ = DOUBLE PRECISION (Given)
*        The nominal solid angle of an image pixel, in steradians.
*     SCALE = REAL (Returned)
*        The scale factor for converting between the input and output
*        units.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.

*  Arguments Given:
      CHARACTER UTS1*(*)
      CHARACTER UTS2*(*)
      INTEGER BAND
      DOUBLE PRECISION PIXSIZ

*  Arguments Returned:
      REAL SCALE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if 2 strings are equal apart
                                 ! from case.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied waveband index. If it is illegal, only report an
*  error if the waveband index is actually needed to find the scale
*  factor.
      IF( BAND .LE. 0 .OR. BAND .GT. I90__BANDS ) THEN
         IF( CHR_SIMLR( IRI__FPS, UTS1 ) .OR.
     :       CHR_SIMLR( IRI__FPP, UTS1 ) .OR.
     :       CHR_SIMLR( IRI__FPS, UTS2 ) .OR.
     :       CHR_SIMLR( IRI__FPP, UTS2 ) ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRM_UNTIV_ERR1',
     :                    'IRM_UNTIV: CPC data cannot be handled.',
     :                    STATUS )
            GO TO 999

         END IF

      END IF

*  Find the SCALE which converts the input data to units of
*  Mega-Jansky's per staradian.
      IF( CHR_SIMLR( IRI__JPS, UTS1 ) ) THEN        !  Input in Jy/sr
         SCALE = 1.0E-6

      ELSE IF( CHR_SIMLR( IRI__MJPS, UTS1 ) ) THEN  !  Input in MJy/sr
         SCALE = 1.0

      ELSE IF( CHR_SIMLR( IRI__JPP, UTS1 ) ) THEN   !  Input in Jy/pixel
         SCALE = 1.0E-6/PIXSIZ

      ELSE IF( CHR_SIMLR( IRI__FPS, UTS1 ) ) THEN   !  Input in flux/sr
         SCALE = I90__JY( BAND )*1.0E-6

      ELSE IF( CHR_SIMLR( IRI__FPP, UTS1 ) ) THEN   !  Input in
         SCALE = I90__JY( BAND )*1.0E-6/PIXSIZ  !  flux/pixel

*  Report an error if the input units are unknown.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'U1', UTS1 )
         CALL ERR_REP( 'IRM_UNTIV_ERR2',
     :               'IRM_UNTIV: Input image units "^U1" are unknown',
     :                 STATUS )
         GO TO 999

      END IF

*  Now modify the SCALE to convert units of Mega-Jansky's per staradian
*  to the required units.
      IF( CHR_SIMLR( IRI__JPS, UTS2 ) ) THEN
         SCALE = SCALE*1.0E6

      ELSE IF( CHR_SIMLR( IRI__MJPS, UTS2 ) ) THEN
         SCALE = SCALE*1.0

      ELSE IF( CHR_SIMLR( IRI__JPP, UTS2 ) ) THEN
         SCALE = SCALE*1.0E6*PIXSIZ

      ELSE IF( CHR_SIMLR( IRI__FPS, UTS2 ) ) THEN
         SCALE = SCALE*1.0E6/I90__JY( BAND )

      ELSE IF( CHR_SIMLR( IRI__FPP, UTS2 ) ) THEN
         SCALE = SCALE*1.0E6*PIXSIZ/I90__JY( BAND )

*  Repor an error if the output units are unknown.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'U2', UTS2 )
         CALL ERR_REP( 'IRM_UNTIV_ERR3',
     :               'IRM_UNTIV: Output image units "^U2" are unknown',
     :                 STATUS )
      END IF

 999  CONTINUE

      END
