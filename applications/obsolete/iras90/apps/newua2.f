      SUBROUTINE NEWUA2( VAR, PIXSOL, BAND, UNITS, OLDUN, EL,
     :                   DATIN, VARIN, DATOUT, VAROUT, DBAD, VBAD,
     :                   STATUS )
*+
*  Name:
*     NEWUA2

*  Purpose:
*     Scale an image to new units.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NEWUA2( VAR, PIXSOL, BAND, UNITS, OLDUN, EL, DATIN, VARIN,
*                  DATOUT, VAROUT, DBAD, VBAD, STATUS )

*  Description:

*  Arguments:
*     VAR = LOGICAL (Given)
*        True if the variance arrays are to be scaled.
*     PIXSOL = DOUBLE PRECISION (Given)
*        The nominal solid angle of a pixel.
*     BAND = INTEGER (Given)
*        The survey waveband index.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units required for the output.
*     OLDUN = CHARACTER * ( * ) (Given)
*        The units of the input.
*     EL = INTEGER (Given)
*        The number of elements in the image.
*     DATIN( EL ) = REAL (Given)
*        The input data array.
*     VARIN( EL ) = REAL (Given)
*        The input variance array. Only accessed if VAR is true.
*     DATOUT( EL ) = REAL (Given)
*        The output data array.
*     VAROUT( EL ) = REAL (Given)
*        The output variance array. Only accessed if VAR is true.
*     DBAD = LOGICAL (Returned)
*        True if any bad values are found in the data array.
*     VBAD = LOGICAL (Returned)
*        True if any bad values are found in the variance array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      LOGICAL VAR
      DOUBLE PRECISION PIXSOL
      INTEGER BAND
      CHARACTER UNITS*(*)
      CHARACTER OLDUN*(*)
      INTEGER EL
      REAL DATIN( EL )
      REAL VARIN( EL )

*  Arguments Returned:
      REAL DATOUT( EL )
      REAL VAROUT( EL )
      LOGICAL DBAD
      LOGICAL VBAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Pixel index.

      REAL SCALE                 ! Factor for unit conversion.
      REAL VAL                   ! Input value.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the conversion factor.
      CALL IRM_UNTIV( OLDUN, UNITS, BAND, PIXSOL, SCALE, STATUS )

*  Initialise the bad value flags.
      DBAD = .FALSE.
      VBAD = .FALSE.

*  Loop round each pixel in the image.
      DO I = 1, EL

*  If valid, write the input value to the output in the new units.
         VAL = DATIN( I )
         IF( VAL .NE. VAL__BADR ) then
            DATOUT( I ) = VAL*SCALE

*  Store a bad value in the output if the input is bad, and set the bad
*  pixel flag.
         ELSE
            DATOUT( I ) = VAL__BADR
            DBAD = .TRUE.
         END IF

*  If required, do the same with the variance value.
         IF( VAR ) THEN

            VAL = VARIN( I )
            IF( VAL .NE. VAL__BADR ) then
               VAROUT( I ) = VAL*SCALE*SCALE
            ELSE
               VAROUT( I ) = VAL__BADR
               VBAD = .TRUE.
            END IF

         END IF

      END DO

      END
