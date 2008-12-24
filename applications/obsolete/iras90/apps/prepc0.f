      SUBROUTINE PREPC0( PFACT, TYPE, BAND, PIXSIZ, UNITS, U, FACTOR,
     :                   STATUS )
*+
*  Name:
*     PREPC0

*  Purpose:
*     Find the input to output units conversion factor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPC0( PFACT, TYPE, BAND, PIXSIZ, UNITS, U, FACTOR, STATUS )

*  Description:
*     If the input and output units are recognised, then FACTOR is
*     returned holding the factor which converts values in the input
*     units to the output units. If either of the two units systems are
*     unrecognised, a conversion factor is obtained from the user.

*  Arguments:
*     PFACT = CHARACTER * ( * ) (Given)
*        The parameter to use when getting the conversion factor between
*        unknown units.
*     TYPE = CHARACTER * ( * ) (Given)
*        The image type. This should be equal to one of the symbolic
*        constants defined within the IRI subsystem.
*     BAND = INTEGER (Given)
*        The survey or CPC waveband index.
*     PIXSIZ = DOUBLE PRECISION (Given)
*        The nominal solid angle of an output pixel, in steradians.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units required for the output NDF. If this is supplied
*        blank, then the units are left in the same system as the
*        input.
*     U = CHARACTER * ( * )  (Given and Returned)
*        On entry U should hold the units of the input NDF. On exit, it
*        holds the units actually used for the output. If UNITS is
*        supplied with a blank value, then U is returned unchanged.
*        Otherwise, it will be retuned equal to UNITS.
*     FACTOR = REAL (Returned)
*        The scale factor for converting between the input and output
*        units. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
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
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.
      INCLUDE 'PAR_ERR'          ! PAR_ error constants.

*  Arguments Given:
      CHARACTER PFACT*(*)
      CHARACTER TYPE*(*)
      INTEGER BAND
      DOUBLE PRECISION PIXSIZ
      CHARACTER UNITS*(*)

*  Arguments Given and Returned:
      CHARACTER U*(*)

*  Arguments Returned:
      REAL FACTOR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if 2 strings are equal apart
                                 ! from case.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the user has requested that the output should have the same units
*  as the input, set the scale factor to 1.
      IF( UNITS .EQ. ' ' .OR. CHR_SIMLR( UNITS, U ) ) THEN
         FACTOR = 1.0

*  Otherwise...
      ELSE

*  CPC images can not be scaled to different units.
         IF( TYPE .EQ. IRI__CPC ) THEN
            CALL MSG_BLANKIF( MSG__QUIET, STATUS )         
            CALL MSG_SETC( 'U1', UNITS )
            CALL MSG_SETC( 'U2', U )
            CALL MSG_OUTIF( MSG__QUIET, 'PREPC0_MSG1',
     :   'WARNING:  CPC image cannot be produced in ^U1. Output will '//
     :   'be in ^U.', STATUS )
            CALL MSG_BLANKIF( MSG__QUIET, STATUS )         
            FACTOR = 1.0
            GO TO 999
         END IF

*  Images with no input units can not be scaled to different units.
         IF( U .EQ. ' ' ) THEN
            CALL MSG_BLANKIF( MSG__QUIET, STATUS )         
            CALL MSG_SETC( 'U1', UNITS )
            CALL MSG_OUTIF( MSG__QUIET, 'PREPC0_MSG2',
     :'WARNING:  No system of units is specified in the FITS header. '//
     :'Therefore the output data cannot be scaled to units of ^U1. ', 
     :                      STATUS )
            CALL MSG_BLANKIF( MSG__QUIET, STATUS )         
            FACTOR = 1.0
            GO TO 999
         END IF

*  For survey array images, get the required sclaing factor.
         CALL IRM_UNTIV( U, UNITS, BAND, PIXSIZ, FACTOR, STATUS )

*  If a conversion factor was found, use the new units.
         IF( STATUS .EQ. SAI__OK ) THEN
            U = UNITS

*  Otherwise, annul the error and get the factor for converting from 
*  the input to output units from the environment.
         ELSE 
            CALL ERR_ANNUL( STATUS )
            CALL MSG_BLANKIF( MSG__QUIET, STATUS )
            CALL MSG_SETC( 'U1', U )
            CALL MSG_SETC( 'U2', UNITS )
            CALL MSG_OUTIF( MSG__QUIET, 'PREPC0_MSG3',
     :           'WARNING: Do not know how to convert from ^U1 to ^U2.',
     :                      STATUS )

            CALL PAR_CANCL( PFACT, STATUS )
            CALL PAR_GET0R( PFACT, FACTOR, STATUS )

*  If a null value was supplied, leave the data in the original units.
            IF( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )               
               FACTOR = 1.0

*  If a value was supplied, use the new units.      
            ELSE
               U = UNITS
            END IF

         END IF

      END IF

 999  CONTINUE

      END
