      SUBROUTINE IRI_CHECK( UNITS, OK, STATUS )
*+
*  Name:
*     IRI_CHECK

*  Purpose:
*     Check that a units system is one of the standard systems.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRI_CHECK( UNITS, OK, STATUS )

*  Description:
*     This routine checks that the supplied units system belongs to the
*     set of standard units system. The comparisons are case
*     insensitive, but the supplied units system is replaced by a
*     string in which the case of each letter corresponds to that of
*     the appropriate standard system (e.g. if "JY/SR" is supplied,
*     "Jy/sr" is returned). If the units system is recognised, OK is
*     returned .true., otherwise it is returned .false.

*  Arguments:
*     UNITS = CHARACTER * ( * ) (Given and Returned)
*        The units.
*     OK = LOGICAL (Returned)
*        Flag indicating if the units are recognised.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRI_PAR'          ! IRI constants.

*  Arguments Given and Returned:
      CHARACTER UNITS*(*)

*  Arguments Returned:
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if two strings are the same
                                 ! except for case.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned flag to indicate that the units are known.
      OK = .TRUE.

*  Check ths supplied units against each of the known system of units,
*  ignoring case. Once found, replace the supplied string with the
*  case-corrected string.
      IF( CHR_SIMLR( UNITS, IRI__JPS ) ) THEN
         UNITS = IRI__JPS

      ELSE IF( CHR_SIMLR( UNITS, IRI__MJPS ) ) THEN
         UNITS = IRI__MJPS

      ELSE IF( CHR_SIMLR( UNITS, IRI__JPP ) ) THEN
         UNITS = IRI__JPP

      ELSE IF( CHR_SIMLR( UNITS, IRI__FPS ) ) THEN
         UNITS = IRI__FPS

      ELSE IF( CHR_SIMLR( UNITS, IRI__FPP ) ) THEN
         UNITS = IRI__FPP

*  If the system of units is not recognised, set the returned flag
*  to indicate this.
      ELSE
         OK = .FALSE.
      END IF

      END
