      SUBROUTINE PSF1_RHRI_HNT( PSID, HINT, DATA, STATUS )
*+
*  Name:
*     PSF1_RHRI_HNT

*  Purpose:
*     Get hints about the ROSAT HRI psf

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF1_RHRI_HNT( PSID, HINT, DATA, STATUS )

*  Description:
*     Returns psf hint value, otherwise raises an error

*  Arguments:
*     PSID = INTEGER (given)
*        ADI identifier of psf storage object
*     HINT = CHARACTER*(*) (given)
*        Hint name
*     DATA[] = BYTE (returned)
*        Hint data bytes. Number set depends on hint
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      4 Apr 1996 (DJA):
*        Original version
*      2 May 1996 (DJA):
*        New header
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PSF_PAR'

*  Arguments Given:
      INTEGER                   PSID
      CHARACTER*(*)		HINT

*  Arguments Returned:
      BYTE			DATA(*)

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Radial symmetry?
      IF ( HINT .EQ. PSF_H_RADSYM ) THEN

*    All our models are radially symmetric about on-axis direction
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

*  Position dependent?
      ELSE IF ( HINT .EQ. PSF_H_POSDEP ) THEN

*    It is position dependent
        CALL ARR_COP1L( 1, .TRUE., DATA, STATUS )

*  Field size?
      ELSE IF ( HINT .EQ. PSF_H_FLDSIZ ) THEN

*    Write value
        CALL ARR_COP1R( 1, 0.33333*MATH__DTOR, DATA, STATUS )

*  Energy dependent?
      ELSE IF ( HINT .EQ. PSF_H_ENDEP ) THEN

*    Doesn't vary with energy
        CALL ARR_COP1L( 1, .FALSE., DATA, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'H', HINT )
        CALL ERR_REP( ' ', 'Unknown psf hint /^H/', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF1_RHRI_HNT', STATUS )
      END IF

      END
