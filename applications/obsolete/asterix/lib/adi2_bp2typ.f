      SUBROUTINE ADI2_BP2TYP( BITPIX, TYPE, STATUS )
*+
*  Name:
*     ADI2_BP2TYP

*  Purpose:
*     Convert FITS BITPIX value to ADI type specifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_BP2TYP( BITPIX, TYPE, STATUS )

*  Description:
*     Convert FITS BITPIX value to ADI type specifier. Returns an error
*     if BITPIX isn't one of the allowed values.

*  Arguments:
*     BITPIX = INTEGER (given)
*        The FITS bits per pixel
*     TYPE = CHARACTER*(*) (returned)
*        The ADI type corresponding to BITPIX
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Sep 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			BITPIX

*  Arguments Returned:
      CHARACTER*(*)		TYPE

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on value
      IF ( BITPIX .EQ. 8 ) THEN
        TYPE = 'BYTE'
      ELSE IF ( BITPIX .EQ. 16 ) THEN
        TYPE = 'WORD'
      ELSE IF ( BITPIX .EQ. 32 ) THEN
        TYPE = 'INTEGER'
      ELSE IF ( BITPIX .EQ. -32 ) THEN
        TYPE = 'REAL'
      ELSE IF ( BITPIX .EQ. -64 ) THEN
        TYPE = 'DOUBLE'
      ELSE
        CALL MSG_SETI( 'BP', BITPIX )
        CALL ERR_REP( 'BAD_BP', 'BITPIX keyword value /^BP/ is not '/
     :             /'one of the allowed set 8,16,32,-32,64', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_BP2TYP', STATUS )

      END
