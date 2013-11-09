      SUBROUTINE ADI1_FOBNAM( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ADI1_FOBNAM

*  Purpose:
*     Return string describing object path to HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_FOBNAM( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     04 Oct 1995 (DJA):
*        Original version, derived from old STR_OBNAM
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*200		FILE, PATH		! HDS trace info
      CHARACTER*(DAT__SZLOC)	LOC			! Object locator
      CHARACTER*200		NAME			! Full name

      INTEGER 			I1,I2			! Character indices
      INTEGER			NLEV			! Levels of structure
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the locator
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )

*  Trace the object
      CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )

*  Strip off the file extension
      I1 = CHR_LEN(FILE)
      DO WHILE (FILE(I1:I1).NE.'.')
        I1 = I1 - 1
      END DO
      I1 = I1 - 1

*  Get HDS path beyond first level
      I2 = 1
      IF ( NLEV .GT. 1 ) THEN
        DO WHILE ( PATH(I2:I2) .NE. '.' )
          I2 = I2 + 1
        END DO
        NAME = FILE(:I1)//PATH(I2:)
      ELSE
        NAME = FILE(:I1)
      END IF

*  Store string in returned object
      CALL ADI_NEWV0C( NAME(:CHR_LEN(NAME)), OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_FOBNAM', STATUS )

      END
