      SUBROUTINE ADI2_FOBNAM( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ADI2_FOBNAM

*  Purpose:
*     Return one line description of FITS object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FOBNAM( NARG, ARGS, OARG, STATUS )

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
*     20 Mar 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! Standard SAE constants

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
      CHARACTER*200		PATH			! Object item

      INTEGER			FSTAT			! I/o status code
      INTEGER			L			! Used bit of PATH
      INTEGER			LUN			! Logical unit number

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract LOGICAL UNIT
      CALL ADI2_GETLUN( ARGS(1), LUN, STATUS )

*  Filename from logical unit
      INQUIRE( UNIT=LUN, NAME=PATH, IOSTAT=FSTAT )

*  Report error if that failed
      IF ( FSTAT .NE. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unable to get file name for FITSfile'/
     :                /' object', STATUS )

      ELSE

        L = CHR_LEN(PATH)

*    The path stored in the Fpath member
        CALL ADI_THERE( ARGS(1), 'Fpath', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CGET0C( ARGS(1), 'Fpath', PATH(L+1:), STATUS )
          L = CHR_LEN(PATH)
        END IF
        CALL ADI_NEWV0C( PATH(:L), OARG, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FOBNAM', STATUS )

      END
