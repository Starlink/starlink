      SUBROUTINE ADI1_FTRACE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ADI1_FTRACE

*  Purpose:
*     Return file trace information about an HDSlocator based object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_FTRACE( NARG, ARGS, OARG, STATUS )

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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*200		FILE, PATH		! HDS trace info
      CHARACTER*(DAT__SZLOC)	LOC			! Object locator

      INTEGER			NLEV			! Levels of structure
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the locator
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )

*  Invoke the HDS trace routine
      CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )

*  Create return object and poke HDS values into it
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL ADI_NEW0( 'STRUC', OARG, STATUS )
        CALL ADI_CPUT0C( OARG, 'File', FILE(:CHR_LEN(FILE)), STATUS )
        CALL ADI_CPUT0C( OARG, 'Path', PATH(:CHR_LEN(PATH)), STATUS )
        CALL ADI_CPUT0I( OARG, 'Nlev', NLEV, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_FTRACE', STATUS )

      END
