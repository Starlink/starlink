      SUBROUTINE FSI1_SETLNK( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     FSI1_SETLNK

*  Purpose:
*     Make link between existing HDS file and a FileSet object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FSI1_SETLNK( NARG, ARGS, OARG, STATUS )

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
*     FSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fsi.html

*  Keywords:
*     package:fsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Oct 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'ADI_ERR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CLOC			! Component locator
      CHARACTER*(DAT__SZLOC)	LOC			! Dataset locator
      CHARACTER*(DAT__SZTYP)	TYP			! Dataset type

      INTEGER			ICOMP			! Loop over components
      INTEGER			NCOMP			! # file components
      INTEGER			NFILE			! # references

      LOGICAL			OK			! Input is a m/g?
      LOGICAL			PRIM			! Input is primitive?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set return argument
      OARG = ADI__NULLID

*  Get locator
      CALL ADI1_GETLOC( ARGS(2), LOC, STATUS )

*  Check if primitive object
      OK = .FALSE.
      CALL DAT_PRIM( LOC, PRIM, STATUS )
      IF ( .NOT. PRIM ) THEN

*    Top level type should be REF_FILE
        CALL DAT_TYPE( LOC, TYP, STATUS )
        OK = (TYP.EQ.'REF_FILE')

      END IF

*  It is a file set?
      IF ( OK ) THEN

*    Count file references
        CALL DAT_NCOMP( LOC, NCOMP, STATUS )
        NFILE = 0
        DO ICOMP = 1, NCOMP
          CALL DAT_INDEX( LOC, ICOMP, CLOC, STATUS )
          CALL DAT_TYPE( CLOC, TYP, STATUS )
          IF ( (TYP .EQ. 'REFERENCE_OBJ') .OR.
     :         (TYP .EQ. 'ADI_REF') ) THEN
            NFILE = NFILE + 1
          END IF
          CALL DAT_ANNUL( CLOC, STATUS )
        END DO

*    Write number of references
        CALL ADI_CPUT0I( ARGS(1), 'NFILE', NFILE, STATUS )

*    Link the objects
        CALL ADI_SETLNK( ARGS(1), ARGS(2), STATUS )

      ELSE

*    Allow retry if not a multi-graph object
        STATUS = ADI__RETRY
        CALL ERR_REP( ' ', 'Input is not a file set', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FSI1_SETLNK', STATUS )

      END
