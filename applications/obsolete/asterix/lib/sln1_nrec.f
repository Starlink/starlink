      SUBROUTINE SLN1_NREC( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     SLN1_NREC

*  Purpose:
*     Get number of selection records for HDSfile

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SLN1_NREC( NARG, ARGS, OARG, STATUS )

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
*     SLN Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/sln.html

*  Keywords:
*     package:sln, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Sep 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	SLOC			! SORT box

      INTEGER			DIM			! Dimension of SORT
      INTEGER			NDIM			! Dimensionality of SORT

      LOGICAL			THERE			! Component exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID

*  Locate sort box
      CALL ADI1_LOCSORT( ARGS(1), .FALSE., SLOC, STATUS )

*  Get dimensions
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        DIM = 0
      ELSE
        CALL DAT_SHAPE( SLOC, 1, DIM, NDIM, STATUS )
        IF ( NDIM .EQ. 0 ) THEN
          CALL DAT_THERE( SLOC, 'SRECS', THERE, STATUS )
          IF ( THERE ) THEN
            CALL CMP_SHAPE( SLOC, 'SRECS', 1, DIM, NDIM, STATUS )
            DIM = DIM + 1
          ELSE
            DIM = 1
          END IF
        END IF
      END IF

*  Write dimensions
      CALL ADI_NEWV0I( DIM, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'SLN1_NREC', STATUS )

      END
