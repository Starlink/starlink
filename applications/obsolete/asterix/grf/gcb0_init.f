      SUBROUTINE GCB0_INIT( STATUS )
*+
*  Name:
*     GCB0_INIT

*  Purpose:
*     Load ADI definitions required for GCB operation

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB0_INIT( STATUS )

*  Description:
*     Loads methods required for reading and writing GCBs to HDS and FITS
*     files.

*  Arguments:
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
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Jul 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GCB_PAR'

*  Global Variables:
      INCLUDE 'GCB_CMN'                                 ! GCB globals
*        G_MTHINIT = LOGICAL (given and returned)
*           GCB definitions load attempted?

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			GCB1_LOAD
      EXTERNAL			GCB1_SAVE

*  Local variables:
      INTEGER                   DID                     ! Ignored identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check not already initialised?
      IF ( .NOT. G_MTHINIT ) THEN

*    Define the methods
        CALL ADI_DEFMTH( 'LoadGCB(HDSfile)', GCB1_LOAD, DID, STATUS )
        CALL ADI_DEFMTH( 'SaveGCB(HDSfile)', GCB1_SAVE, DID, STATUS )

*    Now initialised
        G_MTHINIT = .TRUE.

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB0_INIT', STATUS )

      END
