      SUBROUTINE ADI1_FCLONE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ADI1_FCLONE

*  Purpose:
*     Clone an HDS file to a new file of the specified name

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_FCLONE( NARG, ARGS, OARG, STATUS )

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
*     18 Aug 1995 (DJA):
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

*  Arguments Given:
      INTEGER			NARG, ARGS(*)

*  Arguments Returned:
      INTEGER			OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CLOC			! Component locator
      CHARACTER*(DAT__SZLOC)	ILOC			! Input locator
      CHARACTER*(DAT__SZNAM)	NAME			! Input name
      CHARACTER*(DAT__SZLOC)	OLOC			! Output locator
      CHARACTER*(DAT__SZTYP)	TYPE			! Input type

      INTEGER			DIMS(DAT__MXDIM)	! Dimensions
      INTEGER			I			! Loop over components
      INTEGER			NCOMP			! # components
      INTEGER			NDIM			! Dimensionality
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator of input object
      CALL ADI1_GETLOC( ARGS(1), ILOC, STATUS )

*  Get characteristics of input
      CALL DAT_NAME( ILOC, NAME, STATUS )
      CALL DAT_TYPE( ILOC, TYPE, STATUS )
      CALL DAT_SHAPE( ILOC, DAT__MXDIM, DIMS, NDIM, STATUS )

*  Extract filename
      CALL ADI1_FCREAT_INT( ARGS(2), NAME, TYPE, NDIM, DIMS,
     :                      OARG, STATUS )

*  Extract new locator
      CALL ADI1_GETLOC( OARG, OLOC, STATUS )

*  Copy components
      CALL DAT_NCOMP( ILOC, NCOMP, STATUS )
      DO I = 1, NCOMP
        CALL DAT_INDEX( ILOC, I, CLOC, STATUS )
        CALL DAT_NAME( CLOC, NAME, STATUS )
        CALL DAT_COPY( CLOC, OLOC, NAME, STATUS )
        CALL DAT_ANNUL( CLOC, STATUS )
      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_FCLONE', STATUS )

      END
