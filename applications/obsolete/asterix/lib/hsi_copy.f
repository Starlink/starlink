      SUBROUTINE HSI_COPY( IFID, OFID, STATUS )
*+
*  Name:
*     HSI_COPY

*  Purpose:
*     Copy history from first dataset to the second

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI_COPY( IFID, OFID, STATUS )

*  Description:
*     Copy all history records from the first dataset to the second.

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier to input dataset
*     OFID = INTEGER (given)
*        ADI identifier to output dataset
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
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:public, history, copying

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
*        Original version.
*     14 Mar 1995 (DJA):
*        Now works using ADI method.
*     19 Feb 1996 (DJA):
*        Failure to find copy method is no longer fatal
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_ERR'
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			IFID, OFID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			IFFID, OFFID		! File identifiers
      INTEGER			OARG			! Output from method
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( HSI__PKG ) ) CALL HSI0_INIT( STATUS )

*  Get base file objects
      CALL ADI_GETFILE( IFID, IFFID, STATUS )
      CALL ADI_GETFILE( OFID, OFFID, STATUS )

*  Invoke the CopyHistory method. Don't worry if we don't know how to copy
      CALL ADI_EXEC2( 'CopyHistory', IFFID, OFFID, OARG, STATUS )
      IF ( STATUS .EQ. ADI__NOMTH ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI_COPY', STATUS )

      END
