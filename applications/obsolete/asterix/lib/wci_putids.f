      SUBROUTINE WCI_PUTIDS( ID, PIXID, PRJID, SYSID, STATUS )
*+
*  Name:
*     WCI_PUTIDS

*  Purpose:
*     Write WCI data to a dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_PUTIDS( ID, PIXID, PRJID, SYSID, STATUS )

*  Description:
*     Write the WCS information defined by the 3 identifiers PIXID,
*     PRJID and SYSID to the dataset specified by ID.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of the dataset object
*     PIXID = INTEGER (given)
*        ADI identifier of the Pixellation object
*     PRJID = INTEGER (given)
*        ADI identifier of the Projection object
*     SYSID = INTEGER (given)
*        ADI identifier of the CoordSystem object
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
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
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

*  Global Variables:
      INCLUDE 'WCI_CMN'                 ! ASTERIX WCI common block
*       WCS_INIT = LOGICAL (given)
*         WCI class definitions loaded?

*  Arguments Given:
      INTEGER			ID			! Dataset id

*  Arguments Returned:
      INTEGER			PIXID			! Pixellation id
      INTEGER			PRJID			! Projection id
      INTEGER			SYSID			! Coord system id

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			IARG(4)			! Method input data
      INTEGER			RESID			! Method output data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. WCI_INIT ) CALL WCI1_INIT( STATUS )

*  Construct input arguments
      IARG(1) = ID
      IARG(2) = PIXID
      IARG(3) = PRJID
      IARG(4) = SYSID

*  Simply invoke the WriteWCS method
      CALL ADI_EXEC( 'WriteWCS', 4, ID, RESID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_PUTIDS', STATUS )

      END
