      SUBROUTINE WCI1_INIT( STATUS )
*+
*  Name:
*     WCI1_INIT

*  Purpose:
*     Load ADI definitions required for WCI operation

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WCI1_INIT( STATUS )

*  Description:
*     Loads those class definitions required by the WCI subroutine group.
*     Results in the following classes being defined,
*
*       Pixellation    - Describes pixel grid w.r.t. some fiducial point
*       Projection     - Describes a map projection
*       CoordSystem    - Describes an astronomical coordinate system
*
*     Methods are defined to read and write WCS information from HDS and
*     FITS files.

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
*     ADI:
*        ADI_REQPKG - Load a package from the load path

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'WCI_CMN'					! WCI globals
*        WCI_INIT = LOGICAL (given and returned)
*           WCI definitions load attempted?

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			ADI_REQPKG
      EXTERNAL                  WCI1_READHDS
      EXTERNAL			WCI2_WRITFIT

*  Local variables:
      INTEGER			DID			! Ignored identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check not already initialised?
      IF ( .NOT. WCI_INIT ) THEN

*    Load the ADI classes
        CALL ADI_REQPKG( 'wcs', STATUS )

*    Define the methods
        CALL ADI_DEFMTH( 'ReadWCS(HDSfile)', WCI1_READHDS, DID, STATUS )
        CALL ADI_DEFMTH( 'WriteWCS(HDSfile,Pixellation,Projection,'/
     :                   /'CoordSystem)', WCI2_WRITFIT, DID, STATUS )

*    Now initialised
	WCI_INIT = .TRUE.

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI1_INIT', STATUS )

      END
