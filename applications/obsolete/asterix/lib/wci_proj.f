      SUBROUTINE WCI_PROJ( RPH, PRJID, STD, STATUS )
*+
*  Name:
*     WCI_PROJ

*  Purpose:
*     Project relative physicals coords to linear x,y

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_PROJ( RPH, PRJID, STD, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     RPH[3] = DOUBLE (given)
*        Relative physical coordinates.
*     PRJID = INTEGER (given)
*        ADI identifier of projection object
*     STD[3] = DOUBLE (returned)
*        Position in native spherical coordinates
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

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
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Jan 1995 (DJA):
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
      INCLUDE 'WCI_CMN'                 ! ASTERIX WCI common block
*       WCS_INIT = LOGICAL (given)
*         WCI class definitions loaded?

*  Arguments Given:
      DOUBLE PRECISION		RPH(3)			! Relative physicals
      INTEGER			PRJID			! Projection object

*  Arguments Returned:
      DOUBLE PRECISION		STD(3)			! Native sphericals

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			RPTR			! Routine address
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. WCI_INIT ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'WCI has not been initialised', STATUS )
      END IF

*  Extract projection routine address
      CALL ADI_CGET0I( PRJID, '.WCIRTN', RPTR, STATUS )

*  If ok, invoke the routine
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Must dereference the pointer
        CALL WCI_PROJ1( %VAL(RPTR), RPH, PRJID, STD, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_PROJ', STATUS )

      END



      SUBROUTINE WCI_PROJ1( RTN, RPH, PRJID, STD, STATUS )
*+
*  Name:
*     WCI_PROJ1

*  Purpose:
*     Project relative physicals coords to native system, internal routine

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WCI_PROJ1( RTN, RPH, PRJID, STD, STATUS )

*  Description:
*     Invoke projector routine to project relative physical coords to
*     native spehericals.

*  Arguments:
*     RTN = EXTERNAL (given)
*        Projector routine
*     RPH[3] = DOUBLE (given)
*        Relative physical coordinates.
*     PRJID = INTEGER (given)
*        ADI identifier of projection object
*     STD[3] = DOUBLE (returned)
*        Position in native spherical coordinates
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'WCI_PAR'

*  Arguments Given:
      EXTERNAL			RTN			! Routine
      DOUBLE PRECISION		RPH(3)			! Relative physicals
      INTEGER			PRJID			! Projection object

*  Arguments Returned:
      DOUBLE PRECISION		STD(3)			! Native sphericals

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local constants:
      INTEGER			MAXPAR			! Max control pars
        PARAMETER		( MAXPAR = 2 )

*  Local variables:
      REAL			PARAM(MAXPAR)		! Control params

      INTEGER			NPAR			! # control params

      LOGICAL			THERE			! Component exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract number of control parameters if valid
      CALL ADI_THERE( PRJID, 'PARAM', THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CGET1R( PRJID, 'PARAM', 2, PARAM, NPAR, STATUS )
      ELSE
        NPAR = 0
      END IF

*  Invoke the projector routine
      CALL RTN( WCI__OPN2S, RPH, NPAR, PARAM, STD, STATUS )

      END
