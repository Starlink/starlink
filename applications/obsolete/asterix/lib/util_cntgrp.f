      SUBROUTINE UTIL_CNTGRP( N, INDEX, MAXGRP, STATUS )
*+
*  Name:
*     UTIL_CNTGRP

*  Purpose:
*     Find maximum group number

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UTIL_CNTGRP( N, INDEX, MAXGRP, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     N = INTEGER (given)
*        Number of group elements
*     INDEX[N] = INTEGER (given)
*        Group index array
*     MAXGRP = INTEGER (returned)
*        Number of groups in index array
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
*     UTIL Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/util.html

*  Keywords:
*     package:util, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     3 Apr 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			N, INDEX(*)

*  Arguments Returned:
      INTEGER			MAXGRP

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			MINGRP			! Minimum grp number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find range, discarding lower value
      CALL ARR_RANG1I( N, INDEX, MINGRP, MAXGRP, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'UTIL_CNTGRP', STATUS )

      END
