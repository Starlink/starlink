      SUBROUTINE GCB_SET1<T>( NAME, IFIRST, N, <T>VAL, STATUS )
*+
*  Name:
*     GCB_SET1<T>

*  Purpose:
*     Set a vector <COMM> graphics attribute

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB_SET1<T>( NAME, IFIRST, N, <T>VAL, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NAME = CHARACTER*(*) (given)
*        Name of the graphics attribute
*     IFIRST = INTEGER (given)
*        Number of first element to set
*     N = INTEGER (given)
*        Number of elements to set
*     <T>VAL[] = <TYPE> (given)
*        The value of the attribute elements
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
*     package:gcb, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     RJV: Robert Vallance (ROSAT, University of Birmingham)
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Mar 1996 (DJA):
*        Generic version.
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
      INCLUDE 'GCB_CMN'

*  Arguments Given:
      CHARACTER*(*)		NAME
      INTEGER			IFIRST, N
      <TYPE>			<T>VAL(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*16 		FMT,TYPE
      INTEGER 			DISP,SIZ,J
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over elements to set
      J = 0
      DO INDX = IFIRST, IFIRST + N - 1
        J = J + 1

*    Locate element
        CALL GCB_LOCCOMP( NAME, INDX, DISP, SIZ, FMT, TYPE, STATUS )

*    Set the value
        CALL GCB_SET<T>_SUB( <T>VAL(J), DISP, SIZ, FMT,
     :                       %VAL(G_MEMPTR), STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB_SET1<T>', STATUS )

      END
