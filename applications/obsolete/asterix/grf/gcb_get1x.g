      SUBROUTINE GCB_GET1<T>( NAME, IFIRST, N, OK, <T>VAL, STATUS )
*+
*  Name:
*     GCB_GET1<T>

*  Purpose:
*     Get vector <COMM> graphics attribute value

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB_GET1<T>( NAME, IFIRST, N, OK, <T>VAL, STATUS )

*  Description:
*     Gets vector <COMM> graphics attribute value from the current GCB.
*     An attribute may or may not have a current value - OK is set
*     accordingly. If the attribute has no value then <T>VAL is not
*     altered.

*  Arguments:
*     NAME = CHARACTER*(*) (given)
*        Name of attribute to be read
*     IFIRST = INTEGER (given)
*        First index of vector attribute to be accessed
*     OK[N] = LOGICAL (returned)
*        Attribute value is set?
*     <T>VAL[N] = <TYPE> (returned)
*        The value of the attribute
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
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      4 Mar 1996 (DJA):
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

*  Global variables :
      INCLUDE 'GCB_CMN'

*  Arguments Given:
      CHARACTER*(*)		NAME
      INTEGER			IFIRST, N

*  Arguments Returned:
      LOGICAL			OK(*)
      <TYPE>			<T>VAL(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			GCB_NULL
        EXTERNAL		GCB_NULL

*  Local Variables:
      CHARACTER*16 		FMT,TYPE

      INTEGER 			DISP,SIZ
      INTEGER 			INDX,J
*.

      OK(1) = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over specified attribute elements
      J=0
      DO INDX = IFIRST, IFIRST + N - 1
        J=J+1

*    Locate named attribute
        CALL GCB_LOCCOMP( NAME, INDX, DISP, SIZ, FMT, TYPE, STATUS )

*    Value is defined?
        OK(J) = .NOT. GCB_NULL( %VAL(G_MEMPTR), DISP, SIZ, STATUS )

*    Extract value if ok
        IF ( OK(J) ) THEN
          CALL GCB_GET<T>_SUB( %VAL(G_MEMPTR), DISP, SIZ, FMT,
     :                         <T>VAL(J), STATUS )
        END IF

*  Next vector element
      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB_GET1<T>', STATUS )

      END
