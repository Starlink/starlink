      SUBROUTINE DYN_MAP<T>( NDIM, DIMS, PTR, STATUS )
*+
*  Name:
*     DYN_MAP<T>

*  Purpose:
*     Allocate a dynamic array of specified dimensions of type <TYPE>

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN_MAP<T>( [p]... )

*  Description:
*     Allocate a dynamic array of specified dimensions of type <TYPE>.
*     The supplied dimensions are checked and then the internal allocation
*     routine invoked.

*  Arguments:
*     NDIM = INTEGER (given)
*        Dimensionality of required dynamic array
*     DIMS[] = INTEGER (given)
*        Dimensions of the required dynamic array
*     PTR = INTEGER (returned)
*        Address of the newly allocated dynamic memory section
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
*     DYN Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/dyn.html

*  Keywords:
*     package:dyn, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Mar 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

*  Arguments Given:
      INTEGER			NDIM			! See above
      INTEGER			DIMS(*)			!

*  Arguments Returned:
      INTEGER			PTR			!

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check number of dimensions
      IF ( (NDIM.GT.0) .AND. (NDIM.LE.ADI__MXDIM) ) THEN

*    Get pointer
        CALL DYN_MAP( NDIM, DIMS, '_<HTYPE>', PTR, STATUS )

*  Otherwise report error
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Number of dimensions invalid', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DYN_MAP<T>', STATUS )

      END
