      SUBROUTINE ARR_ELEM1<T>( PTR, DIM, INDEX, VAL, STATUS )
*+
*  Name:
*     ARR_ELEM1<T>

*  Purpose:
*     Returns element of <TYPE> array given pointer and index

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_ELEM1<T>( PTR, DIM, INDEX, VAL, STATUS )

*  Description:
*     Returns element of <TYPE> array given pointer and index. The index
*     is checked for legality.

*  Arguments:
*     PTR = INTEGER (given)
*        Address of the array
*     DIM = INTEGER (given)
*        Size of the array
*     INDEX = INTEGER (given)
*        Index of the element whose value is required
*     VAL = <TYPE> (returned)
*        The value of the requested array element
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

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     8 Mar 1995 (DJA):
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
      INTEGER			PTR			! Address of array
      INTEGER			DIM			! Its dimension
      INTEGER			INDEX			! Element wanted

*  Arguments Returned:
      <TYPE>			VAL			! Element value

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Index is legal?
        IF ( (INDEX.GT.0) .AND. (INDEX.LE.DIM) ) THEN
          CALL ARR_ELEM1<T>_INT( %VAL(PTR), INDEX, VAL )

*    Negative index?
        ELSE IF ( INDEX .LE. 0 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'AST_ERR: zero or negative array index',
     :                  STATUS )

        ELSE IF ( INDEX .GT. DIM ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP(' ','AST_ERR: array index exceeds size of array',
     :                 STATUS)
        END IF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'ARR_ELEM1<T>', STATUS )
        END IF

      END IF

      END



      SUBROUTINE ARR_ELEM1<T>_INT( ARRAY, INDEX, VAL )
*+
*  Name:
*     ARR_ELEM1<T>

*  Purpose:
*     Extract array element

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_ELEM1<T>_INT( ARRAY, INDEX, VAL )

*  Description:
*     {routine_description}

*  Arguments:
*     ARRAY[] = <TYPE> (given)
*        Array of values
*     INDEX = INTEGER (given)
*        Index of the element whose value is required
*     VAL = <TYPE> (returned)
*        Size of the array

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Notes:
*     {routine_notes}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     8 Mar 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      <TYPE>			ARRAY(*)
      INTEGER			INDEX

*  Arguments Returned:
      <TYPE>			VAL
*.

      VAL = ARRAY(INDEX)

      END
