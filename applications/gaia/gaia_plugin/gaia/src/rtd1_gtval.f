      SUBROUTINE RTD1_GTVAL( ARRAY, SIZE, INDEX, VALUE, STATUS )
*+
*  Name:
*     RTD1_GTVAL

*  Purpose:
*     Returns a value from an array of double precision numbers.

*  Language:
*     Fortran-77

*  Invocation:
*     CALL RTD1_GTVAL( ARRAY, SIZE, INDEX, VALUE, STATUS )

*  Description:
*     This routine returns the specified value from an array of double
*     precision numbers. The index into the array is verified to lie
*     within the bounds of the array, otherwise the given value is
*     returned unchanged.

*  Arguments:
*     ARRAY( SIZE ) = DOUBLE PRECISION (Given)
*        The array of values to index.
*     SIZE = INTEGER (Given)
*        The number of elements in ARRAY.
*     INDEX = INTEGER (Given)
*        The index of ARRAY whose value is to be returned.
*     VALUE = DOUBLE PRECISION (Given and Returned).
*         The value of the specified array element, or the input value. 
*     STATUS = INTEGER (Given)
*        The global status.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     12-SEP-1995 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      
*  Arguments Given:
      INTEGER SIZE
      DOUBLE PRECISION ARRAY( SIZE )
      INTEGER INDEX

*  Arguments Given and Returned:
      DOUBLE PRECISION VALUE
      
*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  Check that the given index is in bounds, if not do nothing.
      IF ( INDEX .GT. 0 .AND. INDEX .LE. SIZE ) THEN 
         VALUE = ARRAY( INDEX )
      END IF
      END

