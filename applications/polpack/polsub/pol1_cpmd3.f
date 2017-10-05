      SUBROUTINE POL1_CPMD3( NROW, TABLE, IROW, OUT, STATUS )
*+
*  Name:
*     POL1_CPMD3

*  Purpose:
*     Copy a single row of the table to a 2D map.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPMD3( NROW, TABLE, IROW, OUT, STATUS )

*  Description:
*     This is a service routine for POL1_CPMOD. It copies a single row of
*     the supplied _REAL table to a 1D output _DOUBLE array.

*  Arguments:
*     NROW =  INTEGER (Given)
*        The number of rows in the input catalogue. This equals the
*        number of columns in the table.
*     TABLE( NROW, 8 ) = REAL (Given)
*        The input table.
*     IROW =  INTEGER (Given)
*        The index of the table row to copy.
*     OUT( NROW ) =  DOUBLE PRECISION (Returned)
*        The output array.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     29-SEP-2017 (DSB):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants
      INCLUDE 'AST_PAR'     ! AST symbolic constants
      INCLUDE 'PRM_PAR'     ! VAL__ constants

*  Arguments Given:
      INTEGER NROW
      REAL TABLE( NROW, 8 )
      INTEGER IROW

*  Arguments Returned:
      DOUBLE PRECISION OUT( NROW )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER J
      REAL VAL
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

      DO J = 1, NROW
         VAL = TABLE( J, IROW )
         IF( VAL .NE. VAL__BADR ) THEN
            OUT( J ) = VAL
         ELSE
            OUT( J ) = VAL__BADD
         END IF
      END DO

      END
