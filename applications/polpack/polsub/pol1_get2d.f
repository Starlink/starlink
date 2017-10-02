      SUBROUTINE POL1_GET2D( NCOL, NROW, ARRAY, ICOL, IROW, VALUE,
     :                       STATUS )
*+
*  Name:
*     POL1_GET2D

*  Purpose:
*     Get a single value from a 2D array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_GET2D( NCOL, NROW, ARRAY, ICOL, IROW, VALUE, STATUS )

*  Description:
*     Get a single value from a 2-dimensional array. A value of VAL__BADR
*     is returned if the requested element is outside the array.

*  Arguments:
*     NCOL =  INTEGER (Given)
*        Number of columns in array.
*     NROW =  INTEGER (Given)
*        Number of rows in array.
*     ARRAY( NCOL, NROW ) =  REAL (Given)
*        The array
*     ICOL = INTEGER (Given)
*        The column index of hte require value
*     IROW = INTEGER (Given)
*        The row index of hte require value
*     VALUE = REAL (Returned)
*        The returned value - ARRAY( ICOL, IROW )
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     28-SEP-2017 (DSB):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants
      INCLUDE 'PRM_PAR'     ! VAL__ constants

*  Arguments Given:
      INTEGER NCOL
      INTEGER NROW
      REAL ARRAY( NCOL, NROW )
      INTEGER ICOL
      INTEGER IROW

*  Arguments Given:
      REAL VALUE

*  Status:
      INTEGER STATUS
*.


      VALUE = VAL__BADR

      IF( STATUS .NE. SAI__OK ) RETURN

      IF( IROW .GE. 1 .AND. IROW .LE. NROW .AND.
     :    ICOL .GE. 1 .AND. ICOL .LE. NCOL ) THEN
         VALUE = ARRAY( ICOL, IROW )
      END IF

      END
