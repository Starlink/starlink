      SUBROUTINE POL1_CPMD1( NROW, TABLE, IROW, IXLO, IYLO, NX, NY,
     :                       VAR, MAP, STATUS )
*+
*  Name:
*     POL1_CPMD1

*  Purpose:
*     Copy a single row of the table to a 2D map.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPMD1( NROW, TABLE, IROW, IXLO, IYLO, NX, NY, VAR,
*                      MAP, STATUS )

*  Description:
*     This is a service routine for POL1_CPMOD. It copies a single row of
*     the supplied table to a 2D Map. Rows 1 and 2 in the supplied table
*     are assumed to hold the (X,Y) pixel coordinates of the corresponding
*     point within the map at which each value should be placed.

*  Arguments:
*     NROW =  INTEGER (Given)
*        The number of rows in the input catalogue. This equals the
*        number of columns in the table.
*     TABLE( NROW, 8 ) = REAL (Given)
*        The input table.
*     IROW =  INTEGER (Given)
*        The index of the table row to copy.
*     IXLO =  INTEGER (Given)
*        The pixel index of the first column of the map.
*     IYLO =  INTEGER (Given)
*        The pixel index of the first row of the map.
*     NX =  INTEGER (Given)
*        The number of columns in the map.
*     NY =  INTEGER (Given)
*        The number of rows in the map.
*     VAR = LOGICAL (Given)
*        If .TRUE. then the map values represent variances.
*     MAP( NX, NY ) =  DOUBLE PRECISION (Given)
*        The output map.
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
      INTEGER IXLO
      INTEGER IYLO
      INTEGER NX
      INTEGER NY
      LOGICAL VAR

*  Arguments Returned:
      DOUBLE PRECISION MAP( NX, NY )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IX
      INTEGER IY
      INTEGER J
      REAL VAL
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Fill the map with bad values.
      DO IX = 1, NX
         DO IY = 1, NY
            MAP( IX, IY ) = VAL__BADD
         END DO
      END DO

*  Handle variances cases first. The input row is assumed to be standard
*  deviation and therefore needs squaring.
      IF( VAR ) THEN

*  Loop round all catalogue rows (i.e. table columns).
         DO J = 1, NROW

*  Get the pixel coordinates for the vector described by the current
*  catalogue row. Convert them to integer GRID indices within the map.
            IX = NINT( TABLE( J, 1 ) + 0.5 ) + 1 - IXLO
            IY = NINT( TABLE( J, 2 ) + 0.5 ) + 1 - IYLO

*  Check it walls within the map.
            IF( IX .GE. 1 .AND. IX .LE. NX .AND.
     :          IY .GE. 1 .AND. IY .LE. NY ) THEN

*  Place the required squared value in the above pixel of the map.
               VAL = TABLE( J, IROW )
               IF( VAL .NE. VAL__BADR ) THEN
                  MAP( IX, IY ) = VAL**2
               ELSE
                  MAP( IX, IY ) = VAL__BADD
               END IF
            END IF
         END DO

*  Now handle data cases. The input row is assumed to be the required
*  output value.
      ELSE
         DO J = 1, NROW
            IX = NINT( TABLE( J, 1 ) + 0.5 ) + 1 - IXLO
            IY = NINT( TABLE( J, 2 ) + 0.5 ) + 1 - IYLO
            IF( IX .GE. 1 .AND. IX .LE. NX .AND.
     :          IY .GE. 1 .AND. IY .LE. NY ) THEN
               VAL = TABLE( J, IROW )
               IF( VAL .NE. VAL__BADR ) THEN
                  MAP( IX, IY ) = VAL
               ELSE
                  MAP( IX, IY ) = VAL__BADD
               END IF
            END IF
         END DO
      END IF

      END
