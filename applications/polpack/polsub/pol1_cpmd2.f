      SUBROUTINE POL1_CPMD2( IXLO, IYLO, NX, NY, MAP, VAR, NROW, IROW,
     :                       TABLE, STATUS )
*+
*  Name:
*     POL1_CPMD2

*  Purpose:
*     Copy a single row of the table to a 2D map.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPMD2( IXLO, IYLO, NX, NY, MAP, VAR, NROW, IROW, TABLE,
*                      STATUS )

*  Description:
*     This is a service routine for POL1_CPMOD. It copies the values form
*     a 2D map into a single row of the supplied table. Rows 1 and 2 in the
*     supplied table are assumed to hold the (X,Y) pixel coordinates of the
*     corresponding point within the map at which each value has been placed.

*  Arguments:
*     IXLO =  INTEGER (Given)
*        The pixel index of the first column of the map.
*     IYLO =  INTEGER (Given)
*        The pixel index of the first row of the map.
*     NX =  INTEGER (Given)
*        The number of columns in the map.
*     NY =  INTEGER (Given)
*        The number of rows in the map.
*     MAP( NX, NY ) =  DOUBLE PRECISION (Given)
*        The output map.
*     VAR = LOGICAL (Given)
*        If .TRUE. then the map values represent variances.
*     NROW =  INTEGER (Given)
*        The number of rows in the input catalogue. This equals the
*        number of columns in the table.
*     IROW =  INTEGER (Given)
*        The index of the table row to copy.
*     TABLE( NROW, 8 ) = REAL (Given)
*        The input table.
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
      INTEGER IXLO
      INTEGER IYLO
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION MAP( NX, NY )
      LOGICAL VAR
      INTEGER NROW
      INTEGER IROW

*  Arguments Given and Returned:
      REAL TABLE( NROW, 8 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IX
      INTEGER IY
      INTEGER J
      DOUBLE PRECISION DVAL
      REAL RVAL
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Handle variances cases first. The input map is assumed to hold
*  variances and therefore needs square-rooting to get standard deviations.
      IF( VAR ) THEN

*  Loop round all catalogue rows (i.e. table columns).
         DO J = 1, NROW

*  Get the pixel coordinates for the vector described by the current
*  catalogue row. Convert them to integer GRID indices within the map.
            IX = NINT( TABLE( J, 1 ) + 0.5 ) + 1 - IXLO
            IY = NINT( TABLE( J, 2 ) + 0.5 ) + 1 - IYLO

*  If it is within the map, get its value and turn the variance into a
*  standard deviation (if it is good).
            IF( IX .GE. 1 .AND. IX .LE. NX .AND.
     :          IY .GE. 1 .AND. IY .LE. NY ) THEN
               DVAL = MAP( IX, IY )
               IF( DVAL .NE. VAL__BADD ) THEN
                  RVAL = SQRT( DVAL )
               ELSE
                  RVAL = VAL__BADR
               END IF

*  If is is outside the map, store a bad value in the table.
            ELSE
               RVAL = VAL__BADR
            END IF

*  Place the required value in the current element of the table row.
            TABLE( J, IROW ) = RVAL

         END DO

*  Now handle data cases. The input row is assumed to be the required
*  output value.
      ELSE
         DO J = 1, NROW
            IX = NINT( TABLE( J, 1 ) + 0.5 ) + 1 - IXLO
            IY = NINT( TABLE( J, 2 ) + 0.5 ) + 1 - IYLO

            IF( IX .GE. 1 .AND. IX .LE. NX .AND.
     :          IY .GE. 1 .AND. IY .LE. NY ) THEN
               DVAL = MAP( IX, IY )
               IF( DVAL .NE. VAL__BADD ) THEN
                  RVAL = DVAL
               ELSE
                  RVAL = VAL__BADR
               END IF
            ELSE
               RVAL = VAL__BADR
            END IF

            TABLE( J, IROW ) = RVAL
         END DO
      END IF

      END
