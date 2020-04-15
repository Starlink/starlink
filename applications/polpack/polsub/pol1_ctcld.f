      SUBROUTINE POL1_CTCLD( CI, NROW, NCOL, GI, OUT, STATUS )
*+
*  Name:
*     POL1_CTCLD

*  Purpose:
*     Copy the values from a catalogue expression or column to a
*     _DOUBLE array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CTCLD( CI, NROW, NCOL, GI, OUT, STATUS )

*  Description:
*     This routine copies NROW values from each of NCOL columns in the
*     catalogue identified by CI, to the OUT array. Each column is put in
*     a separate row in the OUT array.

*  Arguments:
*     CI = INTEGER (Given)
*        The CAT identifier for the catalogue, selection or index
*        containing the required data.
*     NROW = INTEGER (Given)
*        The number of rows to be read for each column (starting at row 1).
*     NCOL = INTEGER (Given)
*        The number of columns to read.
*     GI( NCOL ) = INTEGER (Given)
*        The CAT identifiers for the columns to be read. Any CAT__NOID
*        values in this array will result in rows of VAL__BADD being
*        stored in the returned table.
*     OUT( NROW, NCOL ) = DOUBLE PRECISION (Returned)
*        The returned values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2020 EastAsian Observatory

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     13-APR-2020 (DSB):
*        Original version, based on the _REAL equivalent pol1_ctclm.f.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'CAT_PAR'          ! CAT__ constants

*  Arguments Given:
      INTEGER CI
      INTEGER NROW
      INTEGER NCOL
      INTEGER GI( NCOL )

*  Arguments Returned:
      DOUBLE PRECISION OUT( NROW, NCOL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Row index
      INTEGER J                  ! Column index
      LOGICAL NULL               ! Was no value available?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each row.
      DO I = 1, NROW

*  Read this row into the CAT current row buffer.
         CALL CAT_RGET( CI, I, STATUS )

*  Loop round each column.
         DO J = 1, NCOL

*  IF the column identifier is null, return bad values.
            IF( GI( J ) .EQ. CAT__NOID ) THEN
               OUT( I, J ) = VAL__BADD

*  Get the value of this column from the current row buffer.
            ELSE
               CALL CAT_EGT0D( GI( J ), OUT( I, J ), NULL, STATUS )

*  Store a Starlink bad value if the value is null.
               IF( NULL ) OUT( I, J ) = VAL__BADD
            END IF

         END DO

      END DO

      END
