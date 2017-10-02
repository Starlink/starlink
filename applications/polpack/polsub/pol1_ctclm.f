      SUBROUTINE POL1_CTCLM( CI, NROW, NCOL, GI, OUT, STATUS )
*+
*  Name:
*     POL1_CTCLM

*  Purpose:
*     Copy the values from a catalogue expression or column to an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CTCLM( CI, NROW, NCOL, GI, OUT, STATUS )

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
*        values in this array will result in rows of VAL__BADR being
*        stored in the returned table.
*     OUT( NROW, NCOL ) = REAL (Returned)
*        The returned values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1998 (DSB):
*        Original version.
*     28-SEP-2017 (DSB):
*        Allow CAT__NOID values in the GI array.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

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
      REAL OUT( NROW, NCOL )

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
               OUT( I, J ) = VAL__BADR

*  Get the value of this column from the current row buffer.
            ELSE
               CALL CAT_EGT0R( GI( J ), OUT( I, J ), NULL, STATUS )

*  Store a Starlink bad value if the value is null.
               IF( NULL ) OUT( I, J ) = VAL__BADR
            END IF

         END DO

      END DO

      END
