      SUBROUTINE POL1_CPCTD( CI, GI, NEL, OUT, NGOOD, STATUS )
*+
*  Name:
*     POL1_CPCTD

*  Purpose:
*     Copy the values from a catalogue expression or column to an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPCTD( CI, GI, NEL, OUT, NGOOD, STATUS )

*  Description:
*     This routine gets NEL values for a given CAT (see SUN/181) expression,
*     column or parameter, derived from rows 1 to NEL of a given catalogue,
*     selection, or index, and stores them in array OUT.

*  Arguments:
*     CI = INTEGER (Given)
*        The CAT identifier for the catalogue, selection or index
*        containing the required data.
*     GI = INTEGER (Given)
*        The CAT identifier for the column, expression or parameter to be
*        evaluated for rows 1 to NEL of the component identified by CI.
*     NEL = INTEGER (Given)
*        The number of rows to copy.
*     OUT( NEL ) = DOUBLE PRECISION (Returned)
*        The returned values.
*     NGOOD = INTEGER (Returned)
*        The number of good values returned in OUT. Returned equal to zero
*        if an error occurs.
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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER CI
      INTEGER GI
      INTEGER NEL

*  Arguments Returned:
      DOUBLE PRECISION OUT( NEL )
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Row index
      LOGICAL NULL               ! Was no value available?
*.

*  Initialise returned value.
      NGOOD = 0

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each row.
      DO I = 1, NEL

*  Get the value from this row.
         CALL CAT_FGT0D( CI, I, GI, OUT( I ), NULL, STATUS )
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Store a Starlink bad value if the value is null.
         IF( NULL ) OUT( I ) = VAL__BADD

*  Increment the number of good values found.
         IF( OUT( I ) .NE. VAL__BADD ) NGOOD = NGOOD + 1

      END DO

 999  CONTINUE

*  If an error has occurred, indicate that no good values were found.
      IF( STATUS .NE. SAI__OK ) NGOOD = 0

      END
