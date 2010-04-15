      SUBROUTINE POL1_PUT0R( PI, VALUE, NULFLG, STATUS )
*+
*  Name:
*     POL1_PUT0R

*  Purpose:
*     Put a value to a scalar part (field or column) in a catalogue.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL POL1_PUT0R( PI, VALUE, NULFLG, STATUS )

*  Description:
*     This a wrapper round CAT_PUT0R which puts a value to a scalar part
*     (field or column) of a catalogue. This wrapper prevents an error
*     occuring if the supplied identifier is null.

*  Arguments:
*     PI  =  INTEGER (Given)
*        Part identifier.
*     VALUE  =  REAL (Given)
*        Value to PUT to the part.
*     NULFLG  =  LOGICAL (Given)
*        A flag indicating whether or not the value is null or not:
*        .TRUE.  - The value is null,
*        .FALSE. - The value is not null.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-DEC-2000 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'CAT_PAR'          ! CAT_ constants

*  Arguments Given:
      INTEGER PI
      REAL VALUE
      LOGICAL NULFLG

*  Status:
      INTEGER STATUS             ! Global status

*.

*  If the supplied column identifier is not null, store the value.
      IF( PI .NE. CAT__NOID ) THEN
         CALL CAT_PUT0R( PI, VALUE, NULFLG, STATUS )
      END IF

      END
