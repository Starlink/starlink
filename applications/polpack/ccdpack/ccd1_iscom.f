      LOGICAL FUNCTION CCD1_ISCOM( LINE, STATUS )
*+
*  Name:
*     CCD1_ISCOM

*  Purpose:
*     To check whether a given line is a comment line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CCD1_ISCOM( LINE, STATUS )

*  Description:
*     The routine simply checks the first character to see if it is an
*     `!' or a `#'. The routine also checks to see if the line is
*     completly blank, if so it is treat like a comment line.

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        The line which is to be checked.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     CCD1_ISCOM = LOGICAL
*        Set true if the line is to be treat as a comment line.

*  Notes:
*     The input line should have its leading blanks removed.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1992 (PDRAPER):
*        Original Version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) LINE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set default return
      CCD1_ISCOM = .FALSE.

*  Check for comment delimeters.
      IF ( LINE( 1 : 1 ) .EQ. '!' ) THEN
          CCD1_ISCOM = .TRUE.
      ELSE IF ( LINE( 1 : 1 ) .EQ. '#' ) THEN
          CCD1_ISCOM = .TRUE.
      ELSE IF ( LINE( 1: 1 ) .EQ. ' ' ) THEN
          CCD1_ISCOM = .TRUE.
      END IF

      END
