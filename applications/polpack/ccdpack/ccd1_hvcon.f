      LOGICAL FUNCTION CCD1_HVCON( LINE, STATUS )
*+
*  Name:
*     CCD1_HVCON

*  Purpose:
*     To check if a given line is terminated by a continuation
*     character.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CCD1_HVCON( LINE )

*  Description:
*     The routine checks to see if the last non - blank character
*     in the given string is the continuation character.

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        The character buffer which is to be checked for the presence of
*        a continuation character
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     CCD1_HVCON = LOGICAL
*        Set true if a continuation character has been located.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1992 (PDRAPER):
*        Original version.
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

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Length of string exluding trailing
                                 ! blanks

*  Local Variables:
      INTEGER IAT                ! Position of last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the position of the last non-blank character.
      IAT = MAX( 1, CHR_LEN( LINE ) )

*  Is it the continuation character ?
      IF ( LINE( IAT : IAT ) .EQ. '-') THEN
         CCD1_HVCON = .TRUE.
      ELSE
         CCD1_HVCON = .FALSE.
      END IF

      END
