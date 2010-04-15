      SUBROUTINE POL1_DFTAB( MXBUF, BUFFER, BUFLEN, ILINE, EOF, STATUS )
*+
*  Name:
*     POL1_DFTAB

*  Purpose:
*     Returns the next line in a default control table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_DFTAB( MXBUF, BUFFER, BUFLEN, ILINE, EOF, STATUS )

*  Description:
*     This routine returned the next line in the default control table.

*  Arguments:
*     MXBUF = INTEGER (Given)
*        The maximum length of a line.
*     BUFFER = CHARACTER * ( MXBUF ) (Returned)
*        The control table line.
*     BUFLEN = INTEGER (Returned)
*        The actual number of characters in the control table line.
*     ILINE = INTEGER (Given and Returned)
*        The line number of the returned line. Note that this value
*        should be initialised to 0 on the first call to this routine,
*        the value will be incremented on subsequent calls.
*     EOF = LOGICAL (Returned)
*        If true then there is no control table line to return.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berru (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-APR-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER MXBUF

*  Arguments Given and Returned:
      INTEGER ILINE

*  Arguments Returned:
      CHARACTER BUFFER*(*)
      INTEGER BUFLEN
      LOGICAL EOF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER NLINE              ! No. of lines in default control table
      PARAMETER ( NLINE = 10 )

      INTEGER MXLIN              ! Max. length of a table line
      PARAMETER ( MXLIN = 20 )

*  Local Variables:
      CHARACTER TABLE( NLINE )*(MXLIN) ! The table

*  Define the default control table information.
      DATA TABLE / 'ANGROT?  PPCKANGR',
     :             'ANLANG?  PPCKANLA',
     :             'EPS?     PPCKEPS',
     :             'FILTER?  PPCKFILT',
     :             'IMGID?   PPCKIMID',
     :             'RAY?     PPCKRAY',
     :             'STOKES?  PPCKSTOK',
     :             'T?       PPCKT',
     :             'WPLATE?  PPCKWPLT',
     :             'VERSION? PPCKVERS'/

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Return the required line of the defrault control table.
      IF( ILINE .LT. NLINE ) THEN
         ILINE = ILINE + 1
         BUFLEN = MIN( MXBUF, CHR_LEN( TABLE( ILINE ) ) )
         BUFFER = TABLE( ILINE )( : BUFLEN )
         EOF = .FALSE.
      ELSE
         EOF = .TRUE.
         BUFLEN = 1
         BUFFER = ' '
      END IF

      END
