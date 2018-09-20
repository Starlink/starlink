      SUBROUTINE KPG1_NUMFL( FD, COMENT, NLINES, NCOMS, NBLANK, STATUS )
*+
*  Name:
*     KPG1_NUMFL

*  Purpose:
*     Counts the number of lines in a text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NUMFL( FD, COMENT, NLINES, NCOMS, NBLANK, STATUS )

*  Description:
*     This routine counts the number of non-comment, comment, and blank
*     lines in a text file.  Comment lines are those beginning, with
*     any of the characters passed in the COMENT argument (normally !
*     and #).  Here 'beginning' means the first non-blank character.

*  Arguments:
*     FD = INTEGER (Given)
*        Fortran file identifier.
*     COMENT = CHARACTER * ( * ) (Given)
*        A comma-separated list of comment characters.  If any line of
*        the file begins with one of these, the line is treated as a
*        comment line.
*     NLINES = INTEGER (Returned)
*        The number of non-comment lines in the file.
*     NCOMS = INTEGER (Returned)
*        The number of comment lines in the file.
*     NCOMS = INTEGER (Returned)
*        The number of blank lines in the file.
*     STATUS = INTEGER (Given & Returned)
*        The global status.

*  Notes:
*     -  The file is rewound, but not closed on exit.

*  Prior Requirements:
*     -  The Fortran text file must already be opened.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 November 5 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO__ error constants

*  Arguments Given:
      INTEGER FD
      CHARACTER * ( * ) COMENT

*  Arguments Returned:
      INTEGER NLINES
      INTEGER NCOMS
      INTEGER NBLANK

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_INSET          ! A string is a member of a given set?
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER * ( 80 ) BUFFER  ! Text buffer for reading file
      INTEGER NCHAR              ! Number of characters read from file

*.

*  Initialise returned values.
      NLINES = 0
      NCOMS = 0
      NBLANK = 0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start a new error context.
      CALL ERR_MARK

*  Determine the number of points in the list.  Loop round until an
*  error encountered (this will happen when the end of file is reached,
*  if not before).
   10 CONTINUE          ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Read a record of the text file.
         CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

*  Remove leading blanks.
         CALL CHR_LDBLK( BUFFER )
         NCHAR = CHR_LEN( BUFFER( :NCHAR ) )

*  Watch for an error.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Is the line a comment?
            IF ( CHR_INSET( COMENT, BUFFER( 1:1 ) ) ) THEN
               NCOMS = NCOMS + 1

*  Is the line blank?
            ELSE IF ( NCHAR .EQ. 0 ) THEN
               NBLANK = NBLANK + 1

*  It must therefore be a data line.
            ELSE
               NLINES = NLINES + 1
            END IF
         END IF

*  End of 'DO WHILE' loop.
         GO TO 10
      END IF

*  If an end-of-file error has been reported, annul it.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

*  CLose the new error context.
      CALL ERR_RLSE

*  Rewind the file.
      CALL FIO_RWIND( FD, STATUS )

      END
