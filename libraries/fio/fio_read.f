      SUBROUTINE FIO_READ( FD, BUF, NCHAR, STATUS )
*+
*  Name:
*     FIO_READ

*  Purpose:
*     Read sequential record

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_READ( FD, BUF, NCHAR, STATUS )

*  Description:
*     Read a record from the file with the specified file descriptor
*     and return the `used length' of the buffer.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor.
*     BUF = CHARACTER * ( * ) (Returned)
*        Variable to receive the record.
*     NCHAR = INTEGER (Returned)
*        Variable to receive the number of characters read, ignoring
*        trailing spaces.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Check for a valid file descriptor and that the file is open.
*     Read a record from the file using a READ statement.

*  External Routines Used:
*     CHR:
*        CHR_LEN

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     SLW: Sid Wright (Starlink, UCL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     06-Aug-1980 (SLW):
*        Original.
*     10-May-1983 (SLW):
*        Tidy up for Starlink version.
*     16-Feb-1988 (AJC):
*        Rationalize include files.
*     25-Feb-1988 (AJC):
*        Improve prologue.
*     24-May-1989 (AJC):
*        PCA analysis showed that 75% of the PC time was used by
*        CHR_LEN!  Solutions is to put FORTRAN i/o in FIO_READF
*        and call that.  Then call CHR_LEN to get the length.
*        Applications that do not require the length can call
*        FIO_READF.  Idea stolen from SCAR.
*     30-OCT-1991 (PMA):
*        Changed to new style prologue.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     24-AUG-1992 (PMA):
*        Change layout of brackets.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     20-JUL-2001 (AJC):
*        Added Note on EOF terminated records
*     {enter_further_changes_here}

*  Notes:
*     FIO_READ reflects the behaviour of the underlying Fortran I/O system
*     so identical behaviour on different platforms cannot be guaranteed.
*     In particular, platforms differ in the way they handle records which
*     are terminated by EOF rather than newline. Supported platforms behave
*     as follows:
*
*                           Buffer                  STATUS       NCHAR
*          Alpha:       Trailing spaces added      SAI__OK     Used length
*          Solaris:     No trailing spaces added   FIO__EOF        0
*          Linux:       No trailing spaces added   FIO__EOF        0
*
*     In the interests of efficiency, the buffer is not cleared before each
*     READ so it is not possible for FIO_READ to find the used length on
*     Solaris or Linux in this case. The programmer may do so if required.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FD

*  Arguments Returned:
      CHARACTER * ( * ) BUF
      INTEGER NCHAR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Effective length of character string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read
      CALL FIO_READF( FD, BUF, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         NCHAR = CHR_LEN( BUF )
      ELSE
         NCHAR = 0
      ENDIF

      END
