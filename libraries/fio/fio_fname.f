      SUBROUTINE FIO_FNAME( FD, FNAME, STATUS )
*+
*  Name:
*     FIO_FNAME

*  Purpose:
*     Get the full file name of a file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_FNAME( FD, FNAME, STATUS )

*  Description:
*     Get the full name of the file with the specified file
*     descriptor.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor.
*     FNAME = CHARACTER * ( * ) (Returned)
*        Variable to contain the full file name of the file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Use Fortran INQUIRE statement

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     30-JUL-1980 (SLW):
*        Original.
*     10-MAY-1983 (SLW):
*        Tidy up for Starlink version.
*     13-OCT-1986 (AJC):
*        fio_$unit renamed fio_unit
*     16-FEB-1988 (AJC):
*        Rationalize include files
*     25-FEB-1988 (AJC):
*        Improve prologue
*     29-OCT-1991 (PMA):
*        Changed references to FIO_$xxxxx to FIO1_xxxxx.
*        Added check of STATUS on entry.
*        Renamed variable VMSERR to SYSERR.
*     28-JAN-1992 (PMA):
*        Change RETURN statement to GOTO end of code.
*     12-MAR-1992 (PMA):
*        Change the call to FIO1_SERR to FIO_SERR.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     16-JUN-1992 (PMA):
*        Change the variable SYSERR to IOERR
*     24-AUG-1992 (PMA):
*        Change the layout of brackets.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO Symbolic Constants

*  Arguments Given:
      INTEGER FD                 ! File descriptor

*  Arguments Returned:
      CHARACTER * ( * ) FNAME    ! Full file name

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER UNIT               ! Unit number for file
      INTEGER IOERR              ! Operating system error number

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FIO_UNIT( FD, UNIT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         INQUIRE( UNIT=UNIT, NAME=FNAME, IOSTAT=IOERR, ERR=1 )
      ENDIF
      GOTO 999

*  Error on INQUIRE statement.
    1 CALL FIO_SERR( IOERR, STATUS )

  999 CONTINUE
      END
