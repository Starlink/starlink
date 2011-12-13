      SUBROUTINE FIO_READF( FD, BUF, STATUS )
*+
*  Name:
*     FIO_READF

*  Purpose:
*     Fast read sequential record

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_READF( FD, BUF, STATUS )

*  Description:
*     Read a record from the file with the specified file descriptor.
*     Unlike FIO_READ, this routine does not return the `used length'
*     of the buffer and is therefore faster.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor.
*     BUF = CHARACTER * ( * ) (Returned)
*        Variable to receive the record.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Check for a valid file descriptor and that the file is open.
*     Read a record from the file using a READ statement.

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
*     JHF: Jon Fairclough (IPMAF, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     24-May-1989 (JHF):
*        Original.
*     13-Jun-1989 (AJC):
*        Changed name $READ to READF and improved description.
*     29-OCT-1991 (PMA):
*        Changed references to FIO_$xxxxx to FIO1_xxxxx.
*     12-FEB-1992 (PMA):
*        Changed RETURN statement to GOTO
*     9-MAR-1992 (PMA):
*        Add error reporting with EMS.
*        Change variable VMSERR to SYSERR.
*     12-MAR-1992 (PMA):
*        Change the call to FIO1_SERR to FIO_SERR.
*     17-MAR-1992 (PMA):
*        Replace several lines of code with a call to FIO1_CHKFD.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     16-JUN-1992 (PMA):
*        Change the variable SYSERR to IOERR
*     24-AUG-1992 (PMA):
*        Change layout of brackets.
*     8-SEP-1992 (PMA):
*        Report an error when setting the status to FIO__EOF.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     20-JUL-2001 (AJC):
*        Added Note on EOF terminated records
*     {enter_further_changes_here}

*  Notes:
*     FIO_READF reflects the behaviour of the underlying Fortran I/O system
*     so identical behaviour on different platforms cannot be guaranteed.
*     In particular, platforms differ in the way they handle records which
*     are terminated by EOF rather than newline. Supported platforms behave
*     as follows:
*
*                           Buffer                  STATUS
*          Alpha:       Trailing spaces added      SAI__OK
*          Solaris:     No trailing spaces added   FIO__EOF
*          Linux:       No trailing spaces added   FIO__EOF

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO Symbolic Constants
      INCLUDE 'FIO_SYS'          ! FIO Internal symbols
      INCLUDE 'FIO_ERR'          ! FIO Error numbers

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'       ! Information on FIO files
*        FREE( FIO__MXFIL ) = LOGICAL (Read)
*           File descriptor available ?
*        FUNIT( FIO__MXFIL ) = INTEGER (Read)
*           Fortran unit number for file.

*  Arguments Given:
      INTEGER FD

*  Arguments Returned:
      CHARACTER * ( * ) BUF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER RFD                ! Relative file descriptor
      INTEGER IOERR              ! Fortran IOSTAT error number

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check for valid File Descriptor
      CALL FIO1_CHKFD( FD, RFD, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         READ( FUNIT( RFD ), FMT=1, END=10, ERR=20, IOSTAT=IOERR ) BUF
    1    FORMAT( A )
      ENDIF
      GOTO 999

*  Handle end of file condition. IOERR will be equal to -1. Just allow
*  FIO_SERR to translate this to FIO__EOF and report an error.
   10 CONTINUE

*  Handle error condition.
   20 CALL FIO_SERR( IOERR, STATUS )

  999 CONTINUE
      END
