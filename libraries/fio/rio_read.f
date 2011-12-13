      SUBROUTINE RIO_READ( FD, RECNO, NCHAR, BUF, STATUS )
*+
*  Name:
*     RIO_READ

*  Purpose:
*     Read record from direct access file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RIO_READ( FD, RECNO, NCHAR, BUF, STATUS )

*  Description:
*     Read the specified unformatted record from the file with the
*     given file descriptor.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor.
*     RECNO = INTEGER (Given)
*        Expression giving the number of the record to be read.
*     NCHAR = INTEGER (Given)
*        Expression giving the buffer size
*     BUF = BYTE( NCHAR ) (Returned)
*        A byte array to receive the record.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Check for a valid file descriptor and that the file is open.
*     -  Read a record from the file using a READ statement.
*     -  If there are any errors in reading the file, call FIO_SERR to
*        report the error.

*  Implementation Deficiencies:
*     Uses the non-standard BYTE variable type.

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
*     AJC: Alan Chipperfield (Starlink, RAL)
*     KFH: Ken Hartley (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     SLW: Sid Wright (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     06-Aug-1980: Original. (SLW)
*     10-May-1983: Tidy up for Starlink version. (SLW)
*     17-Feb-1988: Rationalize include files  (AJC)
*     25-Feb-1988: Improve prologue
*        and correct use of NCHAR  (AJC)
*     01-Nov-1991: Change to new style prologue (KFH)
*        Insert IMPLICIT NONE (KFH)
*        Change variable vmserr to ioerr (KFH)
*        Change fac_$name to fac1_name (KFH)
*        Replace tabs with spaces in end-of-line comments (KFH)
*     26-FEB-1992 (PMA):
*        Tidy up prologue.
*        The statement   CALL FIO1_CHKFD( FD, RFD, STATUS )
*        was commented out. Remove the comment character.
*     17-MAR-1992 (PMA):
*        Remove the assignment to RFD and the check of its value as this
*        is done by the call to FIO1_CHKFD.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     24-AUG-1992 (PMA):
*        Change the layout of brackets.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     29-NOV-1994 (AJC):
*        Add END= clause as Solaris gives that if RECNO beyond end of file
*      5-DEC-1996 (AJC):
*        Remove both END and ERR clause as all systems behave differently
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO Symbolic Constants
      INCLUDE 'FIO_SYS'          ! FIO Internal symbols
      INCLUDE 'FIO_ERR'          ! FIO error numbers

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'       ! File descriptor tables
*        FUNIT( FIO__MXFIL ) = INTEGER (Read)
*           Fortran unit number for file

*  Arguments Given:
      INTEGER FD
      INTEGER RECNO
      INTEGER NCHAR

*  Arguments Returned:
      BYTE BUF( NCHAR )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IOERR              ! I/O Error number
      INTEGER RFD                ! Relative file descriptor

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check for valid File Descriptor
      CALL FIO1_CHKFD( FD, RFD, STATUS )

*  Read a record.
      IF ( STATUS .EQ. SAI__OK ) THEN
         READ( FUNIT( RFD ), REC=RECNO, IOSTAT=IOERR )
     :    BUF
         IF ( IOERR .NE. 0 ) CALL FIO_SERR( IOERR, STATUS )
      END IF

      END
