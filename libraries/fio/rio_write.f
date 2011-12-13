      SUBROUTINE RIO_WRITE( FD, RECNO, NCHAR, BUF, STATUS )
*+
*  Name:
*     RIO_WRITE

*  Purpose:
*     Write a record to a direct access file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RIO_WRITE( FD, RECNO, NCHAR, BUF, STATUS )

*  Description:
*     Write the specified record number, unformatted, to the file with
*     the specified file descriptor.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor.
*     RECNO = INTEGER (Given)
*        Expression giving the number of the record to be written.
*     NCHAR = INTEGER (Given)
*        Expression giving the buffer size.
*     BUF = BYTE( NCHAR ) (given)
*        A byte array containing the data to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Algorithm:
*     Check for a valid file descriptor and that the file is open.
*     Write a record to the file using a WRITE statement.

*  Implementation Deficiencies:
*     The use of BYTE is non-standard Fortran.

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
*     06-Aug-1980 (SLW):
*        Original.
*     10-May-1983 (SLW):
*        Tidy up for Starlink version.
*     17-Feb-1988 (AJC):
*        Rationalize include files.
*     25-Feb-1988: Improve prologue  (AJC)
*     01-Nov-1991: Change to new style prologue (KFH)
*        Insert IMPLICIT NONE (KFH)
*        Change variable vmserr to ioerr (KFH)
*        Change fac_$name to fac1_name (KFH)
*        Replace tabs with spaces in end-of-line comments (KFH)
*     27-FEB-1992 (PMA):
*        Tidy up prologue.
*        Remove the comment on the statement
*           CALL FIO1_CHKFD( FD, RFD, STATUS )
*     17-MAR-1992 (PMA):
*        Replace several lines of code with a call to FIO1_CHKFD.
*        (That must have been why there was a commented out call to
*        FIO1_CHKFD).
*        Add error reporting with EMS.
*      2-APR-1992 (PMA):
*        Remove unused variable RECSZ.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     2-JUL-1992 (PMA):
*        Remove routine name from error reports.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO symbolic constants
      INCLUDE 'FIO_SYS'          ! FIO internal symbols
      INCLUDE 'FIO_ERR'          ! FIO error numbers

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'       ! File descriptor tables
*        FACMOD( FIO__MXFIL ) = CHARACTER * ( FIO__SZMOD ) (Read)
*           File access mode
*        FUNIT( FIO__MXFIL ) = INTEGER (Read)
*           Fortran unit number for file

*  Arguments Given:
      INTEGER FD
      INTEGER RECNO
      INTEGER NCHAR
      BYTE BUF( NCHAR )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR

*  Local Variables:
      INTEGER IOERR              ! I/O Error number
      INTEGER RFD                ! Relative file descriptor

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Check for a valid File Descriptor.
      CALL FIO1_CHKFD( FD, RFD, STATUS )

*  Check to see if you are trying to write to a read-only file.
*  If not, write a record.
      IF( STATUS .EQ. SAI__OK ) THEN
         IF( CHR_SIMLR( FACMOD( RFD ), 'READ' ) ) THEN
            STATUS = FIO__ILLAC
            CALL EMS_REP( 'RIO_WRITE_ILLAC',
     :         'Illegal access file mode', STATUS )
         ELSE
            WRITE( FUNIT( RFD ), REC=RECNO, ERR=10, IOSTAT=IOERR ) BUF
         END IF
      END IF
      GOTO 999

*  Handle any error condition.
   10 CALL FIO_SERR( IOERR, STATUS )

  999 CONTINUE
      END
