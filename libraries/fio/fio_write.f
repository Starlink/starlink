      SUBROUTINE FIO_WRITE( FD, BUF, STATUS )
*+
*  Name:
*     FIO_WRITE

*  Purpose:
*     Write a sequential record

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_WRITE( FD, BUF, STATUS )

*  Description:
*     Write a buffer to the file with the specified file descriptor.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor.
*     BUF = CHARACTER ( * ) (Given)
*        Expression containing the data to be written.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Check for a valid file descriptor and that the file is open.
*     Write a record to the file using a WRITE statement.

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
*        Rely on access mode in upper case.
*     29-OCT-1991 (PMA):
*        Changed references to FIO_$xxxxx to FIO1_xxxxx.
*     31-OCT-1991 (PMA):
*        Changed prologue to new style
*     23-FEB-1992 (PMA):
*        Add comments for the INCLUDE file FIOFIL_CMN
*     10-MAR-1992 (PMA):
*        Add error reporting with EMS.
*        Change the variable VMSERR to SYSERR.
*     12-MAR-1992 (PMA):
*        Change the call to FIO1_SERR to FIO_SERR.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     16-JUN-1992 (PMA):
*        Change the variable SYSERR to IOERR
*     2-JUL-1992 (PMA):
*        Remove routine name from error reports.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO Symbolic Constants
      INCLUDE 'FIO_SYS'          ! FIO Internal symbols
      INCLUDE 'FIO_ERR'          ! FIO error numbers

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'       ! File descriptor tables
*        FUNIT( FIO__MXFIL ) = INTEGER (Read)
*           Fortran unit number for file
*        FACMOD( FIO__MXFIL ) = CHARACTER * ( FIO__SZMOD ) (Read)
*           File access mode

*  Arguments Given:
      INTEGER FD
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

*  Check the file access mode and write the record.
      IF( STATUS .EQ. SAI__OK ) THEN
         IF( FACMOD( RFD ) .EQ. 'READ') THEN
            STATUS = FIO__ILLAC
            CALL EMS_REP( 'FIO_WRITE_ILLAC',
     :         'Illegal access file mode', STATUS )
         ELSE
            WRITE( FUNIT( RFD ), FMT=1, ERR=10, IOSTAT=IOERR ) BUF
    1       FORMAT( A )
         ENDIF
      ENDIF
      GOTO 999

*  Handle error condition from the WRITE statement.
   10 CALL FIO_SERR( IOERR, STATUS )

  999 CONTINUE
      END
