      SUBROUTINE FIO_CLOSE( FD, STATUS )
*+
*  Name:
*     FIO_CLOSE

*  Purpose:
*     Close a sequential file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_CLOSE( FD, STATUS )

*  Description:
*     Close the file with the specified file descriptor.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the STATUS variable is not SAI__OK on input, then the
*        routine will still attempt to execute, but will return with
*        STATUS set to the import value.

*  Algorithm:
*     Check that the file descriptor is valid, and that the file is open
*     and then close it.
*     Release the relevant table entries.

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
*     30-Jul-1980 (SLW):
*        Original.
*     10-May-1983 (SLW):
*        Tidy up for Starlink version.
*     10-Feb-1988 (AJC):
*        Rationalize include files
*     25-Feb-1988 (AJC):
*        Improve prologue
*     26-Feb-1988 (AJC):
*        Remove call to FIO_FLUSH; it did nothing
*     29-OCT-1991 (PMA):
*        Changed references to FIO_$xxxxx to FIO1_xxxxx
*     28-JAN-1992 (PMA):
*        Remove check on STATUS on entry.
*      9-MAR-1992 (PMA):
*        Change the variable VMSERR to SYSERR
*     12-MAR-1992 (PMA):
*        Change the call to FIO1_SERR to FIO_SERR.
*      3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     16-JUN-1992 (PMA):
*        Change the variable SYSERR to IOERR
*      1-JUL-1992 (PMA):
*        Replace saving the value of STATUS on entry and restoring it
*        on exit with EMS_BEGIN and EMS_END.
*     29-JUL-1992 (PMA):
*        Tidy up some comments.
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

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'       ! File descriptor tables
*        FACMOD( FIO__MXFIL ) = CHARACTER * ( FIO__SZMOD ) (Write)
*           File access mode
*        FNAME( FIO__MXFIL ) = CHARACTER * ( FIO__SZFNM ) (Write)
*           File names
*        FRECSZ( FIO__MXFIL ) = INTEGER (Write)
*           Record size
*        FREE( FIO__MXFIL ) = LOGICAL (Write)
*           File descriptor available ?
*        FUNIT( FIO__MXFIL ) = INTEGER (Read and Write)
*           Fortran unit number for file

*  Arguments Given:
      INTEGER FD                 ! File descriptor

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER RFD                ! Relative file descriptor
      INTEGER IOERR              ! Fortran IOSTAT error number

*.

*  Begin a new error reporting environment so that this routine
*  executes even if status is bad on entry.
      CALL EMS_BEGIN( STATUS )

*  Check that the file descriptor is valid.
      CALL FIO1_CHKFD( FD, RFD, STATUS )

*  If it is, close the file.
      IF( STATUS .EQ. SAI__OK ) THEN
         CLOSE( UNIT=FUNIT( RFD ), ERR=10, IOSTAT=IOERR )
         GOTO 20
   10    CALL FIO_SERR( IOERR, STATUS )
   20    CONTINUE

*  Record the current state.
         FREE( RFD ) = .TRUE.
         FNAME( RFD ) = ' '
         FACMOD( RFD ) = ' '
         FRECSZ( RFD ) = 0

*  Return the unit number to the system.
         CALL FIO_PUNIT( FUNIT( RFD ), STATUS )
         FUNIT( RFD ) = 0

      END IF

*  Return to the previous error reporting environment.
      CALL EMS_END( STATUS )

      END
