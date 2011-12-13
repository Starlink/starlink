      SUBROUTINE RIO_CLOSE( FD, STATUS )
*+
*  Name:
*     RIO_CLOSE

*  Purpose:
*     Close a direct access file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RIO_CLOSE( FD, STATUS )

*  Description:
*     Close the file with the specified file descriptor.

*  Arguments:
*     FD = INTEGER (Given)
*        A variable containing the file descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If the STATUS variable is not SAI__OK on input, then the routine
*     will still attempt to execute, but will return with STATUS set to
*     the import value.

*  Algorithm:
*     Check that the file descriptor is valid, and that the file is
*     open.
*     Flush any buffers yet to be written to the file, and then close
*     it.
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
*     KFH: Ken Hartley (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     30-Jul-1980: Original. (SLW)
*     10-May-1983: Tidy up for Starlink version. (SLW)
*     16-Feb-1988: Rationalize include files  (AJC)
*     26-Feb-1988: Remove call FIO_FLUSH; it does nothing  (AJC)
*     01-Nov-1991: Change to new Prologue (KFH)
*        Put IMPLICIT NONE in (KFH)
*        Change vmserr variable to ioerr (KFH)
*        Change fac_$name to fac1_name (KFH)
*        Replace tabs with spaces in end-of-line comments (KFH)
*     26-FEB-1992 (PMA):
*        General tidying up of the prologue.
*        Change
*           CALL FIO_PUNIT( FUNIT( RFD ), ISTAT )
*        to
*           CALL FIO_PUNIT( FUNIT( RFD ), STATUS )
*        Add the statement  FUNIT( RFD ) = 0
*     17-MAR-1992 (PMA):
*        Change the call to FIO1_SERR to a call to FIO_SERR.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     1-JUL-1992 (PMA):
*        Replace saving the value of STATUS on entry and restoring it
*        on exit with EMS_BEGIN and EMS_END.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'FIO_PAR'         ! FIO symbolic constants
      INCLUDE 'FIO_SYS'         ! FIO internal symbols

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'      ! File descriptor tables
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
      INTEGER FD

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

*  Return the unit number to system.
         CALL FIO_PUNIT( FUNIT( RFD ), STATUS )
         FUNIT( RFD ) = 0
      END IF

*  Return to the previous error reporting environment.
      CALL EMS_END( STATUS )

      END
