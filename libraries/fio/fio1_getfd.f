      SUBROUTINE FIO1_GETFD( FD, RFD, STATUS )
*+
*  Name:
*     FIO1_GETFD

*  Purpose:
*     Get a file descriptor

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_GETFD( FD, RFD, STATUS )

*  Description:
*     Get the next available file descriptor.

*  Arguments:
*     FD = INTEGER (Returned)
*        A variable to contain the file descriptor.
*     RFD = INTEGER (Returned)
*        A variable to contain the relative file descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Search the table of file descriptors for one that is not being
*     used.

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
*     30-Jul-1980 (SLW):
*        Original.
*     10-May-1983 (SLW):
*        Tidy up for Starlink version.
*     10-Feb-1988 (AJC):
*        Rationalize include files.
*     29-OCT-1991 (PMA):
*        Change the name from FIO_$GETFD to FIO1_GETFD.
*     9-MAR-1992 (PMA):
*        Add error reporting with EMS
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     2-JUL-1992 (PMA):
*        Remove routine name from error reports.
*    28-JUL-1992 (PMA):
*        Slight change to the logic to make it clearer what is going on.
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
      INCLUDE 'FIOFIL_CMN'       ! File descriptor table
*        FREE( FIO__MXFIL ) = LOGICAL (Read)
*           Is the file descriptor available ?

*  Arguments Returned:
      INTEGER FD                 ! File descriptor
      INTEGER RFD                ! Relative file descriptor

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, FIO__MXFIL
         IF( FREE( I ) ) THEN
            RFD = I
            GOTO 1
         ENDIF
      END DO

*  The table of file descriptors is full.
*  Set the file descriptor to an invalid value and report an error.
      RFD = 0
      STATUS = FIO__TOOFD
      CALL EMS_REP( 'FIO1_GETFD_NMFD',
     :   'No more file descriptors available', STATUS )

    1 CONTINUE
      FD = RFD + FIO__BASE

      END
