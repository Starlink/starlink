      SUBROUTINE FIO1_CHKFD( FD, RFD, STATUS )
*+
*  Name:
*     FIO1_CHKFD

*  Purpose:
*     Check file descriptor

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO1_CHKFD( FD, RFD, STATUS )

*  Description:
*     Check that the file descriptor is valid and return the
*     corresponding file descriptor.

*  Arguments:
*     FD = INTEGER (Given)
*        File descriptor
*     RFD = INTEGER (Returned)
*        Relative File descriptor
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Algorithm:
*     Check the file tables to see if file has already been accessed.

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
*     SLW: Sid Wright  (Starlink, UCL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     12-Sep-1981 (SLW):
*        Original.
*     05-Feb-1983 (SLW):
*        Starlink-ised version.
*     10-Feb-1988 (AJC):
*        Rationalize include files.
*     13-Jun-1989 (AJC):
*        Improve prologue (after JHF).
*     29-OCT-1991 (PMA):
*        Changed the name from FIO_$CHKFD to FIO1_CHKFD.
*     9-MAR-1992 (PMA):
*        Add error reporting with EMS
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

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! General Symbolic Constants
      INCLUDE 'FIO_PAR'          ! FIO Global Constants
      INCLUDE 'FIO_SYS'	         ! FIO Internal Constants
      INCLUDE 'FIO_ERR'          ! FIO error numbers

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'       ! File descriptor table
*        FREE( FIO__MXFIL ) = LOGICAL (Read)
*           Is the file descriptor available ?

*  Arguments Given:
      INTEGER FD                 ! File descriptor

*  Arguments Returned:
      INTEGER RFD                ! Relative file descriptor

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      RFD = FD - FIO__BASE

      IF ( RFD .LT. 1 .OR. RFD .GT. FIO__MXFIL ) THEN
         STATUS = FIO__ILLFD
         CALL EMS_REP( 'FIO1_CHKFD_ILFD',
     :      'Illegal file descriptor found', STATUS )
      ELSE IF ( FREE( RFD ) ) THEN
         STATUS = FIO__NTOPN
         CALL EMS_REP( 'FIO1_CHKFD_FNOP', 'File is not open', STATUS )
      END IF

      END
