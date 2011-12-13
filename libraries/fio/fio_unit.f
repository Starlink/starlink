      SUBROUTINE FIO_UNIT( FD, UNIT, STATUS )
*+
*  Name:
*     FIO_UNIT

*  Purpose:
*     Get a unit number given a file descriptor

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_UNIT( FD, UNIT, STATUS )

*  Description:
*     The Fortran unit number associated with the given file descriptor
*     is returned.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor.
*     UNIT = INTEGER (Returned)
*        Variable to receive the unit number.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     The file descriptor is checked for validity then, if OK
*     the unit number is obtained from the descriptor tables.

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
*     13-Oct-1986 (AJC):
*        Change name from FIO_$UNIT to make it a user callable routine
*        for ADAM
*     16-Feb-1988 (AJC):
*        Rationalize include files
*     25-Feb-1988 (AJC):
*        Improve prologue  (RAL::AJC)
*     29-OCT-1991 (PMA):
*        Changed references to FIO_$xxxxx to FIO1_xxxxx.
*     31-OCT-1991 (PMA):
*        Changed to new style prologue.
*     10-MAR-1992 (PMA):
*        Tidy up the code and comments.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
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
      INCLUDE 'FIO_SYS'          ! FIO Internal Constants

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'       ! File descriptor tables
*        FUNIT( FIO__MXFIL ) = INTEGER (Read)
*           Fortran unit number for file.

*  Arguments Given:
      INTEGER FD

*  Arguments Returned:
      INTEGER UNIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER RFD                ! Relative file descriptor

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Check that the file descriptor is valid and get the relative file
*  descriptor.
      CALL FIO1_CHKFD( FD, RFD, STATUS )

*  Get the unit number.
      IF( STATUS .EQ. SAI__OK ) THEN
         UNIT = FUNIT( RFD )
      ENDIF

      END
