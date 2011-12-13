      PROGRAM TEST
*+
*  Name:
*     TEST

*  Purpose:
*     A simple test of using FIO

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Main program

*  Invocation:
*     Run the program

*  Description:
*     A simple program to test that an installation of the stand alone
*     version of FIO has been performed correctly.

*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     17-JAN-1992 (PMA):
*        Original version.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FD                 ! File descriptor

*.

*  Set inherited global status.
      STATUS = SAI__OK

      CALL FIO_OPEN( 'TEST.DAT', 'WRITE', 'LIST', 0, FD, STATUS )
      CALL FIO_WRITE( FD, 'This is a test of FIO', STATUS )
      CALL FIO_WRITE( FD, 'This file should contain 3 lines', STATUS )
      CALL FIO_WRITE( FD, 'This is the last line', STATUS )
      CALL FIO_CLOSE( FD, STATUS )

      END
