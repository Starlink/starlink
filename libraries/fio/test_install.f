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
