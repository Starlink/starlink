      SUBROUTINE RIO_ERASE( FILE, STATUS )
*+
*  Name:
*     RIO_ERASE

*  Purpose:
*     Delete a file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RIO_ERASE( FILE, STATUS )

*  Description:
*     Delete the named file.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        Expression giving the name of the file to be deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Get a Fortran unit number and attempt to delete the file using
*     the Fortran 77 OPEN and CLOSE statements.

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
*     KFH: Ken Hartley (Starlink, KFH)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     30-Jul-1980: Original. (UCL::SLW)
*     10-May-1983: Tidy up for Starlink version. (UCL::SLW)
*     26-Oct-1987: Remove DISP from OPEN and replace DISP
*        in CLOSE with STATUS. I.e f77 standards (RAL::AJC)
*     16-Feb-1988: Rationalize include files  (RAL::AJC)
*     25-Feb-1988: Improve prologue  (RAL::AJC)
*     01-Nov-1991: Change to new-style Prologues (RAL::KFH)
*        Put IMPLICIT NONE in (RAL::KFH)
*        Change variable vmserr to ioerr (RAL::KFH)
*        Change fac_$name to fac1_name (RAL::KFH)
*        Replace tabs with spaces in end-of-line comments (RAL::KFH)
*     26-FEB-1992 (PMA):
*        Tidy up prologue.
*     17-MAR-1992 (PMA):
*        Change the call to FIO1_SERR to a call to FIO_SERR.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'FIO_PAR'         ! FIO symbolic constants
      INCLUDE 'FIO_SYS'         ! FIO internal symbols

*  Arguments Given:
      CHARACTER * ( * ) FILE

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER UNIT              ! Fortran unit number
      INTEGER IOERR             ! I/O Error number

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Get Fortran unit number
      CALL FIO_GUNIT( UNIT, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
         OPEN( UNIT=UNIT, FILE=FILE, STATUS='OLD',
     :      ERR=10, IOSTAT=IOERR )
         CLOSE( UNIT=UNIT, STATUS='DELETE',ERR=10, IOSTAT=IOERR )
*  Return unit number to system.
         CALL FIO_PUNIT( UNIT, STATUS )
      ENDIF
      GOTO 999

   10 CALL FIO_SERR( IOERR, STATUS )
      CALL FIO_PUNIT( UNIT, STATUS )

  999 CONTINUE
      END
