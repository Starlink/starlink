      SUBROUTINE FIO_ERASE( FILE, STATUS )
*+
*  Name:
*     FIO_ERASE

*  Purpose:
*     Delete a file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_ERASE( FILE, STATUS )

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
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     30-Jul-1980 (SLW):
*        Original.
*     10-May-1983 (SLW):
*        Tidy up for Starlink version.
*     13-Oct-1986 (AJC):
*        Remove DISP from OPEN and replace DISP in CLOSE with STATUS
*        i.e. F77 standards.
*     16-Feb-1988 (AJC):
*        Rationalize include files.
*     25-Feb-1988 (AJC):
*        Improve prologue.
*     29-OCT-1991 (PMA):
*        Changed references to FIO_$xxxxx to FIO1_xxxxx.
*        Removed include of FIOFIL_CMN as none of the variables in the
*        common block are referenced by this routine.
*     28-JAN-1992 (PMA):
*        Change RETURN statement to GOTO end of code.
*     9-MAR-1992 (PMA):
*        Change thevariable VMSERR to SYSERR
*     12-MAR-1992 (PMA):
*        Change the call to FIO1_SERR to FIO_SERR.
*     17-MAR-1992 (PMA):
*        Add missing IMPLICIT NONE statement
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     24-AUG-1992 (PMA):
*        Fix layout of brackets.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO Symbolic Constants
      INCLUDE 'FIO_SYS'          ! FIO Internal symbols and errors.

*  Arguments Given:
      CHARACTER *( * ) FILE      ! FILE NAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER UNIT               ! Fortran unit number
      INTEGER SYSERR             ! Fortran IOSTAT error number

*.

* Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* Get Fortran unit number
      CALL FIO_GUNIT( UNIT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         OPEN( UNIT=UNIT, FILE=FILE, STATUS='OLD',
     :   ERR=10, IOSTAT=SYSERR )
         CLOSE( UNIT=UNIT, STATUS='DELETE',ERR=10, IOSTAT=SYSERR )
* Return unit number to system.
         CALL FIO_PUNIT( UNIT, STATUS )
      ENDIF
      GOTO 999

* Error opeing or closing file.
   10 CALL FIO_SERR( SYSERR, STATUS )
      CALL FIO_PUNIT( UNIT, STATUS )

  999 CONTINUE
      END
