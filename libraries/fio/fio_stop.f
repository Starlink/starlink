      SUBROUTINE FIO_STOP( STATUS )
*+
*  Name:
*     FIO_STOP

*  Purpose:
*     Close down FIO

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_STOP( STATUS )

*  Description:
*     Close the FIO file descriptor system and all associated files.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If STATUS is not SAI__OK on input, then the routine will
*        still attempt to execute, but will return with STATUS set to
*        the import value.
*     -  This routine is not normally needed in a simple program as FIO
*        is closed down by normal program termination.

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
*     JHF: Jon Fairclough (IPMAF, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     30-Jul-1980 (SLW):
*        Original.
*     10-May-1983 (SLW):
*        Tidy up for Starlink version.
*      3-Dec-1984 (JHF):
*        fiogo_f77 included. (RAL::IPMAF)
*     16-Feb-1988 (AJC):
*        Rationalize include files
*     15-Jul-1988 (AJC):
*        Improve prologue (RAL::AJC)
*     31-OCT-1991 (PMA):
*        Converted prologue to new style.
*     10-MAR-1992 (PMA):
*        Tidy up the code.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*        Add EXTERNAL reference to FIO_BLK.
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
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO Symbolic Constants
      INCLUDE 'FIO_SYS'          ! FIO Internal symbols
      INCLUDE 'FIO_ERR'          ! FIO error numbers

*  Global Variables:
      INCLUDE 'FIOGO_CMN'        ! FIO start-up flag
*        FIOINT = LOGICAL (Read and Write)
*           Whether Fio is started
      INCLUDE 'FIOFIL_CMN'       ! File descriptor table
*        FREE( FIO__MXFIL ) = LOGICAL (Read)
*           File descriptor available ?

*  External References:
      EXTERNAL FIO_BLK           ! Block data subprogram

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      INTEGER FD                 ! File descriptor

*.

*  Begin a new error reporting environment so that this routine
*  executes even if status is bad on entry.
      CALL EMS_BEGIN( STATUS )

*  Check to see that FIO is initialized.
      IF( .NOT. FIOINT ) GOTO 900

*  Close all opened files.
      DO I = 1, FIO__MXFIL
         IF( .NOT. FREE( I ) ) THEN
            FD = I + FIO__BASE
            CALL FIO_CLOSE( FD, STATUS )
         ENDIF
      ENDDO

* Clear initialisation flag
      FIOINT = .FALSE.

  900 CONTINUE

*  Return to the previous error reporting environment.
      CALL EMS_END( STATUS )

      END
