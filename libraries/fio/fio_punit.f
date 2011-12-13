      SUBROUTINE FIO_PUNIT( UNIT, STATUS )
*+
*  Name:
*     FIO_PUNIT

*  Purpose:
*     Release a unit number

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_PUNIT( UNIT, STATUS )

*  Description:
*     Give back a Fortran unit number to FIO.

*  Arguments:
*     UNIT = INTEGER (Given)
*        Variable containing the unit number.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If STATUS is not set to SAI__OK on input, then the routine will
*     still attempt to execute, but will return with STATUS set to the
*     import value.

*  Algorithm:
*     Find the table pointer corresponding to this unit number. If it is
*     marked as 'not free', then mark it as free; otherwise report an
*     error

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
*     16-Feb-1988 (AJC):
*        Rationalize include files
*     29-Feb-1988 (AJC):
*        Improve prologue
*     13-Jun-1989 (AJC):
*        Remove declaration CHR_LEN
*     30-OCT-1991 (PMA):
*        Converted to new style prologue.
*     9-MAR-1992 (PMA):
*        Add error reporting with EMS.
*        Shuffle the assignment to STATUS to facilitate this.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     1-JUL-1992 (PMA):
*        Replace saving the value of STATUS on entry and restoring it
*        on exit with EMS_BEGIN and EMS_END.
*     2-JUL-1992 (PMA):
*        Remove routine name from error reports.
*     24-AUG-1992 (PMA):
*        Change layout of brackets.
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
      INCLUDE 'FIO_SYS'          ! FIO Internal symbols
      INCLUDE 'FIO_ERR'          ! FIO Error numbers

*  Global Variables:
      INCLUDE 'FIOUNT_CMN'
*        FRUNT( FIO__MXUNT ) = INTEGER ({global_access_mode})
*           Fortran unit numbers.
*        FREUN( FIO__MXUNT ) = LOGICAL ({global_access_mode})
*           Unit number available ?

*  Arguments Given:
      INTEGER UNIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Begin a new error reporting environment so that this routine
*  executes even if status is bad on entry.
      CALL EMS_BEGIN( STATUS )

*  Try to match the unit number against the list of FIO unit numbers.
      DO I = 1, FIO__MXUNT
         IF ( UNIT .EQ. FRUNT( I ) ) THEN
            IF ( .NOT. FREUN( I ) ) THEN
               FREUN( I ) = .TRUE.
               GOTO 1
            END IF
         END IF
      ENDDO

*  Could not find a match with an FIO unit number.
      STATUS = FIO__IVUNT
      CALL EMS_REP( 'FIO_PUNIT_IVUNT', 'Invalid unit number', STATUS )

    1 CONTINUE

*  Return to the previous error reporting environment.
      CALL EMS_END( STATUS )

      END
