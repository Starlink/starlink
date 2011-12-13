      SUBROUTINE FIO_GUNIT( UNIT, STATUS )
*+
*  Name:
*     FIO_GUNIT

*  Purpose:
*     Get a unit number

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_GUNIT( UNIT, STATUS )

*  Description:
*     Get an unused Fortran unit number.

*  Arguments:
*     UNIT = INTEGER (Returned)
*        A variable to contain the unit number.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Search for an unused Fortran unit number.

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
*     30-JUL-1980 (SLW):
*        Original.
*     10-MAY-1983 (SLW):
*        Tidy up for Starlink version.
*     13-OCT-1986 (AJC):
*        Ensure FIO is started
*     16-FEB-1988 (AJC):
*        Rationalize include files
*     25-FEB-1988 (AJC):
*        Improve prologue
*     13-JUN-1989 (AJC):
*        Remove declaration CHR_LEN  (RAL::AJC)
*     30-OCT-1991 (PMA):
*        Convert to new style prologue.
*     9-MAR-1992 (PMA):
*        Correct the description in the prologue.
*        Add error reporting with EMS.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*        Add EXTERNAL reference to FIO_BLK.
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
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_SYS'          ! FIO Internal symbols
      INCLUDE 'FIO_ERR'          ! FIO Error numbers

*  Global Variables:
      INCLUDE 'FIOUNT_CMN'
*        FREUN( FIO__MXUNT ) = LOGICAL (Read and Write)
*           Unit number available?
*        FRUNT( FIO__MXUNT ) = INTEGER (Read)
*           Fortran unit numbers.
      INCLUDE 'FIOGO_CMN'        ! FIO Initialisation Switches
*        FIOINT = LOGICAL (Read)
*           Whether Fio is started

*  Arguments Returned:
      INTEGER UNIT               ! Fortran unit number

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL FIO_BLK           ! Block data subprogram

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure FIO is started
      IF ( .NOT. FIOINT ) THEN
         CALL FIO_START( STATUS )
      ENDIF

*  Get the next unit number.
      UNIT = 0
      DO I = 1, FIO__MXUNT
         IF ( FREUN( I ) ) THEN
            UNIT = FRUNT( I )
            FREUN( I ) = .FALSE.
            GOTO 1
         ENDIF
      ENDDO

*  No unit number available.
      STATUS = FIO__NOUNT
      CALL EMS_REP( 'FIO_GUNIT_NOUNT', 'No unit number available',
     :   STATUS )

    1 CONTINUE

      END
