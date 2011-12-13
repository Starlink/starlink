      SUBROUTINE FIO_START( STATUS )
*+
*  Name:
*     FIO_START

*  Purpose:
*     Set up units numbers and open standard I/O streams

*  Language:
*     Starlink FORTRAN

*  Invocation:
*     CALL FIO_START( STATUS )

*  Description:
*     Allocate unit numbers for use by FIO and mark them as available.
*     Open standard input, output and error files.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is not normally needed in a simple program as FIO
*        starts itself when necessary.

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
*         Fiogo_f77 common included.
*     15-Mar-1985 (JHF):
*         Base unit now 40 so as interim environment uses 37
*     10-Oct-1986 (AJC):
*        Remove standard input,output and error streams
*     16-Feb-1988 (AJC):
*        Rationalize include files
*     31-OCT-1991 (PMA):
*        Convert to new style prologue.
*     10-MAR-1992 (PMA):
*        Change the number 40 to the constant FIO__BSUNT
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*        Add EXTERNAL reference to FIO_BLK.
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
      INCLUDE 'FIO_ERR'          ! FIO Error numbers

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'
*        FREE( FIO__MXFIL ) = LOGICAL (Write)
*           File descriptor available ?
*        FUNIT( FIO__MXFIL ) = INTEGER (Write)
*           Fortran unit number for file.
*        FNAME( FIO__MXFIL ) = CHARACTER * ( IO__SZFNM ) (Write)
*           File name
*        FACMOD( FIO__MXFIL ) = CHARACTER * ( FIO__SZMOD ) (Write)
*           File access mode
*        FRECSZ( FIO__MXFIL ) = INTEGER (Write)
*           Record size
      INCLUDE 'FIOUNT_CMN'
*        FREUN( FIO__MXUNT ) = LOGICAL (Write)
*           Unit number available ?
*        FRUNT( FIO__MXUNT) ) = INTEGER (Write)
*           Fortran unit numbers.
      INCLUDE 'FIOGO_CMN'
*        FIOINT = LOGICAL (Read and Write)
*           Whether FIO is started

*  External References:
      EXTERNAL FIO_BLK           ! Block data subprogram

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Check if FIO is started.
      IF( FIOINT ) GOTO 999

*  Set up file descriptors etc.
      DO I = 1, FIO__MXFIL
         FREE( I ) = .TRUE.
         FUNIT( I ) = 0
         FNAME( I ) = ' '
         FACMOD( I ) = ' '
         FRECSZ( I ) = 0
      ENDDO

*  Set up unit numbers for sequential files
      DO I = 1, FIO__MXUNT
         FREUN( I ) = .TRUE.
         FRUNT( I ) = FIO__BSUNT + I
      ENDDO

*  Set initialisation flag
      FIOINT = .TRUE.

  999 CONTINUE
      END
