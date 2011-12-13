      SUBROUTINE FIO_RWIND( FD, STATUS )
*+
*  Name:
*     FIO_RWIND

*  Purpose:
*     Rewind a sequential file

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIO_RWIND( FD, STATUS )

*  Description:
*     Rewind a sequential access file.

*  Arguments:
*     FD = INTEGER (Given)
*        The file descriptor.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine must ONLY be used on sequential access files.


*  Algorithm:
*     Obtain the Fortran unit number corresponding to the file
*     descriptor.
*     Attempt to rewind the file.
*     If the rewind fails then
*       translate the Fortran error status into an FIO error status.
*     end if

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     ACD: Clive Davenhall (ROE)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     16-JUN-1986 (ACD):
*        Original version.
*     17-FEB-1989 (AJC):
*        Revised for inclusion in FIO library.
*     29-OCT-1991 (PMA):
*        Changed references to FIO_$xxxxx to FIO1_xxxxx.
*     9-MAR-1992 (PMA):
*        Tidy up some comments.
*     12-MAR-1992 (PMA):
*        Change the call to FIO1_SERR to FIO_SERR.
*     3-APR-1992 (PMA):
*        Change the name of include files to lower case.
*     24-AUG-1992 (PMA):
*        Change the layout of brackets.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_PAR'          ! FIO Symbolic Constants
      INCLUDE 'FIO_SYS'          ! FIO Internal symbols

*  Global Variables:
      INCLUDE 'FIOFIL_CMN'       ! Information on FIO files
*        FUNIT( FIO__MXFIL ) = INTEGER (Read)
*           Fortran unit number for file.

*  Arguments Given:
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FILOK              ! Success status for a Fortran I/O operation.
      PARAMETER ( FILOK = 0 )

*  Local Variables:
      INTEGER RFD                ! Relative file descriptor.
      INTEGER CUNIT              ! Fortran unit number for catalogue I/O.
      INTEGER RSTAT              ! Status rewinding the file.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the Fortran unit number corresponding to the catalogue.
      CALL FIO1_CHKFD( FD, RFD, STATUS )
      CUNIT = FUNIT( RFD )

*  Attempt to rewind the file.
      REWIND( UNIT=CUNIT, IOSTAT=RSTAT )

*  If the rewind failed then translate the Fortran IOSTAT error status
*  into an FIO status.
      IF ( RSTAT .NE. FILOK ) THEN
         CALL FIO_SERR( RSTAT, STATUS )
      END IF

      END
