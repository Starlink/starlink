      SUBROUTINE KPS1_LOGXY( FD, EL, XP, YP, STATUS )
*+
*  Name:
*     KPS1_LOGXY

*  Purpose:
*     Logs a set of x-y co-ordinates to a Fortran file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LOGXY( FD, EL, XP, YP, STATUS )

*  Description:
*     This routine writes a set of x-y co-ordinates to a Fortran
*     formatted file, one pair per record in free format delimited
*     by a space.

*  Arguments:
*     FD = INTEGER (Given)
*        A FIO file descriptor for the log file.
*     EL = INTEGER (Given)
*        The number of points to write to the log file.
*     XP( EL ) = REAL (Given)
*        The pixel co-ordinates of the points on the first axis.
*     YP( EL ) = REAL (Given)
*        The pixel co-ordinates of the points on the second axis.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The Fortran file must be open for write access.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1995 April 19 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER FD
      INTEGER EL
      REAL XP( EL )
      REAL YP( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) BUF     ! Buffer for output to text file
      INTEGER I                  ! Loop counter
      INTEGER LBUF               ! Used length of log file buffer

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop for each position.
      DO I = 1, EL

*  Create some tokens for the co-ordinates.  Write them into a buffer.
         CALL MSG_SETR( 'X', XP( I ) )
         CALL MSG_SETR( 'Y', YP( I ) )
         CALL MSG_LOAD( ' ', '^X ^Y', BUF, LBUF, STATUS )

*  Write the buffer to the log file.
         CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
      END DO

      END
