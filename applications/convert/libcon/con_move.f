      SUBROUTINE CON_MOVE( NBYTES, FROM, TO, STATUS )
*+
*  Name:
*     CON_MOVE

*  Purpose:
*     Copies bytes of data from one location to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_MOVE( NBYTES, FROM, TO, STATUS )

*  Description:
*     Simply moves bytes of data from one array to another.

*  Arguments:
*     NBYTES = INTEGER (Given)
*        Number of bytes to be moved.
*     FROM( NBYTES ) = BYTE (Given)
*        Source array for data.
*     TO ( NBYTES ) = BYTE (Returned)
*        Destination array for data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     JM: Jo Murray (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 Feb 8 (JM):
*        Original version.
*     {enter_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT    NONE           ! No implicit typing

*  Global Constants:
      INCLUDE    'SAE_PAR'       ! Standard SAE constants

*  Arguments Given:
      INTEGER    NBYTES
      BYTE       FROM( NBYTES )

*  Arguments Reyurned:
      BYTE       TO( NBYTES )

*  Status:
      INTEGER STATUS             ! Global status

*  Local variable:
      INTEGER    I               ! Loop variable

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Move the bytes.
      DO I = 1, NBYTES
         TO( I ) = FROM( I )
      END DO
 
      END
