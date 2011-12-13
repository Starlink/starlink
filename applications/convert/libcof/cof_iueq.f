      SUBROUTINE COF_IUEQ( EL, IUEQUA, QUALIT, STATUS )
*+
*  Name:
*     COF_IUEQ

*  Purpose:
*     Converts IUEFA 16-bit quality to NDF quality.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_IUEQ( EL, IUEQUA, QUALIT, STATUS )

*  Description:
*     This routines takes IUE Final Archive quality information
*     stored at 2's complement 16-bit integers, into unsigned-byte
*     quality as required by the NDF.  The most significant 8 bits of
*     the 14 actually used are transferred.  In practice this means a
*     division by -128.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the quality arrays.
*     IUEQUA( EL ) = INTEGER * 2 (Given)
*        The IUE Final Archive quality array.
*     QUALIT( EL ) = BYTE (Returned)
*        The NDF quality array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 June 29 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      INTEGER * 2 IUEQUA( EL )

*  Arguments Returned:
      BYTE QUALIT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop for all pixels, dividing by -128 to get values in the range
*  0 to 256.
      DO I = 1, EL
         QUALIT( I ) = -IUEQUA( I ) / 128
      END DO

      END
