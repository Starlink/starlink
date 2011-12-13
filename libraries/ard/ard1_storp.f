      SUBROUTINE ARD1_STORP( NDIM, C, ARGS, IPOPND, IOPND, SZOPND,
     :                       STATUS )
*+
*  Name:
*     ARD1_STORP

*  Purpose:
*     Store a transformed position in the operand stack.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_STORP( NDIM, C, ARGS, IPOPND, IOPND, SZOPND, STATUS )

*  Description:
*     The co-ordinate values supplied in ARGS are transformed using the
*     transformation co-efficients supplied in C. The resulting
*     co-ordinates are stored on the operand stack. The operand stack is
*     extended if necessary.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The dimensionality of the ARD description (i.e. the number of
*        values required to specify a position).
*     C( * ) = REAL (Given)
*        The co-efficients of the current mapping from supplied
*        co-ordinates to pixel co-ordinates.
*     ARGS( NDIM ) = REAL (Given)
*        The supplied co-ordinates.
*     IPOPND = INTEGER (Given)
*        The pointer to the array holding the operand stack.
*     IOPND = INTEGER (Given and Returned)
*        The index within the operand stack at which the next value
*        should be stored.
*     SZOPND = INTEGER (Given and Returned)
*        The size of the operand stack. This is increased if necessary.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-FEB-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NDIM
      REAL C( * )
      REAL ARGS( NDIM )
      INTEGER IPOPND

*  Arguments Given and Returned:
      INTEGER IOPND
      INTEGER SZOPND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     : IAXIS,                    ! Index of current input axis
     : ICT,                      ! Index of the constant term
     : NCPA,                     ! No. of co-efficients per output axis
     : OAXIS                     ! Index of current output axis

      REAL
     : VALUE                     ! The current output co-ordinate value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the number of co-efficients for each output co-ordinate (one
*  for each input co-ordinate plus a constant term).
      NCPA = NDIM + 1

*  Loop round the output co-ordinates.
      DO OAXIS = 0, NDIM - 1

*  Store the index (within C) of the constant term for this output
*  co-ordinate.
         ICT = OAXIS*NCPA + 1

*  Initalise the output co-ordinate to hold the constant term.
         VALUE = C( ICT )

*  Loop round the input co-ordinates.
         DO IAXIS = 1, NDIM

*  Increment the output co-ordinate value by the product of the input
*  co-ordinate value and the corresponding co-efficient.
            VALUE = VALUE + C( ICT + IAXIS )*ARGS( IAXIS )

         END DO

*  Store the output co-ordinate value on the operand stack.
         CALL ARD1_STORR( VALUE, SZOPND, IOPND, IPOPND, STATUS )

      END DO

      END
