      SUBROUTINE LD2AR( NX, NY, SX, SY, NBIN, X, Y, Z, ARRAY, STATUS )
*+
*  Name:
*     LD2AR

*  Purpose:
*     Converts a sparse form of a two-dimensional array stored in DOUBLE PRECISION
*     into its REAL two-dimensional counterpart.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL LD2AR( NX, NY, SX, SY, NBIN, X, Y, Z, ARRAY, STATUS )

*  Description:
*     A list of x-y positions and values are converted to a complete
*     2-d array.  Missing elements take the bad value.

*  Arguments:
*     NX = INTEGER (Given)
*        The first dimension of the 2-d array.
*     NY = INTEGER (Given)
*        The second dimension of the 2-d array.
*     SX = REAL (Given)
*        The co-ordinate scale factor (i.e. the length of a pixel) in
*        the x direction.  It is used to determine which pixel a given
*        x-y position lies.  Normally, it will have the value 1.
*     SY = REAL (Given)
*        The co-ordinate scale factor (i.e. the length of a pixel) in
*        the y direction.  It is used to determine which pixel a given
*        x-y position lies.  Normally, it will have the value 1.
*     NBIN = INTEGER (Given)
*        The number of bins in the pixel list.
*     X( NBIN ) = DOUBLE PRECISION (Given)
*        The x position of the pixel in the list.
*     Y( NBIN ) = DOUBLE PRECISION (Given)
*        The y position of the pixel in the list.
*     Z( NBIN ) = DOUBLE PRECISION (Given)
*        The value of the pixel in the list.
*     ARRAY( NX, NY ) = REAL (Returned)
*        The expanded 2-d array formed from the list.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     {routine_notes}...

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     BRADC: Brad Cavanagh (JAC, Hawaii)
*     PWD: Peter W. Draper (Durham University)
*     {enter_new_authors_here}

*  History:
*     1990 Feb 22 (MJC):
*        Original version.
*     2009 DEC 30 (BRADC):
*        Use INTEGER*8 to avoid overflow.
*     2010 JAN 08 (PWD):
*        Don't use Fortran 90 MIN() intrinsic, stick to Fortran 77.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic-value definitions.

*  Arguments Given:
      INTEGER
     :  NX, NY,                  ! Dimensions of the output array
     :  NBIN                     ! Number of bins in the x-y list.

      REAL
     :  SX, SY                   ! Pixel sizes

      DOUBLE PRECISION
     :  X( NBIN ),               ! x positions of the pixel list
     :  Y( NBIN ),               ! y positions of the pixel list
     :  Z( NBIN )                ! Values of the pixels in the list

*  Arguments Returned:
      REAL
     :  ARRAY( NX, NY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER * 8
     :  I, J,                    ! 2-d array indices
     :  L                        ! Loop counter

*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    First initialise the array to the magic value.

      DO  J = 1, NY
         DO  I = 1, NX
            ARRAY( I, J ) = VAL__BADR
         END DO
      END DO

*    Fill the array with the list of values, by computing which bin a
*    given x-y position is situated.
      DO  L = 1, NBIN

*    Use direct assignment to convert to I*8 from REAL rather than the
*    INT intrinsic with an f90 kind. Seems to give the correct promotion
*    and truncation with g77/gfortran/g95/ifort/SUN f95/f77.
         I = X( L ) / SX
         I = MIN( I + 1, NX )
         J = Y( L ) / SY
         J = MIN( J + 1, NY )
         ARRAY( I, J ) = REAL( Z( L ) )
      END DO

      END
