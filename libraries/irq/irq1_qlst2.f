      SUBROUTINE IRQ1_QLST2( BIT, LISTED, SET, NOK, VIND, SIZE, QUAL,
     :                       STATUS )
*+
*  Name:
*     IRQ1_QLST2

*  Purpose:
*     Set a QUALITY bit for pixels determined by a list.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_QLST2( BIT, LISTED, SET, NOK, VIND, SIZE, QUAL, STATUS )

*  Description:
*     This routine performs the work for routine IRQ1_QLST. It differs
*     from IRQ1_QLST in that the selected coordinates are described by
*     one dimensional indices rather than by N-dimensional coordinates.

*  Arguments:
*     BIT = INTEGER (Given)
*        The bit number within the QUALITY component. The least
*        significant bit is called bit 1 (not bit 0).
*     LISTED = LOGICAL (Given)
*        If true, then the operation specified by SET is performed on
*        all the pixels in the list of coordinates given by LIST.  If
*        false, then the operation specified by SET is performed on all
*        the pixels which are not in the list of coordinates given by
*        LIST.
*     SET = LOGICAL (Given)
*        If true, then the selected pixels have the given bit set,
*        indicating that the corresponding quality is held.  If false,
*        then the selected pixels have the given bit cleared,
*        indicating that the corresponding quality is not held.
*     NOK = INTEGER (Given)
*        The number of pixels in the input list.
*     VIND( NOK ) = INTEGER (Given)
*        A list of 1 dimensional indices corresponding to the positions
*        of selected pixels in the QUAL vector.
*     SIZE = INTEGER (Given)
*        The size of the QUAL vector.
*     QUAL( SIZE ) = BYTE (Given and Returned)
*        The QUALITY vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  VAX-specific features used:
*     -  Uses BYTE arrays.
*     -  Uses functions IBSET and IBCLR.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     30-JUL-1991 (DSB):
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
      INTEGER BIT
      LOGICAL LISTED
      LOGICAL SET
      INTEGER NOK
      INTEGER VIND( NOK )
      INTEGER SIZE

*  Arguments Given and Returned:
      BYTE QUAL( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
      INTEGER J                  ! Loop count.
      INTEGER LBIT               ! Corrected bit number.

*  PRIMDAT type conversion functions.
      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Produce a bit number in the range 0 to (IRQ__QBITS - 1 ).
      LBIT = BIT - 1

*  If the bit is to be set...
      IF( SET ) THEN

*  ... and if the listed pixels are to be set...
         IF( LISTED ) THEN

*  ... set the bit for each pixel in the list.
            DO J = 1, NOK
               QUAL( VIND( J ) ) = NUM_ITOUB(
     :                   IBSET( NUM_UBTOI( QUAL( VIND( J ) ) ), LBIT ) )
            END DO

*  If the non-listed pixels are to be set...
         ELSE

*  If there are no listed pixels, set the bit for all pixels.
            IF( NOK .EQ. 0 ) THEN
               DO J = 1, SIZE
                  QUAL( J ) = NUM_ITOUB(
     :                        IBSET( NUM_UBTOI( QUAL( J ) ), LBIT ) )
               END DO

*  Otherwise, order the pixel indices into ascending order.
            ELSE
               CALL IRQ1_SORTI( NOK, VIND, STATUS )

*  Set the bit for all pixels prior to the first listed pixel.
               DO J = 1, VIND( 1 ) - 1
                  QUAL( J ) = NUM_ITOUB(
     :                        IBSET( NUM_UBTOI( QUAL( J ) ), LBIT ) )
               END DO

*  Loop round each remaining listed pixel.
               DO I = 2, NOK

*  Set the bit for each pixel prior to the next listed pixel.
                  DO J = VIND( I - 1 ) + 1, VIND( I ) - 1
                     QUAL( J ) = NUM_ITOUB(
     :                        IBSET( NUM_UBTOI( QUAL( J ) ), LBIT ) )
                  END DO

               END DO

*  Set the bit for all pixels after the last listed pixel.
               DO J = VIND( NOK ) + 1, SIZE
                  QUAL( J ) = NUM_ITOUB(
     :                        IBSET( NUM_UBTOI( QUAL( J ) ), LBIT ) )
               END DO

            END IF

         END IF

*  If the bit is to be cleared...
      ELSE

*  ... and if the listed pixels are to be cleared...
         IF( LISTED ) THEN

*  ... clear the bit for each pixel in the list.
            DO J = 1, NOK
               QUAL( VIND( J ) ) = NUM_ITOUB(
     :                   IBCLR( NUM_UBTOI( QUAL( VIND( J ) ) ), LBIT ) )
            END DO

*  If the non-listed pixels are to be cleared...
         ELSE

*  If there are no listed pixels, clear the bit for all pixels.
            IF( NOK .EQ. 0 ) THEN
               DO J = 1, SIZE
                  QUAL( J ) = NUM_ITOUB(
     :                        IBCLR( NUM_UBTOI( QUAL( J ) ), LBIT ) )
               END DO

*  Otherwise, order the pixel indices into ascending order.
            ELSE
               CALL IRQ1_SORTI( NOK, VIND, STATUS )

*  Clear the bit for all pixels prior to the first listed pixel.
               DO J = 1, VIND( 1 ) - 1
                  QUAL( J ) = NUM_ITOUB(
     :                        IBCLR( NUM_UBTOI( QUAL( J ) ), LBIT ) )
               END DO

*  Loop round each remaining listed pixel.
               DO I = 2, NOK

*  Clear the bit for each pixel prior to the next listed pixel.
                  DO J = VIND( I - 1 ) + 1, VIND( I ) - 1
                     QUAL( J ) = NUM_ITOUB(
     :                        IBCLR( NUM_UBTOI( QUAL( J ) ), LBIT ) )
                  END DO

               END DO

*  Clear the bit for all pixels after the last listed pixel.
               DO J = VIND( NOK ) + 1, SIZE
                  QUAL( J ) = NUM_ITOUB(
     :                        IBCLR( NUM_UBTOI( QUAL( J ) ), LBIT ) )
               END DO

            END IF

         END IF

      END IF

      END
