      SUBROUTINE IRQ1_QMSK( BIT, BAD, SET, INIT, SIZE, MASK, QUAL,
     :                      NSET, NCLEAR, STATUS )
*+
*  Name:
*     IRQ1_QMSK

*  Purpose:
*     Set a QUALITY bit for pixels determined by a mask.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_QMSK( BIT, BAD, SET, INIT, SIZE, MASK, QUAL, NSET,
*                     NCLEAR, STATUS )

*  Description:
*     If SET is true then selected pixels in the QUALITY vector have the
*     specified bit set. If SET is false, then the selected pixels have
*     the specified bit cleared. If BAD is true then pixels are selected
*     if they correspond to bad pixels in the mask. If BAD is false then
*     pixels are selected if they do not correspond to bad pixels in
*     the mask. If INIT is true, then pixels which are not selected have
*     the specified bit set to the opposite value (i.e. cleared if SET
*     is true and set if SET is false).

*  Arguments:
*     BIT = INTEGER (Given)
*        The bit number within the QUALITY component. The least
*        significant bit is called bit 1 (not bit 0).
*     BAD = LOGICAL (Given)
*        If true, then the operation specified by SET is performed on
*        only those pixels in the QUAL vector which correspond to bad
*        pixels in the MASK vector.  If false, then the operation
*        specified by SET is performed on only those pixels in the QUAL
*        vector which do not correspond to bad pixels in the MASK
*        vector.
*     SET = LOGICAL (Given)
*        If true, then the selected pixels have the given bit set,
*        indicating that the corresponding quality is held.  If false,
*        then the selected pixels have the given bit cleared,
*        indicating that the corresponding quality is not held.
*     INIT = LOGICAL (Given)
*        If true, then the unselected pixels have the given bit set (if
*        SET if false) or cleared (if SET is true). If INIT is false,
*        unselected pixels are left unchanged.
*     SIZE = INTEGER*8 (Given)
*        The size of the QUAL and MASK vectors.
*     MASK( SIZE ) = REAL (Given)
*        The mask vector.
*     QUAL( SIZE ) = BYTE (Given and Returned)
*        The QUALITY vector.
*     NSET = INTEGER*8 (Returned)
*        No. of pixels for which the specified QUALITY bit is set on exit.
*     NCLEAR = INTEGER*8 (Returned)
*        No. of pixels for which the specified QUALITY bit is cleared on
*        exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  VAX-specific features used:
*     -  Uses BYTE arrays.
*     -  Uses functions IBSET and IBCLR

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2019 East Asian Observatory
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
*     24-OCT-2019 (DSB):
*        Change to use 8-byte SIZE.
*     1-NOV-2019 (DSB):
*        Added arguments INIT, NSET and NCLEAR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants

*  Arguments Given:
      INTEGER BIT
      LOGICAL BAD
      LOGICAL SET
      LOGICAL INIT
      INTEGER*8 SIZE
      REAL MASK( SIZE )

*  Arguments Given and Returned:
      BYTE QUAL( SIZE )

*  Arguments Returned:
      INTEGER*8 NSET
      INTEGER*8 NCLEAR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 I                ! Loop count.
      INTEGER LBIT               ! Corrected bit number.

*  PRIMDAT type conversion functions.
      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
*.

*  Initialise
      NSET = 0
      NCLEAR = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Produce a bit number in the range 0 to (IRQ__QBITS - 1 ).
      LBIT = BIT - 1

*  If the bit is to be set...
      IF( SET ) THEN

*  ... and if bad pixels are to be set...
         IF( BAD ) THEN

*  ...loop round each pixel in the two vectors.
            DO I = 1, SIZE
               IF( MASK( I ) .EQ. VAL__BADR ) THEN
                  QUAL( I ) = NUM_ITOUB( IBSET( NUM_UBTOI( QUAL( I ) ),
     :                                        LBIT ) )
                  NSET = NSET + 1
               ELSE IF( INIT ) THEN
                  QUAL( I ) = NUM_ITOUB( IBCLR( NUM_UBTOI( QUAL( I ) ),
     :                                        LBIT ) )
               ELSE IF(  BTEST( NUM_UBTOI( QUAL( I ) ), LBIT ) ) THEN
                  NSET = NSET + 1

               END IF
            END DO

*  If good pixels are to be set...
         ELSE

*  ...loop round each pixel in the two vectors.
            DO I = 1, SIZE
               IF( MASK( I ) .NE. VAL__BADR ) THEN
                  QUAL( I ) = NUM_ITOUB( IBSET( NUM_UBTOI( QUAL( I ) ),
     :                                        LBIT ) )
                  NSET = NSET + 1
               ELSE IF( INIT ) THEN
                  QUAL( I ) = NUM_ITOUB( IBCLR( NUM_UBTOI( QUAL( I ) ),
     :                                        LBIT ) )
               ELSE IF(  BTEST( NUM_UBTOI( QUAL( I ) ), LBIT ) ) THEN
                  NSET = NSET + 1

               END IF
            END DO

         END IF

*  If the bit is to be cleared...
      ELSE

*  ... and if bad pixels are to be cleared...
         IF( BAD ) THEN

*  ...loop round each pixel in the two vectors.
            DO I = 1, SIZE
               IF( MASK( I ) .EQ. VAL__BADR ) THEN
                  QUAL( I ) = NUM_ITOUB( IBCLR( NUM_UBTOI( QUAL( I ) ),
     :                                        LBIT ) )
               ELSE IF( INIT ) THEN
                  NSET = NSET + 1
                  QUAL( I ) = NUM_ITOUB( IBSET( NUM_UBTOI( QUAL( I ) ),
     :                                        LBIT ) )
               ELSE IF( BTEST( NUM_UBTOI( QUAL( I ) ), LBIT ) ) THEN
                  NSET = NSET + 1

               END IF
            END DO

*  If good pixels are to be cleared...
         ELSE

*  ...loop round each pixel in the two vectors.
            DO I = 1, SIZE
               IF( MASK( I ) .NE. VAL__BADR ) THEN
                  QUAL( I ) = NUM_ITOUB( IBCLR( NUM_UBTOI( QUAL( I ) ),
     :                                        LBIT ) )
               ELSE IF( INIT ) THEN
                  NSET = NSET + 1
                  QUAL( I ) = NUM_ITOUB( IBSET( NUM_UBTOI( QUAL( I ) ),
     :                                        LBIT ) )
               ELSE IF( BTEST( NUM_UBTOI( QUAL( I ) ), LBIT ) ) THEN
                  NSET = NSET + 1

               END IF
            END DO

         END IF
      END IF

      NCLEAR = SIZE - NSET

      END
