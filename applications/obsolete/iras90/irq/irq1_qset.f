      SUBROUTINE IRQ1_QSET( BIT, SET, SIZE, QUAL, STATUS )
*+
*  Name:
*     IRQ1_QSET

*  Purpose:
*     Set a QUALITY bit for all pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_QSET( BIT, SET, SIZE, QUAL, STATUS )

*  Description:
*     If SET is true then all pixels in the QUALITY vector have the
*     specified bit set. If SET is false, then all pixels have
*     the specified bit cleared.

*  Arguments:
*     BIT = INTEGER (Given)
*        The bit number within the QUALITY component. The least
*        significant bit is called bit 1 (not bit 0).
*     SET = LOGICAL (Given)
*        If true, then all pixels have the given bit set, indicating
*        that the corresponding quality is held.  If false, then all
*        pixels have the given bit cleared, indicating that the
*        corresponding quality is not held.
*     SIZE = INTEGER (Given)
*        The size of the QUAL vector.
*     QUAL( SIZE ) = BYTE (Given and Returned)
*        The QUALITY vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  VAX-specific features used:
*     -  Uses BYTE arrays.
*     -  Uses functions IBSET and IBCLR

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
      LOGICAL SET
      INTEGER SIZE

*  Arguments Given and Returned:
      BYTE QUAL( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
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

*  ...loop round each pixel.
         DO I = 1, SIZE
            QUAL( I ) = NUM_ITOUB( IBSET(
     :                       NUM_UBTOI( QUAL( I ) ), LBIT ) )
         END DO

*  If the bit is to be cleared...
      ELSE

*  ...loop round each pixel.
         DO I = 1, SIZE
            QUAL( I ) = NUM_ITOUB( IBCLR(
     :                       NUM_UBTOI( QUAL( I ) ), LBIT ) )
         END DO

      END IF

      END
