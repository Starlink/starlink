      SUBROUTINE ARY_SBND( NDIM, LBND, UBND, IARY, STATUS )
*+
*  Name:
*     ARY_SBND

*  Purpose:
*     Set new pixel-index bounds for an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_SBND( NDIM, LBND, UBND, IARY, STATUS )

*  Description:
*     The routine sets new pixel-index bounds for an array (or array
*     section). The number of array dimensions may also be changed.  If
*     a base array is specified, then a permanent change is made to the
*     actual data object and this will be apparent through any other
*     array identifiers which refer to it.  However, if an identifier
*     for an array section is specified, then its bounds are altered
*     without affecting other arrays.

*  Arguments:
*     NDIM = INTEGER (Given)
*        New number of array dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        New lower pixel-index bounds of the array.
*     UBND( NDIM ) = INTEGER (Given)
*        New upper pixel-index bounds of the array,
*     IARY = INTEGER (Given)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The bounds of an array section cannot be altered while it is
*     mapped for access through the identifier supplied to this
*     routine.
*     -  The bounds of a base array cannot be altered while any part of
*     it is mapped for access (i.e. even through another identifier).
*     -  The array's pixel values (if defined) will be retained for
*     those pixels which lie within both the old and new bounds. Any
*     pixels lying outside the new bounds will be lost (and cannot
*     later be recovered by further changes to the array's bounds).
*     Any new pixels introduced where the new bounds extend beyond the
*     old ones will be assigned the "bad" value, and the subsequent
*     value of the bad-pixel flag will reflect this.
*     -  If the bounds of a base array are to be altered and retention
*     of the existing pixel values is not required, then a call to
*     ARY_RESET should be made before calling this routine. This will
*     eliminate any processing which might otherwise be needed to
*     retain the existing values. This step is not necessary with an
*     array section, where no processing of pixel values takes place.

*  Algorithm:
*     -  Check the new bounds for validity.
*     -  Import the array identifier.
*     -  Check that BOUNDS access to the array is available.
*     -  Set the new bounds.
*     -  If an error occurred, then report context information.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-DEC-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( * )
      INTEGER UBND( * )
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the new bounds for validity.
      CALL ARY1_VBND( NDIM, LBND, UBND, STATUS )

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Check that BOUNDS access to the array is available.
      CALL ARY1_CHACC( IACB, 'BOUNDS', STATUS )

*  Set the new bounds.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ARY1_SBND( NDIM, LBND, UBND, IACB, STATUS )
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_SBND_ERR',
     :   'ARY_SBND: Error setting new pixel-index bounds for an ' //
     :   'array.', STATUS )
         CALL ARY1_TRACE( 'ARY_SBND', STATUS )
      END IF

      END
