      SUBROUTINE IRA1_STOKN( TOKEN, VALUE, NA, NB, STRA, STRB, STATUS )
*+
*  Name:
*     IRA1_STOKN

*  Purpose:
*     Substitute a numeric value into a set of transformation functions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_STOKN( TOKEN, VALUE, NA, NB, STRA, STRB, STATUS )

*  Description:
*     The functions are supplied in the two arrays STRA and STRB. No
*     error is generated if the token does not appear in the strings.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The name of the token.
*     VALUE = DOUBLE PRECISION (Given)
*        The value to substitute for the token.
*     NA = INTEGER (Given)
*        The number of elements in array STRA.
*     NB = INTEGER (Given)
*        The number of elements in array STRB.
*     STRA( NA ) = CHARACTER * ( * ) (Given and Returned)
*        The first set of functions in which to do the substitution.
*     STRB( NB ) = CHARACTER * ( * ) (Given and Returned)
*        The second set of functions in which to do the substitution.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1990 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      CHARACTER TOKEN*(*)
      DOUBLE PRECISION VALUE
      INTEGER   NA
      INTEGER   NB

*  Arguments Given and Returned:
      CHARACTER STRA( NA )*(*)
      CHARACTER STRB( NB )*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter.
      INTEGER NSUBS              ! No. of substitutions made.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do the substitutions by calling TRANSFORM routine TRN_STOKR.
      DO I = 1, NA
         CALL TRN_STOKD( TOKEN, VALUE, STRA(I), NSUBS, STATUS )
      END DO

      DO I = 1, NB
         CALL TRN_STOKD( TOKEN, VALUE, STRB(I), NSUBS, STATUS )
      END DO

      END
