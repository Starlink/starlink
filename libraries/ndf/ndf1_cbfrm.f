      SUBROUTINE NDF1_CBFRM( NDIM, LBND, UBND, FORM, STATUS )
*+
*  Name:
*     NDF1_CBFRM

*  Purpose:
*     Convert a storage form string if NDF bounds require it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CBFRM( NDIM, LBND, UBND, FORM, STATUS )

*  Description:
*     The routine checks an NDF's bounds for compatibility with an
*     array storage form string and converts the string to describe an
*     alternative storage form if necessary.

*  Arguments:
*     NDIM = INTEGER (Given)
*        Number of NDF dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        NDF lower bounds.
*     UBND( NDIM ) = INTEGER (Given)
*        NDF upper bounds.
*     FORM = CHARACTER * ( * ) (Given and Returned)
*        Array storage form (case insensitive).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine converts a storage form of 'PRIMITIVE' to 'SIMPLE'
*     if any of the NDF's lower bounds are not equal to 1.

*  Algorithm:
*     -  See if the storage form is primitive.
*     -  If so, then loop to check the lower bound of each dimension.
*     -  If any lower bound if not 1, then convert the storage form to
*     simple.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     22-OCT-1990 (RFWS):
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
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Arguments Given and Returned:
      CHARACTER * ( * ) FORM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the storage form is primitive.
      IF ( CHR_SIMLR( FORM, 'PRIMITIVE' ) ) THEN

*  If so, then loop to check the lower bound of each dimension.
         DO 1 I = 1, NDIM

*  If any lower bound is not 1, then convert the storage form to simple.
            IF ( LBND( I ) .NE. 1 ) THEN
               FORM = 'SIMPLE'
               GO TO 2
            END IF
 1       CONTINUE
 2       CONTINUE
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CBFRM', STATUS )

      END
