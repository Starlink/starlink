      SUBROUTINE CCD1_LTRCM( TA, TB, TOUT, STATUS )
*+
*  Name:
*     CCD1_LTRCM

*  Purpose:
*     Combine two linear coordinate transformations into one

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL CCD1_LTRCM( TA, TB, TOUT, STATUS )

*  Description:
*     Forms a set of linear transformation coefficients TOUT which are
*     equivalent to applying the transformation represented by
*     the coefficients TA and TB consecutively.

*  Arguments:
*     TA( 6 ) = DOUBLE PRECISION (Given)
*        The first set of transformation coefficients
*     TB( 6 ) = DOUBLE PRECISION  (Given)
*        The second set of transformation coefficients
*     TOUT( 6 ) = REAL (Returned)
*        The output transformation coefficients.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Authors:
*     DUVAD::RFWS: R.F.Warren-Smith (Durham Polarimetry Group)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     (DUVAD::RFWS):
*        Original version.
*     26-JUN-1991 (DUVAD::TMG):
*        change prologue to STARLINK format
*     27-JUL-1992 (PDRAPER):
*        Changed to use STATUS and DOUBLE PRECISION.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION TA( 6 )
      DOUBLE PRECISION TB( 6 )

*  Arguments Returned:
      DOUBLE PRECISION TOUT( 6 )

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  Combine the transformations.
      TOUT( 1 ) = TB( 1 ) + TB( 2 ) * TA( 1 ) + TB( 3 ) * TA( 4 )
      TOUT( 2 ) = TA( 2 ) * TB( 2 ) + TB( 3 ) * TA( 5 )
      TOUT( 3 ) = TB( 2 ) * TA( 3 ) + TB( 3 ) * TA( 6 )
      TOUT( 4 ) = TB( 4 ) + TB( 5 ) * TA( 1 ) + TB( 6 ) * TA( 4 )
      TOUT( 5 ) = TB( 5 ) * TA( 2 ) + TB( 6 ) * TA( 5 )
      TOUT( 6 ) = TB( 5 ) * TA( 3 ) + TB( 6 ) * TA( 6 )

      END
* $Id$
