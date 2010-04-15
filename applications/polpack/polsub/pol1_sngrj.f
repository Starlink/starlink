      SUBROUTINE POL1_SNGRJ( NSIGMA, EL, DIN, VIN, DOUT, NREJ, NGOOD,
     :                       STATUS )
*+
*  Name:
*     POL1_SNGRJ

*  Purpose:
*     Reject aberrant data values from a single-beam input intensity image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGRJ( NSIGMA, EL, DIN, VIN, DOUT, NREJ, NGOOD, STATUS )

*  Description:
*     This routine copies the image supplied in DIN, returning the copy
*     in DOUT.
*
*     Values are returned bad in DOUT if the supplied value of DOUT
*     is greater than (NSIGMA**2)*VIN

*  Arguments:
*     CALL POL1_SNGRJ( NSIGMA, EL, DIN, VIN, DOUT, NREJ, NGOOD, STATUS )
*     NSIGMA = REAL (Given)
*        The rejection threshold for aberrant points, expressed as a
*        multiple of the standard deviation.
*     EL = INTEGER (Given)
*        The number of pixels in each image.
*     DIN( EL ) = REAL (Given)
*        The input NDF intensity values.
*     VIN( EL ) = REAL (Given)
*        The input variance values (either real or estimated).
*     DOUT( EL ) = REAL (Given and Returned)
*        On entry, this array holds the squared residual between the
*        NDF intensity value (DIN) and the intensity value implied by the
*        current estimate of the Stokes vector. On exit, this array holds
*        the copied values from DIN.
*     NREJ = INTEGER (Returned)
*        The number of values supplied in DOUT which were above the
*        threshold given bu NSIGMA and VIN.
*     NGOOD = INTEGER (Returned)
*        The number of good values returned in DOUT.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-FEB-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      REAL NSIGMA
      INTEGER EL
      REAL DIN( EL )
      REAL VIN( EL )

*  Arguments Given and Returned:
      REAL DOUT( EL )

*  Arguments Returned:
      INTEGER NREJ
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Element index
      REAL VARFAC                ! Variance factor
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of rejected pixels.
      NREJ = 0

*  Initialise the number of good pixels.
      NGOOD = 0

*  Store the variance factor.
      VARFAC = NSIGMA**2

*  Check each pixel.
      DO I = 1, EL

*  Check the supplied data value is valid.
         IF( DIN( I ) .NE. VAL__BADR ) THEN

*  Check the supplied squared residual and variance are valid.
            IF( DOUT( I ) .NE. VAL__BADR .AND.
     :          VIN( I ) .NE. VAL__BADR ) THEN

*  Store bad value if the squared residual is too large. Otherwise, copy
*  the supplied input data value.
               IF( DOUT( I ) .GT. VIN( I )*VARFAC ) THEN
                  DOUT( I ) = VAL__BADR
                  NREJ = NREJ + 1
               ELSE
                  DOUT( I ) = DIN( I )
                  NGOOD = NGOOD + 1
               END IF

*  If the variance or residual is not valid, store a bad output value.
            ELSE
               DOUT( I ) = VAL__BADR
               NREJ = NREJ + 1
            END IF

         END IF

      END DO

      END
