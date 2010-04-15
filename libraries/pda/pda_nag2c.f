      SUBROUTINE PDA_NAG2C( NP, X, Y, R )
*+
*  Name:
*     PDA_NAG2C

*  Purpose:
*     Converts a NAG complex Fourier transform array into an array
*     usable by FFTPACK routine PDA_CFFTB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_NAG2C( NP, X, Y, R )

*  Description:
*     This subroutine returns a modified version of the supplied Fourier
*     co-efficients (as produced by NAG subroutine C06FCE). An inverse FFT
*     can be performed on the returned array using FFTPACK routine PDA_CFFTB,
*     and the resulting inverse will have the same normalisation as the
*     original data transformed using PDA_CFFTF. See PDA_C2NAG for more details.

*  Arguments:
*     NP = INTEGER (Given)
*        The number of points in the transform.
*     X( NP ) = REAL (Given)
*        The real co-efficients, in NAG format.
*     Y( NP ) = REAL (Given)
*        The imaginary co-efficients, in NAG format.
*     R( 2, NP ) = REAL (Returned)
*        The output co-efficients, in FFTPACK format.

*  Notes:
*     -  A call to PDA_C2NAG followed by a call to PDA_NAG2C will result in
*     the original data being divided by NP.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-FEB-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NP
      REAL X( NP )
      REAL Y( NP )

*  Arguments Returned:
      REAL R( 2, NP )

*  Local Variables:
      REAL FAC                   ! Normalisation factor
      INTEGER J                  ! Loop count

*.

*  Store the normalisation factor
      FAC = 1.0/SQRT( REAL( NP ) )

*  Loop round each pair of co-efficients.
      DO J = 1, NP

*  Normalise them and store them in the output array.
         R( 1, J ) = FAC*X( J )
         R( 2, J ) = FAC*Y( J )

      END DO

      END
