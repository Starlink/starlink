      SUBROUTINE PDA_DC2NAG( NP, R, X, Y )
*+
*  Name:
*     PDA_DC2NAG

*  Purpose:
*     Converts an FFTPACK complex Fourier transform array into
*     the equivalent NAG arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_DC2NAG( NP, R, X, Y )

*  Description:
*     This subroutine re-orders and normalises the supplied array of
*     Fourier co-efficients (as produced by FFTPACK subroutine PDA_DCFFTF)
*     so that the returned arrays looks like the equivalent arrays returned
*     by NAG routine C06FCF.
*
*     The real and imaginary co-efficients produced by PDA_DCFFTF are numerically
*     larger than the corresponding C06FCF co-efficients by a factor of
*     SQRT( NP ), and are stored differently. NAG uses two separate
*     1-dimensional arrays to store the real and imaginary co-efficients,
*     whereas FFTPACK stored them in a single two dimensional array (each
*     row holds a pair of corresponding real and imaginary co-efficients).

*  Arguments:
*     NP = INTEGER (Given)
*        The number of points in the transform.
*     R( 2, NP ) = DOUBLE PRECISION (Given)
*        The input co-efficients, in FFTPACK format.
*     X( NP ) = DOUBLE PRECISION (Returned)
*        The real co-efficients, in NAG format.
*     Y( NP ) = DOUBLE PRECISION (Returned)
*        The imaginary co-efficients, in NAG format.

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
      DOUBLE PRECISION R( 2, NP )

*  Arguments Returned:
      DOUBLE PRECISION X( NP )
      DOUBLE PRECISION Y( NP )

*  Local Variables:
      DOUBLE PRECISION FAC       ! Normalisation factor
      INTEGER J                  ! Loop count

*.

*  Store the normalisation factor
      FAC = 1.0/SQRT( DBLE( NP ) )

*  Loop round each pair of co-efficients.
      DO J = 1, NP

*  Normalise them and store them in the output arrays.
         X( J ) = FAC*R( 1, J )
         Y( J ) = FAC*R( 2, J )

      END DO

      END
