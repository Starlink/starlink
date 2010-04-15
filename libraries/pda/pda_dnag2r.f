      SUBROUTINE PDA_DNAG2R( NP, R )
*+
*  Name:
*     PDA_DNAG2R

*  Purpose:
*     Converts an NAG Hermitian Fourier transform array into an array
*     usable by FFTPACK routine PDA_DRFFTB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_DNAG2R( NP, R )

*  Description:
*     This subroutine modifies the supplied array of Fourier co-efficients
*     (as produced by NAG subroutine C06FAF) so that an inverse FFT can be
*     performed on them using FFTPACK routine PDA_DRFFTB. The resulting inverse
*     will have the same normalisation as the original data transformed
*     using PDA_DRFFTF.

*  Arguments:
*     NP = INTEGER (Given)
*        The size of the array.
*     R( NP ) = DOUBLE PRECISION (Given and Returned)
*        The array holding the Fourier co-efficients. Supplied in NAG
*        format and returned in FFTPACK format.

*  Notes:
*     -  A call to PDA_DR2NAG followed by a call to PDA_DNAG2R will result in
*     the original data being divided by NP.
*     -  Some speed is sacrificed in order to perform the conversion
*     in-situ.

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

*  Arguments Given and Returned:
      DOUBLE PRECISION R( NP )

*  Local Variables:
      DOUBLE PRECISION
     :        FAC,               ! Normalisation factor
     :        TMP                ! Temporary storage for imaginary term

      INTEGER
     :        IB,                ! Index of current imaginary term.
     :        J                  ! Loop count

*.

*  Store the normalisation factor
      FAC = 1.0D0/SQRT( DBLE( NP ) )

*  Normalise the real terms.
      DO J = 1, NP/2 + 1
         R( J ) = R( J )*FAC
      END DO

*  Loop round each of the elements where FFTPACK wants the imaginary
*  terms to be placed.
      DO IB = 3, 2*( ( NP + 1 )/2 ) - 1, 2

*  Save the imaginary term which should go in this element in a temporary
*  variable.
         TMP = R( NP )

*  Now shunt the high index end of the array up one place, over-writing
*  the element which previously held the current imaginary term and
*  creating a space for the current imaginary element.
         DO J= NP - 1, IB, -1
            R( J + 1 ) = R( J )
         END DO

*  Normalise the imaginary term and store it in the array.
         R( IB ) = TMP*FAC

      END DO

      END
