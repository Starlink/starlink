      SUBROUTINE PDA_DR2NAG( NP, R )
*+
*  Name:
*     PDA_DR2NAG

*  Purpose:
*     Converts an FFTPACK Hermitian Fourier transform array into
*     the equivalent NAG array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_DR2NAG( NP, R )

*  Description:
*     This subroutine re-orders and normalises the supplied array of
*     Fourier co-efficients (as produced by FFTPACK subroutine PDA_DRFFTF)
*     so that the returned array looks like the equivalent array returned
*     by NAG routine C06FAF.
*
*     The real and imaginary co-efficients produced by PDA_DRFFTF are numerically
*     larger than the corresponding C06FAF co-efficients by a factor of
*     SQRT( NP ), and are ordered differently. Both routines return A0
*     (the zeroth real term, i.e. the DC level in the array) in element 1.
*     PDA_DRFFTF then has corresponding real and imaginary terms in adjacent
*     elements, whereas C06FAF has all the real terms together, followed by
*     all the imaginary terms (in reverse order):
*
*        PDA_DRFFTF:  A0,    A1, B1,     A2, B2,     A3, B3,   ...
*        C06FAF:      A0,    A1, A2, A3, ...,        ..., B3, B2, B1
*
*     The zeroth imaginary term (B0) always has the value zero and so is
*     not stored in the array. Care has to be taken about the parity of the
*     array size. If it is even, then there is one more real term than
*     there is imaginary terms (excluding A0), i.e. if NP = 10, then the
*     co-efficients are stored as follows:
*
*        PDA_DRFFTF:  A0, A1, B1, A2, B2, A3, B3, A4, B4, A5
*        C06FAF:      A0, A1, A2, A3, A4, A5, B4, B3, B2, B1
*
*     If NP = 9, then the co-efficients are stored as follows:
*
*        PDA_DRFFTF:  A0, A1, B1, A2, B2, A3, B3, A4, B4
*        C06FAF:      A0, A1, A2, A3, A4, B4, B3, B2, B1

*  Arguments:
*     NP = INTEGER (Given)
*        The size of the array.
*     R( NP ) = DOUBLE PRECISION (Given and Returned)
*        The array holding the Fourier co-efficients. Supplied in FFTPACK
*        format and returned in NAG format.

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

*  Loop round each of the imaginary terms in reverse order. Save each
*  one in a temporary variable. Even sized FFTPACK arrays end in with
*  a real term, whereas odd sized arrays end with an imaginary term
*  (which doesn't need to be moved). Use integer division to achieve
*  the required effect.
      DO IB = 2*( ( NP + 1 )/2 ) - 1, 3, -2
         TMP = R( IB )

*  Now shunt the high index end of the array down one place, over-writing
*  the element which previously held the current imaginary term.
         DO J= IB, NP - 1
            R( J ) = R( J + 1 )
         END DO

*  Normalise the imaginary term and store it at the end of the array.
         R( NP ) = TMP*FAC

      END DO

*  The real terms have not yet been normalised. Do it now.
      DO J = 1, NP/2 + 1
         R( J ) = R( J )*FAC
      END DO

      END
