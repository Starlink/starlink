      SUBROUTINE KPG1_DNAG2R( NP, R, WORK )
*+
*  Name:
*     KPG1_DNAG2R

*  Purpose:
*     Converts an NAG Hermitian Fourier transform array into an array 
*     usable by FFTPACK routine KPG1_DRFFTB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_DNAG2R( NP, R, WORK )

*  Description:
*     This subroutine modifies the supplied array of Fourier co-efficients 
*     (as produced by NAG subroutine C06FAF) so that an inverse FFT can be 
*     performed on them using FFTPACK routine KPG1_DRFFTB. The resulting inverse 
*     will have the same normalisation as the original data transformed
*     using KPG1_DRFFTF.
*
*     This function is equivalent to PDA_NAG2R except that it uses work
*     space for greater speed.

*  Arguments:
*     NP = INTEGER (Given)
*        The size of the array.
*     R( NP ) = DOUBLE PRECISION (Given and Returned)
*        The array holding the Fourier co-efficients. Supplied in NAG
*        format and returned in FFTPACK format.
*     WORK( NP ) = DOUBLE PRECISION (Given and Returned)
*        Work space.

*  Notes:
*     -  A call to KPG1_DR2NAG followed by a call to KPG1_DNAG2R will result in 
*     the original data being divided by NP.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-DEC-2003 (DSB):
*        Original version based on PDA_DNAG2R.
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
      DOUBLE PRECISION WORK( NP )
      
*  Local Variables:
      DOUBLE PRECISION
     :        FAC                ! Normalisation factor
 
      INTEGER 
     :        I,                 ! Index of current input term.
     :        J                  ! Index of current output term.
   
*.

*  Store the normalisation factor
      FAC = 1.0D0/SQRT( DBLE( NP ) )

*  Scale the first element (A0).
      R( 1 ) = R( 1 )*FAC

* Copy the supplied array into the work array.
      DO I = 2, NP
         WORK( I ) = R( I ) 
      END DO

*  Scale the remaining real terms and store in the work array.
      J = 2
      DO I = 2, NP, 2
         R( I ) = WORK( J )*FAC
         J = J + 1
      END DO

*  Scale the imaginary terms and store in the work array.
      J = NP
      DO I = 3, NP, 2
         R( I ) = WORK( J )*FAC
         J = J - 1
      END DO

      END
