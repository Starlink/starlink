      SUBROUTINE KPG1_NAG2R( NP, R, WORK )
*+
*  Name:
*     KPG1_NAG2R

*  Purpose:
*     Converts an NAG Hermitian Fourier transform array into an array 
*     usable by FFTPACK routine KPG1_RFFTB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NAG2R( NP, R, WORK )

*  Description:
*     This subroutine modifies the supplied array of Fourier co-efficients 
*     (as produced by NAG subroutine C06FAE) so that an inverse FFT can be 
*     performed on them using FFTPACK routine KPG1_RFFTB. The resulting 
*     inverse will have the same normalisation as the original data 
*     transformed using KPG1_RFFTF.
*
*     This function is equivalent to PDA_NAG2R except that it uses work
*     space for greater speed.

*  Arguments:
*     NP = INTEGER (Given)
*        The size of the array.
*     R( NP ) = REAL (Given and Returned)
*        The array holding the Fourier co-efficients. Supplied in NAG
*        format and returned in FFTPACK format.
*     WORK( NP ) = REAL (Given and Returned)
*        Work space.

*  Notes:
*     -  A call to KPG1_R2NAG followed by a call to KPG1_NAG2R will result in 
*     the original data being divided by NP.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-DEC-2003 (DSB):
*        Original version based on PDA_NAG2R.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER NP

*  Arguments Given and Returned:
      REAL R( NP )
      REAL WORK( NP )
      
*  Local Variables:
      REAL
     :        FAC                ! Normalisation factor
 
      INTEGER 
     :        I,                 ! Index of current input term.
     :        J                  ! Index of current output term.
   
*.

*  Store the normalisation factor
      FAC = 1.0/SQRT( REAL( NP ) )

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
