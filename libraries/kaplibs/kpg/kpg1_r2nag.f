      SUBROUTINE KPG1_R2NAG( NP, R, WORK )
*+
*  Name:
*     KPG1_R2NAG

*  Purpose:
*     Converts an FFTPACK Hermitian Fourier transform array into
*     the equivalent NAG array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_R2NAG( NP, R, WORK )

*  Description:
*     This subroutine re-orders and normalises the supplied array of
*     Fourier co-efficients (as produced by FFTPACK subroutine KPG1_RFFTF)
*     so that the returned array looks like the equivalent array returned
*     by NAG routine C06FAE.
*
*     This function is equivalent to PDA_R2NAG except that it uses work
*     space for greater speed.
*
*     The real and imaginary co-efficients produced by KPG1_RFFTF are numerically
*     larger than the corresponding C06FAE co-efficients by a factor of
*     SQRT( NP ), and are ordered differently. Both routines return A0
*     (the zeroth real term, i.e. the DC level in the array) in element 1.
*     KPG1_RFFTF then has corresponding real and imaginary terms in adjacent
*     elements, whereas C06FAE has all the real terms together, followed by
*     all the imaginary terms (in reverse order):
*
*        KPG1_RFFTF :  A0,    A1, B1,     A2, B2,     A3, B3,   ...
*        C06FAE:       A0,    A1, A2, A3, ...,        ..., B3, B2, B1
*
*     The zeroth imaginary term (B0) always has the value zero and so is
*     not stored in the array. Care has to be taken about the parity of the
*     array size. If it is even, then there is one more real term than
*     there is imaginary terms (excluding A0), i.e. if NP = 10, then the
*     co-efficients are stored as follows:
*
*        KPG1_RFFTF :  A0, A1, B1, A2, B2, A3, B3, A4, B4, A5
*        C06FAE:       A0, A1, A2, A3, A4, A5, B4, B3, B2, B1
*
*     If NP = 9, then the co-efficients are stored as follows:
*
*        KPG1_RFFTF :  A0, A1, B1, A2, B2, A3, B3, A4, B4
*        C06FAE:       A0, A1, A2, A3, A4, B4, B3, B2, B1

*  Arguments:
*     NP = INTEGER (Given)
*        The size of the array.
*     R( NP ) = REAL (Given and Returned)
*        The array holding the Fourier co-efficients. Supplied in FFTPACK
*        format, returned in NAG format.
*     WORK( NP ) = REAL (Given and Returned)
*        Work space.

*  Copyright:
*     Copyright (C) 2003 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-DEC-2003 (DSB):
*        Original version based on PDA_R2NAG.
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

*  Scale the remaining real terms and store in the work array.
      J = 2
      DO I = 2, NP, 2
         WORK( J ) = R( I )*FAC
         J = J + 1
      END DO

*  Scale the imaginary terms and store in the work array.
      J = NP
      DO I = 3, NP, 2
         WORK( J ) = R( I )*FAC
         J = J - 1
      END DO

* Copy the work array back into the supplied array.
      DO I = 2, NP
         R( I ) = WORK( I )
      END DO

      END
