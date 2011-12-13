      SUBROUTINE KPG1_HRCPR( M, N, BADVAL, D, STATUS )
*+
*  Name:
*     KPG1_HRCPx

*  Purpose:
*     Finds the reciprocal of a purely real Hermitian image

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_HRCPx( M, N, BADVAL, D, STATUS )

*  Description:
*     This routine replaces the supplied Hermitian image with one in
*     which the real terms have been inverted (i.e. replaced by their
*     reciprocal) and the imaginary terms have been set to zero.
*
*     To form 1/H where H is a general complex Hermitian image, the
*     numerator and denominator are both multiplied by the complex
*     conjugate of H ("H*").  The denominator then becomes purely real
*     and equal to the modulus squared of H.  The reciprocal of the
*     denominator is then taken using this routine and the resulting
*     array is multiplied by H*.  Thus the steps are:
*
*     1) Call KPG1_HCONx to form H*, the complex conjugate of H.
*     2) Call KPG1_HMLTx to multiply H by H*.  This gives a purely real
*        image holding the squared modulus of H.
*     3) Call KPG1_HRCPx to take the reciprocal of H.H*.
*     4) Call KPG1_HMLTx to multiply the reciprocal of H.H* by H*.

*  Arguments:
*     M = INTEGER (Given)
*        The number of columns in the Hermitian image D.
*     N = INTEGER (Given)
*        The number of lines in the Hermitian image D.
*     BADVAL = ? (Given)
*        A value to return instead of the reciprocal of the real term,
*        if any real term is zero.
*     D( M, N ) = ? (Given and Returned)
*        On entry, a purely real Hermitian image.  On exit, an
*        Hermitian image representing the reciprocal of the supplied
*        image (also purely real).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for the double precision and real data
*     types: replace "x" in the routine names by D or R as appropriate.
*     The BADVAL and D arguments must have the data type specified.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-MAR-1995 (DSB):
*        Original version.
*     1995 March 28 (MJC):
*        Added notes.  Made to adhere to standard generic form, and
*        numerous minor stylistic changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER M
      INTEGER N
      REAL BADVAL

*  Arguments Given and Returned:
      REAL D( M, N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL DVAL               ! The input data value
      INTEGER I                  ! Column index
      INTEGER J                  ! Line index
      INTEGER MHP1               ! Commonly used value
      INTEGER MHP2               ! Commonly used value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store often-used values.
      MHP1 = M / 2 + 1
      MHP2 = MHP1 + 1

*  Loop round all the lines in the bottom-left quadrant.
      DO J = 1, N / 2 + 1

*  Loop round all the columns in the bottom-left quadrant.
         DO I = 1, MHP1
            DVAL = D( I, J )

*  Replace the supplied real term with its inverse.  Substitute the
*  supplied bad value if the real value is zero.
            IF ( DVAL .NE. 0.0E0 ) THEN
               D( I, J ) = 1.0E0 / DVAL
            ELSE
               D( I, J ) = BADVAL
            END IF

         END DO

      END DO

      END
