      SUBROUTINE KPS1_WIEFL( EL, M, N, PN, QUIET, FILE_6, FILE_3,
     :                       FILE_2, STATUS )
*+
*  Name:
*     KPS1_WIEFL

*  Purpose:
*     Constructs the Fourier transform of a Wiener-filter function

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WIEFL( EL, M, N, PN, QUIET, FILE_6, FILE_3, FILE_2,
*                      STATUS )

*  Description:
*     This routine constructs the FT of a Wiener filter.  To apply the
*     filter to an image, the FT of the image should be multiplied by
*     the FT of the filter, and the inverse FT found of the product.
*     The FT of the filter function is:
*
*                *
*               H
*        -----------------
*            2      Pn
*         |H|   +  ----
*                   Pg
*
*            *                                                   2
*     where H  is the complex conjugate of the FT of the PSF, |H|  is
*     the square of the modulus of the FT of the PSF, Pn is the mean
*     noise power per pixel in the input image, and Pg is the mean power
*     per pixel in a model image. The model image is assumed to be
*     noiseless in this expression.  If the model image contains noise
*     (i.e. QUIET=.FALSE.) then Pg is replaced by ( Pg - Pn ) in the
*     above expression.
*
*     This routine uses Hermitian arrays to store both the real and
*     imaginary parts of an FT in a single array.  See KPG1_HMLTR for
*     more information about Hermitian arrays.

*  Arguments:
*     EL = INTEGER (Given)
*        Total number of elements in each internal file.
*     M = INTEGER (Given)
*        Number of columns in the 2-d form of each internal file.
*     N = INTEGER (Given)
*        Number of rows in the 2-d form of each internal file.
*     PN = REAL (Given)
*        The mean noise power per pixel.
*     QUIET = LOGICAL (Given)
*        If .TRUE. then the supplied model power in file 6 does not
*        include any noise power.  If .FALSE., then the noise power
*        will be subtracted off the supplied model power.
*     FILE_6( EL ) = REAL (Given and Returned)
*        The 2-d power spectrum for the model image.  If QUIET is
*        .FALSE., this will be returned with the noise power given by
*        PN subtracted from every pixel.
*     FILE_3( EL ) = REAL (Given and Returned)
*        On input it is the Fourier transform of the PSF.  Upon return
*        it is the Fourier transform of the Wiener filter function.
*     FILE_2( EL ) = REAL (Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-1995 (DSB):
*        Original version.
*     1995 March 28 (MJC):
*        Shortened long lines and minor stylistic changes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      INTEGER M
      INTEGER N
      REAL PN
      LOGICAL QUIET

*  Arguments Given and Returned:
      REAL FILE_6( EL )
      REAL FILE_3( EL )

*  Arguments Returned:
      REAL FILE_2( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the FT of the PSF to file 2.
      DO I = 1, EL
         FILE_2( I ) = FILE_3( I )
      END DO

*  Over-write file 3 with the complex conjugate of the FT of the PSF.
      CALL KPG1_HCONR( M, N, FILE_3, STATUS )

*  Multiply the FT of the PSF by the complex conjugate of the FT of the
*  PSF to get an approximation of the PSF power spectrum in file 2.
*  The imaginary parts of the resulting FT will be zero.  This means
*  that only the bottom-left quadrant of the image will have non-zero
*  values in it.
      CALL KPG1_HMLTR( M, N, FILE_3, FILE_2, FILE_2, STATUS )

*  If the model power spectrum includes the noise power, subtract it
*  (the resulting negative imaginary terms will be ignored).
      IF ( .NOT. QUIET ) THEN
         DO I = 1, EL
            FILE_6( I ) = FILE_6( I ) - PN
         END DO
      END IF

*  Form the FT of the filter.  Loop round every element...
      DO I = 1, EL

*  If the model power is not OK, store zero for the filter denominator.
         IF ( FILE_6( I ) .LE. 0.0 ) THEN
            FILE_2( I ) = 0.0

*  Otherwise form the denominator of the filter function.
         ELSE
            FILE_2( I ) = FILE_2( I ) + PN / FILE_6( I )

         END IF

      END DO

*  Find the reciprocal of the real part of the filter denominator (the
*  imaginary parts must be zero).  Any values for which the denominator
*  is zero (i.e. pixels with zero model power) are set to zero.
      CALL KPG1_HRCPR( M, N, 0.0, FILE_2, STATUS )

*  Multiply the reciprocal of the filter denominator by the complex
*  conjugate of the FT of the PSF.  The result is the final filter.
      CALL KPG1_HMLTR( M, N, FILE_3, FILE_2, FILE_3, STATUS )

      END
