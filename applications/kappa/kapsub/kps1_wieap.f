      SUBROUTINE KPS1_WIEAP( BAD, WLIM, EL, M, N, FILE_3, FILE_4,
     :                       FILE_2, FILE_6, STATUS )
*+
*  Name:
*     KPS1_WIEAP

*  Purpose:
*     Applies a pre-calculated Wiener filter to a supplied image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WIEAP( BAD, WLIM, EL, M, N, FILE_3, FILE_4, FILE_2,
*                      FILE_6, STATUS )

*  Description:
*     If no bad pixels are present in the input image, then its FT is
*     found and multiplied by the supplied filter function FT.  The
*     inverse FFT is taken of the resulting product to get the returned
*     image.
*
*     If there are any bad pixels in the input image, then these are
*     replaced by zero before doing the filtering.  By itself, this
*     would result in incorrect normalisation for any output pixels
*     affected by these zero values.  To correct this, a mask image is
*     formed holding the weight of every input pixel.  This is set to 1
*     for good pixels and 0 for bad pixels.  This mask image is also
*     filtered using the same filter, to get the weight of good pixels
*     at each output pixel.  Output pixels which have too low a weight
*     (as determined by WLIM) are set bad in the returned array. Others
*     are normalised by dividing the filtered image value by the
*     filtered mask value.

*  Arguments:
*     BAD = LOGICAL (Given)
*        It is .TRUE. if there may be any bad values in file 4.
*     WLIM = REAL (Given)
*        The lowest weight of good input pixels for which a good output
*        pixel should be produced.
*     EL = INTEGER (Given)
*        Total number of elements in each internal file.
*     M = INTEGER (Given)
*        Number of columns in the 2-d form of each internal file.
*     N = INTEGER (Given)
*        Number of rows in the 2-d form of each internal file.
*     FILE_3( EL ) = REAL (Given)
*        The Fourier transform of the Wiener-filter function.  Stored
*        in Hermitian form (see KPS1_WIEFL).
*     FILE_4( EL ) = REAL (Given and Returned)
*        Supplied holding the input image.  Returned holding the
*        filtered input image.
*     FILE_2( EL ) = REAL (Returned)
*        Work space.
*     FILE_6( EL ) = REAL (Returned)
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
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      LOGICAL BAD
      REAL WLIM
      INTEGER EL
      INTEGER M
      INTEGER N
      REAL FILE_3( EL )

*  Arguments Given and Returned:
      REAL FILE_4( EL )

*  Arguments Returned:
      REAL FILE_2( EL )
      REAL FILE_6( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Element count

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First perform the algorithm assuming there are some bad pixels in
*  the supplied image.
      IF ( BAD ) THEN

*  Replace each bad value with zero, and construct a mask image in
*  which each good pixel is represented by the value 1 and each bad
*  pixel by the value 0.
         DO I = 1, EL
            IF ( FILE_4( I ) .EQ. VAL__BADR ) THEN
               FILE_4( I ) = 0.0
               FILE_2( I ) = 0.0
            ELSE
               FILE_2( I ) = 1.0
            END IF
         END DO

*  Take the FFT of each of these images, storing the FTs back in the
*  original files (in Hermitian form - see KPG1_HMLTR).  File 6 is used
*  as work space.
         CALL KPG1_FFTFR( M, N, FILE_4, FILE_6, FILE_4, STATUS )
         CALL KPG1_FFTFR( M, N, FILE_2, FILE_6, FILE_2, STATUS )

*  Multiply each of these FTs by the FT of the Wiener filter function
*  stored in file 3.  The results are stored back in the same arrays.
         CALL KPG1_HMLTR( M, N, FILE_4, FILE_3, FILE_4, STATUS )
         CALL KPG1_HMLTR( M, N, FILE_2, FILE_3, FILE_2, STATUS )

*  Take the inverse FFT of each of the products, storing the resulting
*  images back in the original files.  File 6 is used as work space.
         CALL KPG1_FFTBR( M, N, FILE_4, FILE_6, FILE_4, STATUS )
         CALL KPG1_FFTBR( M, N, FILE_2, FILE_6, FILE_2, STATUS )

*  We now have the filtered image and the filtered mask.  Normalise
*  each pixel value in the filtered image by dividing it by the
*  corresponding filtered mask value.  This removes the influence of
*  the zero values introduced into the image in place of the original
*  bad values.  Pixels which have too low a weight (i.e. smoothed mask
*  value) are set bad.
         DO I = 1, EL
            IF ( FILE_2( I ) .GT. WLIM ) THEN
               FILE_4( I ) = FILE_4( I ) / FILE_2( I )
            ELSE
               FILE_4( I ) = VAL__BADR
            END IF
         END DO

*  Now perform the algorithm assuming there are no bad pixels in the
*  supplied image.
      ELSE

*  Take the FFT of the input image, storing the FT back in the
*  original file.  File 6 is used as work space.
         CALL KPG1_FFTFR( M, N, FILE_4, FILE_6, FILE_4, STATUS )

*  Multiply the FT by the FT of the Wiener filter function stored in
*  file 3.  The results are stored back in the same array.
         CALL KPG1_HMLTR( M, N, FILE_4, FILE_3, FILE_4, STATUS )

*  Take the inverse FFT of the product, storing the resulting image
*  back in the original files.  File 6 is used as work space.
         CALL KPG1_FFTBR( M, N, FILE_4, FILE_6, FILE_4, STATUS )

      END IF

      END
