      SUBROUTINE KPS1_TROP2( NPIX, NLIN, PSFFFT, IN, OUT, STATUS )
*+
*  Name:
*     KPS1_TROP2

*  Purpose:
*     Creates an image using the transposed response matrix.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_TROP2( NPIX, NLIN, PSFFFT, IN, OUT, STATUS )

*  Description:
*     This is a version of the KPS1_TROP1 (TROPUS) routine which is used
*     by MEM2D.  This version is used by LUCY to do the same job
*     (convolve a image with the transposed PSF to produce a trial
*     image).  The *inverse* Fourier transform of the supplied image is
*     multipled by the *forward* Fourier transform of the PSF.  The
*     *forward* FFT of the product is then taken and returned in OUT.

*  Arguments:
*     NPIX = INTEGER (Given)
*        Number of pixels per line of the image.
*     NLIN = INTEGER (Given)
*        Number of lines in the image.
*     PSFFFT( NPIX, NLIN ) = REAL (Given)
*        The Fourier transform of the point-spread function (NOT
*        transposed).
*     IN( NPIX, NLIN ) = REAL (Given and Return)
*        The input image.  Over-written by internal workings on return.
*        It should contain no bad values.
*     OUT( NPIX, NLIN ) = REAL (Returned)
*        The output image.
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
*     27-FEB-1995 (DSB):
*        Original version, based on KPS1_TROP1.
*     1995 October 28 (MJC):
*        Shortened long lines, minor stylistic changes for consistency
*        and clarity.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPIX
      INTEGER NLIN
      REAL PSFFFT( NPIX, NLIN )

*  Arguments Given and Returned:
      REAL IN( NPIX, NLIN )

*  Arguments Returned:
      REAL OUT( NPIX, NLIN )

*  Global Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Take the inverse FFT of the input data, storing the result back in
*  the array IN.  KPG1_FFTBR cannot be used to do this beccause
*  KPG1_FFTBR expects an Hermitian input image, and we only have a
*  purely real image.  Therefore, take the forward FFT using KPG1_FFTFR
*  (which expects a purely real image as input and creates an Hermitian
*  output image holding the FFT), and then take the complex conjugate
*  of the result to get the inverse FFT in Hermitian format.
      CALL KPG1_FFTFR( NPIX, NLIN, IN, OUT, IN, STATUS )
      CALL KPG1_HCONR( NPIX, NLIN, IN, STATUS )

*  Multiply the inverse FFT of the input image by the FFT of the PSF.
*  Store the result back in the array IN.
      CALL KPG1_HMLTR( NPIX, NLIN, IN, PSFFFT, IN, STATUS )

*  Take the forward FFT of the product.  This is done by first taking
*  the complex conjugate of the product, and then performing an inverse
*  FFT.
      CALL KPG1_HCONR( NPIX, NLIN, IN, STATUS )
      CALL KPG1_FFTBR( NPIX, NLIN, IN, IN, OUT, STATUS )

      END
