      SUBROUTINE KPS1_OP2( NPIX, NLIN, PSFFFT, IN, OUT, STATUS )
*+
*  Name:
*     KPS1_OP2

*  Purpose:
*     Creates a simulated data set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_OP2( NPIX, NLIN, PSFFFT, IN, OUT, STATUS )

*  Description:
*     This is a version of the KPS1_OP1 (OPUS) routine which is used by
*     MEM2D.  This version is used by LUCY to do the same job (convolve
*     an image with the PSF).

*  Arguments:
*     NPIX = INTEGER (Given)
*        Number of pixels per line of the image.
*     NLIN = INTEGER (Given)
*        Number of lines in the image.
*     PSFFFT( NPIX, NLIN ) = REAL (Given)
*        The Fourier transform of the PSF.
*     IN( NPIX, NLIN ) = REAL (Given and Return)
*        The input image.  Over-written by internal workings on return.
*        It should contain no bad values.
*     OUT( NPIX, NLIN ) = REAL (Returned)
*        The output simulated data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1995 (DSB):
*        Original version, based on KPS1_OP1.
*     1995 October 28 (MJC):
*        Shortened long lines, minor stylistic changes for consistency
*        and clarity.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

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
      INTEGER STATUS

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Take the FFT of the input image, storing the result back in the
*  array IN.  The array OUT is used here as work space.
      CALL KPG1_FFTFR( NPIX, NLIN, IN, OUT, IN, STATUS )

*  Multiply the FFT of the input image by the FFT of the PSF.  Store
*  the result back in the array IN.
      CALL KPG1_HMLTR( NPIX, NLIN, IN, PSFFFT, IN, STATUS )

*  Take the inverse FFT of the product to get the simulated data.  IN
*  is used as work space.
      CALL KPG1_FFTBR( NPIX, NLIN, IN, IN, OUT, STATUS )

      END
