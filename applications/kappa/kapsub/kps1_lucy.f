      SUBROUTINE KPS1_LUCY( N, NPIX, NLIN, XMARG, YMARG, AIM, NITER,
     :                      CHIFAC, WLIM, SNYDER, FILE_3, FILE_4,
     :                      FILE_6, FILE_8, FILE_1, FILE_2, FILE_5,
     :                      FILE_7, STATUS )
*+
*  Name:
*     KPS1_LUCY

*  Purpose:
*     Deconvolves an image using the Richardson-Lucy deconvolution
*     algorithm.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LUCY( N, NPIX, NLIN, XMARG, YMARG, AIM, NITER, CHIFAC,
*                     WLIM, SNYDER, FILE_3, FILE_4, FILE_6, FILE_8,
*                     FILE_1, FILE_2, FILE_5, FILE_7, STATUS )

*  Description:
*     A simulated data set is created from the supplied image in file
*     1, and its deviation from the observed data is estimating as a
*     normalised chi-squared value.  If the required chi-squared is
*     higher than this value, then a number of iterations are performed
*     as follows until the required chi-squared is reached or the
*     maximum number of iterations is exhausted:
*
*     1 - A new restored image is created from the old restored image
*     and the corresponding simulated data.  This involves finding a
*     correction factor for each data value (the ratio of observed to
*     simulated data value - but see argument SNYDER), and then mapping
*     these correction factors into an image.  The restored image is
*     then updated by multiplying it by this correction factor image.
*
*     2 - Simulated data is created from the new restored image by
*     smoothing it with the PSF and adding the background.  The
*     chi-squared value for the new simulated data is found at this
*     point.

*  Arguments:
*     N = INTEGER (Given)
*        The number of elements in each internal file.
*     NPIX = INTEGER (Given)
*        The number of pixels per line in each internal file.
*     NLIN = INTEGER (Given)
*        The number of lines in each internal file.
*     XMARG = INTEGER (Given)
*        The width of the left- and right-hand margins in pixels.
*     YMARG = INTEGER (Given)
*        The width of the top and bottom margins in pixels.
*     AIM = REAL (Given)
*        The required normalised chi-squared.
*     NITER = INTEGER (Given)
*        The maximum number of iterations to perform.
*     CHIFAC = REAL (Given)
*        The co-efficient of the simulated data in the denominator of
*        the expression used to evaluate the chi-squared of each data
*        value.
*     WLIM = REAL (Given)
*        Weight limited for good output pixels.
*     SNYDER = LOGICAL (Given)
*        If .TRUE. the Snyder modification to the basic R-L algorithm
*        is to be used.  This causes the correction factor for each
*        data value to be modified by adding the data value variance to
*        both the numerator and the denominator.
*     FILE_3( N ) = REAL (Given)
*        The Fourier transform of the PSF.
*     FILE_4( N ) = REAL (Given)
*        The observed data.
*     FILE_6( N ) = REAL (Given)
*        The background data.
*     FILE_8( N ) = REAL (Given)
*        The data variances.
*     FILE_1( N ) = REAL (Given and Returned)
*        Returned holding the reconstructed image.  On entry, it should
*        hold the initial guess at the reconstructed image.
*     FILE_2( N ) = REAL (Returned)
*        Work space.
*     FILE_5( N ) = REAL (Returned)
*        Work space.
*     FILE_7( N ) = REAL (Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Internal Files:
*     `Internal files' are used to hold data sets or images.  A `data
*     set' holds values which relate to data space, i.e. values which
*     are gridded like the observed data.  An `image' holds values in
*     image space, i.e. values which are gridded like the reconstructed
*     image.  A data set can be created from an image by convolving it
*     with the PSF.  An image can be created from a data set by
*     convolving it with the transposed PSF.  In the current version of
*     this application, data sets and images are the same size.
*
*     Eight internal files are used as follows:
*
*       1 - The current estimate of the reconstructed image.
*       2 - Work space (image or data set).
*       3 - The Fourier transform of the PSF.
*       4 - The observed data.
*       5 - A work array to hold bad-pixel masks.
*       6 - The background data.
*       7 - Work space (image or data set).
*       8 - Data variances.

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
*        Original version.
*     1995 April 6 (MJC):
*        Made messages conditional on reporting level.  Corrected
*        typo's and made minor stylistic changes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Arguments Given:
      INTEGER N
      INTEGER NPIX
      INTEGER NLIN
      INTEGER XMARG
      INTEGER YMARG
      REAL AIM
      INTEGER NITER
      REAL CHIFAC
      REAL WLIM
      LOGICAL SNYDER
      REAL FILE_3( N )
      REAL FILE_4( N )
      REAL FILE_6( N )
      REAL FILE_8( N )

*  Arguments Given and Returned:
      REAL FILE_1( N )

*  Arguments Returned:
      REAL FILE_2( N )
      REAL FILE_5( N )
      REAL FILE_7( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ITER               ! Current iteration
      REAL XSQ                   ! Total chi squared

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Generate simulated data from the current reconstruction, and find
*  the total chi-squared describing the deviation of the simulated data
*  from the real data.  The simulated data are returned in file 7.
      CALL KPS1_LUCDT( N, NPIX, NLIN, XMARG, YMARG, CHIFAC, WLIM,
     :                 FILE_3, FILE_4, FILE_6, FILE_1, FILE_8, FILE_7,
     :                 FILE_2, FILE_5, XSQ, STATUS )

*  Display the initial normalised chi-squared.
      CALL MSG_SETR( 'X', XSQ )
      CALL MSG_OUTIF( MSG__NORM, 'KPS1_LUCY_MSG1',
     :  '  Initial normalised chi squared is ^X', STATUS )

*  Iterate until the required normalised chi-squared value has been
*  achieved, or the maximum number of iterations has been done.
      ITER = 1

      DO WHILE ( XSQ .GT. AIM .AND. ITER .LE. NITER .AND.
     :           STATUS .EQ. SAI__OK )

*  Tell the user which iteration we are starting.
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_SETI( 'I', ITER )
         CALL MSG_OUTIF( MSG__NORM, 'KPS1_LUCY_MSG2',
     :                   '  Iteration ^I', STATUS )

*  Create the next estimate of the reconstructed image.
         CALL KPS1_LUCIM( N, NPIX, NLIN, WLIM, SNYDER, FILE_3, FILE_4,
     :                    FILE_8, FILE_1, FILE_7, FILE_2, FILE_5,
     :                    STATUS )

*  Generate simulated data from the current reconstruction, and find the
*  total chi-squared describing the deviation of the simulated data from
*  the real data.
         CALL KPS1_LUCDT( N, NPIX, NLIN, XMARG, YMARG, CHIFAC, WLIM,
     :                    FILE_3, FILE_4, FILE_6, FILE_1, FILE_8,
     :                    FILE_7, FILE_2, FILE_5, XSQ, STATUS )

*  Display the current normalised chi-squared.
         CALL MSG_SETR( 'X', XSQ )
         CALL MSG_OUTIF( MSG__NORM, 'KPS1_LUCY_MSG3',
     :                   '    Normalised chi squared: ^X', STATUS )

*  Increment the iteration count.
         ITER = ITER + 1

      END DO

*  Warn the user if the required normalised chi-squared has not been
*  reached.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

      IF ( XSQ .GT. AIM ) THEN
         CALL MSG_SETR( 'X', AIM )
         CALL MSG_SETI( 'N', NITER )
         CALL MSG_OUT( 'KPS1_LUCY_MSG4', '  WARNING: The required ' /
     :     /'normalised chi squared (^X) was not reached within the ' /
     :     /'specified maximum number of iterations (^N).  Is the '/
     :     /'value of parameter AIM too low?', STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

      END
