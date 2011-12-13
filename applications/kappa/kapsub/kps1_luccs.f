      SUBROUTINE KPS1_LUCCS( NPIX, NLIN, DATA, SIGMA, STATUS )
*+
*  Name:
*     KPS1_LUCCS

*  Purpose:
*     Finds a rough estimates of the noise in an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LUCCS( NPIX, NLIN, DATA, SIGMA, STATUS )

*  Description:
*     A rough estimate of the variance of the noise in the supplied
*     image is formed as half the mean squared difference between
*     adjacent pixels.  The process is repeated, this time excluding
*     pixels which differ by more than 3 standard deviations from their
*     neighbours.

*  Arguments:
*     NPIX = INTEGER (Given)
*        Number of pixels per line in the input data.
*     NLIN = INTEGER (Given)
*        Number of lines in the input data.
*     DATA( NPIX, NLIN ) = REAL (Given)
*        The input data.
*     SIGMA = REAL (Returned)
*        A rough estimate of the standard deviation of the noise in the
*        data.
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
*        Original version.
*     1995 April 6 (MJC):
*        Corrected typo's and made minor stylistic changes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Data constants

*  Arguments Given:
      INTEGER  NPIX
      INTEGER  NLIN
      REAL     DATA( NPIX, NLIN )

*  Arguments Returned:
      REAL     SIGMA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL     DATVAL            ! Data value
      REAL     DIFF              ! Difference between adjacent data
                                 ! values
      REAL     LIMIT             ! Max. difference allowed between
                                 ! adjacent data values
      INTEGER  LIN               ! Line counter
      REAL     LSTVAL            ! The previous data value
      INTEGER  NSUM2             ! No. of values summed in SUM2
      INTEGER  PIX               ! Pixel counter
      REAL     SUM2              ! Sum of the variance estimates

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Iinitialise the sums used to find the statistics.
      NSUM2 = 0
      SUM2 = 0.0

*  Loop round all the input pixels.  The first pixel in each row has no
*  `previous pixel', so set the previous pixel value bad.
      DO LIN  = 1, NLIN
         LSTVAL = VAL__BADR

         DO PIX = 1, NPIX
            DATVAL = DATA( PIX, LIN )

*  If the data value is good, increment the statistics.
            IF ( DATVAL .NE. VAL__BADR .AND.
     :           LSTVAL .NE. VAL__BADR ) THEN
               SUM2 = SUM2 + ( ABS( DATVAL - LSTVAL ) ) **2
               NSUM2 = NSUM2 + 1
            END IF

*  Save the current pixel value for later use.
            LSTVAL = DATVAL

         END DO

      END DO

*  Find the RMS difference between adjacent data values.  Report an
*  error and abort if there was insufficient good data (i.e. no pairs
*  of adjacent good pixels).
      IF ( NSUM2 .GT. 0 ) THEN
         SIGMA = SQRT( 0.5 * SUM2 / REAL( NSUM2 ) )

      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_LUCCS_ERR2', 'Insufficient good data to '/
     :                 /'calculate noise level in input.', STATUS )
         GO TO 999
      END IF

*  Go through the data again, excluding data points which differ from
*  their neighbour by more than 3 sigma.
      LIMIT = 3.0 * SIGMA

      NSUM2 = 0
      SUM2 = 0.0

      DO LIN = 1, NLIN
         LSTVAL = VAL__BADR

         DO PIX = 1, NPIX
            DATVAL = DATA( PIX, LIN )

            IF ( DATVAL .NE. VAL__BADR ) THEN

               IF ( LSTVAL .NE. VAL__BADR ) THEN
                  DIFF = DATVAL - LSTVAL

                  IF ( ABS( DIFF ) .LE. LIMIT ) THEN
                     SUM2 = SUM2 + DIFF * DIFF
                     NSUM2 = NSUM2 + 1
                  END IF

               END IF

            END IF

            LSTVAL = DATVAL

         END DO

      END DO

*  If possible, calculate a typical sigma value.
      IF ( NSUM2 .GT. 0 ) THEN
         SIGMA = SQRT( 0.5 * SUM2 / REAL( NSUM2 ) )

      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_LUCCS_ERR3', 'Cannot find the noise '/
     :                 /'level in the input image.', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
