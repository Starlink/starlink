      SUBROUTINE KPS1_CLPCP( SLBND, SUBND, SKBAX, SPBAX, CGX, CGY,
     :                       NSAMP, ARRAY, X0, Y0, DX, DY, YTOP,
     :                       YBOT, GOOD, WORK1, WORK2, WORK3,
     :                       STATUS )
*+
*  Name:
*     KPS1_CLPCP

*  Purpose:
*     Extracts the spatral data to be plotted in a single CLINPLOT cell.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLPCP( SLBND, SUBND, SKBAX, SPBAX, CGX, CGY,
*                      NSAMP, ARRAY, X0, Y0, DX, DY, YTOP,
*                      YBOT, GOOD, WORK1, WORK2, WORK3, STATUS )

*  Description:
*     This routine extracts the spatral data to be plotted in a single
*     CLINPLOT cell.  This will be one row of pixels within the supplied
*     data array, binned up to produce the required number of samples.
*     The data are returned in two arrays representing graphics X and Y
*     values along the polyline that represents the spectrum.

*  Arguments:
*     SLBND( 3 ) = INTEGER (Given)
*        The lower pixel bounds of the supplied NDF cube.
*     SUBND( 3 ) = INTEGER (Given)
*        The upper pixel bounds of the supplied NDF cube.
*     SKBAX( 2 ) = INTEGER (Given)
*        The indices of the two pixels axes that correspond to the
*        spatial WCS axes.
*     SPBAX = INTEGER (Given)
*        The index of the pixel axes that corresponds to the spectral
*        WCS axis.
*     CGX = INTEGER (Given)
*        The GRID-axis value on the first spatial pixel axis at which
*        the spectrum is to be extracted.
*     CGY = INTEGER (Given)
*        The GRID-axis value on the second spatial pixel axis at which
*        the spectrum is to be extracted.
*     NSAMP = INTEGER (Given)
*        The number of spectral samples to return.
*     ARRAY( * ) = REAL (Given)
*        The vectorised data cube.
*     X0 = REAL (Given)
*        The GRAPHICS X-axis value at the bottom left of the cell in
*        which the spectrum will be drawn.
*     Y0 = REAL (Given)
*        The GRAPHICS Y-axis value at the bottom left of the cell in
*        which the spectrum will be drawn.
*     DX = REAL (Given)
*        The width (in GRAPHICS units) of the cell in which the spectrum
*        will be drawn.
*     DY = REAL (Given)
*        The height (in GRAPHICS units) of the cell in which the
*        spectrum will be drawn.
*     YTOP = REAL (Given)
*        The maximum data value to be displayed in a cell.
*     YBOT = REAL (Given)
*        The minimum data value to be displayed in a cell.
*     GOOD = LOGICAL (Returned)
*        Returned .TRUE. if the returned spectrum contains any good
*        data values.
*     WORK1( NSAMP ) = DOUBLE PRECISION (Returned)
*        Returned holding a series of X-axis values in the GRAPHICS
*        coordinate system that represents the polyline to be displayed.
*     WORK2( NSAMP ) = DOUBLE PRECISION (Returned)
*        Returned holding a series of Y-axis values in the GRAPHICS
*        coordinate system that represents the polyline to be displayed.
*     WORK3( NSAMP ) = INTEGER (Returned)
*        Work space.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     MJC: MAlcolm J. Currie (STARLIMNK)
*     {enter_new_authors_here}

*  History:
*     6-JUN-2006 (DSB):
*        Original version.
*     2008 November 14 (MJC):
*        No longer return bad values in WORK2 when the ARRAY values lie
*        outside the bounds of the cell.  Clipping to prevent plotting
*        in a vertically adjacent cell is now handled by redefining the
*        viewport and window bounds for each cell in CLINPLOT itself.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER SLBND( 3 )
      INTEGER SUBND( 3 )
      INTEGER SKBAX( 2 )
      INTEGER SPBAX
      INTEGER CGX
      INTEGER CGY
      INTEGER NSAMP
      REAL ARRAY( * )
      DOUBLE PRECISION X0
      DOUBLE PRECISION Y0
      DOUBLE PRECISION DX
      DOUBLE PRECISION DY
      REAL YTOP
      REAL YBOT

*  Arguments Returned:
      LOGICAL GOOD
      DOUBLE PRECISION WORK1( NSAMP )
      DOUBLE PRECISION WORK2( NSAMP )
      INTEGER WORK3( NSAMP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A         ! Constant term in A + B*Data
      DOUBLE PRECISION B         ! Factor in A + B*Data
      DOUBLE PRECISION SX        ! Step in GRAPHICS X beytween samples
      DOUBLE PRECISION X         ! GRAPHICS X at centre of next sample
      INTEGER DIM( 3 )           ! GRID dimensions
      INTEGER I                  ! Pixel index
      INTEGER IV                 ! Vector index of spectrum pixel
      INTEGER J                  ! Sample index
      INTEGER ROOT( 3 )          ! GRID indices of 1st spectrum pixel
      INTEGER STEP               ! Step between adjacent spectrum pixels
*.

*  Initialise
      GOOD = .FALSE.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the MAX and MIN data values are not equal.
      IF( YTOP .EQ. YBOT ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_CLPCP_ERR1', 'Maximum and minimum data '//
     :                 'values are equal', STATUS )
         GO TO 999
      END IF

*  Get the GRID co-ordinates at the first pixel in the spectrum.
      ROOT( SKBAX( 1 ) ) = CGX
      ROOT( SKBAX( 2 ) ) = CGY
      ROOT( SPBAX ) = 1

*  Get the dimensions of the pixel axes.
      DIM( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
      DIM( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1
      DIM( 3 ) = SUBND( 3 ) - SLBND( 3 ) + 1

*  Get the step between adjacent elements of the data array on the
*  spectral axis.
      IF( SPBAX .EQ. 1 ) THEN
         STEP = 1

      ELSE IF( SPBAX .EQ. 2 ) THEN
         STEP = DIM( 1 )

      ELSE IF( SPBAX .EQ. 3 ) THEN
         STEP = DIM( 1 )*DIM( 2 )

      END IF

*  Get the vector index of the first element in the spectrum.
      IV = ROOT( 1 ) + DIM( 1 )*( ROOT( 2 ) - 1 +
     :                 DIM( 2 )*( ROOT( 3 ) - 1 ) )

*  In WORK1, store the GRAPHICS X value at the centre of each of the
*  NSAMP spectral samples.
      SX = DX/NSAMP
      X = X0 - 0.5*SX
      DO I = 1, NSAMP
         X = X + SX
         WORK1( I ) = X
      END DO

*  Get the scale and zero for converting data value into GRAPHICS Y
*  value.
      B = DY/( YTOP - YBOT )
      A = Y0 - B*YBOT

*  If the number of samples equals the spectral axis dimension, there is
*  no binning to be done.
      IF( NSAMP .EQ. DIM( SPBAX ) ) THEN

*  Copy the data values from the supplied array to the returned work
*  array.
         DO I = 1, NSAMP
            IF( ARRAY( IV ) .NE. VAL__BADR ) THEN
               GOOD = .TRUE.
               WORK2( I ) = A + B*ARRAY( IV )
            ELSE
               WORK2( I ) = VAL__BADD
            END IF
            IV = IV + STEP
         END DO

*  If the number of samples is fewer than the spectral axis dimension,
*  there is some binning to be done.
      ELSE IF( NSAMP .LT. DIM( SPBAX ) ) THEN

*  Initialise the work arrays to hold zero.
         DO J = 1, NSAMP
            WORK2( J ) = 0.0D0
            WORK3( J ) = 0
         END DO

*  Loop round all pixels in the spectrum
         DO I = 1, DIM( SPBAX )

*  If the data value is good, determine which bin to put it in, and then
*  increment the bin sum and count.
            IF( ARRAY( IV ) .NE. VAL__BADR ) THEN
               J = NINT( 0.5 + NSAMP*( I - 0.5 )/DIM( SPBAX ) )
               WORK2( J ) = WORK2( J ) + ARRAY( IV )
               WORK3( J ) = WORK3( J ) + 1
            END IF

*  Get the index of the next pixel in the spectrum.
            IV = IV + STEP
         END DO

*  Normalise the returned spectral values.
         DO J = 1, NSAMP
            IF( WORK3( J ) .GT. 0 ) THEN
               WORK2( J ) = A + B*WORK2( J )/WORK3( J )
               GOOD = .TRUE.
            ELSE
               WORK2( J ) = VAL__BADD
            END IF
         END DO

*  Report an error otherwise.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_CLPCP_ERR1', 'KPS1_CLPCP: NSAMP is '//
     :                 'larger than the spectral axis dimension '//
     :                 '(internal KAPPA programming error).', STATUS )
      END IF

 999  CONTINUE

      END
