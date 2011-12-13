      SUBROUTINE KPS1_CCMQN( NLUT, NDATA, RDATA, RHI, RLO, GDATA, GHI,
     :                       GLO, BDATA, BHI, BLO, BAD, RSVPN, NHIST,
     :                       RHIST, GHIST, BHIST, PHIST, LUT, CI,
     :                       STATUS )

*+
*  Name:
*     KPS1_CCMQN

*  Purpose:
*     Creates an array of colour indices and a colour look up table to
*     describe the supplied red, green and blue images.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CCMQN( NLUT, NDATA, RDATA, RHI, RLO, GDATA, GHI, GLO,
*                      BDATA, BHI, BLO, BAD, RSVPN, NHIST, RHIST, GHIST,
*                      BHIST, PHIST, LUT, CI, STATUS )

*  Description:
*     The rgb intensities at each point are supplied in three separate
*     arrays. A 3-dimensional histogram is formed of these RGB values, in
*     which the three axes represent red, green and blue intensity. The
*     number of cells in this histogram which contain any points is then
*     found. If the number of non-empty cells is larger than the number of
*     elements in the colour table, then a new histogram is formed in which
*     there are fewer (but bigger) cells. This process is repeated until
*     the number of non-empty cells is less than or equal to the size of
*     the colour table. The RGB intensities at the centre of each non-empty
*     cell is then stored in the returned colour table. The cell in which
*     each input data point falls is then found, and the corresponding
*     indices into the returned colour table are stored in the returned
*     image array.
*
*     Any point for which any of the input data values are bad has the
*     specified BAD colour index stored in the output image array.

*  Arguments:
*     NLUT = INTEGER (Given)
*       The number of colours which can be stored in the returned colour
*       table.
*     NDATA = INTEGER (Given)
*       The number of supplied data points.
*     RDATA( NDATA ) = REAL (Given)
*       The red data values.
*     RHI = REAL (Given)
*       The red data value corresponding to full red intensity.
*     RLO = REAL (Given)
*       The red data value corresponding to zero red intensity.
*     GDATA( NDATA ) = REAL (Given)
*       The green data values.
*     GHI = REAL (Given)
*       The green data value corresponding to full green intensity.
*     GLO = REAL (Given)
*       The green data value corresponding to zero green intensity.
*     BDATA( NDATA ) = REAL (Given)
*       The blue data values.
*     BHI = REAL (Given)
*       The blue data value corresponding to full blue intensity.
*     BLO = REAL (Given)
*       The blue data value corresponding to zero blue intensity.
*     BAD = INTEGER (Given)
*       The colour index to store in LUT for each pixel which has one or
*       more bad input intensities.
*     RSVPN = INTEGER (Given)
*       The number of colours reserved for the pallette. The returned
*       image will use a value of RSVPN to refer to the first entry
*       in the returned colour table.
*     NHIST = INTEGER (Given)
*       The maximum number of cells which can be used in the 3D histogram.
*     RHIST( NHIST ) = REAL (Returned)
*       Work space which is used to hold red intensity (in the range 0.0
*       to 1.0) at the centre of each 3D histogram cell.
*     GHIST( NHIST ) = REAL (Returned)
*       Work space which is used to hold green intensity (in the range 0.0
*       to 1.0) at the centre of each 3D histogram cell.
*     BHIST( NHIST ) = REAL (Returned)
*       Work space which is used to hold blue intensity (in the range 0.0
*       to 1.0) at the centre of each 3D histogram cell.
*     PHIST( NHIST ) = INTEGER (Returned)
*       Work space which is used to hold the population in each 3D
*       histogram cell.
*     LUT( 3, NLUT ) = REAL (Returned)
*       The returned colour table.
*     CI( NDATA ) = INTEGER (Returned)
*       The returned image of colour indices.
*     STATUS = INTEGER (Given and Returned)
*       The global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-NOV-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER NLUT
      INTEGER NDATA
      REAL RDATA( NDATA )
      REAL RHI
      REAL RLO
      REAL GDATA( NDATA )
      REAL GHI
      REAL GLO
      REAL BDATA( NDATA )
      REAL BHI
      REAL BLO
      INTEGER BAD
      INTEGER RSVPN
      INTEGER NHIST

*  Arguments Returned:
      REAL RHIST( NHIST )
      REAL GHIST( NHIST )
      REAL BHIST( NHIST )
      REAL PHIST( NHIST )
      REAL LUT( 3, NLUT )
      INTEGER CI( NDATA )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BI
      INTEGER GI
      INTEGER I
      INTEGER J
      INTEGER NEWUSE
      INTEGER NH
      INTEGER NHM1
      INTEGER NONZ
      INTEGER NUSE
      INTEGER RI
      INTEGER VI
      LOGICAL MORE
      REAL B
      REAL FB
      REAL FG
      REAL FR
      REAL G
      REAL R
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied values.
      IF( NHIST .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', NHIST)
         CALL ERR_REP( 'KPS1_CCMQN_ERR4','KPS1_CCMQN: NHIST (^N) must'//
     :                 ' be at least 1 - programming error.', STATUS )
         GO TO 999
      END IF

*  Form a 3D histogram of the supplied colours at each of the supplied data
*  points. RHIST holds the RED value
*  ========================================================================
*  Form factors for scaling data values into RGB intensity.
      IF( RLO .NE. VAL__BADR ) THEN
         IF( RLO .NE. RHI ) THEN
            FR = 1.0/( RHI - RLO )
         ELSE
            FR = 1.0E6
         END IF
      ELSE
         FR = 0.0
      END IF

      IF( GLO .NE. VAL__BADR ) THEN
         IF( GLO .NE. GHI ) THEN
            FG = 1.0/( GHI - GLO )
         ELSE
            FG = 1.0E6
         END IF
      ELSE
         FG = 0.0
      END IF

      IF( BLO .NE. VAL__BADR ) THEN
         IF( BLO .NE. BHI ) THEN
            FB = 1.0/( BHI - BLO )
         ELSE
            FB = 1.0E6
         END IF
      ELSE
         FB = 0.0
      END IF

*  Find the number of 3D histogram bins to use on the frist pass through
*  the loop.
      NUSE = ( INT( REAL( NHIST )**(1.0/3.0) ) )**3

*  Loop until we have a histogram in which the number of non-empty cells
*  is less than or equal to the number of colours which can be stored in
*  the returned colour table.
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Find the number of bins along each axis of the RGB cube.
         NH = INT( REAL( NUSE )**(1.0/3.0) )
         NHM1 = NH - 1

*  Initialise the histogram arrays.
         I = 1
         DO BI = 1, NH
            DO GI = 1, NH
               DO RI = 1, NH

*  Set the cell population to zero.
                  PHIST( I ) = 0

*  Store the RGB intensities at the centre of the cell, in the range 0.0
*  to 1.0.
                  IF( FR .NE. 0.0 ) THEN
                     RHIST( I ) = ( REAL( RI ) - 0.5 )/REAL( NH )
                  ELSE
                     RHIST( I ) = 0.0
                  ENDIF

                  IF( FG .NE. 0.0 ) THEN
                     GHIST( I ) = ( REAL( GI ) - 0.5 )/REAL( NH )
                  ELSE
                     GHIST( I ) = 0.0
                  ENDIF

                  IF( FB .NE. 0.0 ) THEN
                     BHIST( I ) = ( REAL( BI ) - 0.5 )/REAL( NH )
                  ELSE
                     BHIST( I ) = 0.0
                  ENDIF

                  I = I + 1

               END DO

            END DO

         END DO

*  Loop round all pixels.
         DO I = 1, NDATA

*  Check that all 3 pixel values are good.
            R = 0
            G = 0
            B = 0
            IF( FR .NE. 0.0 ) R = RDATA( I )
            IF( FG .NE. 0.0 ) G = GDATA( I )
            IF( FB .NE. 0.0 ) B = BDATA( I )

            IF( R .NE. VAL__BADR .AND. B .NE. VAL__BADR .AND.
     :          B .NE. VAL__BADR ) THEN

*  Convert pixel values to RGB intensities in the range 0.0 to 1.0
               R = MAX( 0.0, MIN( 1.0, ( R - RLO )*FR ) )
               G = MAX( 0.0, MIN( 1.0, ( G - GLO )*FG ) )
               B = MAX( 0.0, MIN( 1.0, ( B - BLO )*FB ) )

*  Get the corresponding indices, in the range 0 to (NH-1).
               RI = MIN( NHM1, MAX( 0, INT( NH*R ) ) )
               GI = MIN( NHM1, MAX( 0, INT( NH*G ) ) )
               BI = MIN( NHM1, MAX( 0, INT( NH*B ) ) )

*  Form the vector index, in the range 1 to NUSE
               VI = RI + NH*( GI + BI*NH ) + 1

*  Increment the bin population.
               PHIST( VI ) = PHIST( VI ) + 1

            END IF

         END DO

*  Count the number of non-zero bins.
         NONZ = 0
         DO I = 1, NUSE
            IF( PHIST( I ) .GT. 0 ) NONZ = NONZ + 1
         END DO

*  If too many colours are used, increase the size of each bin in the
*  histogram, and try again.
         IF( NONZ .GT. NLUT ) THEN
            NEWUSE = INT( REAL( NUSE* NLUT )/REAL( NONZ ) )
            IF( NEWUSE .GE. NUSE ) NEWUSE = NUSE - 1
            NUSE = NEWUSE
         ELSE
            MORE = .FALSE.
         END IF

      END DO

*  Set up the returned colour table values. Replace the bin populations
*  in PHIST with the corresponding bin colour index.
      J = 0
      DO I = 1, NUSE
         IF( PHIST( I ) .GT. 0 ) THEN
            J = J + 1
            LUT( 1, J ) = RHIST( I )
            LUT( 2, J ) = GHIST( I )
            LUT( 3, J ) = BHIST( I )
            PHIST( I ) = J
         END IF
      END DO

*  Loop round all pixels.
      DO I = 1, NDATA

*  Check that all 3 pixel values are good.
         R = 0
         G = 0
         B = 0
         IF( FR .NE. 0.0 ) R = RDATA( I )
         IF( FG .NE. 0.0 ) G = GDATA( I )
         IF( FB .NE. 0.0 ) B = BDATA( I )

         IF( R .NE. VAL__BADR .AND. B .NE. VAL__BADR .AND.
     :       B .NE. VAL__BADR ) THEN

*  Convert pixel values to RGB intensities in the range 0.0 to 1.0
            R = MAX( 0.0, MIN( 1.0, ( R - RLO )*FR ) )
            G = MAX( 0.0, MIN( 1.0, ( G - GLO )*FG ) )
            B = MAX( 0.0, MIN( 1.0, ( B - BLO )*FB ) )

*  Get the corresponding indices, in the range 0 to (NH-1).
            RI = MIN( NHM1, MAX( 0, INT( NH*R ) ) )
            GI = MIN( NHM1, MAX( 0, INT( NH*G ) ) )
            BI = MIN( NHM1, MAX( 0, INT( NH*B ) ) )

*  Form the vector index, in the range 1 to NUSE.
            VI = RI + NH*( GI + BI*NH ) + 1

*  The value stored in PHIST at this index is the returned colour index. Make
*  the first entry in the table equal to the number of reserved pens.
            CI( I ) = PHIST( VI ) + RSVPN - 1

*  Bad pixels are assigned the supplied colour index.
         ELSE
            CI( I ) = BAD
         END IF

      END DO

* Tidy up.
 999  CONTINUE

      END
