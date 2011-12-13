      SUBROUTINE KPS1_CCMPP( FD, NX, NY, RDATA, RHI, RLO, GDATA,
     :                       GHI, GLO, BDATA, BHI, BLO, BAD, STATUS )
*+
*  Name:
*     KPS1_CCMPP

*  Purpose:
*     Creates a PPM file containing the supplied RGB intensities.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CCMPP( FD, NX, NY, RDATA, RHI, RLO, GDATA, GHI, GLO,
*                      BDATA, BHI, BLO, BAD, STATUS )

*  Description:
*     The required PPM header is written to the output file, followed by
*     the formatted RGB intensities at each point.

*  Arguments:
*     FD = INTEGER (Given)
*       The FIO file descriptor for the output PPM file.
*     NX = INTEGER (Given)
*       The number of pixels in one row of the image.
*     NY = INTEGER (Given)
*       The number of rows in the image.
*     RDATA( NX, NY ) = REAL (Given)
*       The red data values.
*     RHI = REAL (Given)
*       The red data value corresponding to full red intensity.
*     RLO = REAL (Given)
*       The red data value corresponding to zero red intensity.
*     GDATA( NX, NY ) = REAL (Given)
*       The green data values.
*     GHI = REAL (Given)
*       The green data value corresponding to full green intensity.
*     GLO = REAL (Given)
*       The green data value corresponding to zero green intensity.
*     BDATA( NX, NY ) = REAL (Given)
*       The blue data values.
*     BHI = REAL (Given)
*       The blue data value corresponding to full blue intensity.
*     BLO = REAL (Given)
*       The blue data value corresponding to zero blue intensity.
*     BAD( 3 ) = REAL (Given)
*       The RGB intensities in the range 0.0 to 1.0 to represent any
*       pixel which has one or more bad input intensities.
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
      INTEGER FD
      INTEGER NX
      INTEGER NY
      REAL RDATA( NX, NY )
      REAL RHI
      REAL RLO
      REAL GDATA( NX, NY )
      REAL GHI
      REAL GLO
      REAL BDATA( NX, NY )
      REAL BHI
      REAL BLO
      REAL BAD( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXVAL
      PARAMETER ( MXVAL = 255 )

*  Local Variables:
      CHARACTER BUF*80
      INTEGER I
      INTEGER IAT
      INTEGER IB
      INTEGER IG
      INTEGER IR
      INTEGER J
      REAL B
      REAL FB
      REAL FG
      REAL FR
      REAL G
      REAL R
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct the PPM header.
      BUF = 'P3'
      IAT = 3
      CALL CHR_PUTI( NX, BUF, IAT )
      IAT = IAT + 1
      CALL CHR_PUTI( NY, BUF, IAT )
      IAT = IAT + 1
      CALL CHR_PUTI( MXVAL, BUF, IAT )

*  Write out the PPM header.
      CALL FIO_WRITE( FD, BUF( : IAT ), STATUS )

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

*  Loop round every image pixel, working from top left to bottom right.
      DO J = NY, 1, -1
         DO I = 1, NX

*  Check that all 3 pixel values are good.
            R = 0
            G = 0
            B = 0
            IF( FR .NE. 0.0 ) R = RDATA( I, J )
            IF( FG .NE. 0.0 ) G = GDATA( I, J )
            IF( FB .NE. 0.0 ) B = BDATA( I, J )

            IF( R .NE. VAL__BADR .AND. B .NE. VAL__BADR .AND.
     :          B .NE. VAL__BADR ) THEN

*  Convert pixel values to RGB intensities in the range 0 to MXVAL
               IR = MAX( 0, MIN( MXVAL, NINT( MXVAL*( R - RLO )*FR ) ) )
               IG = MAX( 0, MIN( MXVAL, NINT( MXVAL*( G - GLO )*FG ) ) )
               IB = MAX( 0, MIN( MXVAL, NINT( MXVAL*( B - BLO )*FB ) ) )

*  If any pixel is bad, use the bad colour.
            ELSE
               IR = MAX( 0, MIN( MXVAL, NINT( MXVAL*BAD( 1 ) ) ) )
               IG = MAX( 0, MIN( MXVAL, NINT( MXVAL*BAD( 2 ) ) ) )
               IB = MAX( 0, MIN( MXVAL, NINT( MXVAL*BAD( 3 ) ) ) )
            END IF

*  Format them.
            BUF = ' '
            IAT = 0
            CALL CHR_PUTI( IR, BUF, IAT )
            IAT = IAT + 1
            CALL CHR_PUTI( IG, BUF, IAT )
            IAT = IAT + 1
            CALL CHR_PUTI( IB, BUF, IAT )

*  Write out the buffer.
            CALL FIO_WRITE( FD, BUF( : IAT ), STATUS )

         END DO

      END DO

      END
