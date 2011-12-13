      SUBROUTINE ARD1_LINFL( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR,
     :                       PAR, B, LBINTB, UBINTB, STATUS )
*+
*  Name:
*     ARD1_LINFL

*  Purpose:
*     Draw a line into a mask.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_LINFL( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, PAR, B,
*                      LBINTB, UBINTB, STATUS )

*  Description:
*     Pixels which intersect the specified line are assigned the value
*     given by rindex. Other pixels are left unchanged. The interior
*     bounding box is updated to include the new line.  The supplied
*     parameters are the pixel co-ordinates of the two end points of
*     the line.
*
*     The section of the line which intersects the array is first
*     found. Each axis is then processed as follows: The points at
*     which the line passes through each pixel boundary on the current
*     axis are found. The pixel indices of the pixels on each side of
*     this boundary are found, and marked as interior.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the B array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the B array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the B array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        A list of pixel co-ordinates, in groups of NDIM.
*     B( MSKSIZ ) = INTEGER (Given and Returned)
*        The array.
*     LBINTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-MAR-1994 (DSB):
*        Original version.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants

*  Arguments Given:
      INTEGER RINDEX
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER NPAR
      DOUBLE PRECISION PAR( NPAR )

*  Arguments Given and Returned:
      INTEGER B( MSKSIZ )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION EPS                   ! Rounding error safety margin
      PARAMETER ( EPS = 0.001D0 )

      DOUBLE PRECISION MARGIN                ! Second safety margin
      PARAMETER ( MARGIN = 1.0 - VAL__EPSD )

*  Local Variables:
      INTEGER
     :        I,                 ! Loop count
     :        J,                 ! Loop count
     :        K,                 ! Loop count
     :        VA                 ! Vector address eqv to Cartesian pnt.

      DOUBLE PRECISION
     :        ALPHA,             ! Line parameter at pixel boundary
     :        END( ARD__MXDIM ), ! Indices of end pixel
     :        HILIM,             ! Largest usable line parameter
     :        INC( ARD__MXDIM ), ! Pixel co-ord. increments along line
     :        LALPHA,            ! Line parameter at array lower bound
     :        LOLIM,             ! Smallest usable line parameter
     :        PAC( ARD__MXDIM ), ! Pixel co-ord.s at line start
     :        PBC( ARD__MXDIM ), ! Pixel co-ord.s at line end
     :        PCC,               ! Pixel co-ord. at usable line start
     :        PCI                ! Pixel index at usable line start

      DOUBLE PRECISION
     :        PDC,               ! Pixel co-ord. at usable line end
     :        PDI,               ! Pixel index at usable line end
     :        PEC,               ! Pixel co-ord. at pixel boundary
     :        PEI( ARD__MXDIM ), ! Pixel indices at pixel boundary
     :        RECINC( ARD__MXDIM ),! Reciprocals of INC values
     :        START( ARD__MXDIM ),! Pixel indices of start pixel
     :        TEMP,              ! Temporary value
     :        UALPHA             ! Line parameter at array upper bound

      integer mm
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the limits of ALPHA on the usable section of the line.
      LOLIM = 0.0
      HILIM = 1.0

*  The line is parameterised by ALPHA, which is zero at the first
*  supplied position (the "start") and unity at the second point (the
*  "end"). The first thing to do is to find the range of ALPHA which
*  corresponds to the section of the line which lies within the bounds
*  of the array. Loop round each axis.
      DO I = 1, NDIM

*  Store the supplied pixel co-ordinates of the start and end of the
*  line.
         PAC( I ) = PAR( I )
         PBC( I ) = PAR( I + NDIM )

*  Store the increment.
         INC( I ) = PBC( I ) - PAC( I )

*  If the increment is not zero...
         IF( ABS( INC( I ) ) .GT. VAL__SMLD ) THEN

*  ...store the reciprocal of the increment.
            RECINC( I ) = 1.0/INC( I )

*  Find the value of ALPHA at which the line intersect the lower bound
*  of this axis (plus a little bit to allow for rounding errors).
            LALPHA = ( EPS + DBLE( LBND( I ) ) - 1.0 - PAC( I ) )
     :                 *RECINC( I )

*  Find the value of ALPHA at which the line intersect the upper bound
*  of this axis (minus a little bit to allow for rounding errors).
            UALPHA = ( -EPS + DBLE( UBND( I ) ) - PAC( I ) )*RECINC( I )

*  Ensure that LALPHA is less than or equal to UALPHA
            IF( LALPHA .GT. UALPHA ) THEN
               TEMP = LALPHA
               LALPHA = UALPHA
               UALPHA = TEMP
            END IF

*  Update the lowest and highest usable values of ALPHA
            IF( LALPHA .GT. LOLIM ) LOLIM = LALPHA
            IF( UALPHA .LT. HILIM ) HILIM = UALPHA

*  If the co-ordinate value is constant on this axis, check that it is
*  within the bounds of the array. If it is not, return.
         ELSE IF( PAC( I ) .LE. LBND( I ) - 1.0 .OR.
     :            PAC( I ) .GT. UBND( I ) ) THEN

            GO TO 999

         END IF

      END DO

*  If none of the line is usable, return.
      IF( LOLIM .GT. HILIM ) GO TO 999

*  Loop round each dimension...
      DO I = 1, NDIM

*  Find the pixel co-ordinate of the start of the usable section of
*  the line.
         PCC = PAC( I ) + INC( I )*LOLIM

*  Find the corresponding pixel index.
         PCI = DBLE( INT( PCC ) )
         IF( PCI .LT. PCC ) PCI = PCI + 1

*  Save it.
         START( I ) = PCI

*  Find the pixel co-ordinate of the end of the usable section of
*  the line.
         PDC = PAC( I ) + INC( I )*HILIM

*  Find the corresponding pixel index.
         PDI = DBLE( INT( PDC ) )
         IF( PDI .LT. PDC ) PDI = PDI + 1

*  Save it.
         END( I ) = PDI

*  Ensure that PDI is larger than or equal to PCI.
         IF( PCI .GT. PDI ) THEN
            TEMP = PDI
            PDI = PCI
            PCI = TEMP
         END IF

*  Update the interior bounding box.
         LBINTB( I ) = MIN( LBINTB( I ), NINT( PCI ) )
         UBINTB( I ) = MAX( UBINTB( I ), NINT( PDI ) )

*  Loop round all the pixel boundaries encompassed by the line on this
*  axis.
         DO J = INT( PCI ), INT( PDI ) - 1

*  Find the value of ALPHA at which the line reaches this pixel
*  boundary.
            ALPHA = ( DBLE( J ) - PAC( I ) )*RECINC( I )

*  Find the corresponding pixel co-ordinates and indices on all axes, of
*  the pixel which the line is leaving, and the pixel which the line is
*  entering (the indices of these two pixels will be the same except on
*  the current axis).
            DO K = 1, NDIM
               PEC = PAC( K ) + INC( K )*ALPHA

               PEI( K )  = DBLE( INT( PEC ) )
               IF( PEI( K ) .LT. PEC*MARGIN ) PEI( K ) = PEI( K ) + 1

            END DO

*  Rounding errors may have caused the point on the line corresponding
*  to ALPHA to drift into the next pixel. Ensure that it refers to the
*  pixel which the line is leaving.
            PEI( I ) =  J

*  Convert these pixel indices to a vector address.
            CALL ARD1_CTOV( NDIM, LBND, UBND, PEI, VA, STATUS )

*  Store an interior value in the array.
            IF( VA .NE. 0 ) B( VA ) = RINDEX

*  Now increment the pixel index on the current axis so that it refers
*  to the pixel which the line is entering.
            PEI( I ) =  J + 1

*  Convert these pixel indices to a vector address.
            CALL ARD1_CTOV( NDIM, LBND, UBND, PEI, VA, STATUS )

*  Store an interior value in the array.
            IF( VA .NE. 0 ) B( VA ) = RINDEX

         END DO

      END DO

*  The above algorithm can miss the pixel containing the start and/or
*  end of the usable section of the line. Do these two pixel explicitly
*  now. Convert the pixel indices at the start to a vector address.
      CALL ARD1_CTOV( NDIM, LBND, UBND, START, VA, STATUS )

*  Store an interior value in the array.
      IF( VA .NE. 0 ) B( VA ) = RINDEX

*  Do the same for the end pixel.
      CALL ARD1_CTOV( NDIM, LBND, UBND, END, VA, STATUS )
      IF( VA .NE. 0 ) B( VA ) = RINDEX

*  Jump to here if an error occurs or if the line does not intersect the
*  array.
 999  CONTINUE

      END
