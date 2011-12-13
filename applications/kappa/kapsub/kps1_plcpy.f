      SUBROUTINE KPS1_PLCPY( INDF1, INDF2, INDF3, COMP, ITYPE, USE1,
     :                       USE2, AXES, SLBND1, SUBND1, SLBND2, SUBND2,
     :                       MASK, FILL, STATUS )
*+
*  Name:
*     KPS1_PLCPY

*  Purpose:
*     Copies polygonal segments from a pair of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PLCPY( INDF1, INDF2, INDF3, COMP, ITYPE, USE1, USE2,
*                      AXES, SLBND1, SUBND1, SLBND2, SUBND2, MASK,
*                      FILL, STATUS )

*  Description:
*     This subroutine copies pixel values into INDF3 from INDF1 which
*     are selected by the supplied mask.  Pixels which are not selected
*     in the mask are copied from INDF2.  It is a bit more complex than
*     one might think necessary because of the need to deal with N-d
*     data arrays.  If the NDFs have more than 2 dimensions the user can
*     choose to apply the polygonal segmentation in any plane.  The
*     polygons are presumed to lie within the plane defined by the axes
*     supplied in argument AXES.  The polygons are then projected
*     through all the other dimensions.  We need to ensure that the 3
*     data arrays are accessed in `natural' order (i.e with the first
*     axis incrementing fastest, second axis next fastest, etc.),
*     otherwise it takes an age to execute.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        NDF identifier for the data which is to go inside the polygons.
*     INDF2 = INTEGER (Given)
*        NDF identifier for the data which is to go outside the
*        polygons.
*     INDF3 = INTEGER (Given)
*        Identifier for the output NDF.
*     COMP = CHARACTER * ( * ) (Given)
*        The name of the NDF array component which is to be copied.
*     ITYPE = CHARACTER * ( * ) (Given)
*        The numerical type with which the data is to be accessed.  It
*        must be one of the following: '_DOUBLE', '_INTEGER', '_REAL',
*        '_UBYTE', or 'WORD'.
*     USE1 = LOGICAL (Given)
*        If supplied .FALSE., then the NDF identified by INDF1 is
*        ignored.  In this case the inside of the polygonal segments are
*        filled with the value supplied by argument FILL.
*     USE2 = LOGICAL (Given)
*        If supplied .FALSE., then the NDF identified by INDF2 is
*        ignored.  In this case the outside of the polygonal segments are
*        filled with the value supplied by argument FILL.
*     AXES( 2 ) = INTEGER (Given)
*        The indices of the axes which span the plane containing the
*        polygons.
*     SLBND1 = INTEGER (Given)
*        The lower bound of the output NDF on the axis with index stored
*        in AXES(1).
*     SUBND1 = INTEGER (Given)
*        The upper bound of the output NDF on the axis with index stored
*        in AXES(1).
*     SLBND2 = INTEGER (Given)
*        The lower bound of the output NDF on the axis with index stored
*        in AXES(2).
*     SUBND2 = INTEGER (Given)
*        The upper bound of the output NDF on the axis with index stored
*        in AXES(2).
*     MASK( SLBND1:SUBND1, SLBND2:SUBND2 ) = LOGICAL (Given)
*        A mask identifying the pixels which are inside any of the
*        polygonal segments.  This is a 2-dimensional array which
*        corresponds to a 2-dimensionasl slice through the output NDF spanned by
*        the axes supplied in argument AXES. a .TRUE. value means that
*        the pixel is inside a polygon.
*     FILL = DOUBLE PRECISION (Given)
*        The constant pixel value to assume if either of the two input
*        NDFs cannot be used.  This is a double-precision value which is
*        converted within this routine to the numerical type requested
*        in argument ITYPE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1993 (DSB):
*        Original version.
*     1995 April 12 (MJC):
*        Used modern-style variable declarations.  Minor stylistic
*        changes and documentation improved.  Renamed variable INDEX to
*        avoid name clash with intrinsic function.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2
      INTEGER INDF3
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) ITYPE
      LOGICAL USE1
      LOGICAL USE2
      INTEGER AXES( 2 )
      INTEGER SLBND1
      INTEGER SUBND1
      INTEGER SLBND2
      INTEGER SUBND2
      LOGICAL MASK( SLBND1:SUBND1, SLBND2:SUBND2 )
      DOUBLE PRECISION FILL

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      BYTE VAL_DTOUB             ! _DOUBLE to _UBYTE conversion
      INTEGER*2 VAL_DTOW         ! _DOUBLE to _WORD conversion
      INTEGER VAL_DTOI           ! _DOUBLE to _INTEGER conversion
      REAL VAL_DTOR              ! _DOUBLE to _REAL conversion

*  Local Variables:
      INTEGER DIM( NDF__MXDIM )  ! Size of each dimension
      INTEGER EL                 ! Element counter
      INTEGER FILLI              ! _INTEGER fill value
      REAL FILLR                 ! _REAL fill value
      BYTE FILLUB                ! _BYTE fill value
      INTEGER*2 FILLW            ! _WORD fill value
      INTEGER I                  ! Dimension counter
      INTEGER IPIN               ! Pointer to source array
      INTEGER IPIN1( 1 )         ! Pointer to 1st input array
      INTEGER IPIN2( 1 )         ! Pointer to 2nd input array
      INTEGER IPOUT( 1 )         ! Pointer to output array
      INTEGER J                  ! Offset of element into next dimension
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of the output array
      INTEGER NDIM               ! No. of dimensions in the output array
      INTEGER NEL                ! No. of elements in the output array
      INTEGER PINDEX( NDF__MXDIM ) ! Pixel indices of current element
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of the output array
      LOGICAL USE                ! Should value in source array be used?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Map the required arrays.
      IF ( USE1 ) CALL KPG1_MAP( INDF1, COMP, ITYPE, 'READ', IPIN1, NEL,
     :                          STATUS )
      IF ( USE2 ) CALL KPG1_MAP( INDF2, COMP, ITYPE, 'READ', IPIN2, NEL,
     :                          STATUS )
      CALL KPG1_MAP( INDF3, COMP, ITYPE, 'WRITE', IPOUT, NEL, STATUS )

*  Get the bounds of the output NDF.
      CALL NDF_BOUND( INDF3, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Store the size of each dimension.
      DO I = 1, NDIM
         DIM( I ) = UBND( I ) - LBND( I ) + 1
      END DO

*  Loop round all pixels in the output array.
      DO EL = 1, NEL

*  Find the pixel indices of the current pixel.
         J = EL - 1
         DO I = 1, NDIM
            PINDEX( I ) = MOD( J, DIM( I ) ) + LBND( I )
            J = J / DIM( I )
         END DO

*  Store the pointer to the relevant input array, depending on whether
*  or not this pixel is inside the polygon.
         IF ( MASK( PINDEX( AXES( 1 ) ), PINDEX( AXES( 2 ) ) ) ) THEN
            IPIN = IPIN1( 1 )
            USE = USE1
         ELSE
            IPIN = IPIN2( 1 )
            USE = USE2
         END IF

*  Copy the appropriate value into the output NDF.
         IF ( ITYPE .EQ. '_UBYTE' ) THEN
            FILLUB = VAL_DTOUB( .TRUE., FILL, STATUS )
            CALL KPS1_PLCPUB( EL, NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                        USE, FILLUB,
     :                        %VAL( CNF_PVAL( IPOUT( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            FILLW = VAL_DTOW( .TRUE., FILL, STATUS )
            CALL KPS1_PLCPW( EL, NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                       USE, FILLW,
     :                       %VAL( CNF_PVAL( IPOUT( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            FILLI = VAL_DTOI( .TRUE., FILL, STATUS )
            CALL KPS1_PLCPI( EL, NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                       USE, FILLI,
     :                       %VAL( CNF_PVAL( IPOUT( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
            FILLR = VAL_DTOR( .TRUE., FILL, STATUS )
            CALL KPS1_PLCPR( EL, NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                       USE, FILLR,
     :                       %VAL( CNF_PVAL( IPOUT( 1 ) ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_PLCPD( EL, NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                       USE, FILL,
     :                       %VAL( CNF_PVAL( IPOUT( 1 ) ) ), STATUS )
         END IF

      END DO

*  Unmap the arrays.
      IF ( USE1 ) CALL NDF_UNMAP( INDF1, COMP, STATUS )
      IF ( USE2 ) CALL NDF_UNMAP( INDF2, COMP, STATUS )
      CALL NDF_UNMAP( INDF3, COMP, STATUS )

      END
