      SUBROUTINE KPS1_PLCPY( INDF1, INDF2, INDF3, COMP, ITYPE, USE1,
     :                       USE2, AXES, SLBND1, SUBND1, SLBND2, SUBND2,
     :                       MASK, FILL, STATUS )
*+
*  Name:
*     KPS1_PLCPY

*  Purpose:
*     Copy polygonal segments from a pair of NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PLCPY( INDF1, INDF2, INDF3, COMP, ITYPE, USE1, USE2,
*                      AXES, SLBND1, SUBND1, SLBND2, SUBND2, MASK,
*                      FILL, STATUS )

*  Description:
*     This subroutine copies pixel values into INDF3 from INDF1 which
*     are selected by the supplied mask. Pixels which are not selected
*     in the mask are copied from INDF2. It is a bit more complex than
*     one might think necessary because of the need to deal with N-d
*     data arrays. If the NDFs have more than 2 dimensions the user can
*     choose to apply the polygonal segmentation in any plane. The
*     polygons are presumed to lie within the plane defined by the axes
*     supplied in argument AXES. The polygons are then projected
*     through all the other dimensions. We need to ensure that the 3
*     data arrays are accessed in "natural" order (i.e with the first
*     axis incrementing fastest, second axis next fastest, etc),
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
*        The numerical type with which the data is to be accessed.
*     USE1 = LOGICAL (Given)
*        If supplied .FALSE., then the NDF identified by INDF1 is
*        ignored. In this case the inside of the polygonal segments are
*        filled with the value supplied by argument FILL.
*     USE2 = LOGICAL (Given)
*        If supplied .FALSE., then the NDF identified by INDF2 is
*        ignored. In this case the outside of the polygonal segments are
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
*        polygonal segments. This is a 2d array which corresponds to a
*        2d slice through the output NDF spanned by the axes supplied
*        in argument AXES. a .TRUE. value means that the pixel is inside
*        a polygon.
*     FILL = DOUBLE PRECISION (Given)
*        The constant pixel value to assume if either of the two input
*        NDFs cannot be used. This is a double precision value which is
*        converted within this routine to the numerical type requested
*        in argument ITYPE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2
      INTEGER INDF3
      CHARACTER COMP*(*)
      CHARACTER ITYPE*(*)
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
      BYTE
     :        FILLUB             ! _BYTE fill value

      INTEGER*2
     :        FILLW              ! _WORD fill value

      INTEGER
     :        DIM( NDF__MXDIM ), ! Size of each dimension
     :        EL,                ! Element counter
     :        FILLI,             ! _INTEGER fill value
     :        I,                 ! Dimension counter
     :        INDEX( NDF__MXDIM ),! Pixel indices of current element
     :        IPIN,              ! Pointer to source array
     :        IPIN1,             ! Pointer to 1st input array
     :        IPIN2,             ! Pointer to 2nd input array
     :        IPOUT,             ! Pointer to output array
     :        J,                 ! Offset of element into next dimension
     :        LBND( NDF__MXDIM ),! Lower bounds of the output array
     :        NDIM,              ! No. of dimensions in the output array
     :        NEL,               ! No. of elements in the output array
     :        UBND( NDF__MXDIM ) ! Upper bounds of the output array

      LOGICAL
     :        USE                ! Should value in source array be used?

      REAL
     :        FILLR              ! _REAL fill value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Map the required arrays.
      IF( USE1 ) CALL NDF_MAP( INDF1, COMP, ITYPE, 'READ', IPIN1, NEL,
     :                         STATUS )
      IF( USE2 ) CALL NDF_MAP( INDF2, COMP, ITYPE, 'READ', IPIN2, NEL,
     :                         STATUS )
      CALL NDF_MAP( INDF3, COMP, ITYPE, 'WRITE', IPOUT, NEL, STATUS )

*  Get the bounds of the output NDF.
      CALL NDF_BOUND( INDF3, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Store the size of each dimension.
      DO I = 1, NDIM
         DIM( I ) = UBND( I ) - LBND( I ) + 1
      END DO

*  Loop round all pixels in the output.
      DO EL = 1, NEL

*  Find the pixel indices of the current pixel.
         J = EL - 1
         DO I = 1, NDIM
            INDEX( I ) = MOD( J, DIM( I ) ) + LBND( I )
            J = J/DIM( I )
         END DO

*  Store the pointer to the relevant input array, depending on whether
*  or not this pixel is inside the polygon.
         IF( MASK( INDEX( AXES( 1 ) ), INDEX( AXES( 2 ) ) ) ) THEN
            IPIN = IPIN1
            USE = USE1
         ELSE
            IPIN = IPIN2
            USE = USE2
         END IF

*  Copy the appropriate value into the output NDF.
         IF( ITYPE .EQ. '_UBYTE' ) THEN
            FILLUB = VAL_DTOUB( .TRUE., FILL, STATUS )
            CALL KPS1_PLCPUB( EL, NEL, %VAL( IPIN ), USE, FILLUB,
     :                        %VAL( IPOUT ), STATUS )

         ELSE IF( ITYPE .EQ. '_WORD' ) THEN
            FILLW = VAL_DTOW( .TRUE., FILL, STATUS )
            CALL KPS1_PLCPW( EL, NEL, %VAL( IPIN ), USE, FILLW,
     :                       %VAL( IPOUT ), STATUS )

         ELSE IF( ITYPE .EQ. '_INTEGER' ) THEN
            FILLI = VAL_DTOI( .TRUE., FILL, STATUS )
            CALL KPS1_PLCPI( EL, NEL, %VAL( IPIN ), USE, FILLI,
     :                       %VAL( IPOUT ), STATUS )

         ELSE IF( ITYPE .EQ. '_REAL' ) THEN
            FILLR = VAL_DTOR( .TRUE., FILL, STATUS )
            CALL KPS1_PLCPR( EL, NEL, %VAL( IPIN ), USE, FILLR,
     :                       %VAL( IPOUT ), STATUS )

         ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_PLCPD( EL, NEL, %VAL( IPIN ), USE, FILL,
     :                       %VAL( IPOUT ), STATUS )
         END IF

      END DO

*  Unmap the arrays.
      IF( USE1 ) CALL NDF_UNMAP( INDF1, COMP, STATUS )
      IF( USE2 ) CALL NDF_UNMAP( INDF2, COMP, STATUS )
      CALL NDF_UNMAP( INDF3, COMP, STATUS )

      END
