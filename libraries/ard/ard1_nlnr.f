      SUBROUTINE ARD1_NLNR( RINDEX, TYPE, NDIM, LBND, UBND, MSKSIZ,
     :                      NPAR, PAR, IWCS, DPP, IPB, LBEXTB,
     :                      UBEXTB, LBINTB, UBINTB, STATUS )
*+
*  Name:
*     ARD1_NLNR

*  Purpose:
*     Set an array to hold a keyword region using a non-linear
*     mapping.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_NLNR( RINDEX, TYPE, NDIM, LBND, UBND, MSKSIZ, NPAR,
*                     PAR, IWCS, DPP, IPB, LBEXTB, UBEXTB, LBINTB,
*                     UBINTB, STATUS )

*  Description:
*     The array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values are then assigned to the points specified by the
*     supplied parameters.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     TYPE = INTEGER (Given)
*        Integer code for region type.
*     NDIM = INTEGER (Given)
*        The number of pixel axes in the B array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the B array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the B array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     PAR( * ) = DOUBLE PRECISION (Given)
*        The region parameters, as supplied in the ARD expression. The
*        length of this array should be MAX( 16, NPAR ).
*     IWCS = INTEGER (Given)
*        An AST FrameSet. The Base Frame should be pixel coords within
*        the B array, and the Current Frame should be used cords.
*     DPP = DOUBLE PRECISION (Given)
*        An estimate of the distance in user coords covered by 1 pixel in
*        the B array. The precise value may vary across the array, and
*        may be different for displacements in different directions. The
*        supplied estimate should be a lower bound.
*     IPB = INTEGER (Given)
*        A point to the array to be filled.
*     LBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B.
*     LBINTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B.
*     LOADED = LOGICAL (Given and Returned)
*        Have the contents of the supplied mask already been loaded onto
*        the stack? Always returned .TRUE. if the keyword is an INPUT
*        keyword.
*     RINDEX = INTEGER (Returned)
*        The region index used for the keyword.
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
*     3-NOV-2020 (DSB):
*        Fix a bug in the handling of regions that completely enclose the supplied 
*        array. Previously, the array was returned filled with exterior values in 
*        such cases. Now. it is returned filled with interior values.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Global Variables:
      INCLUDE 'ARD_COM'          ! ARD common blocks
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*        CMN_RNDXC = INTEGER (Write)
*           The RINDEX value passed to the ARD "drawing" routines.
*        CMN_TYPEC = INTEGER (Write)
*           The region type.
*        CMN_FRMC = INTEGER (Write)
*           An AST Frame representing user coords.
*        CMN_NPARC = INTEGER (Write)
*           Number of region parameters.
*        CMN_INIT = LOGICAL (Write)
*           Is a new region being done?

*  Arguments Given:
      INTEGER RINDEX
      INTEGER TYPE
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER NPAR
      DOUBLE PRECISION PAR( * )
      INTEGER IWCS
      DOUBLE PRECISION DPP
      INTEGER IPB

*  Arguments Given and Returned:
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_UINTERP

*  Local Variables:
      CHARACTER
     :        CLASS*30           ! User coords Frame class

      DOUBLE PRECISION
     :      DP,                  ! A dummy argument
     :      HL,                  ! Half the requested box length
     :      HW,                  ! Half the requested box width
     :      INA( ARD__MXDIM ),   ! Point A in grixel coords
     :      INB( ARD__MXDIM ),   ! Point B in grixel coords
     :      OUTA( ARD__MXDIM ),  ! Point A in pixel coords
     :      OUTB( ARD__MXDIM ),  ! Point B in pixel coords
     :      P0( 2 ),             ! The box centre
     :      PA0,                 ! Requested angle as a position angle
     :      PA1,                 ! The position angle at the end
     :      PA2,                 ! The position angle at the end
     :      RLB,                 ! Axis lower bound in pixel coords
     :      RUB,                 ! Axis upper bound in pixel coords
     :      UBXLB( ARD__MXDIM ), ! Lower bounds of bounding box in user coords
     :      UBXUB( ARD__MXDIM ), ! Upper bounds of bounding box in user coords
     :      XL( ARD__MXDIM ),    ! User coords at axis lower bound
     :      XU( ARD__MXDIM )     ! User coords at axis upper bound

      INTEGER
     :        CFRM,              ! Pointer to current Frame
     :        GMAP,              ! Mapping from grixel to user
     :        I,                 ! Loop count
     :        LBIN( ARD__MXDIM ),! Lower bounds for pretend input image
     :        MAP,               ! Pixel to user coords Mapping
     :        MXPIX,             ! Max dimension of array
     :        NBAD,              ! No. of missing pixels found in B
     :        NWCS,              ! No. of user axes
     :        UBIN( ARD__MXDIM ),! Upper bounds for pretend input image
     :        WINMAP             ! Mapping from grixel to pixel

      DATA LBIN /ARD__MXDIM*1/,
     :     UBIN /ARD__MXDIM*1/
*.

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reset all pixels within the interior bounding box so that they
*  hold exterior values. The pixels outside the interior bounding box
*  already hold exterior values.
      CALL ARD1_BXSET( NDIM, LBND, UBND, MSKSIZ, 0, LBINTB,
     :                 UBINTB, %VAL( CNF_PVAL( IPB ) ), STATUS )
      LBEXTB( 1 ) = VAL__MAXI
      LBINTB( 1 ) = VAL__MINI

*  Regions with zero volume/area are handled differently.
      IF( TYPE .EQ. ARD__POI .OR. TYPE .EQ. ARD__PIX .OR.
     :    TYPE .EQ. ARD__LIN .OR. TYPE .EQ. ARD__ROW .OR.
     :    TYPE .EQ. ARD__COL ) THEN

         CALL ARD1_KDRAW( RINDEX, NDIM, LBND, UBND, MSKSIZ, TYPE,
     :                    IWCS, NPAR, PAR, IPB, LBEXTB, UBEXTB,
     :                    LBINTB, UBINTB, STATUS )

*  Report an error for any regions not supported by this routine.
      ELSE IF( TYPE .EQ. ARD__FRA ) THEN
         STATUS = ARD__NONLN
         CALL MSG_SETI( 'TYPE', TYPE )
         CALL ERR_REP( 'ARD1_NLNR_ERR5', 'Illegal FRAME keyword '//
     :                 'found in an ARD expression.', STATUS )
         CALL ERR_REP( 'ARD1_NLNR_ERR5', 'FRAME keywords cannot be '//
     :                 'used since the mapping to pixel coordinates '//
     :                 'is non-linear.', STATUS )

      ELSE IF( TYPE .EQ. ARD__WHO .OR. TYPE .EQ. ARD__INP ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'TYPE', TYPE )
         CALL ERR_REP( 'ARD1_NLNR_ERR5', 'Illegal keyword identifier '//
     :                 ' (^TYPE) encountered in routine ARD1_NLNR '//
     :                 '(programming error).', STATUS )

*  For supported keywords...
      ELSE

*  Begin an AST context.
         CALL AST_BEGIN( STATUS )

*  Get a pointer to the user coordinate Frame, and the number of WCS axes.
         CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
         NWCS = AST_GETI( CFRM, 'Naxes', STATUS )

*  We can simplify some regions...

*  Convert RECT regions into BOX regions.
         IF( TYPE .EQ. ARD__REC ) THEN
            DO I = 1, NWCS
               HW = 0.5*AST_AXDISTANCE( CFRM, I, PAR( I ),
     :                                  PAR( I + NWCS ), STATUS )
               PAR( I ) = AST_AXOFFSET( CFRM, I, PAR( I ), HW, STATUS )
               PAR( I + NWCS ) = 2*ABS( HW )
            END DO
            TYPE = ARD__BOX

*  Convert ROTBOX regions into POLYGON regions. A polygon vertex is put at
*  the middle of each side in case we are dealing with spherical coords, in
*  which case a polygon edge greater than 180 arc-degrees could cause
*  problems.
         ELSE IF( TYPE .EQ. ARD__ROT ) THEN

*  Offset away from the centre point along the first edge, at the
*  requested angle, going half the box length. Using AST caters for both
*  Cartesian and spherical user coords. Store points back in the PAR
*  array, in a suitable order to make the points a continuous curve.
            PA0 = ( 90.0 - PAR( 5 ) )*ARD__DTOR
            HL = 0.5*PAR( 3 )
            HW = 0.5*PAR( 4 )
            P0( 1 ) = PAR( 1 )
            P0( 2 ) = PAR( 2 )

            PA1 = AST_OFFSET2( CFRM, P0, PA0, HL, PAR, STATUS )

*  Now turn to the left by 90 degrees and offset up by half the box height.
            PA1 = PA1 - ARD__PIBY2
            PA2 = AST_OFFSET2( CFRM, PAR, PA1, HW, PAR( 3 ), STATUS )

*  Now offset down by half the box height.
            PA2 = AST_OFFSET2( CFRM, PAR, PA1, -HW, PAR( 15 ),
     :                         STATUS )

*  Now offset up and down by half the box height, starting at the box
*  centre.
            PA1 = PA0 - ARD__PIBY2
            PA2 = AST_OFFSET2( CFRM, P0, PA1, HW, PAR( 5 ), STATUS )
            PA2 = AST_OFFSET2( CFRM, P0, PA1, -HW, PAR( 13 ), STATUS )

*  Offset away from the centre point along the first edge, away from the
*  requested angle, going half the box length.
            PA1 = AST_OFFSET2( CFRM, P0, PA0, -HL, PAR( 9 ), STATUS )

*  Now turn to the left by 90 degrees and offset up by half the box height.
            PA1 = PA1 - ARD__PIBY2
            PA2 = AST_OFFSET2( CFRM, PAR( 9 ), PA1, HW, PAR( 7 ),
     :                         STATUS )

*  Now offset down by half the box height.
            PA2 = AST_OFFSET2( CFRM, PAR( 9 ), PA1, -HW, PAR( 11 ),
     :                         STATUS )

*  Reset the region type and the number of parameters.
            TYPE = ARD__POL
            NPAR = 16

         END IF

*  We first try to find a bounding box within pixel coords which contains
*  the region. How this is done depends on the class of the Frame
*  describing user coords. Note the class of the current Frame.
         CLASS = AST_GETC( CFRM, 'CLASS', STATUS )

*  Extract and simplify the pixel->user Mapping.
         MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE,
     :                                       AST__CURRENT, STATUS ),
     :                       STATUS )

*  If user coords is cartesion cordinate system, we can use the
*  AST_MAPBOX method.
         IF( CLASS .EQ. 'Frame' ) THEN

*  Get a bounding box for the region in user coords.
            CALL ARD1_UBBOX( NDIM, CFRM, TYPE, NPAR, PAR, UBXLB, UBXUB,
     :                       STATUS )

*  Find the corresponding bounding box in pixel indices.
            DO I = 1, NDIM

*  Find the bounds in pixel coords on the current axis.
               CALL AST_MAPBOX( MAP, UBXLB, UBXUB, .FALSE., I, RLB, RUB,
     :                          XL, XU, STATUS )

*  Convert from pixel coords to pixel indices and store as the new
*  interior bounding box. Include a safety margin of 1 pixel.
               LBINTB( I ) = NINT( RLB + 0.5 ) - 1
               UBINTB( I ) = NINT( RUB + 0.5 ) + 1

            END DO

*  If user coords is a 2D spherical cordinate system, we "draw" the
*  region boundary and then find the bounds of the "drawing".
         ELSE IF( CLASS .EQ. 'SkyFrame' ) THEN

*  Draw the region boundary into the B array. This returns a bounding
*  box.
            CALL ARD1_DRAW2( RINDEX, LBND, UBND, MSKSIZ, TYPE,
     :                       IWCS, NPAR, PAR, IPB, LBINTB, UBINTB,
     :                       STATUS )

*  If the region encloses the whole array, fill the array with the
*  interior value.
            IF( LBINTB( 1 ) .EQ. VAL__MAXI ) THEN
               CALL ARD1_BXSET( NDIM, LBND, UBND, MSKSIZ, RINDEX,
     :                          LBND, UBND, %VAL( CNF_PVAL( IPB ) ),
     :                          STATUS )

*  Otherwise, reset all pixels so that they hold exterior values and then
*  extend the bounding box by a safety margin of 2 pixels.
            ELSE
               CALL ARD1_BXSET( NDIM, LBND, UBND, MSKSIZ, 0, LBINTB,
     :                          UBINTB, %VAL( CNF_PVAL( IPB ) ),
     :                          STATUS )
               IF( LBINTB( 1 ) .NE. VAL__MINI ) THEN
                  LBINTB( 1 ) = LBINTB( 1 ) - 2
                  UBINTB( 1 ) = UBINTB( 1 ) + 2
                  LBINTB( 2 ) = LBINTB( 2 ) - 2
                  UBINTB( 2 ) = UBINTB( 2 ) + 2
               END IF
            END IF

*  For any other class of user coords, we cannot confidently find a
*  bounding box, so use the entire image.
         ELSE
            DO I = 1, NDIM
               LBINTB( I ) = LBND( I )
               UBINTB( I ) = UBND( I )
            END DO

         END IF

*  Ensure that the bounding box does not exceed the bounds of the mask.
         IF( LBINTB( 1 ) .NE. VAL__MINI .AND.
     :       LBINTB( 1 ) .NE. VAL__MAXI ) THEN

            DO I = 1, NDIM
               LBINTB( I ) = MAX( LBINTB( I ), LBND( I ) )
               UBINTB( I ) = MIN( UBINTB( I ), UBND( I ) )

*  If the lower bound is higher than the upper bound, use a null box
               IF( LBINTB( I ) .GT. UBINTB( I ) ) THEN
                  LBINTB( 1 ) = VAL__MINI
               END IF

            END DO
         END IF

*  If the interior bounding box is now null or infinite, no change needs
*  to be made so return with the mask as it is (full of exterior or
*  interior values).
         IF( LBINTB( 1 ) .NE. VAL__MINI .AND.
     :       LBINTB( 1 ) .NE. VAL__MAXI ) THEN

*  AST_RESAMPLE requires the mapping from user coords to "GRIXEL" coords
*  (which are like PIXEL coords except that the centre of pixel index I
*  has grixel coord REAL(I), instead of REAL( I ) - 0.5). Create a WinMap
*  which goes from grixel to pixel.
            DO I = 1, NDIM
               INA( I ) = 0.5D0
               INB( I ) = 1.5D0
               OUTA( I ) = 0.0D0
               OUTB( I ) = 1.0D0
            END DO
            WINMAP = AST_WINMAP( NDIM, INA, INB, OUTA, OUTB, ' ',
     :                           STATUS )

*  Combine this Mapping with the Mapping from pixel to user, to create a
*  compound mapping (CmpMap) from grixel to user. Simplify it.
            GMAP = AST_SIMPLIFY( AST_CMPMAP( WINMAP, MAP, .TRUE., ' ',
     :                                       STATUS ), STATUS )

*  Invert it, since AST_RESAMPLE requires the Mapping from user to grixel.
            CALL AST_INVERT( GMAP, STATUS )

*  Find the largest dimension of the B array.
            MXPIX = 0
            DO I = 1, NDIM
               MXPIX = MAX( MXPIX, UBND( I ) - LBND( I ) + 1 )
            END DO

*  Store the common values needed by ARD1_UINTERP.
            CMN_RNDXC = RINDEX
            CMN_TYPEC = TYPE
            CMN_FRMC = CFRM
            CMN_NPARC = NPAR
            CMN_INIT = .TRUE.

*  In order to identify which mask pixels are within the region, we
*  "resample" the mask array, transforming each mask pixel position into
*  user coords, and storing an interior value in the mask if the transformed
*  position is within the region defined in user coords. We use
*  AST_RESAMPLE to do this, supplying routine ARD1_UINTERP to do the
*  specific form of pseudo-"resampling" required.
            NBAD = AST_RESAMPLEI( GMAP, NWCS, LBIN, UBIN, DP, DP,
     :                            AST__UINTERP, ARD1_UINTERP, PAR, 0,
     :                            DPP*0.2, MXPIX, 0, NDIM, LBND, UBND,
     :                            LBINTB, UBINTB,
     :                            %VAL( CNF_PVAL( IPB ) ), DP,
     :                            STATUS )

         END IF

*  End the AST context.
         CALL AST_END( STATUS )

      END IF

*  If the interior bounding box is null, return the usual value
*  (VAL__MINI for LBINTB( 1 ) ).
      DO I = 1, NDIM
         IF( LBINTB( I ) .GT. UBINTB( I ) ) LBINTB( 1 ) = VAL__MINI
      END DO

*  Ensure the the exterior bounding box is returned "infinite".
      LBEXTB( 1 ) = VAL__MAXI

      END
