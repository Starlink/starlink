      SUBROUTINE NDF1_WPLIM( IWCS, NAX, LBNDD, UBNDD, VALUE1, VALUE2, 
     :                       ISPIX1, ISPIX2, ISBND, ISDEF1, ISDEF2, 
     :                       LBND, UBND, STATUS )
*+
*  Name:
*     NDF1_WPLIM

*  Purpose:
*     Determine pixel limits for all NDF axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_WPLIM( IWCS, NAX, LBNDD, UBNDD, VALUE1, VALUE2, ISPIX1,
*                      ISPIX2, ISBND, ISDEF1, ISDEF2, LBND, UBND, 
*                      STATUS )

*  Description:
*     This routine accepts values which have been supplied as either
*     pixel indices or WCS axis values in a NDF section specification, 
*     and calculates the corresponding NDF pixel-index bounds.

*  Arguments:
*     IWCS = INTEGER (Given)
*        AST pointer to the NDFs WCS FrameSet.
*     NAX = INTEGER (Given)
*        The number of axes for which bounds are supplied.
*     LBNDD( * ) = INTEGER (Given)
*        Lower pixel index bounds for the NDF.
*     UBNDD( * ) = INTEGER (Given)
*        Upper pixel index bounds for the NDF.
*     VALUE1( NAX ) = DOUBLE PRECISION (Given)
*        First value specifying the bound on each axis. 
*     VALUE2( NAX ) = DOUBLE PRECISION (Given)
*        Second value specifying the bound on each axis. 
*     ISPIX1( NAX ) = LOGICAL (Given)
*        Whether VALUE1 is a pixel index (as opposed to a WCS value).
*     ISPIX2( NAX ) = LOGICAL (Given)
*        Whether VALUE2 is a pixel index (as opposed to a WCS value).
*     ISBND( NAX ) = LOGICAL (Given)
*        Whether VALUE1 and VALUE2 specify the lower and upper bounds
*        directly (as opposed to specifying the centre and width). 
*     ISDEF1( NAX ) = LOGICAL (Given)
*        Is the value supplied VALUE1 a default value?
*     ISDEF2( NAX ) = LOGICAL (Given)
*        Is the value supplied VALUE2 a default value?
*     LBND( NAX ) = INTEGER (Returned)
*        Lower pixel-index bounds.
*     UBND( NAX ) = INTEGER (Returned)
*        Upper pixel-index bounds.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S Berry (JACH, UCLan)
*     {enter_new_authors_here}

*  History:
*     1-JUN-2007 (DSB):
*        Original version.
*     6-JUN-2007 (DSB):
*        Correct conversion of pixel index "centre/width" values to 
*        upper/lower bounds. Also report an error if thereis no overlap
*        between the pixel and WCS boxes. Also, do not clip the supplied
*        WCS box at the edges of hte NDF if no pixel limits were given.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants      
      INCLUDE 'AST_PAR'          ! AST_ constants and functions
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER IWCS
      INTEGER NAX
      INTEGER LBNDD( * )
      INTEGER UBNDD( * )
      DOUBLE PRECISION VALUE1( NAX )
      DOUBLE PRECISION VALUE2( NAX )
      LOGICAL ISPIX1( NAX )
      LOGICAL ISPIX2( NAX )
      LOGICAL ISBND( NAX )
      LOGICAL ISDEF1( NAX )
      LOGICAL ISDEF2( NAX )

*  Arguments Returned:
      INTEGER LBND( NAX )
      INTEGER UBND( NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION PLBND( NDF__MXDIM )
      DOUBLE PRECISION DELTA
      DOUBLE PRECISION WUBND( NDF__MXDIM )
      DOUBLE PRECISION V1
      DOUBLE PRECISION WLBND( NDF__MXDIM )
      DOUBLE PRECISION V2
      DOUBLE PRECISION DUBNDD( NDF__MXDIM )
      DOUBLE PRECISION DLBNDD( NDF__MXDIM )
      DOUBLE PRECISION PUBND( NDF__MXDIM )
      DOUBLE PRECISION XL( NDF__MXDIM )
      DOUBLE PRECISION XU( NDF__MXDIM )
      INTEGER CFRM
      INTEGER CMPREG
      INTEGER I
      INTEGER MAP
      INTEGER NWCS
      INTEGER PBOX
      INTEGER PFRM
      INTEGER TEMP                   
      INTEGER WBOX
      INTEGER WBOXP
      LOGICAL ALLPIX
      LOGICAL ALLWCS
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of WCS axes in the NDF.
      NWCS = AST_GETI( IWCS, 'Nout', STATUS )

*  Initialise a box that encloses the required section of WCS space.
      DO I = 1, NWCS
         WLBND( I ) = AST__BAD    
         WUBND( I ) = AST__BAD    
      END DO

*  Indicate that so far we have not found any WCS bounds.
      ALLPIX = .TRUE.

*  We now set up the bounds of two boxes; PLBND/PUBND hold the bounds of
*  a box in PIXEL coordinates, and WLBND/WUBND hold the bounds of a box 
*  in WCS current Frame coordinates. Both boxes have edges parallel to
*  their respective coordinate axes. Any WCS bounds in the supplied 
*  VALUE1/VALUE2 arrays are used to set the bounds of the corresponding
*  axes of the WCS box. Any pixel index bounds in the supplied 
*  VALUE1/VALUE2 arrays are used to set the bounds of the corresponding
*  axes of the PIXEL box. If any axis has no bounds then defaults are
*  used that encompass the whole NDF. For PIXEL axes these default bounds
*  are inserted into PLBND/PUBND immediately. For WCS axes, default bounds
*  are only calculated as needed after the supplied WCS bounds have been
*  identified. The need for a default WCS bound to be calculated is
*  flagged by storing an AST__BAD value in WLBND/WUBND. Loop round each
*  axis for which bounds have been supplied.
      DO I = 1, NAX

*  Get the pixel coordinate bounds of the NDF on this axis.
         DLBNDD( I ) = DBLE( LBNDD( I ) ) - 1.0D0
         DUBNDD( I ) = DBLE( UBNDD( I ) ) 

*  If upper and lower limits have been supplied for this axis...
         IF( ISBND( I ) ) THEN

*  If the lower limit was specified as a pixel index, store the
*  corresponding pixel coordinate as the lower bound for the pixel
*  coordinate box. The lower limit of the WCS box is left set at AST__BAD.
            IF( ISPIX1( I ) ) THEN
               PLBND( I ) = VALUE1( I ) - 1.0D0

*  Otherwise the lower limit was specified as a WCS value, so store the
*  corresponding value as the lower limit of the WCS box, and store the
*  lower pixel bound of the NDF as the lower bound of the pixel box. Set
*  the ALLPIX flag to ndicate that at least one bound is specified as a
*  WCS value.
            ELSE
               PLBND( I ) = DLBNDD( I )
               WLBND( I ) = VALUE1( I )
               ALLPIX = .FALSE.
            END IF

*  Do the same for the upper bound.
            IF( ISPIX2( I ) ) THEN
               PUBND( I ) = VALUE2( I ) 
            ELSE
               PUBND( I ) = DUBNDD( I )
               WUBND( I ) = VALUE2( I )
               ALLPIX = .FALSE.
            END IF

*  If the bounds for this axis are specified by centre and width, then we
*  require that both centre and width be in the same coordinate system
*  (pixel or WCS). Check that this is the case.
         ELSE IF( ISPIX1( I ) .EQV. ISPIX2( I ) ) THEN

*  If pixel, then store the bounds of the pixel box and leave the WCS box
*  bounds set to AST__BAD.
            IF( ISPIX1( I ) ) THEN
               DELTA = DBLE( NINT( VALUE2( I ) )/2 )
               PLBND( I ) = VALUE1( I )  - 1.0D0 - DELTA
               PUBND( I ) = PLBND( I ) + VALUE2( I )

*  If WCS, then use the NDF Bounds as the pixel box and store the WCS box
*  bounds.
            ELSE
               DELTA = 0.5*VALUE2( I )
               PLBND( I ) = DLBNDD( I ) 
               PUBND( I ) = DUBNDD( I ) 
               WLBND( I ) = VALUE1( I ) - DELTA
               WUBND( I ) = VALUE1( I ) + DELTA
               ALLPIX = .FALSE.
            END IF

*  Report an error if centre and width are in different coordinate systems.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = NDF__BNDIN
            CALL MSG_SETI( 'I', I )
            CALL ERR_REP( 'NDF1_WPLIM_WAX', 'Bounds given for axis'//
     :                    ' ^I mixes WCS and pixel units.', STATUS )
         END IF

      END DO

*  If all bounds were specified using pixel indices, then we can pass
*  on to cut the required section from the NDF. If some bounds were
*  specified using WCS coordinates we need to find the overlap, in pixel
*  indices, between the WCS box and the pixel box.
      IF( .NOT. ALLPIX .AND. STATUS .EQ. SAI__OK ) THEN

*  Begin an AST context. 
         CALL AST_BEGIN( STATUS )

*  Get the current WCS Frame -> PIXEL mapping.
         MAP = AST_GETMAPPING( IWCS, AST__CURRENT, 2, STATUS )      

*  Get the current WCS coordinate Frame.
         CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Indicate that we have not yet found any pixel index bounds
         ALLWCS = .TRUE.
   
*  Ensure the WCS box is complete by replacing any AST__BAD values by the
*  appropriate limit than encompasses the whole pixel box. Check each WCS 
*  axis.
         DO I = 1, NWCS

*  Pass on to the next axis if this WCS axis already has upper and lower
*  bounds.
            IF( WLBND( I ) .EQ. AST__BAD .OR. 
     :          WUBND( I ) .EQ. AST__BAD ) THEN

*  Inidcate that at least one axis was specified by pixel index bounds.
               ALLWCS = .FALSE.

*  Map the pixel box into WCS coords and get the limits of the box on
*  this WCS axis.
               CALL AST_MAPBOX( MAP, PLBND, PUBND, .FALSE., I, V1, V2,
     :                          XL, XU, STATUS )

*  Whether a WCS value is a "lower" or "upper" bound is determined not by
*  the WCS values themselves but by which one gives the lower or upper 
*  value on the corresponding pixel axis. Use this criterion to fill in
*  values for which ever WCS bound has not been supplied.
               IF( WLBND( I ) .EQ. AST__BAD ) THEN
                  IF( XL( I ) .LT. XU( I ) ) THEN
                     WLBND( I ) = V1
                  ELSE 
                     WLBND( I ) = V2
                  END IF
               END IF

               IF( WUBND( I ) .EQ. AST__BAD ) THEN
                  IF( XL( I ) .GT. XU( I ) ) THEN
                     WUBND( I ) = V1
                  ELSE 
                     WUBND( I ) = V2
                  END IF
               END IF
            END IF

*  The AST Box class knows nothing about axis normalisation. To avoid
*  problems ensure that the upper and lower axis values are in the same
*  "cylce". This applied particularly to RA values where the lower limit 
*  may have a value of (say) 359 degrees and the upper limit be (say) 2
*  degrees. In this example the following code converts the upper limit
*  to 361 degrees.
            DELTA = AST_AXDISTANCE( CFRM, I, WLBND( I ), WUBND( I ),
     :                              STATUS )
            WUBND( I ) = WLBND( I ) + DELTA

         END DO

*  Define an AST Box within the current WCS Frame, using the bounds
*  stored in WLBND/WUBND.
         WBOX = AST_BOX( CFRM, 1, WLBND, WUBND, AST__NULL, ' ', STATUS )

*  Map this region into the PIXEL Frame. The resulting Region will (in
*  general) be a rotated box with curvi-linear edges.
         PFRM = AST_GETFRAME( IWCS, 2, STATUS )
         WBOXP = AST_MAPREGION( WBOX, MAP, PFRM, STATUS )

*  If all bounds were specified as WCS  values, find the bounds (in 
*  PIXEL coords) of the above Box, and store in PLBND/PUBND. 
         IF( ALLWCS ) THEN
            CALL AST_GETREGIONBOUNDS( WBOXP, PLBND, PUBND, STATUS )

*  Otherwise, we restrict the returned section to the overlap of the WCS 
*  and PIXEL boxes.
         ELSE

*  Replace any defaulted bounds in the pixel box with bad values. This
*  causes AST_INTERVAL to ignore the bound, making the axis value
*  unlimited.
            DO I = 1, NAX
               IF( ISBND( I ) ) THEN
                  IF( ISDEF1( I ) .OR. .NOT. ISPIX1( I ) ) 
     :                                             PLBND( I ) = AST__BAD
                  IF( ISDEF2( I ) .OR. .NOT. ISPIX2( I ) ) 
     :                                             PUBND( I ) = AST__BAD
               END IF
            END DO

*  Define an AST Interval within the PIXEL Frame, using the bounds stored 
*  in PLBND/PUBND.
            PBOX = AST_INTERVAL( PFRM, PLBND, PUBND, AST__NULL, ' ', 
     :                           STATUS )

*  Now form a compound region that is the intersection of the two aboves
*  Boxes (both now defined in the PIXEL Frame).
            CMPREG = AST_CMPREGION( PBOX, WBOXP, AST__AND, ' ', STATUS )

*  Find the bounds (in PIXEL coords) of the compound Region, and store in
*  PLBND/PUBND. 
            CALL AST_GETREGIONBOUNDS( CMPREG, PLBND, PUBND, STATUS )

*  Report an error if the pixel and WCS boxes do not overlap.
            DO I = 1, NAX
               IF ( PLBND( I ) .GT. PUBND( I ) .AND.
     :              STATUS .EQ. SAI__OK ) THEN
                  STATUS = NDF__BNDIN
                  CALL ERR_REP( 'NDF1_WPLIM_NOV', 'The requested '//
     :                          'section does not contain any pixels.',
     :                          STATUS )
               END IF
            END DO

         END IF

*  End the AST context. 
         CALL AST_END( STATUS )
      END IF

*  Convert the pixel coordinate box to pixel indices.
      IF( STATUS .EQ. SAI__OK ) THEN
         DO I = 1, NAX
            LBND( I ) = NINT( PLBND( I ) ) + 1
            UBND( I ) = NINT( PUBND( I ) )
            IF ( LBND( I ) .GT. UBND( I ) ) THEN
               TEMP = LBND( I )
               LBND( I ) = UBND( I )
               UBND( I ) = TEMP
            END IF
         END DO
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_WPLIM', STATUS )

      END
