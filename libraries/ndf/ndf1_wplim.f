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
*        upper/lower bounds. Also report an error if there is no overlap
*        between the pixel and WCS boxes. Also, do not clip the supplied
*        WCS box at the edges of hte NDF if no pixel limits were given.
*        Also, provide WCS defaults for the centre if the centre is
*        defaulted in a "centre/width" axis.
*     8-JUN-2007 (DSB):
*        Added support for mixed-mode "centre/width" bounds.
*     29-JUN-2007 (DSB):
*        Correct final conversion from pixel coords to pixel indices.
*     17-AUG-2007 (DSB):
*        Improve error messages.
*     10-OCT-2007 (DSB):
*        - Handle axes correctly that have no inverse transformation.
*        - Handle cases where the I'th pixel axis does not feed the I'th
*        WCS axis.
*     20-NOV-2007 (DSB):
*        Ensure the section width is correct if the user has requested a
*        specified width in pixels (rounding errors could cause this not
*        to be the case in the previous version of this file).
*     21-NOV-2007 (DSB):
*        Avoid compiler warnings.
*     23-NOV-2007 (DSB):
*        Correction to the 20-NOV-2007 fix.
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
      DOUBLE PRECISION CENPIX( NDF__MXDIM )
      DOUBLE PRECISION CENWCS( NDF__MXDIM )
      DOUBLE PRECISION DELTA
      DOUBLE PRECISION DLBNDD( NDF__MXDIM )
      DOUBLE PRECISION DUBNDD( NDF__MXDIM )
      DOUBLE PRECISION MAXRNG
      DOUBLE PRECISION PLBND( NDF__MXDIM )
      DOUBLE PRECISION PLBND2( NDF__MXDIM )
      DOUBLE PRECISION PRANGE
      DOUBLE PRECISION PUBND( NDF__MXDIM )
      DOUBLE PRECISION PUBND2( NDF__MXDIM )
      DOUBLE PRECISION V1
      DOUBLE PRECISION V2
      DOUBLE PRECISION WLBND( NDF__MXDIM )
      DOUBLE PRECISION WLBND1( NDF__MXDIM )
      DOUBLE PRECISION WUBND( NDF__MXDIM )
      DOUBLE PRECISION WUBND1( NDF__MXDIM )
      DOUBLE PRECISION XL( NDF__MXDIM )
      DOUBLE PRECISION XU( NDF__MXDIM )
      INTEGER CFRM
      INTEGER CFRM1
      INTEGER CMPREG
      INTEGER FSMAP
      INTEGER I
      INTEGER INTERV
      INTEGER J
      INTEGER JJ
      INTEGER JUNK
      INTEGER MAP
      INTEGER MAP1
      INTEGER MAP2
      INTEGER NPIX
      INTEGER NWCS
      INTEGER PBOX
      INTEGER PFRM
      INTEGER PNAX1
      INTEGER PNAX2
      INTEGER PPERM( NDF__MXDIM )
      INTEGER TEMP                   
      INTEGER WBOX
      INTEGER WBOXP
      INTEGER WNAX1
      INTEGER WPERM( NDF__MXDIM )
      LOGICAL ALLPIX
      LOGICAL ALLWCS
      LOGICAL MIXED
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context. 
      CALL AST_BEGIN( STATUS )

*  Get the pixel->WCS Mapping. Note, the PIXEL Frame is always index 2.
      FSMAP = AST_GETMAPPING( IWCS, 2, AST__CURRENT, STATUS )

*  Split this Mapping up into two parallel Mappings, one of which (MAP1)
*  contains those inputs/outputs that have defined inverse transformations, 
*  and the other of which (MAP2) contains those inputs/outputs that have no 
*  inverse transformations (either of these Mappings may be AST__NULL). In
*  addition this returns a Mapping (MAP) that is equivalent to the supplied
*  pixel->WCS Mapping except that it will always have a defined inverse
*  (even if this means adding in a transformation that returns bad values).
*  Arrays are returned that allow the inputs and outputs of the full
*  Mapping to be associated with the corresponding inputs and outputs of
*  the component Mappings.
      CALL NDF1_MPSPT( FSMAP, MAP, MAP1, MAP2, PPERM, WPERM, STATUS )

*  Invert the Mappings so that their forward transformation goes from the 
*  current Frame to the pixel Frame. Check that we are bnverting a
*  different Mapping each time, to avoid inverting the same Mapping twice.
      CALL AST_INVERT( MAP, STATUS )

      IF( MAP1 .NE. AST__NULL ) THEN
         IF( .NOT. AST_SAME( MAP1, MAP, STATUS ) ) THEN
            CALL AST_INVERT( MAP1, STATUS )
         END IF
      END IF

      IF( MAP2 .NE. AST__NULL ) THEN
         IF( .NOT. AST_SAME( MAP2, MAP, STATUS ) .AND.
     :       .NOT. AST_SAME( MAP2, MAP1, STATUS ) ) THEN
            CALL AST_INVERT( MAP2, STATUS )
         END IF
      END IF

*  Get the number of pixel and WCS axes in the NDF.
      NPIX = AST_GETI( IWCS, 'Nin', STATUS )
      NWCS = AST_GETI( IWCS, 'Nout', STATUS )

*  Get the current WCS coordinate Frame.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Initialise a box that encloses the required section of WCS space.
      DO I = 1, NWCS
         WLBND( I ) = AST__BAD    
         WUBND( I ) = AST__BAD    
      END DO

*  If any axis that has been specified using "centre/width" format has a
*  WCS width value and a defaulted centre value, then we use a default
*  equal to the axis value at the centre of the NDF.
      CENWCS( 1 ) = AST__BAD
      DO I = 1, NAX
         IF( .NOT. ISBND( I ) .AND. ISPIX1( I ) .AND.
     :       .NOT. ISPIX2( I ) .AND. ISDEF1( I ) ) THEN

            IF( CENWCS( 1 ) .EQ. AST__BAD ) THEN

               DO J = 1, NPIX
                  CENPIX( J ) = 0.5*( UBNDD( I ) + LBNDD( I ) - 1 )
               END DO

               CALL AST_TRANN( MAP, 1, NAX, 1, CENPIX, .FALSE., NWCS, 
     :                         1, CENWCS, STATUS )

            END IF

            VALUE1( I ) = CENWCS( I )
            ISPIX1( I ) = .FALSE.
            
         END IF
      END DO

*  Indicate that so far we have not found any WCS bounds.
      ALLPIX = .TRUE.

*  Indicate that so far we have not found any centre/width bounds that
*  ause a WCS value for one limit and a pixel value for the other.
      MIXED = .FALSE.

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
*  the ALLPIX flag to indicate that at least one bound is specified as a
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

*  If the bounds for this axis are specified by centre and width, and have
*  been supplied in the same coordinate system (pixel or WCS)...
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

*  If the bounds for this axis are specified by centre and width, and the
*  centre is a WCS value but the the width is a pixel value...
         ELSE IF( .NOT. ISPIX1( I ) .AND. ISPIX2( I ) ) THEN
            ALLPIX = .FALSE.
            MIXED = .TRUE.

*  Store the central WCS values as the upper and lower bounds of the WCS
*  box.
            WUBND( I ) = VALUE1( I )
            WLBND( I ) = VALUE1( I )

*  Use defaults for the upper and lower bounds of the pixel box.
            PLBND( I ) = DLBNDD( I ) 
            PUBND( I ) = DUBNDD( I ) 

*  If the bounds for this axis are specified by centre and width, and the
*  centre is a pixel value but the the width is a WCS value...
         ELSE 
            ALLPIX = .FALSE.
            MIXED = .TRUE.

*  Store the central pixel values as the upper and lower bounds of the
*  pixel box. Leave the WCS box bounds set to bad so that defaults will
*  be fund and used.
            PUBND( I ) = VALUE1( I )
            PLBND( I ) = VALUE1( I )

         END IF

      END DO

c      write(*,*) 'A:'
c      write(*,*) '   PLBND: ',PLBND(1),PLBND(2)
c      write(*,*) '   PUBND: ',PUBND(1),PUBND(2)
c      write(*,*) '   WLBND: ',WLBND(1),WLBND(2)
c      write(*,*) '   WUBND: ',WUBND(1),WUBND(2)
c      write(*,*) '   '

*  If any centre/values bounds were specified in which the centre is
*  given as a pixel value and the width as a WCS value, then we need 
*  to convert the centre into a WCS value, so that we can get upper 
*  and lower bounds. We need to do this before we find the default WCS 
*  bounds since the WCS bounds depend on the pixel box.
      IF( MIXED ) THEN
         ALLPIX = .TRUE.
         MIXED = .FALSE.
         
*  Find the central positions in the pixel box.
         DO I = 1, NAX
            CENPIX( I ) = 0.5*( PLBND( I ) + PUBND( I ) )
         END DO

*  Convert to WCS.
         CALL AST_TRANN( MAP, 1, NPIX, 1, CENPIX, .FALSE., NWCS, 1, 
     :                   CENWCS, STATUS )

*  Rescan the supplied bounds looking for mixed mode centre/value  bounds.
         DO I = 1, NAX

*  If the bounds for this axis are specified by centre and width...
            IF( .NOT. ISBND( I ) ) THEN

*  ...and the width is a WCS value...
               IF( .NOT. ISPIX2( I ) ) THEN

*  ...and the centre is a PIXEL value...
                  IF( ISPIX1( I ) ) THEN

*  Re-calculate the WCS bounds using the central WCS value and the
*  supplied WCS width.
                     ISPIX1( I ) = .FALSE.
                     VALUE1( I ) = CENWCS( I )

                     DELTA = 0.5*VALUE2( I )
                     WLBND( I ) = VALUE1( I ) - DELTA
                     WUBND( I ) = VALUE1( I ) + DELTA
                     ALLPIX = .FALSE.

*  Store default bounds for the pixel box on this axis.
                     PLBND( I ) = DLBNDD( I ) 
                     PUBND( I ) = DUBNDD( I ) 
                     
*  If the centre was originally a WCS value, we have some WCS bounds.
                  ELSE
                     ALLPIX = .FALSE.
                  END IF                     

*  If the width is a pixel value and the centre is a WCS value, we still 
*  have a mixed mode bounds including WCS limits.
               ELSE IF( .NOT. ISPIX1( I ) ) THEN
                  ALLPIX = .FALSE.
                  MIXED = .TRUE.
               END IF

*  Note if we have a lower/upper bounds that includes any non-PIXEL values.
            ELSE IF( .NOT. ISPIX1( I ) .OR. .NOT. ISPIX2( I ) ) THEN
               ALLPIX = .FALSE.
            END IF

         END DO
      END IF

c      write(*,*) 'B:'
c      write(*,*) '   PLBND: ',PLBND(1),PLBND(2)
c      write(*,*) '   PUBND: ',PUBND(1),PUBND(2)
c      write(*,*) '   WLBND: ',WLBND(1),WLBND(2)
c      write(*,*) '   WUBND: ',WUBND(1),WUBND(2)
c      write(*,*) '   '


*  If all bounds are now specified using pixel indices, then we can pass
*  on to cut the required section from the NDF. If some bounds were
*  specified using WCS coordinates we need to find the overlap, in pixel
*  indices, between the WCS box and the pixel box.
      IF( .NOT. ALLPIX .AND. STATUS .EQ. SAI__OK ) THEN

*  Indicate that we have not yet found any pixel index bounds
         ALLWCS = .TRUE.
   
*  Ensure the WCS box is complete by replacing any AST__BAD values by the
*  appropriate limit that encompasses the whole pixel box. Check each WCS 
*  axis.
         DO I = 1, NWCS

*  Pass on to the next axis if this WCS axis already has upper and lower
*  bounds.
            IF( WLBND( I ) .EQ. AST__BAD .OR. 
     :          WUBND( I ) .EQ. AST__BAD ) THEN

*  Indicate that at least one axis was specified by pixel index bounds.
               ALLWCS = .FALSE.

*  Map the pixel box into WCS coords and get the limits of the box on
*  this WCS axis.
               CALL AST_MAPBOX( MAP, PLBND, PUBND, .FALSE., I, V1, V2,
     :                          XL, XU, STATUS )

*  Find the pixel axis that covers the largest range between the
*  positions at which the upper and lower WCS limits were found.
               MAXRNG = -1.0
               JJ = 0
               DO J = 1, NPIX

                  IF( XU( J ) .NE. AST__BAD .AND. 
     :                XL( J ) .NE. AST__BAD ) THEN

                     PRANGE = ABS( XU( J ) - XL( J ) )
                     IF( PRANGE .GT. MAXRNG ) THEN
                        JJ = J
                        MAXRNG = PRANGE
                     END IF

                  END IF
               END DO

*  Whether a WCS value is a "lower" or "upper" bound is determined not by
*  the WCS values themselves but by which one gives the lower or upper 
*  value on the corresponding pixel axis. Use this criterion to fill in
*  values for which ever WCS bound has not been supplied.
               IF( WLBND( I ) .EQ. AST__BAD .AND. JJ .GT. 0 ) THEN
                  IF( XL( JJ ) .LT. XU( JJ ) ) THEN
                     WLBND( I ) = V1
                  ELSE 
                     WLBND( I ) = V2
                  END IF
               END IF

               IF( WUBND( I ) .EQ. AST__BAD  .AND. JJ .GT. 0 ) THEN
                  IF( XL( JJ ) .GT. XU( JJ ) ) THEN
                     WUBND( I ) = V1
                  ELSE 
                     WUBND( I ) = V2
                  END IF
               END IF
            END IF

*  The AST Box class knows nothing about axis normalisation. To avoid
*  problems ensure that the upper and lower axis values are in the same
*  "cycle". This applied particularly to RA values where the lower limit 
*  may have a value of (say) 359 degrees and the upper limit be (say) 2
*  degrees. In this example the following code converts the upper limit
*  to 361 degrees.
            DELTA = AST_AXDISTANCE( CFRM, I, WLBND( I ), WUBND( I ),
     :                              STATUS )
            WUBND( I ) = WLBND( I ) + DELTA

         END DO

*  If any centre/width bounds remain in which the centre is a WCS value 
*  and the width is a pixel value, then we need to convert them into 
*  upper and lower bounds now, since this will not have been done earlier.
         IF( MIXED ) THEN
            ALLPIX = .TRUE.

*  Find the central WCS position.
            DO I = 1, NWCS
               CENWCS( I ) = 0.5*( WLBND( I ) + WUBND( I ) )
            END DO

*  Convert to pixel.
            CALL AST_TRANN( MAP, 1, NWCS, 1, CENWCS, .TRUE., NPIX, 1, 
     :                      CENPIX, STATUS )

*  Rescan the supplied bounds looking for mixed mode centre/value  bounds.
            DO I = 1, NAX

*  If the bounds for this axis are specified by centre and width...
               IF( .NOT. ISBND( I ) ) THEN

*  ...and the width is a PIXEL value, and the centre is a WCS value...
                  IF( .NOT. ISPIX1( I ) ) THEN
                     IF( ISPIX2( I ) ) THEN

*  Re-calculate the PIXEL bounds using the central PIXEL value and the
*  supplied PIXEL width.
                        IF( CENPIX( I ) .NE. AST__BAD ) THEN
                           ISPIX1( I ) = .TRUE.
                           VALUE1( I ) = CENPIX( I )
   
                           DELTA = DBLE( NINT( VALUE2( I ) )/2 )
                           PLBND( I ) = VALUE1( I )  - 1.0D0 - DELTA
                           PUBND( I ) = PLBND( I ) + VALUE2( I )

                        ELSE IF( STATUS .EQ. SAI__OK ) THEN
                           STATUS = NDF__BNDIN
                           CALL ERR_REP( 'NDF1_WPLIM_NOV', 'The WCS '//
     :                   'coordinates at the centre of the requested'//
     :                   ' section are invalid.', STATUS )

                        END IF
                     ELSE 
                        ALLPIX = .FALSE.
                     END IF

                  END IF

               ELSE IF( .NOT. ISPIX1( I ) .OR. .NOT. ISPIX2( I ) ) THEN
                  ALLPIX = .FALSE.
               END IF

            END DO
         END IF

c      write(*,*) 'D:'
c      write(*,*) '   PLBND: ',PLBND(1),PLBND(2)
c      write(*,*) '   PUBND: ',PUBND(1),PUBND(2)
c      write(*,*) '   WLBND: ',WLBND(1),WLBND(2)
c      write(*,*) '   WUBND: ',WUBND(1),WUBND(2)
c      write(*,*) '   '

*  If we still need to find the overlap of the WCS and PIXEL boxes, do it
*  now.
         IF( .NOT. ALLPIX ) THEN

*  Report an error if no axes had an inverse transformation.
            IF( MAP1 .EQ. AST__NULL ) THEN
               IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = NDF__BNDIN
                  CALL ERR_REP( 'NDF1_WPLIM_NOI', 'The transformation'//
     :                          ' from WCS to pixel coordinates is '//
     :                          'undefined.', STATUS )
               END IF
               GO TO 999
            END IF

*  Define an AST Box within the subset of WCS axes that have an inverse
*  transformation, using the bounds stored in WLBND/WUBND. we will use the 
*  inverse transformation below to convert the box from WCS to pixel coords.
            WNAX1 = AST_GETI( MAP1, 'Nin', STATUS )
            DO I = 1, WNAX1
               WLBND1( I ) = WLBND( WPERM( I ) )
               WUBND1( I ) = WUBND( WPERM( I ) )
            END DO
            CFRM1 = AST_PICKAXES( CFRM, WNAX1, WPERM, JUNK, STATUS )
            WBOX = AST_BOX( CFRM1, 1, WLBND1, WUBND1, AST__NULL, ' ', 
     :                      STATUS )

*  Map this region into the PIXEL Frame (we know that this will work
*  since we have selected the axes that have the required inverse
*  transformation). The resulting Region will (in general) be a rotated 
*  box with curvi-linear edges.
            PNAX1 = AST_GETI( MAP1, 'Nout', STATUS )
            PFRM = AST_FRAME( PNAX1, 'Domain=PIXEL', STATUS )
            WBOXP = AST_MAPREGION( WBOX, MAP1, PFRM, STATUS )

*  If there are any WCS axes that do not have an inverse transformation,
*  then create an Interval describing the entire extent of the
*  corresponding pixel axes. Use this Interval to extrude the WBOX1P
*  region along the extra pixel axes to form a Prism.
            IF( MAP2 .NE. AST__NULL ) THEN
               PNAX2 = AST_GETI( MAP2, 'Nout', STATUS )

               DO I = 1, NPIX
                  J = PPERM( I ) - PNAX1
                  IF( J .GT. 0 ) THEN
                     PLBND2( J ) = DLBNDD( I )
                     PUBND2( J ) = DUBNDD( I )
                  END IF
               END DO

               PFRM = AST_FRAME( PNAX2, 'Domain=PIXEL', STATUS )
               INTERV = AST_INTERVAL( PFRM, PLBND2, PUBND2, AST__NULL, 
     :                                ' ', STATUS )
               WBOXP = AST_PRISM( WBOXP, INTERV, ' ', STATUS )

            END IF

*  Re-arrange the axes so that they correspond to the original order of pixel 
*  axes.
            CALL AST_PERMAXES( WBOXP, PPERM, STATUS )

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
               PFRM = AST_FRAME( NAX, 'Domain=PIXEL', STATUS )
               PBOX = AST_INTERVAL( PFRM, PLBND, PUBND, AST__NULL, ' ', 
     :                              STATUS )

*  Now form a compound region that is the intersection of the two aboves
*  Boxes (both now defined in the PIXEL Frame).
               CMPREG = AST_CMPREGION( PBOX, WBOXP, AST__AND, ' ', 
     :                                 STATUS )

*  Find the bounds (in PIXEL coords) of the compound Region, and store in
*  PLBND/PUBND. 
               CALL AST_GETREGIONBOUNDS( CMPREG, PLBND, PUBND, STATUS )

*  Report an error if the pixel and WCS boxes do not overlap.
               DO I = 1, NAX
                  IF ( PLBND( I ) .GT. PUBND( I ) .AND.
     :                 STATUS .EQ. SAI__OK ) THEN
                     STATUS = NDF__BNDIN
                     CALL ERR_REP( 'NDF1_WPLIM_NOV', 'The requested '//
     :                           'section does not contain any pixels.',
     :                           STATUS )
                  END IF
               END DO
            END IF
         END IF
      END IF

*  Convert the pixel coordinate box to pixel indices.
      IF( STATUS .EQ. SAI__OK ) THEN
         DO I = 1, NAX
            IF ( PLBND( I ) .GT. PUBND( I ) ) THEN
               TEMP = PLBND( I )
               PLBND( I ) = PUBND( I )
               PUBND( I ) = TEMP
            END IF
            UBND( I ) = NINT( PUBND( I ) ) 

*  If the section width was specified in pixels, ensure that the section
*  has the requested width (rounding problems can otherwise cause this not
*  to be the case).
            IF( ISPIX2( I ) .AND. .NOT. ISBND( I ) ) THEN
               LBND( I ) = UBND( I ) - VALUE2( I ) + 1
            ELSE
               LBND( I ) = UBND( I ) - NINT( PUBND( I ) - PLBND( I ) ) 
     :                     + 1 
            END IF

         END DO
      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  End the AST context. 
      CALL AST_END( STATUS )

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_WPLIM', STATUS )

      END
