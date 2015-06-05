      SUBROUTINE POLROTREF ( STATUS )
*+
*  Name:
*     POLROTREF

*  Purpose:
*     Rotate the reference direction in a pair of Q and U images.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POLROTREF( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new pair of Q and U images from a
*     supplied pair of Q and U images, by rotating the reference
*     direction by a specified angle. It is assumed that the supplied
*     Q and U images are aligned in pixel coordinates, and have the
*     same reference direction.

*  Usage:
*     polrotref qin uin qout uout like angle

*  ADAM Parameters:
*     ANGLE = _REAL (Read)
*        Number of clockwise degrees by which the reference direction is
*        to be rotated. It must lie between -360 and 360 degrees. Only
*        accessed if parameter LIKE is set to null (!). The suggested
*        default is the current value.  If a null (!) value is supplied,
*        then the direction of north at the centre of the field is used
*        as the new reference direction. If the current co-ordinate Frame
*        in the input NDF is not a celestial co-ordinate frame, then the
*        second axis of the current Frame is used as the new reference
*        direction.
*     LIKE = NDF (Read)
*        A 2D Q or U NDF that defines the new reference direction. The
*        supplied NDF should have a Frame with Domain "POLANAL" in its
*        WCS component. The supplied Q and U images are modified so that
*        they use the same reference direction as the supplied NDF. If
*        null (!) is supplied, the rotation is defined by parametrer
*        ANGLE. [!]
*     QIN = NDF (Read)
*        The 2D input Q image. The WCS component of this NDF must contain
*        a POLANAL Frame.
*     QOUT = NDF (Write)
*        The 2D output Q image.
*     UIN = NDF (Read)
*        The 2D input U image. The WCS component of this NDF must contain
*        a POLANAL Frame.
*     UOUT = NDF (Write)
*        The 2D output U image.

*  Notes:
*     - It is assumed that the supplied Q and U images are aligned in
*     pixel coordinates, and have the same reference direction.
*     - The supplied Q and U arrays are mapped as double precision values.
*     - Variance arrays are rotated in the same was as Data arrays.
*     - Quality arrays are copied unchanged from input to output.

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
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
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-DEC-2012 (DSB):
*        Original version.
*     7-JAN-2013 (DSB):
*        Fix degs/rads conversion bug that caused incorrect POLANAL
*        orientation in the output NDFs.
*     12-MAR-2013 (DSB):
*        Correct rotation of Variance values.
*     5-JUN-2015 (DSB):
*        Ensure the output NDFs have exactly two pixel axes.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION A( 2 )    ! First point
      DOUBLE PRECISION B( 2 )    ! Second point
      DOUBLE PRECISION C( 2 )    ! Third point
      DOUBLE PRECISION DANGLE    ! Clockwise radians rotation
      DOUBLE PRECISION GXC       ! GRID X at centre of image
      DOUBLE PRECISION GYC       ! GRID Y at centre of image
      DOUBLE PRECISION XP( 2 )   ! Axis-1 values
      DOUBLE PRECISION YP( 2 )   ! Axis-2 values
      INTEGER EL                 ! Number of elements mapped
      INTEGER FRM2D              ! WCS rotation plane
      INTEGER IAXIS              ! Index of latitude axis
      INTEGER INDFQI             ! Input Q NDF identifier
      INTEGER INDFQO             ! Output Q NDF identifier
      INTEGER INDFUI             ! Input U NDF identifier
      INTEGER INDFUO             ! Output U NDF identifier
      INTEGER INDFL              ! Template NDF identifier
      INTEGER IPQIN              ! Pointer to mapped input Q array
      INTEGER IPQOUT             ! Pointer to mapped output Q array
      INTEGER IPUIN              ! Pointer to mapped input U array
      INTEGER IPUOUT             ! Pointer to mapped output U array
      INTEGER IWCS               ! WCS FrameSet for input Q and U NDFs
      INTEGER IWCSL              ! WCS FrameSet for template NDF
      INTEGER MAP2D              ! Mapping from 2D GRID to 2D WCS
      INTEGER SDIM( NDF__MXDIM)  ! Indices of significant dimensions
      INTEGER SLBND( NDF__MXDIM )! Lower pixel bounbds on significat anxes
      INTEGER SUBND( NDF__MXDIM )! Upper pixel bounbds on significat anxes
      INTEGER SDIML( NDF__MXDIM)  ! Indices of significant dimensions
      INTEGER SLBNDL( NDF__MXDIM )! Lower pixel bounbds on significat anxes
      INTEGER SUBNDL( NDF__MXDIM )! Upper pixel bounbds on significat anxes
      LOGICAL QVAR               ! Q NDF has variance?
      LOGICAL THERE              ! Does component exist?
      LOGICAL UVAR               ! U NDF has variance?
      REAL ANGLE                 ! Clockwise radians rotation
      REAL ANGROT                ! Orientation of reference direction
      REAL ANGROTL               ! Orientation of reference direction

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the identifier of the input Q and U NDFs.
      CALL LPG_ASSOC( 'QIN', 'READ', INDFQI, STATUS )
      CALL LPG_ASSOC( 'UIN', 'READ', INDFUI, STATUS )

*  Get matching sections from these NDFs.
      CALL NDF_MBND( 'TRIM', INDFQI, INDFUI, STATUS )

*  Get an AST pointer to a FrameSet describing the co-ordinate Frames
*  present in the q NDF's WCS component.  Always ensure that the Base,
*  PIXEL and Current frames all have two dimensions. The NDF must
*  have no more than two significant pixel axes (i.e. pixel axes
*  spanning more than one pixel).
      CALL KPG1_ASGET( INDFQI, 2, .FALSE., .TRUE., .TRUE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Calculate the original ANGROT value, in degrees.
      CALL POL1_GTANG( INDFQI, 0, IWCS, ANGROT, STATUS )

*  If an NDF is specified via parameter LIKE, get it, and determine the
*  angles through which the reference direction is to be rotated.
      IF( STATUS .NE. SAI__OK ) GO TO 999
      CALL LPG_ASSOC( 'LIKE', 'READ', INDFL, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Get the WCS FrameSet from the template NDF.
         CALL KPG1_ASGET( INDFL, 2, .FALSE., .TRUE., .TRUE., SDIML,
     :                    SLBNDL, SUBNDL, IWCSL, STATUS )

*  Get the anti-clockwise angle from the GRID X axis to the reference
*  direction in the template, in degrees.
         CALL POL1_GTANG( INDFL, 0, IWCSL, ANGLE, STATUS )

*  Modify this angle to that it refers to the GRID coordinate system of
*  the supplied Q image.
         CALL POL1_TRANG( IWCS, IWCSL, SLBND, SUBND, ANGLE, STATUS )

*  Find the clockwise rotation angle from the original Q reference
*  direction to the reference direction of the template, in degrees.
         IF( ANGLE .NE. VAL__BADR ) THEN
            ANGLE = ANGROT - ANGLE
         ELSE
            ANGLE = VAL__BADR
         END IF

*  Otherwise, annul the error and get the angle via parameter ANGLE.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Get the number of clockwise degrees rotation to be applied
         CALL PAR_GDR0R( 'ANGLE', 90.0, -360.0 + VAL__SMLR,
     :                   360.0 - VAL__SMLR, .FALSE., ANGLE, STATUS )

*  If a null value was supplied, annull the error and find the angle
*  between the second significant pixel axis and north.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  Get the Mapping from GRID coords to WCS coords.
            MAP2D = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT,
     :                              STATUS )

*  Get the current WCS Frame.
            FRM2D = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  If the 2D WCS Frame is a celestial co-ord Frame, get the index
*  of the latitude axis.  Otherwise, use the second axis.
            IF ( AST_ISASKYFRAME( FRM2D, STATUS ) ) THEN
               IAXIS = AST_GETI( FRM2D, 'LATAXIS', STATUS )
            ELSE
               IAXIS = 2
            END IF

*  GRID coords at centre of rotation plane.
            GXC = 0.5D0*( 1.0D0 + SUBND( 1 ) - SLBND( 1 ) + 1 )
            GYC = 0.5D0*( 1.0D0 + SUBND( 2 ) - SLBND( 2 ) + 1 )

*  Transform two points on the second GRID axis into the current Frame.
            XP( 1 ) = GXC
            YP( 1 ) = GYC
            XP( 2 ) = GXC
            YP( 2 ) = GYC + 1.0D0
            CALL AST_TRAN2( MAP2D, 2, XP, YP, .TRUE., XP, YP, STATUS )

*  Find another point (C) which is to the north of point 1 (A). The
*  arc-distance from C to A is equal to the arc-distance form B to A.
            A( 1 ) = XP( 1 )
            A( 2 ) = YP( 1 )
            B( 1 ) = XP( 2 )
            B( 2 ) = YP( 2 )
            C( IAXIS ) = AST_AXOFFSET( FRM2D, IAXIS, A( IAXIS ),
     :                              AST_DISTANCE( FRM2D, A, B, STATUS ),
     :                              STATUS )
            C( 3 - IAXIS ) = A( 3 - IAXIS )

*  Convert A and C back into GRID co-ords.
            XP( 1 ) = A( 1 )
            YP( 1 ) = A( 2 )
            XP( 2 ) = C( 1 )
            YP( 2 ) = C( 2 )
            CALL AST_TRAN2( MAP2D, 2, XP, YP, .FALSE., XP, YP, STATUS )

*  Find the angle between the line joining these transformed points in
*  the GRID Frame, and the second GRID axis.
            A( 1 ) = XP( 1 )
            A( 2 ) = YP( 1 )
            B( 1 ) = XP( 2 )
            B( 2 ) = YP( 2 )

            DANGLE = AST_AXANGLE( AST_FRAME( 2, 'Domain=GRID', STATUS ),
     :                            A, B, 2, STATUS )
            IF( DANGLE .EQ. AST__BAD ) THEN
               ANGLE = VAL__BADR
            ELSE
               ANGLE = DANGLE*AST__DR2D
            END IF

         END IF
      END IF

*  Check the angle is OK.  If so, report it. Otherwise report an error.
      IF ( ANGLE .NE. VAL__BADR ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETR( 'A', REAL( ANGLE ) )
         CALL MSG_OUT( 'POLROTREF_MSG1', '  Rotating by ^A degrees',
     :                 STATUS )
         CALL MSG_BLANK( STATUS )
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLROTREF_ERR1', 'Rotation angle is '//
     :                 'undefined.', STATUS )
      END IF

*  Propagate the two input NDFs to form the output NDFs.
      CALL LPG_PROP( INDFQI, 'Title,Label,Units', 'QOUT', INDFQO,
     :               STATUS )
      CALL LPG_PROP( INDFUI, 'Title,Label,Units', 'UOUT', INDFUO,
     :               STATUS )

*  Set their shapes to exclude any insignificant axes
      CALL NDF_SBND( 2, SLBND, SUBND, INDFQO, STATUS )
      CALL NDF_SBND( 2, SLBND, SUBND, INDFUO, STATUS )

*  Map the data array of the two input and two output NDFs.
      CALL NDF_MAP( INDFQI, 'Data', '_DOUBLE', 'Read', IPQIN, EL,
     :              STATUS )
      CALL NDF_MAP( INDFUI, 'Data', '_DOUBLE', 'Read', IPUIN, EL,
     :              STATUS )
      CALL NDF_MAP( INDFQO, 'Data', '_DOUBLE', 'Write', IPQOUT, EL,
     :              STATUS )
      CALL NDF_MAP( INDFUO, 'Data', '_DOUBLE', 'Write', IPUOUT, EL,
     :              STATUS )

*  Produce the rotated Q and U values.
      CALL POL1_ROTQU( EL, ANGLE, .FALSE., %VAL( CNF_PVAL( IPQIN ) ),
     :                 %VAL( CNF_PVAL( IPUIN ) ),
     :                 %VAL( CNF_PVAL( IPQOUT ) ),
     :                 %VAL( CNF_PVAL( IPUOUT ) ), STATUS )

*  Unmap the DATA arrays.
      CALL NDF_UNMAP( INDFQI, '*', STATUS )
      CALL NDF_UNMAP( INDFUI, '*', STATUS )
      CALL NDF_UNMAP( INDFQO, '*', STATUS )
      CALL NDF_UNMAP( INDFUO, '*', STATUS )

*  If both input NDFs have a Variance array, rotate them.
      CALL NDF_STATE( INDFQI, 'Variance', QVAR, STATUS )
      CALL NDF_STATE( INDFUI, 'Variance', UVAR, STATUS )
      IF( QVAR .AND. UVAR ) THEN
         CALL NDF_MAP( INDFQI, 'Variance', '_DOUBLE', 'Read', IPQIN, EL,
     :                 STATUS )
         CALL NDF_MAP( INDFUI, 'Variance', '_DOUBLE', 'Read', IPUIN, EL,
     :                 STATUS )
         CALL NDF_MAP( INDFQO, 'Variance', '_DOUBLE', 'Write', IPQOUT,
     :                 EL, STATUS )
         CALL NDF_MAP( INDFUO, 'Variance', '_DOUBLE', 'Write', IPUOUT,
     :                 EL, STATUS )
         CALL POL1_ROTQU( EL, ANGLE, .TRUE., %VAL( CNF_PVAL( IPQIN ) ),
     :                    %VAL( CNF_PVAL( IPUIN ) ),
     :                    %VAL( CNF_PVAL( IPQOUT ) ),
     :                    %VAL( CNF_PVAL( IPUOUT ) ), STATUS )
         CALL NDF_UNMAP( INDFQI, '*', STATUS )
         CALL NDF_UNMAP( INDFUI, '*', STATUS )
         CALL NDF_UNMAP( INDFQO, '*', STATUS )
         CALL NDF_UNMAP( INDFUO, '*', STATUS )
      END IF

*  Create a new POLANAL Frame describing the new ANGROT value.
      CALL POL1_PTANG( REAL( ANGROT - ANGLE ), IWCS, STATUS )

*  If the Q input NDF has a Quality array, copy it to the output.
      CALL NDF_STATE( INDFQI, 'Quality', THERE, STATUS )
      IF( THERE ) THEN
         CALL NDF_MAP( INDFQI, 'Quality', '_UBYTE', 'Read', IPQIN, EL,
     :                 STATUS )
         CALL NDF_MAP( INDFQO, 'Quality', '_UBYTE', 'Write', IPQOUT, EL,
     :                 STATUS )
         CALL KPG1_COPY( '_UBYTE', EL, IPQIN, IPQOUT, STATUS )
         CALL NDF_UNMAP( INDFQI, 'Quality', STATUS )
      END IF

*  If the U input NDF has a Quality array, copy it to the output.
      CALL NDF_STATE( INDFUI, 'Quality', THERE, STATUS )
      IF( THERE ) THEN
         CALL NDF_MAP( INDFUI, 'Quality', '_UBYTE', 'Read', IPUIN, EL,
     :                 STATUS )
         CALL NDF_MAP( INDFUO, 'Quality', '_UBYTE', 'Write', IPUOUT, EL,
     :                 STATUS )
         CALL KPG1_COPY( '_UBYTE', EL, IPUIN, IPUOUT, STATUS )
         CALL NDF_UNMAP( INDFUI, 'Quality', STATUS )
      END IF

* Store the modified WCS FrameSets.
      CALL NDF_PTWCS( IWCS, INDFQO, STATUS )
      CALL NDF_PTWCS( IWCS, INDFUO, STATUS )

*  Tidy up.
  999 CONTINUE

*  Release the NDF resources.
      CALL NDF_END( STATUS )

*  Release the AST resources.
      CALL AST_END( STATUS )

*  ADd a context message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POLROTREF_ERR', 'POLROTREF: Failed to rotate '//
     :                 'the polarimetric reference direction.', STATUS )
      END IF

      END
