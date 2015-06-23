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
*     supplied pair of Q and U images, by setting the reference
*     direction to a specified angle. It is assumed that the supplied
*     Q and U images are aligned in pixel coordinates, and have the
*     same reference direction.

*  Usage:
*     polrotref qin uin qout uout like angle

*  ADAM Parameters:
*     ANGLE = _REAL (Read)
*        The angle, in degrees, from north to the required new reference
*        direction, measured positive in the same sense as rotation from
*        north to east. It must lie between -360 and 360 degrees. Only
*        accessed if parameter LIKE is set to null.
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
*     - The reference direction in the output NDFs and in "LIKE" (if
*     supplied) is defined within the PIXEL coordinate system, and is
*     parallel to the projection of the first axis of the POLANAL Frame
*     at the centre of the map.
*     - The reference direction in the input NDFs is defined within the
*     POLANAL Frame itself, and is parallel to the first axis of the
*     POLANAL Frame. For instance, if the first axis of the POLANAL Frame
*     in the input Q or U image corresponds to the RA axis within an
*     (RA,Dec) Frame (i.e. the POLANAL Frame is connected to the SKY
*     Frame using a UnitMap), then the reference direction is parallel to
*     RA at every point in the map.  If the map is close to a pole, this
*     means that the reference direction will vary across the map as it
*     rotates round the pole.

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     Copyright (C) 2015 East Asian Observatory.
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
*     22-JUN-2015 (DSB):
*        - Changed ANGLE from a rotation angle to a position angle.
*        - Take account of variations iun the direction of north across
*        the map.
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
      DOUBLE PRECISION ANGLE     ! New position angle for POLANAL
      INTEGER DIMS( NDF__MXDIM)  ! Lengths of significant dimensions
      INTEGER EL                 ! Number of elements mapped
      INTEGER INDFL              ! Template NDF identifier
      INTEGER INDFQI             ! Input Q NDF identifier
      INTEGER INDFQO             ! Output Q NDF identifier
      INTEGER INDFUI             ! Input U NDF identifier
      INTEGER INDFUO             ! Output U NDF identifier
      INTEGER IPQIN              ! Pointer to mapped input Q array
      INTEGER IPQOUT             ! Pointer to mapped output Q array
      INTEGER IPUIN              ! Pointer to mapped input U array
      INTEGER IPUOUT             ! Pointer to mapped output U array
      INTEGER IWCS               ! WCS FrameSet for input Q and U NDFs
      INTEGER IWCSL              ! WCS FrameSet for template NDF
      INTEGER NTHAX              ! Zero-based index of north axis
      INTEGER SDIM( NDF__MXDIM)  ! Indices of significant dimensions
      INTEGER SDIML( NDF__MXDIM)  ! Indices of significant dimensions
      INTEGER SLBND( NDF__MXDIM )! Lower pixel bounbds on significat anxes
      INTEGER SLBNDL( NDF__MXDIM )! Lower pixel bounbds on significat anxes
      INTEGER SUBND( NDF__MXDIM )! Upper pixel bounbds on significat anxes
      INTEGER SUBNDL( NDF__MXDIM )! Upper pixel bounbds on significat anxes
      LOGICAL QVAR               ! Q NDF has variance?
      LOGICAL THERE              ! Does component exist?
      LOGICAL UVAR               ! U NDF has variance?
      REAL RANGLE                ! REAL angle value
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

*  Get the lengths of the significant pixel axes.
      DIMS( 1 ) = SUBND( 1 ) - SLBND( 1 ) + 1
      DIMS( 2 ) = SUBND( 2 ) - SLBND( 2 ) + 1

*  If an NDF is specified via parameter LIKE, get it, and determine the
*  position angle of the new reference direction.
      IF( STATUS .NE. SAI__OK ) GO TO 999
      CALL LPG_ASSOC( 'LIKE', 'READ', INDFL, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Get the WCS FrameSet from the template NDF.
         CALL KPG1_ASGET( INDFL, 2, .FALSE., .TRUE., .TRUE., SDIML,
     :                    SLBNDL, SUBNDL, IWCSL, STATUS )

*  Get the anti-clockwise angle from the GRID X axis to the reference
*  direction in the template, in degrees.
         CALL POL1_GTANG( INDFL, 0, IWCSL, RANGLE, STATUS )

*  Modify this angle to that it refers to the GRID coordinate system of
*  the supplied Q image.
         CALL POL1_TRANG( IWCS, IWCSL, SLBND, SUBND, RANGLE, STATUS )

*  Convert to double precision radians.
         IF( RANGLE .NE. VAL__BADD ) THEN
            ANGLE = AST__DD2R*RANGLE
         ELSE
            ANGLE = AST__BAD
         END IF

*  Otherwise, annul the error and get the angle via parameter ANGLE.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Get the position angle of the new reference direction.
         CALL PAR_GDR0R( 'ANGLE', 0.0, -360.0 + VAL__SMLR,
     :                   360.0 - VAL__SMLR, .FALSE., RANGLE, STATUS )

*  If a null value was supplied, use zero (i.e. put the reference
*  direction parallel to north).
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            RANGLE = 0.0
         END IF

*  Convert to double precision radians.
         ANGLE = AST__DD2R*RANGLE

*  Get the zero-based index of the latitude (north) axis in the current
*  Frame of the FrameSet. If it is not a SkyFrame, annull the error and
*  assume that north is the second axis (i.e. zero-based axis index 1).
         IF( STATUS .EQ. SAI__OK ) THEN
            NTHAX = AST_GETI( IWCS, 'LatAxis', STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               NTHAX = NTHAX - 1
            ELSE
               CALL ERR_ANNUL( STATUS )
               NTHAX = 1
            END IF
         END IF

*  Convert the position angle into an angle from the GRID X axis within
*  the output Q WCS.
         CALL POL1_PA2GR( IWCS, NTHAX, 0.5D0*(SLBND(1)+SLBND(2)),
     :                    0.5D0*(SUBND(1)+SUBND(2)), ANGLE,
     :                    STATUS )
      END IF

*  Check the angle is OK.
      IF ( ANGLE .EQ. AST__BAD .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLROTREF_ERR1', 'Required reference '//
     :                 'direction is undefined.', STATUS )
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
      CALL POL1_ROTQU( DIMS(2), DIMS(1), IWCS, ANGLE, .FALSE.,
     :                 %VAL( CNF_PVAL( IPQIN ) ),
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
         CALL POL1_ROTQU( DIMS(2), DIMS(1), IWCS, ANGLE, .TRUE.,
     :                    %VAL( CNF_PVAL( IPQIN ) ),
     :                    %VAL( CNF_PVAL( IPUIN ) ),
     :                    %VAL( CNF_PVAL( IPQOUT ) ),
     :                    %VAL( CNF_PVAL( IPUOUT ) ), STATUS )
         CALL NDF_UNMAP( INDFQI, '*', STATUS )
         CALL NDF_UNMAP( INDFUI, '*', STATUS )
         CALL NDF_UNMAP( INDFQO, '*', STATUS )
         CALL NDF_UNMAP( INDFUO, '*', STATUS )
      END IF

*  Create a new POLANAL Frame describing the new ANGROT value.
      CALL POL1_PTANG( REAL( AST__DR2D*ANGLE ), IWCS, STATUS )

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
