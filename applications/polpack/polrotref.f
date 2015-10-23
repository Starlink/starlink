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
*     supplied pair of Q and U images, by changing the polarimetric
*     reference direction. The required direction can either be inherited
*     from another NDF (see parameter LIKE) or specified as a specified
*     axis within a specified coordinate Frame (see parameters AXIS and
*     FRAME). It is assumed that the supplied Q and U images are aligned
*     in pixel coordinates, and have the same reference direction.

*  Usage:
*     polrotref qin uin qout uout like

*  ADAM Parameters:
*     AXIS = _INTEGER (Read)
*        Parameter AXIS is used only if a null value is supplied for parameter
*        LIKE, in which case AXIS is the index of the axis within the
*        coordinate frame specified by parameter FRAME that is to be used
*        as the reference direction in the output NDFs. The first axis has
*        index 1. [2]
*     FRAME = LITERAL (Read)
*        A string specifying the co-ordinate Frame to which parameter
*        AXIS refers. If a null parameter value is supplied, then the
*        current Frame within the NDF specified by parameter QIN is used.
*        The string can be one of the following:
*
*        - A domain name such as SKY, SPECTRUM, AXIS, PIXEL, etc.
*
*        - An integer value giving the index of the required Frame within
*        the WCS component.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000) (see
*        section "Sky Co-ordinate Systems" in SUN/95). Using an SCS value
*        is equivalent to specifying "SKY" for this parameter and then setting
*        the System attribute (to "FK5", "Galactic", etc.) using KAPPA command
*        WCSATTRIB. ["PIXEL"]
*     LIKE = NDF (Read)
*        A 2D Q or U NDF that defines the new reference direction. The
*        supplied NDF should have a Frame with Domain "POLANAL" in its
*        WCS component. The supplied Q and U images are modified so that
*        they use the same reference direction as the supplied NDF. If
*        null (!) is supplied, the rotation is defined by parametrer
*        AXIS. [!]
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
*     - The reference direction is defined as being constant within the
*     POLANAL Frame. It will not be constant within another Frame if the
*     transformation from POLANAL to that Frame is non-linear.

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
*     26-JUN-2015 (DSB):
*        Added parameter FRAME.
*     29-JUN-2015 (DSB):
*        Replace parameter ANGLE with parameter AXIS.
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
      INTEGER AXIS               ! The FRAME axis to use
      INTEGER DIMS( NDF__MXDIM)  ! Lengths of significant dimensions
      INTEGER EL                 ! Number of elements mapped
      INTEGER ICUR               ! Index of current Frame
      INTEGER IFRM               ! Index of required frame
      INTEGER INDFL              ! Template NDF identifier
      INTEGER INDFQI             ! Input Q NDF identifier
      INTEGER INDFQO             ! Output Q NDF identifier
      INTEGER INDFUI             ! Input U NDF identifier
      INTEGER INDFUO             ! Output U NDF identifier
      INTEGER IPQIN              ! Pointer to mapped input Q array
      INTEGER IPQINV             ! Pointer to mapped input Q variances
      INTEGER IPQOUT             ! Pointer to mapped output Q array
      INTEGER IPQOUTV            ! Pointer to mapped output Q variances
      INTEGER IPUIN              ! Pointer to mapped input U array
      INTEGER IPUINV             ! Pointer to mapped input U variances
      INTEGER IPUOUT             ! Pointer to mapped output U array
      INTEGER IPUOUTV            ! Pointer to mapped output U variances
      INTEGER IWCS               ! WCS FrameSet for input Q and U NDFs
      INTEGER IWCSL              ! WCS FrameSet for template NDF
      INTEGER NAX                ! No. of axes in required frame
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

*  Propagate the two input NDFs to form the output NDFs.
      CALL LPG_PROP( INDFQI, 'Title,Label,Units', 'QOUT', INDFQO,
     :               STATUS )
      CALL LPG_PROP( INDFUI, 'Title,Label,Units', 'UOUT', INDFUO,
     :               STATUS )

*  Set their shapes to exclude any insignificant axes
      CALL NDF_SBND( 2, SLBND, SUBND, INDFQO, STATUS )
      CALL NDF_SBND( 2, SLBND, SUBND, INDFUO, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If an NDF is specified via parameter LIKE, get its WCS FrameSet.
      CALL LPG_ASSOC( 'LIKE', 'READ', INDFL, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL KPG1_ASGET( INDFL, 2, .FALSE., .TRUE., .TRUE., SDIML,
     :                    SLBNDL, SUBNDL, IWCSL, STATUS )

*  The reference direction is axis 1 (i.e. the first axis) of the POLANAL
*  Frame within IWCSL.
         AXIS = 1

*  Otherwise, annul the error and get the required AXIS and FRAME.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         IWCSL = AST__NULL

*  Record the index of the original current Frame.
         ICUR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Set the new Current Frame using parameter FRAME. If "WORLD" co-ordinates
*  are requested, use PIXEL. If "DATA" co-ordinates are requested, use
*  "AXIS".
         CALL NDF_MSG( 'NDF', INDFQI )
         CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IWCS, 'PIXEL', 'AXIS',
     :                    .TRUE., '^NDF', STATUS )

*  Get the index of the Frame, and the number of axes in the Frame, and
*  then restore the original current Frame.
         IFRM = AST_GETI( IWCS, 'CURRENT', STATUS )
         NAX = AST_GETI( IWCS, 'NAXES', STATUS )
         CALL AST_SETI( IWCS, 'CURRENT', ICUR, STATUS )

*  Get the index of the axis to use.
         CALL PAR_GDR0I( 'AXIS', 2, 1, NAX, .TRUE., AXIS, STATUS )

      END IF

*  Map the data array of the two input and two output NDFs.
      CALL NDF_MAP( INDFQI, 'Data', '_DOUBLE', 'Read', IPQIN, EL,
     :              STATUS )
      CALL NDF_MAP( INDFUI, 'Data', '_DOUBLE', 'Read', IPUIN, EL,
     :              STATUS )
      CALL NDF_MAP( INDFQO, 'Data', '_DOUBLE', 'Write', IPQOUT, EL,
     :              STATUS )
      CALL NDF_MAP( INDFUO, 'Data', '_DOUBLE', 'Write', IPUOUT, EL,
     :              STATUS )

*  If both input NDFs have a Variance array, map them, and also the
*  output variance arrays.
      CALL NDF_STATE( INDFQI, 'Variance', QVAR, STATUS )
      CALL NDF_STATE( INDFUI, 'Variance', UVAR, STATUS )
      IF( QVAR .AND. UVAR ) THEN
         CALL NDF_MAP( INDFQI, 'Variance', '_DOUBLE', 'Read', IPQINV,
     :                 EL, STATUS )
         CALL NDF_MAP( INDFUI, 'Variance', '_DOUBLE', 'Read', IPUINV,
     :                 EL, STATUS )
         CALL NDF_MAP( INDFQO, 'Variance', '_DOUBLE', 'Write', IPQOUTV,
     :                 EL, STATUS )
         CALL NDF_MAP( INDFUO, 'Variance', '_DOUBLE', 'Write', IPUOUTV,
     :                 EL, STATUS )
      END IF

*  Rotate everything.
      CALL POL1_ROTRF( DIMS(2), DIMS(1), IWCS, IWCSL, IFRM, AXIS - 1,
     :                 QVAR .AND. UVAR, %VAL( CNF_PVAL( IPQIN ) ),
     :                 %VAL( CNF_PVAL( IPUIN ) ),
     :                 %VAL( CNF_PVAL( IPQOUT ) ),
     :                 %VAL( CNF_PVAL( IPUOUT ) ),
     :                 %VAL( CNF_PVAL( IPQINV ) ),
     :                 %VAL( CNF_PVAL( IPUINV ) ),
     :                 %VAL( CNF_PVAL( IPQOUTV ) ),
     :                 %VAL( CNF_PVAL( IPUOUTV ) ), STATUS )

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
