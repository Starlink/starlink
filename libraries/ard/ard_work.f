      SUBROUTINE ARD_WORK( IGRP, NDIM, LBND, UBND, TRCOEF, CONCAT,
     :                     REGVAL, MASK, LBNDI, UBNDI, LBNDE, UBNDE,
     :                     STATUS )
*+
*  Name:
*     ARD_WORK

*  Purpose:
*     Convert an ARD description into a pixel mask

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_WORK( IGRP, NDIM, LBND, UBND, TRCOEF, CONCAT, REGVAL,
*                    MASK, LBNDI, UBNDI, LBNDE, UBNDE, STATUS )

*  Description:
*     This routine returns an array which contains a positive value
*     for all pixels within the areas specified by a given ARD
*     description, and zero for all other pixels.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group holding the ARD description.
*     NDIM = INTEGER (Given)
*        The number of pixl axes in the mask array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the mask array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the mask array.
*     TRCOEF( 0:NDIM, NDIM ) = REAL (Given)
*        The co-efficients of the mapping from application co-ordinates
*        (i.e. default user coordinates) to pixel co-ordinates. If the
*        first element is equal to VAL__BADR, then a unit mapping is used.
*        This argument is ignored if a call to ARD_WCS has already been
*        made to establish WCS Information.
*     CONCAT = LOGICAL (Given)
*        If .TRUE., then an INPUT keyword is inserted at the start of
*        the ARD description so long as the ARD description does not
*        already contain any INPUT keywords. If .FALSE., the ARD
*        description is left as supplied.
*     REGVAL = INTEGER (Given and Returned)
*        A positive integer to use to represent the first keyword in
*        the ARD description (excluding INPUT keywords). An error is
*        reported if the value 1 is supplied. If the supplied value is
*        negative or zero, then the value used is one greater than the
*        maximum pixel value supplied in MASK (except that 2 is used if
*        the maximum mask value is 1 or less). On return, REGVAL holds
*        one more than the largest value used to represent any of the
*        keywords in the ARD description.
*     MASK( * ) = INTEGER (Given and Returned)
*        The mask array. Any negative values in the supplied array are
*        treated as zero.
*     LBNDI( NDIM ) = INTEGER (Returned)
*        The lower pixel bounds of a box which encompasses all internal
*        pixels. If there are no internal pixels in the returned mask,
*        each lower bound is returned greater than the corresponding
*        upper bound.
*     UBNDI( NDIM ) = INTEGER (Returned)
*        The upper pixel bounds of a box which encompasses all internal
*        pixels.
*     LBNDE( NDIM ) = INTEGER (Returned)
*        The lower pixel bounds of a box which encompasses all external
*        pixels. If there are no external pixels in the returned mask,
*        each lower bound is returned greater than the corresponding
*        upper bound.
*     UBNDE( NDIM ) = INTEGER (Returned)
*        The upper pixel bounds of a box which encompasses all external
*        pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  An error is reported if the dimensionality of the ARD
*     description is different to that of the mask array (as specified
*     by argument NDIM).

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
*     28-APR-1994 (DSB):
*        Original version.
*     5-JUN-2001 (DSB):
*        Modified to use AST FrameSets instead of linear coeffs.
*     18-JUL-2001 (DSB):
*        Modified for ARD version 2.0.
*     1-OCT-2007 (DSB):
*        Add IWCS argument to ARD1_ADANL call.
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
      INCLUDE 'ARD_ERR'          ! ARD_ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IGRP
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      REAL TRCOEF( 0:NDIM, NDIM )
      LOGICAL CONCAT

*  Arguments Given and Returned:
      INTEGER REGVAL
      INTEGER MASK( * )

*  Arguments Returned:
      INTEGER LBNDI( NDIM )
      INTEGER UBNDI( NDIM )
      INTEGER LBNDE( NDIM )
      INTEGER UBNDE( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :  DLBND( ARD__MXDIM ),     ! Lower bounds of pixel coords
     :  DUBND( ARD__MXDIM )      ! Upper bounds of pixel coords

      INTEGER                    ! Pointers to...
     :  IPASTK,                  ! argument stack for conversion to r.p.
     :  IPEXP2,                  ! expanded algebraic ARD expression
     :  IPEXPR,                  ! supplied algebraic ARD expression
     :  IPLSTE,                  ! stack of ext. box lower bounds
     :  IPLSTI,                  ! stack of int. box lower bounds
     :  IPMSTK,                  ! stack of intermediate masks
     :  IPOPCO,                  ! reverse polish ARD expression
     :  IPOPND,                  ! operand stack
     :  IPSTK,                   ! op. code stack for conversion to r.p.
     :  IPUSTE,                  ! stack of ext. box upper bounds
     :  IPUSTI                   ! stack of int. box upper bounds

      INTEGER
     :  AWCS,                    ! WCS Frameset supplied by application
     :  IWCS,                    ! pixel->user FrameSet
     :  I,                       ! Loop count
     :  INDEX1,                  ! Index for 1st keyword
     :  MSKSIZ,                  ! No. of pixels in mask
     :  MXSTK,                   ! Max. no. of intermediate masks
     :  OUTSIZ,                  ! Hi limit on expanded expression size
     :  SZEXPR,                  ! Size of supplied algebraic expression
     :  SZOPND                   ! Size of operand stack

      LOGICAL
     :  INP                      ! INPUT keywords in ARD description?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Abort if an illegal number of dimensions has been supplied.
      IF( NDIM .LE. 0 .OR. NDIM .GT. ARD__MXDIM ) THEN
         STATUS = ARD__BADDM
         CALL MSG_SETI( 'NDIM', NDIM )
         CALL ERR_REP( 'ARD_WORK_ERR1', 'ARD_WORK: Invalid value '//
     :                 '(^NDIM) supplied for argument NDIM '//
     :                 '(programming error?).', STATUS )
         GO TO 999
      END IF

*  Abort if an illegal REGVAL value has been supplied.
      IF( REGVAL .EQ. 1 ) THEN
         STATUS = ARD__BADIN
         CALL ERR_REP( 'ARD_WORK_ERR2', 'ARD_WORK: Invalid value '//
     :                 '(1) supplied for argument REGVAL '//
     :                 '(programming error?).', STATUS )
         GO TO 999
      END IF

*  Find the total number of pixels in the mask.
      MSKSIZ = 1
      DO I = 1, NDIM
         MSKSIZ = MSKSIZ*( UBND( I ) - LBND( I ) + 1 )
      END DO

*  If a zero or negative value has been supplied for REGVAL, find the
*  maximum value in the supplied mask and limit it to be at least 2.
*  This value will be used to represent the first keyword in the ARD
*  description (excluding INPUT keywords) .
      IF( REGVAL .LE. 0 ) THEN
         INDEX1 = 2

         DO I = 1, MSKSIZ
            IF( MASK( I ) .GT. INDEX1 ) INDEX1 = MASK( I )
         END DO

*  If a positive value was supplied for REGVAL, use it to represent the
*  first keyword in the ARD description.
      ELSE
         INDEX1 = REGVAL
      END IF

*  Create an AST FrameSet describing the known coordinate Frames. The
*  base Frame of this FrameSet will be pixel coords within the pixel
*  mask, and the current Frame will be "Application co-ordinates" (i.e.
*  the default user coordinate system). This Frame wil have Domain ARDAPP.
*  The FrameSet may also contain other Frames specified using the ARD_WCS
*  routine.
      CALL ARD1_APWCS( NDIM, TRCOEF, AWCS, STATUS )

*  Get work space to hold the algebraic Boolean expression
*  corresponding to the supplied ARD description, and an array of
*  operands.
      SZEXPR = 50
      CALL PSX_CALLOC( SZEXPR, '_INTEGER', IPEXPR, STATUS )

      SZOPND = 200
      CALL PSX_CALLOC( SZOPND, '_DOUBLE', IPOPND, STATUS )

*  Store _DOUBLE versions of the mask pixel bounds.
      DO I = 1, NDIM
         DLBND( I ) = DBLE( LBND( I ) - 1 )
         DUBND( I ) = DBLE( UBND( I ) )
      END DO

*  Create an algebraic Boolean expression in which operators and
*  operands are represented by integer codes by analysing the ARD
*  description into operators, keywords and statement. Also store
*  information within the operand array describing the keywords
*  included in the ARD description. The returned expression corresponds
*  to the ARD description as supplied (i.e. no implicit .OR.s are
*  inserted).
      CALL ARD1_ADANL( IGRP, NDIM, AWCS, DLBND, DUBND, IPEXPR, IPOPND,
     :                 SZEXPR, SZOPND, INP, IWCS, STATUS )

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  It may be necessary to expand the algebraic expression found above
*  by inclusion of any missing .OR. operators, etc. In the worst case
*  the expanded expression will be twice as big as the supplied
*  expression (plus one extra for the INPUT keyword which may be
*  inserted at the start of the expression). Obtain workspace to hold
*  the expanded expression.
      OUTSIZ = 2*SZEXPR + 1
      CALL PSX_CALLOC( OUTSIZ, '_INTEGER', IPEXP2, STATUS )

*  Expand the algebraic expression by inclusion of any missing .OR.
*  operators, etc. Also report an error if the order of operands and
*  operators within the expression is incorrect. Insert an INPUT keyword
*  if there were no INPUT keywords in the supplied ARD description and
*  if the CONCAT argument is .TRUE.
      CALL ARD1_CHECK( ( (.NOT. INP) .AND. CONCAT ), SZEXPR,
     :                 %VAL( CNF_PVAL( IPEXPR ) ), OUTSIZ,
     :                 %VAL( CNF_PVAL( IPEXP2 ) ), STATUS )

*  Get work space needed to convert the algebraic expression to reverse
*  polish form.
      CALL PSX_CALLOC( OUTSIZ + 1, '_INTEGER', IPSTK, STATUS )
      CALL PSX_CALLOC( OUTSIZ + 1, '_INTEGER', IPASTK, STATUS )
      CALL PSX_CALLOC( OUTSIZ, '_INTEGER', IPOPCO, STATUS )

*  Convert the algebraic expression to reverse polish form.
      CALL ARD1_ALTRP( OUTSIZ, %VAL( CNF_PVAL( IPEXP2 ) ),
     :                 %VAL( CNF_PVAL( IPSTK ) ),
     :                 %VAL( CNF_PVAL( IPASTK ) ),
     :                 %VAL( CNF_PVAL( IPOPCO ) ), MXSTK, STATUS )

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain workspace needed to hold intermediate results while evaluating
*  the ARD expression.
      CALL PSX_CALLOC( MSKSIZ*MXSTK, '_INTEGER', IPMSTK, STATUS )
      CALL PSX_CALLOC( NDIM*MXSTK, '_INTEGER', IPLSTE, STATUS )
      CALL PSX_CALLOC( NDIM*MXSTK, '_INTEGER', IPUSTE, STATUS )
      CALL PSX_CALLOC( NDIM*MXSTK, '_INTEGER', IPLSTI, STATUS )
      CALL PSX_CALLOC( NDIM*MXSTK, '_INTEGER', IPUSTI, STATUS )

*  Evaluate the ARD expression.
      CALL ARD1_EVAL( INDEX1, NDIM, LBND, UBND, OUTSIZ, SZOPND, MXSTK,
     :                MSKSIZ, %VAL( CNF_PVAL( IPOPCO )),
     :                %VAL( CNF_PVAL( IPOPND ) ),
     :                %VAL( CNF_PVAL( IPMSTK ) ),
     :                %VAL( CNF_PVAL( IPLSTE ) ),
     :                %VAL( CNF_PVAL( IPUSTE ) ),
     :                %VAL( CNF_PVAL( IPLSTI ) ),
     :                %VAL( CNF_PVAL( IPUSTI ) ), MASK, LBNDE,
     :                UBNDE, LBNDI, UBNDI, REGVAL, STATUS )

*  Check for infinite bounding boxes (these are represented internally
*  by the lower bound of the first axis being set to VAL__MAXI).
*  Return the dimensions of the mask if any are found.
      IF( LBNDI( 1 ) .EQ. VAL__MAXI ) THEN
         DO I = 1, NDIM
            LBNDI( I ) = LBND( I )
            UBNDI( I ) = UBND( I )
         END DO
      END IF

      IF( LBNDE( 1 ) .EQ. VAL__MAXI ) THEN
         DO I = 1, NDIM
            LBNDE( I ) = LBND( I )
            UBNDE( I ) = UBND( I )
         END DO
      END IF

*  Check for null bounding boxes (these are represented internally by
*  the lower bound of the first axis being set to VAL__MINI).  Return
*  lower bounds greater than upper bounds if any are found.
      IF( LBNDI( 1 ) .EQ. VAL__MINI ) THEN
         DO I = 1, NDIM
            LBNDI( I ) = LBND( I )
            UBNDI( I ) = LBND( I ) - 1
         END DO
      END IF

      IF( LBNDE( 1 ) .EQ. VAL__MINI ) THEN
         DO I = 1, NDIM
            LBNDE( I ) = LBND( I )
            UBNDE( I ) = LBND( I ) - 1
         END DO
      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  Attempt to free the work space.
      CALL PSX_FREE( IPMSTK, STATUS )
      CALL PSX_FREE( IPLSTE, STATUS )
      CALL PSX_FREE( IPUSTE, STATUS )
      CALL PSX_FREE( IPLSTI, STATUS )
      CALL PSX_FREE( IPUSTI, STATUS )
      CALL PSX_FREE( IPOPCO, STATUS )
      CALL PSX_FREE( IPASTK, STATUS )
      CALL PSX_FREE( IPSTK, STATUS )
      CALL PSX_FREE( IPEXP2, STATUS )
      CALL PSX_FREE( IPEXPR, STATUS )
      CALL PSX_FREE( IPOPND, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error has occured, give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARD_WORK_ERR3', 'ARD_WORK: Unable to convert '//
     :                 'an ARD description into a pixel mask.', STATUS )
      END IF

      END
