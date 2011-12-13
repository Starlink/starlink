      SUBROUTINE ARD_GTWCS( IGRP, NDIM, IWCS, STATUS )
*+
*  Name:
*     ARD_GTWCS

*  Purpose:
*     Return a FrameSet connecting pixel and user co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD_GTWCS( IGRP, NDIM, IWCS, STATUS )

*  Description:
*     This routine returns an AST pointer for a FrameSet describing the
*     relationship between the user co-ordinate system used by the
*     supplied ARD description, and the pixel coordinate Frame of the
*     FrameSet stored by the most recent call to ARD_WCS.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group holding the ARD description.
*     NDIM = INTEGER (Given)
*        The number of pixl axes in the mask array.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the FrameSet. The base Frame will be the pixel
*        coordinate Frame in the FrameSet supplied via the most recent
*        call to ARD_WCS. The current Frame will be the user co-ordinate
*        system specified by the supplied ARD description.

*  Notes:
*     -  An error is reported if the dimensionality of the ARD
*     description is different to that of the mask array (as specified
*     by argument NDIM).

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-2007 (DSB):
*        Original version.
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

*  Arguments Given:
      INTEGER IGRP
      INTEGER NDIM

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :  DLBND( ARD__MXDIM ),     ! Lower bounds of pixel coords
     :  DUBND( ARD__MXDIM )      ! Upper bounds of pixel coords

      INTEGER                    ! Pointers to...
     :  I,                       ! Loop counter
     :  IPEXPR,                  ! supplied algebraic ARD expression
     :  IPOPND                   ! operand stack

      INTEGER
     :  AWCS,                    ! WCS Frameset supplied by application
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
         CALL ERR_REP( 'ARD_GTWCS_ERR1', 'ARD_GTWCS: Invalid value '//
     :                 '(^NDIM) supplied for argument NDIM '//
     :                 '(programming error?).', STATUS )
         GO TO 999
      END IF

*  Create an AST FrameSet describing the known coordinate Frames. The
*  base Frame of this FrameSet will be pixel coords within the pixel
*  mask, and the current Frame will be "Application co-ordinates" (i.e.
*  the default user coordinate system). This Frame wil have Domain ARDAPP.
*  The FrameSet may also contain other Frames specified using the ARD_WCS
*  routine.
      CALL ARD1_APWCS( NDIM, VAL__BADR, AWCS, STATUS )

*  Get work space to hold the algebraic Boolean expression
*  corresponding to the supplied ARD description, and an array of
*  operands.
      SZEXPR = 50
      CALL PSX_CALLOC( SZEXPR, '_INTEGER', IPEXPR, STATUS )

      SZOPND = 200
      CALL PSX_CALLOC( SZOPND, '_DOUBLE', IPOPND, STATUS )

*  Store arbitrary values for the mask bounds.
      DO I = 1, NDIM
         DLBND( I ) = 0.0
         DUBND( I ) = 1000.0
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

*  Free resources
      CALL PSX_FREE( IPOPND, STATUS )
      CALL PSX_FREE( IPEXPR, STATUS )

*  Ensure the IWCS pointer is not annulled by the following call to
*  AST_END.
      CALL AST_EXPORT( IWCS, STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error has occured, give a context message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARD_GTWCS_ERR1', 'ARD_GTWCS: Unable to get '//
     :                 'the WCS FrameSet from an ARD description.',
     :                 STATUS )
      END IF

      END
