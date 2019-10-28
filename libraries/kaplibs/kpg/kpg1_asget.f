      SUBROUTINE KPG1_ASGET( INDF, NDIM, EXACT, TRIM, REQINV, SDIM,
     :                       SLBND, SUBND, IWCS, STATUS )
*+
*  Name:
*     KPG1_ASGET

*  Purpose:
*     Gets an AST FrameSet from the WCS component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASGET( INDF, NDIM, EXACT, TRIM, REQINV, SDIM, SLBND, SUBND,
*                      IWCS, STATUS )

*  Description:
*     This routine determines the axes to be used from an NDF and returns a
*     FrameSet representing the WCS information in the NDF.
*
*     Each axis of the supplied NDF is checked to see if it is significant
*     (i.e. has a size greater than 1).  The index of each significant axis
*     is returned in SDIM, and the bounds of the axis are returned in SLBND
*     and SUBND.  If EXACT is .TRUE., an error is reported if the number of
*     significant axes is not exactly NDIM.  This mode is intended for case
*     where (say) the user has supplied a single plane from a
*     three-dimensional data cube to an application that requires a
*     two-dimensional array.
*
*     If EXACT is .FALSE. an error is only reported if the number of
*     significant dimensions is higher than NDIM.  If there are fewer than
*     NDIM significant dimensions then the insignificant dimensions are
*     used (starting from the lowest) to ensure that the required number
*     of dimensions are returned. This mode is intended for cases where (say)
*     the user supplies a one-dimensional data stream to an application
*     that requires a two-dimensional array.
*
*     The GRID Frame (i.e. the Base Frame) obtained from the NDFs WCS
*     component is modified so that it has NDIM axes corresponding to the
*     axes returned in SDIM (the value 1.0 is used for the other axes).
*
*     Likewise, the PIXEL Frame obtained from the NDFs WCS component is
*     modified so that it has NDIM axes corresponding to the axes returned
*     in SDIM (the lower pixel bound is used for the other axes). The
*     original PIXEL Frame is retained, but with Domain changed to
*     NDF_PIXEL.
*
*     If TRIM is .TRUE., then the Current Frame obtained from the NDFs WCS
*     component is also modified so that it has NDIM axes. If the original
*     Current Frame has more than NDIM axes, then the axes to use are
*     obtained from the environment using parameter USEAXIS. A new Current
*     Frame is then made by picking these axes from the original Current
*     Frame, assigning the value AST__BAD to the axes which have not been
*     chosen.
*
*     If the original Current Frame has fewer than NDIM axes, then simple
*     axes are added into the new Current Frame to make up a total of
*     NDIM. These axes are given the value 1.0.
*
*     Various environment parameters may be used to obtain options, etc. The
*     names of these parameters are hard-wired into this subroutine in
*     order to ensure conformity between applications.

*  Environment Parameters:
*     USEAXIS = LITERAL (Read)
*        A set of NDIM axes to be selected from the Current Frame. Each
*        axis can be specified either by giving its index within the Current
*        Frame in the range 1 to the number of axes in the Frame, or by
*        giving its symbol. This parameter is only accessed if TRIM is
*        .TRUE. and the original Current Frame in the supplied NDF has
*        too many axes. The dynamic default selects the axes with the same
*        indices as the selected NDF axes. The value should be given as a
*        GRP group expression, with default control characters.

*  Arguments:
*     INDF = INTEGER (Given)
*        The identifier for the NDF.
*     NDIM = INTEGER (Given)
*        The number of dimensions required by the application.
*     EXACT = LOGICAL (Given)
*        Must the NDF have exactly NDIM significant axes? Otherwise it is
*        acceptable for the NDIM axes to include some insignificant axes.
*     TRIM = LOGICAL (Given)
*        Should the returned FrameSet be trimmed to ensure that the
*        Current Frame has NDIM axes? Otherwise, the Current Frame read
*        from the NDF is not changed.
*     REQINV = LOGICAL (Given)
*        Is the inverse mapping (from Current Frame to Base Frame)
*        required? If it is, an error is reported if the inverse mapping
*        is not available. REQINV should be supplied .TRUE. in most cases.
*     SDIM( NDIM ) = INTEGER (Returned)
*        The indices of the significant dimensions.
*     SLBND( NDIM ) = INTEGER (Returned)
*        The lower pixel index bounds of the significant dimensions.  These
*        are stored in the same order as the indices in SDIM.
*     SUBND( NDIM ) = INTEGER (Returned)
*        The upper pixel index bounds of the significant dimensions.  These
*        are stored in the same order as the indices in SDIM.
*     IWCS = INTEGER (Returned)
*        An AST pointer to the WCS FrameSet. Returned equal to AST__NULL
*        if an error occurs. The Base Frame is an NDIM-dimensional GRID
*        Domain.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the Current Frame in the returned FrameSet has no Title, then
*     the Title is set to the value of the NDF TITLE component (so long
*     as the NDF TITLE is not blank or undefined).
*     -  The routine KPG1_ASGET8 is equivalent to this function but uses
*     INTEGER*8 for the SLBND and SUBND arguments.

*  Copyright:
*     Copyright (C) 1998, 1999, 2000, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2019 East Asian Observatory
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
*     DSB: David Berry (STARLINK)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     30-JUN-1998 (DSB):
*        Original version.
*     22-JUN-1999 (DSB):
*        Remove call to NDF_SECT which set the pixel bounds fo all
*        insignificant axes to (1:1).
*     15-JAN-2000 (DSB):
*        Do not remove insignificant axes from the current Frame if TRIM
*        indicates that a precise number of axes is required in the
*        current Frame.
*     30-AUG-2004 (DSB):
*        Replaced hardwired "3" by "NDIMS" in call to KPG1_ASSIG!
*     2-DEC-2005 (DSB):
*        Added INPRM to argument list for KPG1_ASTRM.
*     14-FEB-2006 (DSB):
*        Ensure INPRM is initialised even if NDIM is equal to NBAX. Lack
*        of initialisation caused KPG1_ASTRM to crash in the case where
*        NDIM and NBAX are equal.
*     12-SEP-2006 (DSB):
*        If the current Frame is AXIS and the AXIS structures are
*        non-monotonic, reset the current Frame to PIXEL.
*     16-NOV-2006 (PWD):
*        Stop modfication of NDIM given argument in call to NDF_BOUND.
*     19-AUG-2010 (DSB):
*        Add SDIM argument to KPG1_ASNDF.
*     20-JUL-2011 (DSB):
*        When testing axis monotonicity, use the precision appropriate to
*        the AXIS data type.
*     4-OCT-2019 (DSB):
*        Changed to be a thin wrapper round KPG1_ASGET8.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER INDF
      INTEGER NDIM
      LOGICAL EXACT
      LOGICAL TRIM
      LOGICAL REQINV

*  Arguments Returned:
      INTEGER SDIM( NDIM )
      INTEGER SLBND( NDIM )
      INTEGER SUBND( NDIM )
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER*8 SLBND8( NDF__MXDIM )
      INTEGER*8 SUBND8( NDF__MXDIM )
*.

*  Call the 8-byte version of this routine.
      CALL KPG1_ASGET8( INDF, NDIM, EXACT, TRIM, REQINV, SDIM,
     :                  SLBND8, SUBND8, IWCS, STATUS )

*  Copy the INTEGER*8 values into the returned INTEGER arrays, checking
*  for overflow.
      IF( STATUS .EQ. SAI__OK ) THEN
         DO I = 1, NDIM
            SLBND( I ) = SLBND8( I )
            SUBND( I ) = SUBND8( I )
            IF( SLBND( I ) .NE. SLBND8( I ) .OR.
     :          SUBND( I ) .NE. SUBND8( I ) ) THEN
               CALL NDF_MSG( 'N', INDF )
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'KPG1_ASGET: Supplied NDF ''^N'' ' //
     :                       'is too big - use KPG1_ASGET8 instead.',
     :                       STATUS )
               GO TO 999
            END IF
         END DO
      END IF

 999  CONTINUE

      END
