      SUBROUTINE KPS1_LOOK2( MXLO, MXHI, MYLO, MYHI, MASK, NOVAL, XLO,
     :                       XHI, YLO, YHI, IN, OUT, STATUS )
*+
*  Name:
*     KPS1_LOOK2

*  Purpose:
*     Copies a 2D masked region of an NDF into an output array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LOOK2( MXLO, MXHI, MYLO, MYHI, MASK, NOVAL, XLO, XHI, YLO,
*                      YHI, IN, OUT, STATUS )

*  Description:
*     This routine copies the input array to the output array,
*     substituting the value NOVAL for each pixel which has a zero
*     value in the supplied mask array.

*  Arguments:
*     MXLO = INTEGER (Given)
*        The lower X pixel index bound of the mask array.
*     MXHI = INTEGER (Given)
*        The upper X pixel index bound of the mask array.
*     MYLO = INTEGER (Given)
*        The lower Y pixel index bound of the mask array.
*     MYHI = INTEGER (Given)
*        The upper Y pixel index bound of the mask array.
*     MASK( MXLO:MXHI, MYLO:MYHI ) = INTEGER (Given)
*        The mask.
*     NOVAL = DOUBLE PRECISION (Given)
*        The value to place in OUT for each pixel which has a zero value
*        in MASK.
*     XLO = INTEGER (Given)
*        The lower X pixel index bound of the in and out arrays.
*     XHI = INTEGER (Given)
*        The upper X pixel index bound of the in and out arrays.
*     YLO = INTEGER (Given)
*        The lower Y pixel index bound of the in and out arrays.
*     YHI = INTEGER (Given)
*        The upper Y pixel index bound of the in and out arrays.
*     IN( XLO:XHI, YLO:YHI ) = DOUBLE PRECISION (Given)
*        The input array.
*     OUT( XLO:XHI, YLO:YHI ) = DOUBLE PRECISION (Returned)
*        The output array.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-OCT-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER MXLO
      INTEGER MXHI
      INTEGER MYLO
      INTEGER MYHI
      INTEGER MASK( MXLO:MXHI, MYLO:MYHI )
      DOUBLE PRECISION NOVAL
      INTEGER XLO
      INTEGER XHI
      INTEGER YLO
      INTEGER YHI
      DOUBLE PRECISION IN( XLO:XHI, YLO:YHI )

*  Arguments Returned:
      DOUBLE PRECISION OUT( XLO:XHI, YLO:YHI )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count
      INTEGER IHI                ! High bound of X pixel index in overlap
      INTEGER ILO                ! Low bound of X pixel index in overlap
      INTEGER J                  ! Loop count
      INTEGER JHI                ! High bound of Y pixel index in overlap
      INTEGER JLO                ! Low bound of Y pixel index in overlap
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the bounds of the overlap of the mask and the in/out images.
      JLO = MAX( MYLO, YLO )
      JHI = MIN( MYHI, YHI )
      ILO = MAX( MXLO, XLO )
      IHI = MIN( MXHI, XHI )

*  Fill any edge areas in the output image outside the overlap with NOVAL.
      DO J = YLO, JLO
         DO I = XLO, XHI
            OUT( I, J ) = NOVAL
         END DO
      END DO

      DO J = JHI, YHI
         DO I = XLO, XHI
            OUT( I, J ) = NOVAL
         END DO
      END DO

      DO J = YLO, YHI
         DO I = XLO, ILO
            OUT( I, J ) = NOVAL
         END DO
      END DO

      DO J = YLO, YHI
         DO I = IHI, XHI
            OUT( I, J ) = NOVAL
         END DO
      END DO

*  Now copy the overlap area from the input to the output, replacing masked
*  pixels by NOVAL.
      DO J = JLO, JHI
         DO I = ILO, IHI
            IF( MASK( I, J ) .GT. 0 ) THEN
               OUT( I, J ) = IN( I, J )
            ELSE
               OUT( I, J ) = NOVAL
            END IF
         END DO
      END DO

      END
