      SUBROUTINE SMARB0( XLO, XHI, YLO, YHI, DATA, NPNT, X, Y, MXMNSZ,
     :                   MKSIZ, STATUS )
*+
*  Name:
*     SMARB0

*  Purpose:
*     Calculate the size of the marks to be drawn over an image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SMARB0( XLO, XHI, YLO, YHI, DATA, NPNT, X, Y, MXMNSZ,
*                  MKSIZ, STATUS )

*  Description:
*     The size of the marks calculated by this subroutine is
*     proportional to the data value of the image at the mark position.

*  Arguments:
*     XLO, XHI = INTEGER (Given)
*        The lower and upper bounds of the image data array in X
*        dimension.
*     YLO, YHI = INTEGER (Given)
*        The lower and upper bounds of the image data array in Y
*        dimension.
*     DATA( XLO: XHI, YLO: YHI ) = REAL (Given)
*        The image data array.
*     NPNT = INTEGER (Given)
*        The number of points at which a marker to be drawn.
*     X( NPNT ), Y( NPNT ) = DOUBLE PRECISION (Given)
*        The image coordinates of the positions to be marked.
*     MXMNSZ( 2 ) = REAL (Given)
*        The max and min mark size.
*     MKSIZ( NPNT ) = REAL (Returned)
*        The calculated size of all markers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     16-JUN-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants definition

*  Arguments Given:
      INTEGER XLO, XHI
      INTEGER YLO, YHI
      REAL DATA( XLO: XHI, YLO: YHI )
      INTEGER NPNT
      DOUBLE PRECISION X( NPNT ), Y( NPNT )
      REAL MXMNSZ( 2 )

*  Arguments Returned:
      REAL MKSIZ( NPNT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL IMGMAX, IMGMIN        ! Max & min image value of marker point
      REAL IMGVAL                ! interpolated image value
      INTEGER I                  ! Do loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the max. and min. values of the imag at the positions to be
*  marked.
      IMGMAX = VAL__MINR
      IMGMIN = VAL__MAXR
      DO I = 1, NPNT

*  Get the value at the position by bi-linear interpolation.
         CALL IRM_BILN1( REAL( X( I ) ), REAL( Y( I ) ),
     :                   XLO, XHI, YLO, YHI, DATA, IMGVAL, STATUS )

*  Consider the interpolated value only when it is not bad.
         IF ( IMGVAL .NE. VAL__BADR ) THEN
            IMGMAX = MAX( IMGMAX, IMGVAL )
            IMGMIN = MIN( IMGMIN, IMGVAL )
         END IF
      END DO

*  If the max value and the min value are the same, the image values on
*  these positions are a constant. Set the marker size for these point
*  as the median size.
      IF ( ( IMGMAX - IMGMIN ) .LE. VAL__SMLR ) THEN
         DO I = 1, NPNT
            MKSIZ( I ) = 0.5 * ( MXMNSZ( 2 ) + MXMNSZ( 1 ) )
         END DO

*  If the image value at these points are not constant, calculate the
*  size of the marks according to the image value at the positions.
      ELSE
         DO I = 1, NPNT

*  Get the value at the position by bi-linear interpolation.
            CALL IRM_BILN1( REAL( X( I ) ), REAL( Y( I ) ),
     :                      XLO, XHI, YLO, YHI, DATA, IMGVAL, STATUS )

*  Consider the interpolated value only when it is not bad.
            IF ( IMGVAL .NE. VAL__BADR ) THEN
               MKSIZ( I ) = MXMNSZ( 1 ) * ( IMGVAL - IMGMIN ) +
     :                      MXMNSZ( 2 ) * ( IMGMAX - IMGVAL )
               MKSIZ( I ) = MKSIZ( I ) / ( IMGMAX - IMGMIN )

*  Otherwise set the size of the image as bad.
            ELSE
               MKSIZ( I ) = VAL__BADR
            END IF
         END DO
      END IF

      END
