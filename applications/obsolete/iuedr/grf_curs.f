      SUBROUTINE grf_CURS( X, Y, N )

*+
*
*   Name:
*      SUBROUTINE grf_CURS
*
*   Description:
*      Get cursor position from current workstation.
*
*   Authors:
*      Patrick Wallace
*
*   History:
*      Patrick Wallace        MAY-87
*         SNX version.
*      Paul Rees           18-MAR-88     IUEDR Vn. 2.0
*         Conversion to IUEDR needs.
*      Paul Rees           09-MAY-89     IUEDR Vn. 2.1
*         Final conversion to SGP/16 style.
*
*   Method:
*      This subroutine is based upon snx_CURS. However, where snx_CURS
*      assumes to begin in NCAR, this subroutine assumes that SGS
*      coordinates are current.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import/Export:
      REAL X           ! cursor position in user coordinates
      REAL Y           ! cursor position in user coordinates

*   Export:
      INTEGER N        ! SGS choice

*   External references:
      REAL snx_AGGUX   ! X-axis grid to user transformation
      REAL snx_AGGUY   ! Y-axus grid to user transformation
      REAL snx_AGUGX   ! X-axis user to grid transformation
      REAL snx_AGUGY   ! T-axis user to grid transformation

*   Local variables:
      REAL XG          ! X-axis grid coordinate
      REAL YG          ! Y-axis grid coordinate

*   Flush
      CALL PLOTIT( 0, 0, 2 )
      CALL sgs_FLUSH

*   Transform preset user coordinates to grid coordinates
      XG = snx_AGUGX(X)
      YG = snx_AGUGY(Y)

*   Set the cursor position (if possible)
      CALL sgs_SETCU( XG, YG )

*   Get a cursor position (grid coordinates)
      CALL sgs_REQCU( XG, YG, N )

*   Convert to user coordinates
      X = snx_AGGUX(XG)
      Y = snx_AGGUY(YG)

      END
