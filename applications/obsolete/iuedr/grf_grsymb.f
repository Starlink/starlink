      SUBROUTINE grf_GRSYMB(X, Y)

*+
*
*   Name:
*      SUBROUTINE grf_GRSYMB
*
*   Description:
*      Graticule symbol.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings     20-JUN-81
*         AT4 version.
*      Paul Rees         14-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         09-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*      Mark the specified position with a "graticule" type symbol.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Local constants:
      INTEGER GRPAL    ! graticule colour index

      PARAMETER (GRPAL=1)

      REAL LONG        ! long length for graticule
      REAL SHORT       ! short length fro graticule

      PARAMETER (LONG=0.025, SHORT=0.01)

*   Import:
      REAL X           ! X-axis centre of graticule
      REAL Y           ! Y-axis centre of graticule

*   External references:
      REAL snx_AGUGX   ! X-axis user to grid transformation
      REAL snx_AGUGY   ! Y-axis user to grid transformation

*   Local variables:
      INTEGER JPAL     ! current colour index

      REAL XG          ! grid X-axis centre
      REAL YG          ! grid Y-axis centre
      REAL X1          ! grid start X-axis coordinate
      REAL Y1          ! grid start Y-axis coordinate
      REAL X2          ! grid end X-axis coordinate
      REAL Y2          ! grid end Y-axis coordinate

*   Inquire current colour
      CALL grf_QPALET(JPAL)

*   Set graticule colour
      CALL grf_PPALET(GRPAL)

*   Calculate line vertices for graticule and plot
      XG = snx_AGUGX(X)
      YG = snx_AGUGY(Y)

      X1 = XG - SHORT - LONG
      Y1 = YG
      X2 = XG - SHORT
      Y2 = YG
      CALL sgs_LINE(X1, Y1, X2, Y2)

      X1 = XG + SHORT
      X2 = XG + SHORT + LONG
      CALL sgs_LINE(X1, Y1, X2, Y2)

      X1 = XG
      Y1 = YG - SHORT - LONG
      X2 = XG
      Y2 = YG - SHORT
      CALL sgs_LINE(X1, Y1, X2, Y2)

      Y1 = YG + SHORT
      Y2 = YG + SHORT + LONG
      CALL sgs_LINE(X1, Y1, X2, Y2)

*   Flush SGS
      CALL snx_AGCS

*   Return to original colour
      CALL grf_PPALET(JPAL)

      END
