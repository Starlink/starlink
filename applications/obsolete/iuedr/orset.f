      SUBROUTINE ORSET( M )
*+
*  Name:
*     SUBROUTINE ORSET

*  Description:
*     The contents of CMDISH are defined for the specified order M.
*     If is assumed that HISET has been used to define the Echelle-wide
*     dispersion constants.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ORSET( M )

*  Arguments:
*     M = INTEGER (Given)
*        Order number to be selected.

*  Method:
*     Assume that all data are valid.
*     The line of constant wavelength is fixed for the echelle central
*     wavelength using linear interpolation between M-1 and M+1.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     03-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Deficiencies:
*     The expression should be reformulated so that use of FLOAT
*     coefficients is more effective.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER M     ! Echelle order number.

*  Global Variables:
      INCLUDE 'CMDISP'
      INCLUDE 'CMDISH'
      INCLUDE 'CMROTR'

*  Local Variables:
      REAL*8 DELX   ! X-step.
      REAL*8 DELY   ! Y-step.
      REAL*8 DM     ! M-step.
      REAL*8 DR     ! R-step.
      REAL*8 DW     ! W-step.
      REAL*8 L1     ! L-value.
      REAL*8 L2
      REAL*8 S1     ! S-value.
      REAL*8 S2
      REAL*8 U1     ! U-value.
      REAL*8 U2
      REAL*8 V1     ! V-value.
      REAL*8 V2
      REAL*8 W1     ! W-value.
      REAL*8 W2
      REAL*8 X1     ! X-value.
      REAL*8 X2
      REAL*8 Y1     ! Y-value.
      REAL*8 Y2
*.

*  Store current order.
      CORD = M

*  Constants used for (R,W) to/from (X,Y).
      A0 = A( 1 ) + M * A( 4 )
      A1 = A( 5 ) + M * ( A( 2 ) + M * A( 6 ) )
      A2 = M * ( A( 7 ) + M * A( 3 ) )
      B0 = B( 1 ) + M * B( 4 )
      B1 = B( 5 ) + M * ( B( 2 ) + M * B( 6 ) )
      B2 = M * ( B( 7 ) + M * B( 3 ) )
      DM = 2.0D0
      DW = RIPCON * ( 1.0D0 / DBLE( M + 1 ) - 1.0D0 / DBLE( M - 1 ) )
      DELX = A( 4 ) * DM + A( 5 ) * DW + RIPCON *
     :       ( A( 6 ) * DM + A( 7 ) * DW )
      DELY = B( 4 ) * DM + B( 5 ) * DW + RIPCON *
     :       ( B( 6 ) * DM + B( 7 ) * DW )
      DR = SQRT( DELX * DELX + DELY * DELY )
      DXDR = DELX / DR
      DYDR = DELY / DR
      WC = RIPCON / DBLE( M )

*  DU/DR.
      DUDR = DUDX * DXDR + DUDY * DYDR

*  Average spacing between orders.
      DRDM = DR / DM

*  Average displacement for unit wavelength interval.
      W1 = WC - 1.0D0
      W2 = WC + 1.0D0

      CALL WTOG( 0.0D0, W1, X1, Y1 )
      CALL WTOG( 0.0D0, W2, X2, Y2 )

      DRDW = SQRT( ( X2 - X1 ) ** 2 + ( Y2 - Y1 ) ** 2 ) /
     :       ABS( W2 - W1 )

*  V-displacement for unit wavelength.
      CALL GTOR( X1, Y1, S1, L1 )
      CALL RTOU( S1, L1, U1, V1 )

      CALL GTOR( X2, Y2, S2, L2 )
      CALL RTOU( S2, L2, U2, V2 )

      DVDW = ( V2 - V1 ) / ( W2 - W1 )

      END
