      SUBROUTINE SPOT(X,Y,R, STATUS )
*+
*   This Routine Plots a filled circle or Spot of
*   radius R at (X,Y)
*
*   Gets
*   ----
*      X,Y  - Co-ordinates of star to be plotted
*      R    - radius of Star to be Plotted

*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     4-MAR-1993 (AJJB):
*       STATUS argument added.
*
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   First clear any other output out of the way

      CALL SGS_FLUSH

*
*   Inquire resolution of plotter so that the lines can be
*   s suitable distance apart.
*
      CALL SGS_IDUN(DX,DY)

*
*   Now move to start of circle - it will be filled with a vertical
*   raster starting at left centre
*
      XP=X-R
      YP=Y
      CALL SGS_BPOLY(XP,YP)
      XP=XP+DX

*
*   Now loop around adding another raster until the circle is filled
*
      DO WHILE (XP.LT.(X+R))

         CALL SGS_APOLY(XP,YP)
         DY = SQRT( R*R - (XP-X)*(XP-X) )
         YP = Y +DY
         CALL SGS_APOLY(XP,YP)
         XP = XP + DX
         CALL SGS_APOLY(XP,YP)
         YP = Y-DY
         CALL SGS_APOLY(XP,YP)
         XP=XP+DX

      END DO

      CALL SGS_APOLY(XP,Y)
      CALL SGS_APOLY(X+R,Y)

      CALL SGS_OPOLY

      END
