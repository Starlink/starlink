      SUBROUTINE TILTX(X,Y,R, STATUS )

*+
*  This routine plots an exploded cross of half width R, rotated by an
*  angle of 45 degrees, at a point (X,Y).
*
*   Gets
*   ----
*      X,Y  - Point at which Rotated Exploded Cross is to be Plotted
*      R    - Half-width of Rotated Exploded Cross
*
*   Arguments:
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     4-MAR-1993 (AJJB):
*       STATUS argument added.
*
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

        RAY=(R*4.0)/5.0
        GAP=R/3.5
        TILT=SIN(45.0)
        RADIUS=R*TILT
        DIAGAP=GAP*TILT
        CALL SGS_BPOLY(X-RADIUS,Y+RADIUS)
        CALL SGS_APOLY(X-DIAGAP,Y+DIAGAP)
        CALL SGS_BPOLY(X+DIAGAP,Y-DIAGAP)
        CALL SGS_APOLY(X+RADIUS,Y-RADIUS)
        CALL SGS_BPOLY(X+RADIUS,Y+RADIUS)
        CALL SGS_APOLY(X+DIAGAP,Y+DIAGAP)
        CALL SGS_BPOLY(X-DIAGAP,Y-DIAGAP)
        CALL SGS_APOLY(X-RADIUS,Y-RADIUS)
        CALL SGS_OPOLY
        END
