      SUBROUTINE CROSS(X,Y,R, STATUS )
*+
*   This Routine Plots an Exploded Cross of
*   Half-Width R at (X,Y)
*
*   Gets
*   ----
*      X,Y  - Co-ordinates of star to be plotted
*      R    - Half-Width of Star to be Plotted

*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added.
*-

*  Global constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      GAP = R/5.0

      CALL SGS_BPOLY (X+GAP,Y)
      CALL SGS_APOLY (X+R,Y)
      CALL SGS_BPOLY (X,Y+GAP)
      CALL SGS_APOLY (X,Y+R)
      CALL SGS_BPOLY (X-GAP,Y)
      CALL SGS_APOLY (X-R,Y)
      CALL SGS_BPOLY (X,Y-GAP)
      CALL SGS_APOLY (X,Y-R)
      CALL SGS_OPOLY

      END
