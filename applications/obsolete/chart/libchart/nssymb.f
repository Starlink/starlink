      SUBROUTINE NSSYMB(X,Y,R, STATUS )
*+
*   Draws a Lozenge to Represent a Nonstellar Object
*
*   Gets
*   ----
*      X,Y   - Position of Object Centre
*      R     - Size of Side of the Shape Plotted

*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

!
      SIZE = R * 0.70711
      CALL SGS_BPOLY (X-SIZE,Y)
      CALL SGS_APOLY (X,Y+SIZE)
      CALL SGS_APOLY (X+SIZE,Y)
      CALL SGS_APOLY (X,Y-SIZE)
      CALL SGS_APOLY (X-SIZE,Y)
      CALL SGS_OPOLY
      R=SIZE

      END
