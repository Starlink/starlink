      SUBROUTINE SQU (X,Y,R, STATUS )
*+
*   Routine to Draw a Square of half width R
*   centered on point (X,Y).
*
*   Gets
*   ----
*      X,Y  - Point upon which square is centered
*      R    - Half width of square
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

      CALL SGS_BPOLY (X-R,Y-R)
      CALL SGS_APOLY (X+R,Y-R)
      CALL SGS_APOLY (X+R,Y+R)
      CALL SGS_APOLY (X-R,Y+R)
      CALL SGS_APOLY (X-R,Y-R)
      CALL SGS_OPOLY

      END
