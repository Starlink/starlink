      SUBROUTINE GUID13( X, Y, STATUS )
*+
*   Calculates and prints the guide star co-ordinates
*   for the 13-inch at RGO
*
*   Gets
*   ----
*      X   - The X Co-ordinate of this star
*      Y   - The Y Co-ordinate of this star
*
*   Returns
*   -------
*      Writes output to logical unit 7 (without page throw)

*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IX=NNINT(X)
      IY=NNINT(Y)
      WRITE (7,900) IX,IY
  900 FORMAT ('+',104X,2(I6,1X))
      END
