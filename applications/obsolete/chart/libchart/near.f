      SUBROUTINE NEAR(EX,ARR,N,J, STATUS )
*+
*   This Returns the Number of the Element
*   of the Array ARR(N) Which is Closest
*   to an Exact Value EX
*
*   Gets
*   ----
*      EX     - The Exact Value
*      ARR    - The Array of Rounded Values
*      N      - Dimension of Array ARR
*
*   Returns
*   -------
*      J      - The Number of the Element in ARR
*               Which is Closest to EX.
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      DIMENSION ARR(N)

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      J = 0
 100  CONTINUE
      J = J + 1
      DIFF = EX - ARR(J)
      IF (J.LT.N.AND.DIFF.GT.0.0) GOTO 100
      IF (J.GT.1.AND.J.LT.N) THEN
        IF (ABS(EX-ARR(J-1)).LT.ABS(DIFF)) J = J - 1
      ENDIF

      END
