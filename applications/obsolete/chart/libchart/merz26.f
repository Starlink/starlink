      SUBROUTINE MERZ26(X,Y, STATUS )

*+
*   Calculates and prints the co-ordinates for the
*   Merz guide telescope on the RGO 26-inch refractor.
*
*   Gets
*   ----
*      X,Y   -  Input Co-ordinates
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

      DOUBLE PRECISION X,Y

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      XS = REAL( X )
      YS = REAL( Y )
      X1=1.56*XS+500.0
      Y1=1.56*YS+200.0
      IF (X1.LT.430.0.OR.X1.GT.559.0.OR.
     :   Y1.LT.138.0.OR.Y1.GT.265.0)    THEN
*
*   If it is outside then print "-       -"
*
         WRITE (7,900)
  900    FORMAT ('+',93X,'-',6X,'-')
      ELSE
*
*   If it is inside then print the values.
*
         IX=NNINT(X1)
         IY=NNINT(Y1)
         WRITE (7,910) IX,IY
  910    FORMAT ('+',90X,2(I6,1X))
      END IF
      END
