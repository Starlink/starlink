      FUNCTION ROUNUP(X,Y)
*+
*    Function Returns the value of
*    X Rounded up to the Nearest whole Multiple
*    of Y
*
*    Gets
*    ----
*       X (DOUBLE PRECISION)  - value to be rounded.
*       Y (REAL)              - multiple X is to be rounded up in
*
*    Returns
*    -------
*       ROUNUP - Rounded value.
*
*    History:
*      20-MAY-1993 (AJJB):
*        Made arguments X double precision and Y real.
*-

      DOUBLE PRECISION X
      REAL Y

      Z = Y
      IF (X.LT.0.0) Z = 0.0
      ROUNUP = AINT((X + Z)/Y) * Y
      END

