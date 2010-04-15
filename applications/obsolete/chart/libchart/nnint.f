      FUNCTION NNINT(X)
*+
*   This is a variation on the standard NINT
*   but in this case it rounds to the numerically larger value
*   whether positive or negative
*
*   Gets
*   ----
*      X  - Real Value Input
*
*   Returns
*   -------
*      NNINT - Integer value of type described above
*-

      IF (X.NE.0.0) THEN
         NNINT=INT(X + SIGN(0.5,X))
      ELSE
         NNINT=0
      END IF
      END
