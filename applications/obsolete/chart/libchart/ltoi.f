      INTEGER FUNCTION LTOI(L)

*  History:
*    15-MAR-1993 (AJJB):
*      Removed this function from file IIDATE and put it in it's own
*      file.
*
      BYTE L

      IF (L.LT.0) THEN
        LTOI = L +256
      ELSE
        LTOI = L
      ENDIF
      END

