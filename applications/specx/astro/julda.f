C------------------------------------------------------------------------------

      INTEGER FUNCTION JULDA(NYR)

C   THIS FUNCTION COMPUTES THE JULIAN DAY NUMBER (MODIFIED) AT 12HRS U.T.
C   ON JANUARY 0 OF THE YEAR NYR. JULDA IS AN INTEGER BECAUSE OF THIS
C   DEFINITION.

      NYRM1=NYR-1
      IC=NYRM1/100
      JULDAT=1721425+365*NYRM1+NYRM1/4-IC+IC/4
      JULDA=JULDAT-2415020

      RETURN
      END

C------------------------------------------------------------------------------
