*  History:
*     16 Nov 1993 (hme):
*        Replace backslash in string with CHAR(92).
*     18 Nov 1993 (hme):
*        Can no longer use CHAR as a variable then.
*-----------------------------------------------------------------------

      INTEGER FUNCTION GEN_RANK(CHR)

      CHARACTER CHR*1
      CHARACTER DCH(3)*1

      DATA DCH/ ',', ' ', ';'/

      DCH(2) = CHAR(92)

      GEN_RANK = 0
      DO GEN_RANK = 1,3
       IF (CHR .EQ. DCH(GEN_RANK)) RETURN
      END DO
      GEN_RANK = 0

      RETURN
      END
