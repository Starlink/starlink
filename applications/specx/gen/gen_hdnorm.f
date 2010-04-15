C--------------------------------------------------------------------------

      SUBROUTINE GEN_HDNORM (STRING, ITEM, LITEM, IERR)

C   Routine to remove first and last hollerith delimiters from a string
C   if present, and to convert internal double delimiters to single ones.

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER STRING*(*)
      CHARACTER ITEM*(*)
      INTEGER*4 LITEM
      INTEGER*4 IERR

*     Local variables:

      INTEGER*4 IFIN
      INTEGER*4 J
      CHARACTER HD*1

      DATA HD /''''/

*     Functions:

      INTEGER*4 GEN_ILEN

*  Ok, go..

      IERR = 0

      IFIN = GEN_ILEN(STRING)
      IF (STRING(1:1).NE.HD.OR.STRING(IFIN:IFIN).NE.HD) THEN
        ITEM  = STRING(:IFIN)//' '
        LITEM = IFIN
        RETURN
      END IF

      LITEM = 0
      J = 2
      DO WHILE (J.LE.IFIN-1)
        IF (STRING(J:J+1).EQ.HD//HD)   THEN
          J = J+1
        END IF
        LITEM = LITEM+1
        ITEM (LITEM:LITEM) = STRING(J:J)
        J = J+1
      END DO

      IF (LITEM.LT.LEN(ITEM)) ITEM(LITEM+1:) = ' '

      RETURN
      END
