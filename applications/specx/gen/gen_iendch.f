*  History:
*     31 July 2000 (ajc):
*        Change TYPE * to PRINT *
C--------------------------------------------------------------------------

      INTEGER*4 FUNCTION GEN_IENDCH (STRING)

C   Routine to find the end-point in character string STRING of the current
C   hollerith string as enclosed in quotation marks.

      INTEGER*4 GEN_ILEN
      CHARACTER STRING*(*)
      CHARACTER HD,CHAR,NCHAR

      DATA HD /''''/

      GEN_IENDCH = 1
      ILS        = GEN_ILEN (STRING)
      IF (ILS.LE.1)   THEN
        Print *,'Insufficient data in line - abandoning string'
        RETURN
      END IF
      GEN_IENDCH = 2

   10 CHAR = STRING(GEN_IENDCH:GEN_IENDCH)
      IF (CHAR.EQ.HD)   THEN
        IF (GEN_IENDCH.EQ.ILS)   RETURN
        NCHAR = STRING(GEN_IENDCH+1:GEN_IENDCH+1)
        IF (NCHAR.NE.HD)   THEN
          RETURN
        ELSE
          GEN_IENDCH=GEN_IENDCH+1
        END IF
      END IF

      IF (GEN_IENDCH.EQ.ILS)   RETURN
      GEN_IENDCH = GEN_IENDCH+1
      GO TO 10

      END
