*----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_DIGITS (STRING, IDIGITS)

      IMPLICIT NONE

*     Formal parameter

      CHARACTER STRING*(*)
      INTEGER*4 IDIGITS

      INTEGER*4 IST,ILS
      INTEGER*4 GEN_ILEN
      CHARACTER TEST*1
      INTEGER*4 ITEST

*  Ok? Go..

      GEN_DIGITS = .TRUE.
      IST = 1
      ILS = GEN_ILEN(STRING)

      IF (ILS.GE.IST) THEN
        DO WHILE (GEN_DIGITS .AND. IST.LE.ILS)
          ITEST = ICHAR(STRING(IST:IST))
          IF (ITEST.LT.'30'X .OR. '39'X.LT.ITEST) GEN_DIGITS = .FALSE.
          IST = IST+1
        END DO
          IDIGITS = IST - 1
      ELSE
        GEN_DIGITS = .FALSE.
      END IF

      RETURN
      END
