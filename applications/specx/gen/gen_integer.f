*  History:
*     25 Nov 1993 (hme):
*        Declare GEN_DIGITS as logical rather than integer.
*-----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_INTEGER (STRING, IDIGITS)

      IMPLICIT NONE

*     Formal parameters

      CHARACTER STRING*(*)
      INTEGER   IDIGITS

      INTEGER*4 ILS
      INTEGER*4 IST
      INTEGER*4 GEN_ILEN

      LOGICAL   GEN_DIGITS

      IST     = 1
      IDIGITS = 0
      ILS     = GEN_ILEN(STRING)

      GEN_INTEGER = .FALSE.

      IF (ILS.NE.0) THEN
        IF (STRING(1:1).EQ.'+' .OR. STRING(1:1).EQ.'-') IST = 2
        IF (ILS.GE.IST) THEN
          IF (GEN_DIGITS (STRING(IST:ILS), IDIGITS)) THEN
            GEN_INTEGER = .TRUE.
            IDIGITS     =  ILS
          END IF
        END IF
      END IF

      RETURN
      END
