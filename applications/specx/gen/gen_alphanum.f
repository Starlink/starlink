*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Eliminate CHR_UCASE
*-----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_ALPHANUM (STRING)

      IMPLICIT NONE

*     Formal parameter

      CHARACTER STRING*(*)

      INTEGER*4 IST,ILS
      INTEGER*4 GEN_ILEN
      INTEGER*4 ITEST

      GEN_ALPHANUM = .TRUE.
      ILS = GEN_ILEN(STRING)

*     Strip leading blanks (if any)

      IST = 1
      DO WHILE (STRING(IST:IST).EQ.' ' .AND. IST.LE.ILS)
        IST = IST + 1
      END DO

      IF (ILS.GE.IST) THEN
        DO WHILE (GEN_ALPHANUM .AND. IST.LE.ILS)
          ITEST = ICHAR(STRING(IST:IST))
          IF (.NOT.(
     &                 ITEST.EQ.'2E'X                          ! Decimal point
     &            .OR. ITEST.EQ.'5F'X                          ! Underscore
     &            .OR. '30'X.LE.ITEST .AND. ITEST.LE.'39'X     ! Numeral 0..9
     &            .OR. '41'X.LE.ITEST .AND. ITEST.LE.'5A'X     ! Capital letter
     &            .OR. '61'X.LE.ITEST .AND. ITEST.LE.'7A'X     ! Small letter
     &       )) THEN
            GEN_ALPHANUM = .FALSE.
          ELSE
            IST = IST+1
          END IF
        END DO
      END IF

      RETURN
      END
