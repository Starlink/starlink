*  History:
*     25 Nov 1993 (hme):
*        Declare GEN_DIGITS and GEN_INTEGER as logical rather than
*        integer.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_FLOATING (STRING, IDIGITS, FDIGITS)

      IMPLICIT NONE

*     Formal parameters

      CHARACTER STRING*(*)
      INTEGER*4 IDIGITS
      INTEGER*4 FDIGITS

*     Local storage

      INTEGER*4 ILS
      INTEGER*4 IX

*     Functions

      INTEGER*4 GEN_ILEN
      LOGICAL GEN_DIGITS
      LOGICAL GEN_INTEGER

      IDIGITS = 0
      FDIGITS = 0
      GEN_FLOATING = .TRUE.

      ILS = GEN_ILEN(STRING)

      IF (ILS.EQ.0) THEN
        GEN_FLOATING = .FALSE.
      ELSE
        IX = INDEX (STRING, '.')
        IF (IX.EQ.0) THEN
          IF (.NOT. GEN_INTEGER  (STRING, IDIGITS)) THEN
            GEN_FLOATING = .FALSE.
          ELSE
            FDIGITS = -1
          END IF
        ELSE
          IF (IX.GT.1   .AND.
     &                  .NOT. GEN_INTEGER (STRING(1:IX-1),   IDIGITS))
     &        GEN_FLOATING = .FALSE.
          IF (IX.LT.ILS .AND.
     &                  .NOT. GEN_DIGITS  (STRING(IX+1:ILS), FDIGITS))
     &        GEN_FLOATING = .FALSE.
        END IF
      END IF

CD    PRINT *, ' -- gen_floating --'
CD    PRINT *, '    idigits, fdigits = ', idigits, fdigits

      RETURN
      END
