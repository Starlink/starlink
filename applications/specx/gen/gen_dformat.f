*-----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_DFORMAT (STRING, IDIGITS, FDIGITS, EDIGITS)

      IMPLICIT NONE

*     Formal parameters

      CHARACTER STRING*(*)
      INTEGER*4 IDIGITS
      INTEGER*4 FDIGITS
      INTEGER*4 EDIGITS

      INTEGER*4 ID, IDL, IDU
      INTEGER*4 GEN_ILEN
      LOGICAL GEN_FLOATING
      LOGICAL GEN_INTEGER

      GEN_DFORMAT = .TRUE.

      IF (GEN_ILEN(STRING).EQ.0) THEN
        GEN_DFORMAT = .FALSE.
      ELSE
        IDL = INDEX (STRING, 'd')
        IDU = INDEX (STRING, 'D')
        ID  = MAX (IDL, IDU)        ! ID must be in string
        IF (ID.EQ.0) THEN
          IF (.NOT. GEN_FLOATING (STRING, IDIGITS, FDIGITS))
     &         GEN_DFORMAT = .FALSE.
        ELSE
          IF (.NOT. GEN_FLOATING (STRING(:ID-1), IDIGITS, FDIGITS)) THEN
            GEN_DFORMAT = .FALSE.
          ELSE IF (.NOT. GEN_INTEGER  (STRING(ID+1:), EDIGITS)) THEN
            GEN_DFORMAT = .FALSE.
          ELSE
            STRING(ID:ID) = 'E'
          END IF
        END IF
      END IF

      RETURN
      END
