*  History:
*     25 Nov 1993 (hme):
*        Declare GEN_FLOATING and GEN_INTEGER as logical rather than
*        integer.
*-----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_EFORMAT (STRING, IDIGITS, FDIGITS, EDIGITS)

      IMPLICIT NONE

*     Formal parameters

      CHARACTER STRING*(*)
      INTEGER*4 IDIGITS
      INTEGER*4 FDIGITS
      INTEGER*4 EDIGITS

      INTEGER*4 IE, IEL, IEU
      INTEGER*4 GEN_ILEN
      LOGICAL GEN_FLOATING
      LOGICAL GEN_INTEGER

      GEN_EFORMAT = .TRUE.

      IF (GEN_ILEN(STRING).EQ.0) THEN
        GEN_EFORMAT = .FALSE.
      ELSE
        IEL = INDEX (STRING, 'e')
        IEU = INDEX (STRING, 'E')
        IE  = MAX (IEL, IEU)        ! IE must be in string
        IF (IE.EQ.0) THEN
          IF (.NOT. GEN_FLOATING (STRING, IDIGITS, FDIGITS)) THEN
            GEN_EFORMAT = .FALSE.
          END IF
        ELSE
          IF (.NOT. GEN_FLOATING (STRING(:IE-1), IDIGITS, FDIGITS))
     &         GEN_EFORMAT = .FALSE.
          IF (.NOT. GEN_INTEGER  (STRING(IE+1:), EDIGITS))
     &         GEN_EFORMAT = .FALSE.
        END IF
      END IF

      RETURN
      END
