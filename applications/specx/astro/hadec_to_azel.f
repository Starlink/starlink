*-----------------------------------------------------------------------

      SUBROUTINE HADEC_TO_AZEL (HA, DEC, ALAT, AZ, EL)

*  Subroutine to convert hour-angle and declination to azimuth and
*  elevation.

      IMPLICIT   NONE

*     Formal parameters:

      DOUBLE PRECISION HA      ! input hour angle
      DOUBLE PRECISION DEC     ! input declination
      DOUBLE PRECISION ALAT    ! input latitude
      DOUBLE PRECISION AZ      ! output azimuth
      DOUBLE PRECISION EL      ! output elevation

*     Local variables

      DOUBLE PRECISION CELSAZ
      DOUBLE PRECISION CELCAZ
      DOUBLE PRECISION SEL

*  Ok, go...

      CELSAZ = - COS(DEC)*SIN(HA)
      CELCAZ = + SIN(DEC)*COS(ALAT) - COS(DEC)*COS(HA)*SIN(ALAT)
      SEL    = + SIN(DEC)*SIN(ALAT) + COS(DEC)*COS(HA)*COS(ALAT)

      EL     =   ASIN  (SEL)
      AZ     =   ATAN2 (CELSAZ, CELCAZ)

      RETURN
      END

*-----------------------------------------------------------------------

