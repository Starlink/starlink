      SUBROUTINE COCOMO (RM,DM,KQ,EQ,KP,EP,PX,PRMS,ROB,DOB,ZOB)
*+
*
*     - - - - - - -
*      C O C O M O
*     - - - - - - -
*
*  Convert a mean place to observed place
*
*  Given:
*     RM,DM    d      given RA,Dec
*     KQ       c*(*)  'B' OR 'J' for equinox of given RA,Dec
*     EQ       d      equinox of given RA,Dec
*     KP       c*(*)  'B' or 'J' for epoch of given RA,Dec
*     EP       d      epoch of given RA,Dec
*     PX       d      parallax (arcsec)
*     PRMS     d(35)  J2000 to observed parameters (see note)
*
*  Returned:
*     ROB,DOB  d      observed RA,Dec
*     ZOB      d      observed ZD
*
*  The array PRMS contains precomputed J2000-to-apparent and
*  apparent-to-observed parameters as required by the routines
*  sla_MAPQKZ (first 21 elements) and sla_AOPQK (remaining 14
*  elements).
*
*  Called:    COCOMM, sla_MAPQK, sla_AOPQK
*
*  P T Wallace   Starlink   16 October 1992
*-

      IMPLICIT NONE

      DOUBLE PRECISION RM,DM
      CHARACTER KQ*(*)
      DOUBLE PRECISION EQ
      CHARACTER KP*(*)
      DOUBLE PRECISION EP,PX,PRMS(35),ROB,DOB,ZOB

      DOUBLE PRECISION R2000,D2000,RAP,DAP,AOB,HOB



*  Given to J2000
      CALL COCOMM(RM,DM,1,0D0,0D0,PX,KQ,EQ,KP,EP,
     :            'J',2000D0,KP,EP,R2000,D2000)

*  J2000 to apparent
      CALL sla_MAPQK(R2000,D2000,0D0,0D0,PX,0D0,PRMS,RAP,DAP)

*  Apparent to observed
      CALL sla_AOPQK(RAP,DAP,PRMS(22),AOB,ZOB,HOB,DOB,ROB)

      END
