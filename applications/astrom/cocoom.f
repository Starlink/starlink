      SUBROUTINE COCOOM (ROB,DOB,PRMS,KQ,EQ,KP,EP,RM,DM)
*+
*
*     - - - - - - -
*      C O C O O M
*     - - - - - - -
*
*  Convert an observed place to mean place
*
*  Given:
*     ROB,DOB  d      observed RA,Dec
*     PRMS     d(35)  J2000 to observed parameters (see note)
*     KQ       c*(*)  'B' OR 'J' for equinox of returned RA,Dec
*     EQ       d      equinox of returned RA,Dec
*     KP       c*(*)  'B' or 'J' for epoch of returned RA,Dec
*     EP       d      epoch of returned RA,Dec
*
*  Returned:
*     RM,DM    d      mean RA,Dec
*
*  The array PRMS contains precomputed J2000-to-apparent and
*  apparent-to-observed parameters as required by the routines
*  sla_MAPQKZ (first 21 elements) and sla_AOPQK (remaining 14
*  elements).
*
*  Called:    sla_OAPQK, sla_AMPQK, COCOMM
*
*  P T Wallace   16 October 1992
*-

      IMPLICIT NONE

      DOUBLE PRECISION ROB,DOB,PRMS(35)
      CHARACTER KQ*(*)
      DOUBLE PRECISION EQ
      CHARACTER KP*(*)
      DOUBLE PRECISION EP,RM,DM

      DOUBLE PRECISION R2000,D2000,RAP,DAP



*  Observed to apparent
      CALL sla_OAPQK('R',ROB,DOB,PRMS(22),RAP,DAP)

*  Apparent to J2000
      CALL sla_AMPQK(RAP,DAP,PRMS,R2000,D2000)

*  J2000 to nominated mean coordinates
      CALL COCOMM(R2000,D2000,1,0D0,0D0,0D0,'J',2000D0,'J',2000D0,
     :            KQ,EQ,KP,EP,RM,DM)

      END
