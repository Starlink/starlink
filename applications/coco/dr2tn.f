      SUBROUTINE DR2TN (NDP,ANGLE,SIGN,IHMSF)
*+
*
*  DR2TN:  subroutine of COCO utility which calls sla_DR2TF
*          and then performs cosmetic adjustments
*
*  Given:
*     NDP       int      number of decimal places of seconds
*     ANGLE     dp       angle in radians
*
*  Returned:
*     SIGN      char     '+' or '-'
*     IHMSF     int(4)   hours, minutes, seconds, fraction
*
*  Called:
*     sla_DR2TF
*
*  The cosmetic adjustment is to prevent an output of 24 0 0 0.
*
*  The sign is ignored.
*
*  P T Wallace   Starlink   18 May 1992
*-

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER*1 SIGN
      INTEGER IHMSF(4)



*  Encode
      CALL sla_DR2TF(NDP,ANGLE,SIGN,IHMSF)

*  Normalise
      IF (IHMSF(1).GE.24) THEN
         IHMSF(1)=0
         IHMSF(2)=0
         IHMSF(3)=0
         IHMSF(4)=0
      END IF

      END
