      SUBROUTINE DP (INREC, IPTR, JBEPD, EPD, JBEP, EP, J)
*+
*
*  DP:  subroutine of COCO utility which decodes an epoch.
*
*  Given:
*     INREC   char    string
*     IPTR    int     string pointer
*     JBEPD   char    default epoch type (B or J)
*     EPD     dp      default epoch
*
*  Returned:
*     IPTR    int     advanced
*     JBEP    char    epoch type (B or J)
*     EP      dp      epoch
*     J       int     status:
*                       0 = valid epoch
*                       1 = default
*                       2 = invalid epoch
*
*  Two formats of epoch are recognised:
*     either   [B or J] year
*     or       year month day
*
*  Called:
*     sla_INTIN, sla_DFLTIN, sla_DBJIN, sla_CALDJ,
*     sla_EPJ, sla_KBJ, KTEST
*
*  P T Wallace   Starlink   18 May 1992
*-

      IMPLICIT NONE

      CHARACTER INREC*(*)
      INTEGER IPTR
      CHARACTER JBEPD
      DOUBLE PRECISION EPD
      CHARACTER JBEP
      DOUBLE PRECISION EP
      INTEGER J

      DOUBLE PRECISION sla_EPJ
      INTEGER KTEST

      INTEGER IPTRT
      CHARACTER JBEPT
      DOUBLE PRECISION EPT
      INTEGER JT

      INTEGER IY,J1,IM,J2,J3,ID,JF,J1A

      DOUBLE PRECISION D,DJM



*  Attempt y,m,d decode
      IPTRT=IPTR
      CALL sla_INTIN(INREC,IPTRT,IY,J1)
      CALL sla_INTIN(INREC,IPTRT,IM,J2)
      CALL sla_DFLTIN(INREC,IPTRT,D,J3)
      IF (J1.EQ.0.AND.J2.EQ.0.AND.J3.EQ.0) THEN

*     OK so far:  convert and check
         ID=INT(D)
         CALL sla_CALDJ(IY,IM,ID,DJM,JF)
         IF (JF.EQ.0.OR.JF.EQ.3) THEN
            JBEPT='J'
            EPT=sla_EPJ(DJM+MOD(D,1D0))
            JT=0
         ELSE
            JT=2
         END IF
      ELSE

*     Unsuccessful decode
         JT=2
      END IF

      IF (JT.NE.0) THEN

*     Not valid y,m,d:  attempt single field decode
         IPTRT=IPTR
         CALL sla_DBJIN(INREC,IPTRT,EPT,J1,J1A)
         IF (J1.EQ.1) THEN

*        Default
            JT=1

         ELSE IF (J1.EQ.0) THEN

*        OK so far:  convert and check
            CALL sla_KBJ(J1A,EPT,JBEPT,JF)
            IF (JF.EQ.0) THEN
               JT=0
            ELSE
               JT=2
            END IF
         ELSE

*        Error
            JT=2
         END IF

      END IF

*  Handle default case
      IF (JT.EQ.1) THEN
         JBEPT=JBEPD
         EPT=EPD
      END IF

*  Final validation
      IF (KTEST(INREC,IPTRT).NE.0.OR.
     :    (JBEPT.NE.'B'.AND.JBEPT.NE.'J').OR.
     :    EPT.LT.1800D0.OR.EPT.GT.2200D0) JT=2

*  Return results
      IPTR=IPTRT
      IF (JT.NE.2) THEN
         JBEP=JBEPT
         EP=EPT
      END IF
      J=JT

      END
