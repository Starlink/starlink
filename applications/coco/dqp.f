      SUBROUTINE DQP (INREC, IPTR, JBEQD, EQD, JBEQ, EQ, JBEP, EP, J)
*+
*
*  DQP:  subroutine of COCO utility which decodes an equinox
*        and an epoch.
*
*  Given:
*     INREC   char    string
*     IPTR    int     string pointer
*     JBEQD   char    default equinox type (B or J)
*     EQD     dp      default equinox
*
*  Returned:
*     IPTR    int     advanced
*     JBEQ    char    equinox type (B or J)
*     EQ      dp      equinox
*     JBEP    char    epoch type (B or J)
*     EP      dp      epoch
*     J       int     status:
*                       0 = valid equinox & epoch
*                       1 = equinox and epoch both defaulted
*                       2 = invalid equinox & epoch
*
*  Equinox is a Besselian or Julian Epoch optionally preceded
*  by 'B' or 'J'.
*
*  Epoch can likewise be a B or J epoch, or it can be year,
*  month day.
*
*  Equinox defaults to the one specified;  epoch defaults to
*  the equinox.
*
*  Called:
*     sla_DBJIN, sla_KBJ, DP
*
*  P T Wallace   Starlink   18 May 1992
*-

      IMPLICIT NONE

      CHARACTER INREC*(*)
      INTEGER IPTR
      CHARACTER JBEQD
      DOUBLE PRECISION EQD
      CHARACTER JBEQ
      DOUBLE PRECISION EQ
      CHARACTER JBEP
      DOUBLE PRECISION EP
      INTEGER J


      CHARACTER JBEQT
      DOUBLE PRECISION EQT
      CHARACTER JBEPT
      DOUBLE PRECISION EPT
      INTEGER JT

      INTEGER J1,J1A,JF




*  Look for equinox
      CALL sla_DBJIN(INREC,IPTR,EQT,J1,J1A)
      IF (J1.EQ.0) THEN

*     Equinox superficially OK
         CALL sla_KBJ(J1A,EQT,JBEQT,JF)
         IF (JF.EQ.0) THEN
            JT=0
         ELSE
            JT=2
         END IF
      ELSE IF (J1.EQ.1) THEN

*     Equinox default
         JBEQT=JBEQD
         EQT=EQD
         JT=1
      ELSE

*     Bad equinox
         JT=2
      END IF

*  Further validate equinox
      IF (JT.NE.2) THEN
         IF ((JBEQT.NE.'B'.AND.JBEQT.NE.'J').OR.
     :       EQT.LT.1800D0.AND.EQT.GT.2200D0) JT=2
      END IF

*  If OK so far, look for epoch
      IF (JT.NE.2) THEN
         CALL DP(INREC,IPTR,JBEQT,EQT,JBEPT,EPT,JF)
*     Bad epoch
         IF (JF.EQ.2) JT=2
      END IF

*  Return results as appropriate
      IF (JT.NE.2) THEN
         IF (JT.EQ.1.AND.JF.EQ.0) JT=0
         JBEQ=JBEQT
         EQ=EQT
         JBEP=JBEPT
         EP=EPT
      END IF
      J=JT

      END
