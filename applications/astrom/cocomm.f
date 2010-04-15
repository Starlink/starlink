      SUBROUTINE COCOMM (R0,D0,JZ,PR,PD,PX,KQ0,EQ0,KP0,EP0,
     :                                 KQ1,EQ1,KP1,EP1,R1,D1)
*+
*
*     - - - - - - -
*      C O C O M M
*     - - - - - - -
*
*  Convert a star position from mean position for one equinox and
*  epoch to mean position for another equinox and epoch.
*
*  Given:
*     R0,D0       d      given RA,Dec
*     JZ          i      0 = proper motions supplied
*     PR,PD       d      proper motions in given system (rad/yr)
*     PX          d      parallax (arcsec)
*     KQ0         c*(*)  'B' or 'J' for equinox of given RA,Dec
*     EQ0         d      equinox of given RA,Dec
*     KP0         c*(*)  'B' or 'J' for epoch of given RA,Dec
*     EP0         d      epoch of given RA,Dec
*     KQ1         c*(*)  'B' or 'J' for equinox of returned RA,Dec
*     EQ1         d      equinox of returned RA,Dec
*     KP1         d      'B' or 'J' for epoch of returned RA,Dec
*     EP1         d      epoch of returned RA,Dec
*
*  Returned:
*     R1,D1       d      RA,Dec for new epoch and equinox
*
*  Called:    sla_EPCO, sla_PM, sla_PRECES, sla_FK54Z, sla_FK45Z
*
*  n.b.  All epochs and equinoxes are in the form of either Besselian
*        (prefix 'B') or Julian (prefix 'J') epochs.  Two common
*        examples are:    K        E
*                        'B'     1950D0
*                        'J'     2000D0
*
*  P T Wallace   Starlink   9 June 1997
*-

      IMPLICIT NONE

      DOUBLE PRECISION R0,D0
      INTEGER JZ
      DOUBLE PRECISION PR,PD,PX
      CHARACTER*(*) KQ0
      DOUBLE PRECISION EQ0
      CHARACTER*(*) KP0
      DOUBLE PRECISION EP0
      CHARACTER*(*) KQ1
      DOUBLE PRECISION EQ1
      CHARACTER*(*) KP1
      DOUBLE PRECISION EP1
      DOUBLE PRECISION R1,D1

      DOUBLE PRECISION sla_EPCO

      DOUBLE PRECISION E0,E1G,E1,RA,DC,WR,WD,PRF,PDF




*  Copy RA,Dec
      RA=R0
      DC=D0

*
*  Convert the epochs into B or J to match the equinoxes
*
*  Given position's epoch to match given position's equinox
      E0=sla_EPCO(KQ0,KP0,EP0)
*  Returned position's epoch to match given position's equinox
      E1G=sla_EPCO(KQ0,KP1,EP1)
*  Returned position's epoch to match returned position's equinox
      E1=sla_EPCO(KQ1,KP1,EP1)

*  Allow for proper motion (unless flagged zero)
      IF (JZ.EQ.0) CALL sla_PM(R0,D0,PR,PD,PX,0D0,E0,E1G,RA,DC)

*  Examine the equinox for the given position
      IF (KQ0.EQ.'J') THEN

*     Position is FK5

*     Examine results equinox
         IF (KQ1.EQ.'J') THEN

*
*        Position and results both FK5
*        -----------------------------

*        FK5 precession from given to results equinox
            CALL sla_PRECES('FK5',EQ0,EQ1,RA,DC)
         ELSE

*
*        Position FK5, results FK4
*        -------------------------

*        FK5 precession from given equinox to J2000
            CALL sla_PRECES('FK5',EQ0,2000D0,RA,DC)

*        Transform to B1950 assuming zero proper motion in FK5
            WR=RA
            WD=DC
            CALL sla_FK54Z(WR,WD,E1,RA,DC,PRF,PDF)

*        FK4 precession from B1950 to results equinox
            CALL sla_PRECES('FK4',1950D0,EQ1,RA,DC)
         END IF
      ELSE

*     Position is FK4:  examine results equinox
         IF (KQ1.EQ.'B') THEN

*        Results are FK4 as well:  examine proper motions flag
            IF (JZ.EQ.0) THEN

*
*           Position and results both FK4;  proper motions given
*           ----------------------------------------------------

*           FK4 precession from given to results equinox
               CALL sla_PRECES('FK4',EQ0,EQ1,RA,DC)
            ELSE

*
*           Position and results both FK4;  no proper motion
*           ------------------------------------------------

*           FK4 precession from given equinox to B1950
               CALL sla_PRECES('FK4',EQ0,1950D0,RA,DC)

*           Transform to J2000 assuming zero proper motion in FK5
               CALL sla_FK45Z(RA,DC,E0,WR,WD)

*           Transform back to B1950 specifying results epoch
               CALL sla_FK54Z(WR,WD,E1,RA,DC,PRF,PDF)

*           FK4 precession from B1950 to results equinox
               CALL sla_PRECES('FK4',1950D0,EQ1,RA,DC)
            END IF
         ELSE

*        Position is FK4, results FK5:  examine proper motions flag
            IF (JZ.EQ.0) THEN

*
*           Position FK4, results FK5;  proper motions given
*           ------------------------------------------------

*           FK4 precession from given equinox to B1950
               CALL sla_PRECES('FK4',EQ0,1950D0,RA,DC)

*           Transform to J2000 assuming zero proper motion in FK5
               CALL sla_FK45Z(RA,DC,1950D0,WR,WD)
               RA=WR
               DC=WD

*           FK5 precession from J2000 to results equinox
               CALL sla_PRECES('FK5',2000D0,EQ1,RA,DC)
            ELSE

*
*           Position FK4, results FK5;  no proper motion
*           --------------------------------------------

*           FK4 precession to B1950
               CALL sla_PRECES('FK4',EQ0,1950D0,RA,DC)

*           Transform to J2000 assuming no proper motion in FK5
               CALL sla_FK45Z(RA,DC,E0,WR,WD)
               RA=WR
               DC=WD

*           FK5 precession from J2000 to results equinox
               CALL sla_PRECES('FK5',2000D0,EQ1,RA,DC)
            END IF
         END IF
      END IF

*  Return results
      R1=RA
      D1=DC

      END
