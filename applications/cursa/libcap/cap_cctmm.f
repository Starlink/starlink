      SUBROUTINE CAP_CCTMM (R0, D0, JZ, PR, PD, PX, RV, KQ0, EQ0,
     :  KP0, EP0, KQ1, EQ1, KP1, EP1, R1, D1, STATUS)
*+
*  Name:
*     CAP_CCTMM
*  Purpose:
*     Transform mean coord. from one equinox and epoch to another.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CCTMM (R0, D0, JZ, PR, PD, PX, KQ0, EQ0, KP0, EP0,
*       KQ1, EQ1, KP1, EP1; R1, D1; STATUS)
*  Description:
*     Convert a star position from mean position for one equinox and
*     epoch to mean position for another equinox and epoch.
*
*     n.b.  All epochs and equinoxes are in the form of either Besselian
*           (prefix 'B') or Julian (prefix 'J') epochs.  Two common
*           examples are:    K        E
*                           'B'     1950D0
*                           'J'     2000D0
*  Arguments:
*     R0  =  DOUBLE PRECISION (Given)
*        Given Right Ascension expressed in the input equinox and
*        epoch (Radians).
*     D0  =  DOUBLE PRECISION (Given)
*        Given Declination expressed in the input equinox and
*        epoch (Radians).
*     JZ  =  INTEGER (Given)
*        Flag indicating whether proper motions associated with the
*        input positions are available, coded as follows:
*        0  -  proper motions are supplied,
*        all other values  -  no proper motions are available.
*     PR  =  DOUBLE PRECISION (Given)
*        Proper motion in Right Ascension (Radians/year).
*     PD  =  DOUBLE PRECISION (Given)
*        Proper motion in Declination (Radians/year).
*     PX  =  DOUBLE PRECISION (Given)
*        Parallax (Radians)
*     RV  =  DOUBLE PRECISION (Given)
*        Radial velocity (Km/sec).  Positive values indicate recession.
*     KQ0  =  CHARACTER*(*) (Given)
*        Time system for input equinox: 'B' or 'J'.
*     EQ0  =  DOUBLE PRECISION (Given)
*        Input equinox (years).
*     KP0  =  CHARACTER*(*) (Given)
*        Time system for input epoch: 'B' or 'J'.
*     EP0  =  DOUBLE PRECISION (Given)
*        Input epoch (years).
*     KQ1  =  CHARACTER*(*) (Given)
*        Time system for returned equinox: 'B' or 'J'.
*     EQ1  =  DOUBLE PRECISION (Given)
*        Returned equinox (years).
*     KP1  =  CHARACTER*(*) (Given)
*        Time system for returned epoch: 'B' or 'J'.
*     EP1  =  DOUBLE PRECISION (Given)
*        Returned epoch (years).
*     R1  =  DOUBLE PRECISION (Returned)
*        Right Ascension in the new equinox and epoch (Radians).
*     D1  =  DOUBLE PRECISION (Returned)
*        Declination in the new equinox and epoch (Radians).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     (None available).
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     PTW: P T Wallace   (Starlink, RAL)
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     16/10/92 (PTW): Original version (COCOMM for ASTROM).
*     19/6/96  (ACD): CAP_CCTMM created for CURSA: added running status
*        argument and 'ADAM' style prologue.
*     12/6/97  (ACD): Two bugs fixed (by PTW): proper motions of FK4 stars
*        were being computed using Julian years rather than tropical years
*        (though in practice the difference is negligible, amounting to a
*        factor of 365.2422/365.25 in the amount of proper motion) and in
*        the case of converting equinox and epoch J2000 to B1950 wrong
*        epoch was being specified (giving 50 years of fictitious proper
*        motion).  Also added the correction for radial velocity.
*     20/5/98  (ACD): Corrected a couple of typing mistakes (in the
*        comments).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAP_PAR'           ! CAP library constants.
*  Arguments Given:
      DOUBLE PRECISION R0,D0
      INTEGER JZ
      DOUBLE PRECISION PR,PD,PX,RV
      CHARACTER*(*) KQ0
      DOUBLE PRECISION EQ0
      CHARACTER*(*) KP0
      DOUBLE PRECISION EP0
      CHARACTER*(*) KQ1
      DOUBLE PRECISION EQ1
      CHARACTER*(*) KP1
      DOUBLE PRECISION EP1
*  Arguments Returned:
      DOUBLE PRECISION R1,D1
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      DOUBLE PRECISION sla_EPCO
*  Local Variables:
      DOUBLE PRECISION E0,E1G,E1,RA,DC,WR,WD,PRF,PDF
      DOUBLE PRECISION PXASEC
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*  Convert the parallax from radians to seconds of arc.

         PXASEC = (PX * 1.8D2 * 6.0D1 * 6.0D1) / CAP__PI

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
         IF (JZ.EQ.0) CALL sla_PM(R0,D0,PR,PD,PXASEC,RV,E0,E1G,RA,DC)

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

      END IF

      END

