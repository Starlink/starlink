      SUBROUTINE sla_FK54Z (R2000,D2000,BEPOCH,
     :                      R1950,D1950,DR1950,DD1950)
*+
*     - - - - - -
*      F K 5 4 Z
*     - - - - - -
*
*  Convert a J2000.0 FK5 star position to B1950.0 FK4 assuming
*  zero proper motion and parallax (double precision)
*
*  This routine converts star positions from the new, IAU 1976,
*  FK5, Fricke system to the old, Bessel-Newcomb, FK4 system.
*
*  Given:
*     R2000,D2000     dp    J2000.0 FK5 RA,Dec (rad)
*     BEPOCH          dp    Besselian epoch (e.g. 1950D0)
*
*  Returned:
*     R1950,D1950     dp    B1950.0 FK4 RA,Dec (rad) at epoch BEPOCH
*     DR1950,DD1950   dp    B1950.0 FK4 proper motions (rad/trop.yr)
*
*  Notes:
*
*  1)  The proper motion in RA is dRA/dt rather than cos(Dec)*dRA/dt.
*
*  2)  Conversion from Julian epoch 2000.0 to Besselian epoch 1950.0
*      only is provided for.  Conversions involving other epochs will
*      require use of the appropriate precession routines before and
*      after this routine is called.
*
*  3)  Unlike in the sla_FK524 routine, the FK5 proper motions, the
*      parallax and the radial velocity are presumed zero.
*
*  4)  It is the intention that FK5 should be a close approximation
*      to an inertial frame, so that distant objects have zero proper
*      motion;  such objects have (in general) non-zero proper motion
*      in FK4, and this routine returns those fictitious proper
*      motions.
*
*  5)  The position returned by this routine is in the B1950
*      reference frame but at Besselian epoch BEPOCH.  For
*      comparison with catalogues the BEPOCH argument will
*      frequently be 1950D0.
*
*  Called:  sla_FK524, sla_PM
*
*  P.T.Wallace   Starlink   10 April 1990
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION R2000,D2000,BEPOCH,
     :                 R1950,D1950,DR1950,DD1950

      DOUBLE PRECISION R,D,PX,RV



*  FK5 equinox J2000 (any epoch) to FK4 equinox B1950 epoch B1950
      CALL sla_FK524(R2000,D2000,0D0,0D0,0D0,0D0,
     :               R,D,DR1950,DD1950,PX,RV)

*  Fictitious proper motion to epoch BEPOCH
      CALL sla_PM(R,D,DR1950,DD1950,0D0,0D0,1950D0,BEPOCH,
     :            R1950,D1950)

      END
