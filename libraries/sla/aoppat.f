      SUBROUTINE sla_AOPPAT (DATE, AOPRMS)
*+
*     - - - - - - -
*      A O P P A T
*     - - - - - - -
*
*  Recompute the sidereal time in the apparent to observed place
*  star-independent parameter block.
*
*  Given:
*     DATE   d      UTC date/time (modified Julian Date, JD-2400000.5)
*                   (see AOPPA source for comments on leap seconds)
*
*     AOPRMS d(14)  star-independent apparent-to-observed parameters
*
*       (1-12)   not required
*       (13)     longitude + eqn of equinoxes + sidereal DUT
*       (14)     not required
*
*  Returned:
*     AOPRMS d(14)  star-independent apparent-to-observed parameters:
*
*       (1-13)   not changed
*       (14)     local apparent sidereal time (radians)
*
*  For more information, see sla_AOPPA.
*
*  Called:  sla_GMST
*
*  P.T.Wallace   Starlink   1 July 1993
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION DATE,AOPRMS(14)

      DOUBLE PRECISION sla_GMST



      AOPRMS(14) = sla_GMST(DATE)+AOPRMS(13)

      END
