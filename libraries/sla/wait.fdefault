      SUBROUTINE sla_WAIT (DELAY)
*+
*     - - - - -
*      W A I T
*     - - - - -
*
*  Interval wait
*
*  !!! Version for: SPARC/SunOS4, 
*                   SPARC/Solaris2, 
*                   DEC Mips/Ultrix
*                   DEC AXP/Digital Unix
*                   Intel/Linux
*                   Convex
*
*  Given:
*     DELAY     real      delay in seconds
*
*  Called:  SLEEP (a Fortran Intrinsic on all obove platforms)
*
*  P.T.Wallace   Starlink   22 January 1998
*
*  Copyright (C) 1998 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL DELAY

      CALL SLEEP(NINT(DELAY))

      END
