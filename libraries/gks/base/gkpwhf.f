C# IL>=a, OL>=1
      SUBROUTINE GKPWHF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
*
* (C) CORYRIGHT ICL & SERC  1985
*
*  Type of routine:    Pick utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Supply hardware font details (stub).
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised.
*     21/01/87  ARG   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     OUT RHT      Height from base to cap line
*     OUT RMAXWD   Width of widest character
*     OUT RBOT     Distance from base to bottom line
*     OUT RTOP     Distance from cap to top line
*     OUT RWD      Character widths array
*
      INTEGER IFID
      REAL RHT,RMAXWD,RBOT,RTOP,RWD(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
*  LOCALS
*  ------
*
*  ERRORS
*  ------
*     -9999  Bug - this routine should never be entered
*
* --------------------------------------------------------------------

      CALL GKBUG (-9999,'GKPWHF')
      END
