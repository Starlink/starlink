C# IL>=a, OL>=0
      SUBROUTINE GKSYN(ISPEED,TIME)

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation Utility
*  Author:             xxx
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To generate filler characters to a graphics device which is
*     also a terminal.
*
*  MAINTENANCE LOG
*  ---------------
*     21/05/84  JRG   First version
*
*  ARGUMENTS
*  ---------
*      INP ISPEED  Speed of terminal line in bits per second
*      INP TIME    Period for which filling is required, in seconds
*
      INTEGER ISPEED
      REAL TIME
*
*  COMMON BLOCK USAGE
*  ------------------
*     None
*
      INCLUDE '../include/gkio.par'
*
*  LOCALS
*  ------
*     LOOP    Loop counter
*     ISYN    Syn character (variable not parameter, since it's used
*             as argument where array element is expected).
*     NLEFT   Returned from buffer filling routine (not used here)
*
      INTEGER LOOP,ISYN,NLEFT
*
*  COMMENTS
*  --------
*     Number of filler characters to use depends on speed of line
*     (need more if line is faster) and on the time required.
*
*-----------------------------------------------------------------------


*   Start new buffer before and after SYN's
      CALL GKIOBO(KIOSN,1,ISYN,NLEFT)
      ISYN=22
      DO 100 LOOP=1,NINT(FLOAT(ISPEED)*TIME/8.0)+1
  100   CALL GKIOBO(KIOPB,1,ISYN,NLEFT)
      CALL GKIOBO(KIOSN,1,ISYN,NLEFT)

      END
