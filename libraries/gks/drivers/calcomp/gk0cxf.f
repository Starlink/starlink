

* --------------------------------------------------------------
      SUBROUTINE GK0CXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81 Supply Font Details
*
*  MAINTENANCE LOG
*  ---------------
*     07/03/84  MGC  Original version stabilized
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
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER II
*
* --------------------------------------------------------------------

      RHT=FLOAT(KWCHHT(KWKIX))
      RMAXWD=FLOAT(KWCHWD(KWKIX))
      RBOT=FLOAT((KWCHHT(KWKIX)/6)*2)
      RTOP=0.0
*     widths for chars [32..126]
      DO 10 II=1,95
        RWD(II)=RMAXWD
 10   CONTINUE
      RETURN
      END
