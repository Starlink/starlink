

*----------------------------------------------------------------------
      SUBROUTINE GK0BXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
*
* (C) COPYRIGHT ICL & SERC  1986
*

*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of workstation driver
*  Author:             DRJF
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     BENSON Supply Font Details
*
*  MAINTENANCE LOG
*  ---------------
*     24/01/86  DRJF  Original version stabilized
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
*----------------------------------------------------------------------


      RHT=FLOAT(KWCHHT(KWKIX))
      RMAXWD=FLOAT(KWCHWD(KWKIX))
      RBOT=FLOAT(KWCHHT(KWKIX))/3
      RTOP=0.0
*     widths for chars [32..126]
      DO 10 II=1,95
        RWD(II)=RMAXWD
 10   CONTINUE
      RETURN
      END
