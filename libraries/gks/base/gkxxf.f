      SUBROUTINE GKXXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
* --------------------------------------------------------------
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Supply Font Details
*     for as STRING or CHARACTER precision font
*     that are identical to Hershey fonts
*
*  MAINTENANCE LOG
*  ---------------
*     24/04/91   KEVP  Created as part of the fix to bug C78.
*
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
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkxfd.cmn'
*
*  LOCALS
*  ------
*
*     Do loop index over the characters of a font
      INTEGER IFCHAR

*     Height vector and magnitude
      REAL    HTX,HTY, HT

*  COMMENT
*  -------
*     In the pick utility GKRQPK, this utility can be used
*     as the text details routine, when Hershey fonts are
*     used for all precisions.
*
*     It should not be explicitly called (used only as external).

* --------------------------------------------------------------------

*     Get Hershey Details
      CALL GKXFD

*     Translate to Arguments
      HTX = QWCHHX(KWKIX)
      HTY = QWCHHY(KWKIX)
      HT = SQRT(HTX*HTX + HTY*HTY)
      RHT    = HT*QFHT
      RMAXWD = HT*QFWDMX
      RTOP   = HT*QFCAP
      RBOT   = HT*QFBASE
      DO 10 IFCHAR=1,95
         RWD(IFCHAR)=HT*QFWIDS(IFCHAR)
 10   CONTINUE

      RETURN
      END
