

      SUBROUTINE GK1TXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Tek 4107 Supply Font Details
*
*  MAINTENANCE LOG
*  ---------------
*     07/03/84  MGC  Original version stabilized (CC81)
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
*     ICHAHI  Offset in QWKDAT for hardware character height
*     ICHAWI  Offset in QWKDAT for hardware character width
*
      INTEGER    ICHAHI,   ICHAWI
      PARAMETER( ICHAHI=1, ICHAWI=2)
      INTEGER II
*
* --------------------------------------------------------------------

      RHT=QWKDAT(ICHAHI,KWKIX)
      RMAXWD=QWKDAT(ICHAWI,KWKIX)
      RBOT=0.0
      RTOP=0.0
*     widths for chars [32..126]
      DO 10 II=1,95
      RWD(II)=RMAXWD
 10   CONTINUE
      RETURN
      END
