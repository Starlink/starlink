

      SUBROUTINE GK1TSP(INDEC)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Tek 4107 Select pen according to specification
*
*  MAINTENANCE LOG
*  ---------------
*     25/01/85  GGT  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP INDEC    Colour index
*
      INTEGER INDEC
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
*     ICOL    Offset in KWKDAT for current device colour index
*
      INTEGER    ICOL
      PARAMETER( ICOL=2)
      INTEGER IBUFF(4),NLEFT
      DATA IBUFF /27,77,76,0/
*                 esc,m,l,lineindex
*
*  ALGORITHM
*  ---------
*
*       If required index is not current index then
*          Select line index - es M L lineindex
*          Update common block with current index
*       else
*          exit
*       endif
*
*
* --------------------------------------------------------------
*
      IF(INDEC.NE.KWKDAT(ICOL,KWKIX)) THEN
      IBUFF(4)=INDEC+48
      CALL GKIOBO(KIOPB,4,IBUFF,NLEFT)
      KWKDAT(ICOL,KWKIX)=INDEC
      ENDIF
      RETURN
      END
