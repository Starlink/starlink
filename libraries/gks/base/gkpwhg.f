C# IL>=a, OL>=1
      SUBROUTINE GKPWHG(NR,NI,NC,IPR,IPI,IPC)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Pick utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To scan GDP for a hit.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     NR      Number of stack REALs
*     NI      Number of stack INTEGERs
*     NC      Number of stack CHARACTER*80's
*     IPR     Stack pointer for array of reals (x & y)
*     IPI     Stack pointer for integers
*     IPC     Stack pointer for CHARACTER*80 information
*
      INTEGER  NR,NI,NC,IPR,IPI,IPC
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /STK/    Real arrays supplied on stack
*     Read   /WCA/    GDP parameters (KWI1 and QWR1..6)
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkcca.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
*
*  EXTERNALS
*  ---------
*     GKPWHL  Scan polyline
*     GKPWHP  Scan polygon
*     GKPWHD  Scan raster (stub)
*
      EXTERNAL GKPWHL,GKPWHP,GKPWHD
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      IF(KWI1.EQ.1) THEN
        KWLNTY(KWKIX)=1
        CALL GKCIRC(KWI1,NR,QSTACK(IPR),QSTACK(IPR+NR),.FALSE.,20.0,
     :                 GKPWHL,GKPWHD)
      ELSE
        KWFAIS(KWKIX)=GHOLLO
        CALL GKCIRC(KWI1,NR,QSTACK(IPR),QSTACK(IPR+NR),.FALSE.,20.0,
     :                 GKPWHP,GKPWHD)
      ENDIF

      END
