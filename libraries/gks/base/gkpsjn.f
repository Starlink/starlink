C# IL>=a, OL>=0
      SUBROUTINE GKPSJN (IWN,ISA,IWA,NSA,IWSA,
     :                       ISB,IWB,NSB,IWSB, NSO,IWSO)
*
* (C) COPYRIGHT ICL & SERC  1989
*

*-------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Generate a section list for single polygon from
*     those of two polygons so as to
*     join them at specified vertices adding a new vertex
*     at the join.
*
*
*  MAINTENANCE LOG
*  ---------------
*     23/02/89  KEVP  Stabilised
*     20/03/89  KEVP  Put in check for error in arguments ISA,NSA,ISB,NSB
*     16/11/89  RMK   Removed unused local variable.
*     22/07/90  PLP   Commenting brought in line with standard format.
*     12/03/91  PLP   Removed second inclusion of check.inc.
*
*  ARGUMENTS
*  ---------
*     INP  IWN   Index of new vertex to be added at the join
*
*     INP  ISA   Section of specified vertex in 1st polygon
*     INP  IWA   Index of specified vertex in 1st polygon
*     INP  NSA   Number of sections in 1st polygon
*     INP  IWSA  Section list of 1st polygon
*
*     INP  ISB   Section of specified vertex in 2nd polygon
*     INP  IWB   Index of specified vertex in 2nd polygon
*     INP  NSB   Number of sections in 2nd polygon
*     INP  IWSB  Section list of 2nd polygon
*
*     OUT  NSO   Number of sections in joined polygon
*     OUT  IWSO  Section list of joined polygon
*
      INTEGER IWN, ISA, IWA, NSA, IWSA(2,NSA)
      INTEGER      ISB, IWB, NSB, IWSB(2,NSB)
      INTEGER NSO, IWSO(2,NSA+NSB+4)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NA  Number of sections for 1st polygon
*     NB  Number of sections for 2nd polygon
*
      INTEGER NA, NB
*
*  STACK USAGE
*  -----------
*     2*NSECT for copy of section list
*
*  ERRORS
*  ------
*     -2004 Section ISA or ISB outside of polyggon's section list
*     -2004 in GKPSAD is either IWA is not in section ISA or
*                               IWB is not in section ISB.
*
*----------------------------------------------------------------------
      NSO = 0
      NA = NSA
*     Check arguments ISA,NSA, ISB,NSB
      IF((1 .GT. ISA) .OR. (ISA .GT. NSA))THEN
         CALL GKBUG (-2004,'GKPSJN')
      ELSEIF((1 .GT. ISB) .OR. (ISB .GT. NSB))THEN
         CALL GKBUG (-2004,'GKPSJN')
      ELSE
*        Add new vertex to 1st polygon and move its beginning there
         CALL GKPSAD(IWN,ISA,IWA,NSA,IWSA,NA,IWSO)
         IF(KERROR .NE. 0)GOTO 999
*        Do the same with the 2nd polygon and place it
*        straight afterwards, so to join them together.
         NB = NSB
         CALL GKPSAD(IWN,ISB,IWB,NSB,IWSB,NB,IWSO(1,NA+1))
         IF(KERROR .NE. 0)GOTO 999
         NSO = NA + NB
      ENDIF
  999 CONTINUE
      END
