C# IL>=a, OL>=0
      SUBROUTINE GFA(N,PX,PY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  FILL AREA
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level Fill Area primitive routine
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83 NGB  Original version stabilized
*     17/02/83 PGLS Call to PRLG updated. Min number points now 3.
*     25/04/83 NBG  Add "catch-up" provisions.
*     30/06/83 NGB  Add KERROR check.
*     04/07/83 PGLS More KERROR changes.
*     20/02/86 DCS  Replace call to GKSCTG by its contents.
*
*  ARGUMENTS
*  ---------
*     INP   N     number of points
*     INP   PX    x-coordinates of points
*     INP   PY    y-coordinates of points
*
      INTEGER N
      REAL PX(*),PY(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    RGN: regenerate
*     Read   /ERR/    KERROR
*     Modify /SL/     KSTRWK, KSFAWK
*
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     100  Number of points invalid
*
*  ALGORITHM
*  ---------
*     The Polygon defined by the set of N vertices in
*     PX,PY is output using the current FillArea
*     attributes.
*
*---------------------------------------------------------------------

      CALL GKPRLG(EFA,GWSAC,GSGOP)
      IF (KERROR .NE. 0) GOTO 888

      IF ( N.LT.3 ) THEN
        CALL GKERR(100)
      ELSE
        IF (KSTRWK.NE.KGKSFN) THEN
          CALL GKCCTG
          CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
          IF (KERROR .NE. 0) GOTO 888
          KSTRWK=KGKSFN
        ENDIF
        IF (KSFAWK.NE.KGKSFN) THEN
          CALL GKCFAG
          CALL GKSACW(KSFAA,1,KDAT,1,QDAT,QDAT,1,CH)
          IF (KERROR .NE. 0) GOTO 888
          KSFAWK=KGKSFN
        ENDIF
        CALL GKSACW(KFA,1,KDAT,N,PX,PY,1,CH)
        IF (KERROR .NE. 0) GOTO 888
        IF (KRGN) THEN
          CALL GKRGN
          IF (KERROR .NE. 0) GOTO 888
        ENDIF
      ENDIF
      RETURN

  888 CONTINUE
      CALL GKERR(KERROR)

      END
