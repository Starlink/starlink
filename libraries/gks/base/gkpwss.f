C# IL>=a, OL>=0
      SUBROUTINE GKPWSS(XP,YP,ISO,ISN)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Select candidate segment
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP XP   - Pick position X
*     INP YP   - Pick position Y
*     INP ISO  - Name of old candidate segment (nil if none)
*     OUT ISN  - Name of new candidate segment (nil if none)
*
      REAL    XP,YP
      INTEGER ISO,ISN
*
*  COMMON BLOCK USAGE
*  ------------------
*
*   Read   /WSL/   Segment pointers
*   Read   /WCA/   Workstation index
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*     SEGPRI   Segment priority
*     SEGBOX   Segment extent
*
      REAL SEGPRI,SEGBOX(4)
*
*  ERRORS
*  ------
*
*---------------------------------------------------------------------

* Get segment name from workstation segment list
      IF(ISO.EQ.KNIL) THEN
*       set to detect that data is available
        SEGBOX(1)=-1.0
*       start at highest priority segment
        CALL GKSLGE(KSSGPT(KWKIX),KHIEST,ISN,SEGPRI,SEGBOX)
      ELSE
*       select current segment
        CALL GKSLGE(KSSGPT(KWKIX),ISO,ISN,SEGPRI,SEGBOX)
*       set to detect that data is available
        SEGBOX(1)=-1.0
*       get next lowest priority segment
        CALL GKSLGE(KSSGPT(KWKIX),KLOER,ISN,SEGPRI,SEGBOX)
      ENDIF

* Loop to find candidate segment
 100  CONTINUE
      IF(ISN.NE.KNIL) THEN
        IF(SEGBOX(1).GE.0.0) THEN
*         test for pick point within segment extent
          IF(.NOT. (XP.GT.SEGBOX(1) .AND. XP.LT.SEGBOX(2) .AND.
     :              YP.GT.SEGBOX(3) .AND. YP.LT.SEGBOX(4))) THEN
*           get next lowest priority segment
            CALL GKSLGE(KSSGPT(KWKIX),KLOER,ISN,SEGPRI,SEGBOX)
            GOTO 100
          ENDIF
        ENDIF
      ENDIF

      END
