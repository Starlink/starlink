C# IL>=a, OL>=0
      SUBROUTINE GSTXI(ITXI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET TEXT INDEX
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set text index
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83    FY  Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSTXWK
*                     since now only two possible values.
*
*  ARGUMENTS
*  ---------
*     INP   ITXI   text index
*
      INTEGER ITXI
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCTXI,KSTXWK
*     Read   /gkysl/   khange,kgksfn..(par)
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     72   Text index invalid
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESTXI,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (ITXI .LE. 0) THEN
          CALL GKERR(72)
        ELSE
          KCTXI = ITXI
          KSTXWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF
      END
