C# IL>=a, OL>=0
      SUBROUTINE GSCHH(RCHH)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET CHARACTER HEIGHT
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set character height
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83  FY    Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSTXWK
*                     since now only two possible values.
*                     Set character width to character height.
*
*  ARGUMENTS
*  ---------
*     INP   RCHH   character height
*
      REAL RCHH
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   QCCHH,QCCHW,KSTXWK
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
*     78     height less than or equal to zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESCHH,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        IF (RCHH .LE. 0.0) THEN
          CALL GKERR(78)
        ELSE
          QCCHH = RCHH
          QCCHW = RCHH
          KSTXWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF

      END
