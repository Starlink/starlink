C# IL>=a, OL>=0
      SUBROUTINE GSCHUP(RCHUX,RCHUY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET CHARACTER UP VECTOR
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     set character up vector
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83    FY  Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     20/01/87  DCS   IS conversion. Remove test on value of KSTXWK
*                     since now only two possible values.
*                     Set character base vector to vector at right
*                     angles in clockwise direction to character up
*                     vector.
*
*  ARGUMENTS
*  ---------
*     INP   RCHUX  x-component of char. up vector
*     INP   RCHUY  y_component of char. up vector
*
      REAL    RCHUX,RCHUY
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   QCCHUX,QCCHUY,QCCHBX,QCCHBY,KSTXWK
*     Read   /gkysl/   khange,kgksfn..(par)
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     RLEN  length of up vector
*
      REAL RLEN
*
*  ERRORS
*  ------
*     79     length of char. up vector is zero
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESCHUP,GGKOP,GSGOP)
      IF (KERROR .EQ. 0) THEN
        RLEN = SQRT(RCHUX*RCHUX + RCHUY*RCHUY)
        IF (RLEN .LE. 0.0) THEN
          CALL GKERR(79)
        ELSE
          QCCHUX = RCHUX
          QCCHUY = RCHUY
          QCCHBX = RCHUY
          QCCHBY = -RCHUX
          KSTXWK = KHANGE
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF

      END
