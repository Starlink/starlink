C# IL>=a, OL>=0
      SUBROUTINE GQTXFP (IER,IFONT,IPREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT INDIVIDUAL ATTRIBUTE VALUES
*                      TEXT FONT AND PRECISION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Current Individual Attribute Value for text font and
*     precision.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   IFONT  Text font
*     OUT   IPREC  Text precision
*
      INTEGER IER, IFONT, IPREC
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gksl.cmn'
*
*---------------------------------------------------------------------

      CALL GKPRLG (KNIL, GGKOP, GSGOP)
      IER = KERROR
      IF (IER.EQ.0) THEN
        IFONT = KCTXFN
        IPREC = KCTXPR
      ENDIF

      END
