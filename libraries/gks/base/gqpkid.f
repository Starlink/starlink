C# IL>=b, OL>=1
      SUBROUTINE GQPKID (IER,IPKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT PRIMITIVE ATTRIBUTE VALUES
*                      PICK IDENTIFIER
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the current primitive attribute value for pick identifier.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS    Original version stabilized
*     21/01/87  RMK   IS conversion. Changed preprocessor control to 1b.
*
*  ARGUMENTS
*  ---------
*     OUT  IER    Error indicator
*     OUT  IPKID  Pick identifier
*
      INTEGER IER, IPKID
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gksl.cmn'
*
*---------------------------------------------------------------------

      CALL GKPRLG (KNIL, GGKOP, GSGOP)
      IER = KERROR
      IF (IER.EQ.0) IPKID = KCPCID

      END
