C# IL>=a, OL>=0
      SUBROUTINE GSWKVP(IWKID, XMIN,XMAX, YMIN,YMAX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET WORKSTATION VIEWPORT
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To set the workstation viewport of the specified workstation
*
*  MAINTENANCE LOG
*  ---------------
*     29/06/83  JRG   Original version stabilized
*     28/09/83  AS    Change subroutine name
*     20/01/87  ARG   IS conversion. Report negative errors.
*
*  ARGUMENTS
*  ---------
*     Inp   IWKID   Workstation Identifier
*     Inp   XMIN,XMAX  Extent in x of workstation viewport
*     Inp   YMIN,YMAX  Extent in y of workstation viewport
*
      INTEGER IWKID
      REAL XMIN,XMAX,YMIN,YMAX
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKERR/  Read KERROR
*
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKSWKL(IWKID, XMIN,XMAX, YMIN,YMAX, ESWKVP, KSWKVP)
      IF (KERROR .NE. 0) CALL GKERR (KERROR)
      END
