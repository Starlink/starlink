C# IL>=a, OL>=1
      SUBROUTINE GQWKM ( IER, MXOPWK, MXACWK, MXWKAS )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Inquire Workstation Maximum Numbers
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns Workstation Maximum Numbers
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*     22/11/83  AS   Change MXWKAS = KWK   (previously KASWK)
*     16/01/85  DRJF Bug fix S116. Minimum GKS operating state changed from
*                    GGKCL to GGKOP.
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   MXOPWK Maximum number of simultaneously open workstations
*     OUT   MXACWK Maximum number of simultaneously active workstations
*     OUT   MXWKAS Maximum number of workstations associated with a segment
*
      INTEGER IER, MXOPWK, MXACWK, MXWKAS
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IER = KERROR

      IF (KERROR .EQ. 0) THEN

         MXOPWK =  KWK
         MXACWK =  KWK
         MXWKAS =  KWK

      END IF

      END
