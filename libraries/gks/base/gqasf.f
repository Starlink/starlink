C# IL>=a, OL>=0
      SUBROUTINE GQASF (IER,LASF)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CURRENT INDIVIDUAL ATTRIBUTE VALUES
*                      ASPECT SOURCE FLAGS
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Current Individual Attribute Values for the aspect source
*     flags.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     OUT   IER    Error indicator
*     OUT   LASF   Aspect source flags
*
      INTEGER IER, LASF(13)
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

      CALL GKPRLG(KNIL, GGKOP, GSGOP)
      IER = KERROR
      IF (IER.EQ.0) THEN
        LASF(1) = KCPLAF(1)
        LASF(2) = KCPLAF(2)
        LASF(3) = KCPLAF(3)
        LASF(4) = KCPMAF(1)
        LASF(5) = KCPMAF(2)
        LASF(6) = KCPMAF(3)
        LASF(7) = KCTXAF(1)
        LASF(8) = KCTXAF(2)
        LASF(9) = KCTXAF(3)
        LASF(10) = KCTXAF(4)
        LASF(11) = KCFAAF(1)
        LASF(12) = KCFAAF(2)
        LASF(13) = KCFAAF(3)
      ENDIF

      END
