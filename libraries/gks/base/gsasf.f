C# IL>=a, OL>=0
      SUBROUTINE GSASF(IASFS)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET ASPECT SOURCE FLAGS
*  Author:             DCS/FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set aspect source flags.
*
*  MAINTENANCE LOG
*  ---------------
*     02/03/83  FY    Original version stabilized
*     01/06/83  AS    Change KASP to 13
*     19/01/87  ARG   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP   IASFS  list of aspect source flags
*
      INTEGER IASFS(13)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYSL/   KCPLAF,KCPMAF,KCTXAF,KCFAAF
*     Modify /GKYSL/   KCPLAF,KCPMAF,KCTXAF,KCFAAF
*                      KSPLWK,KSPMWK,KSTXWK,KSFAWK
*     Read   /gkysl/   khange..(par)
*     Read   /aspct/  klntya,kplcia,kmktya,kpmcia,ktxfna,
*                      ktxcia,kfaisa,kfacia..(par)
*     Read   /gks/     indivi,bundld..(par)
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gaspct.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IPL   offset to aspect list for polyline
*     IPM   offset to aspect list for polymarker
*     ITX   offset to aspect list for text
*     IFA   offset to aspect list for fill area
*     I     loop counter
*
      INTEGER    IPL,  IPM,  ITX, IFA,     I
      PARAMETER (IPL=0,IPM=3,ITX=6,IFA=10)
*
*  ERRORS
*  ------
*     2000  Enumeration type out of range
*
*---------------------------------------------------------------------



      CALL GKPRLG (ESASF,GGKOP,GSGOP)

      IF (KERROR .EQ. 0) THEN

*  test argument, exit if argument invalid

         DO 10 I=1,13
           IF ((IASFS(I) .NE. GBUNDL) .AND. (IASFS(I) .NE. GINDIV)) THEN
             CALL GKERR (2000)
             GOTO 999
           ENDIF
  10     CONTINUE

         DO 30 I=KLNTYA,KPLCIA
           IF (IASFS(I+IPL) .NE. KCPLAF(I)) THEN
             KCPLAF(I) = IASFS(I+IPL)
             KSPLWK = KHANGE
           ENDIF
 30      CONTINUE

         DO 40 I=KMKTYA,KPMCIA
           IF (IASFS(I+IPM) .NE. KCPMAF(I)) THEN
             KCPMAF(I) = IASFS(I+IPM)
             KSPMWK = KHANGE
           ENDIF
 40      CONTINUE

         DO 50 I=KTXFNA,KTXCIA
           IF (IASFS(I+ITX) .NE. KCTXAF(I)) THEN
             KCTXAF(I) = IASFS(I+ITX)
             KSTXWK = KHANGE
           ENDIF
 50      CONTINUE

         DO 60 I=KFAISA,KFACIA
           IF (IASFS(I+IFA) .NE. KCFAAF(I)) THEN
             KCFAAF(I) = IASFS(I+IFA)
             KSFAWK = KHANGE
           ENDIF
 60      CONTINUE

      ELSE
        CALL GKERR(KERROR)
      ENDIF

  999 CONTINUE
      END
