C# IL>=a, OL>=1
      SUBROUTINE GKPKRQ(IWKID,IPKDN,ISTAT,ISGNAM,IPKID)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To perform a pick request as directed by workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC  Created
*
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP IPKDN   pick device number
*     OUT ISTAT   status
*     OUT ISGNAM  segment name
*     OUT IPKID   pick identifier
*
      INTEGER IWKID,IPKDN,ISTAT,ISGNAM,IPKID
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkpca.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     IREQ    Echoplay request
*     IACT    Actioned request
*
      INTEGER IREQ,IACT
*
*
*  ERRORS
*  ------
*
*---------------------------------------------------------------------

* Initial actioned request set to nil to indicate first entry
      IACT=KNIL

* Loop on pick request
 10   CONTINUE

*     device number
      KWI1=IPKDN
*     actioned request
      KWI2=IACT

      CALL GKSONW(IWKID,KRQPK,1,KDAT,1,QDAT,QDAT,1,CH)
      IF(KERROR.EQ.0) THEN
*       store request
        IREQ=KWI1
*       request is nil if pick complete
        IF(IREQ.EQ.KNIL) THEN
          ISGNAM=KWI2
          IPKID=KWI3
          ISTAT=KWI4
        ELSE
*         perform specified request
          CALL GKPKPB(IWKID)
          IF(KERROR.EQ.0) THEN
*           specified request actioned
            IACT=IREQ
            GOTO 10
          ENDIF
        ENDIF

      ENDIF

* Set state of output attributes on workstation
      CALL GKSGAT(IWKID)

* Clear echoplay
      KPKECO=KNIL

      END
