C# IL>=b, OL>=0
      SUBROUTINE GQLCS (IWKID,IDNR,ITYPE,MLDR,IER,IMODE,IESW,ITNR,
     :                     PX,PY,IPET,EAREA,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE LOCATOR DEVICE STATE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns locator device state.
*
*  MAINTENANCE LOG
*  ---------------
*     03/10/83  AS    Original version stabilized
*     10/02/84  JL    Call to GKPRLG inserted (I89)
*     10/02/84  JL    Change parameters to GKQXXD to make
*                     NID & NRD distinct variables (I114)
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP IDNR    locator device number
*     INP ITYPE   type of returned values
*     INP MLDR    dimension of data record
*     OUT IER     error indicator
*     OUT IMODE   operating mode
*     OUT IESW    echo switch
*     OUT ITNR    initial normalization transformation number
*     OUT PX      initial locator position in WC
*     OUT PY      initial locator position in WC
*     OUT IPET    prompt and echo type
*     OUT EAREA   echo area
*     OUT LDR     length of data record
*     OUT DATREC  data record
*
      INTEGER IWKID, IDNR, ITYPE, MLDR, IER, IMODE, IESW, ITNR, IPET,LDR
      REAL PX, PY, EAREA(4)
      CHARACTER*(*) DATREC(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I, NID, NRD
      REAL R, XWC(1), YWC(1)
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

        CALL GKQXXD (IWKID,KQLCS,IDNR,1,1,MLDR,0,IER,IMODE,IESW,
     :                  IPET,EAREA,NID,KDAT,NRD,XWC,YWC,LDR,DATREC,
     :                  ITNR,I,I,R,R,R)
        PX = XWC(1)
        PY = YWC(1)

      ELSE
         IER = KERROR
      ENDIF

      END
