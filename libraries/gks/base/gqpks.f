C# IL>=b, OL>=1
      SUBROUTINE GQPKS (IWKID,IDNR,ITYPE,MLDR,IER,IMODE,IESW,ISTAT,
     :                     ISGNA,IPKID,IPET,EAREA,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PICK DEVICE STATE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns pick device state.
*
*  MAINTENANCE LOG
*  ---------------
*     03/10/83  AS    Original version stabilized
*     10/02/84  JL    Call to GKPRLG inserted (I89)
*     10/02/84  JL    Change parameters to GKQXXD to make
*                     NID & NRD distinct variables (I114)
*     29/02/84  JL    Changed second IER in parameter list to ISTAT (I155)
*     18/05/87  DCS   Set IER (rather than ISTAT) to KERROR (S261).
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP IDNR    pick device number
*     INP ITYPE   type of returned values
*     INP MLDR    dimension of data record
*     OUT IER     error indicator
*     OUT IMODE   operating mode
*     OUT IESW    echo switch
*     OUT ISTAT   initial status
*     OUT ISGNA   initial segment
*     OUT IPKID   initial pick identifier
*     OUT IPET    prompt and echo type
*     OUT EAREA   echo area
*     OUT LDR     length of data record
*     OUT DATREC  data record
*
      INTEGER IWKID, IDNR, ITYPE, MLDR, IER, IMODE, IESW, ISTAT, ISGNA,
     :        IPKID, IPET, LDR
      REAL EAREA(4)
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
      INTEGER  NID, NRD
      REAL     R
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

        CALL GKQXXD (IWKID,KQPKS,IDNR,1,1,MLDR,ITYPE,IER,IMODE,IESW,
     :                  IPET,EAREA,NID,KDAT,NRD,QDAT,QDAT,LDR,DATREC,
     :                  ISTAT,ISGNA,IPKID,R,R,R)

      ELSE
         IER = KERROR
      ENDIF

      END
