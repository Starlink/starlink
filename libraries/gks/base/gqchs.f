C# IL>=b, OL>=0
      SUBROUTINE GQCHS (IWKID,IDNR,MLDR,IER,IMODE,IESW,ISTAT,ICHNR,IPET,
     :                     EAREA,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE CHOICE DEVICE STATE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns choice device state.
*
*  MAINTENANCE LOG
*  ---------------
*     03/10/83  AS    Original version stabilized
*     10/02/84  JL    Call to GKPRLG inserted (I89)
*     10/02/84  JL    Change parameters to GKQXXD to make
*                     NID & NRD distinct variables (I114)
*     21/01/87  RMK   IS conversion. Added initial status to the
*                     argument list, and picked up answer from GKQXXD.
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP IDNR    choice device number
*     INP MLDR    dimension of data record
*     OUT IER     error indicator
*     OUT IMODE   operating mode
*     OUT IESW    echo switch
*     OUT ISTAT   initial status
*     OUT ICHNR   initial choice number
*     OUT IPET    prompt and echo type
*     OUT EAREA   echo area
*     OUT LDR     length of data record
*     OUT DATREC  data record
*
      INTEGER IWKID, IDNR, MLDR, IER, IMODE, IESW, ISTAT,ICHNR,IPET,LDR
      REAL EAREA(4)
      CHARACTER*(*) DATREC(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I, NID, NRD
      REAL R
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

        CALL GKQXXD (IWKID,KQCHS,IDNR,1,1,MLDR,0,IER,IMODE,IESW,
     :                  IPET,EAREA,NID,KDAT,NRD,QDAT,QDAT,LDR,DATREC,
     :                  ICHNR,ISTAT,I,R,R,R)

      ELSE
         IER = KERROR
      ENDIF

      END
