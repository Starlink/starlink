C# IL>=b, OL>=0
      SUBROUTINE GQVLS (IWKID,IDNR,MLDR,IER,IMODE,IESW,VALUE,IPET,
     :                     EAREA,VALMN,VALMX,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE VALUATOR DEVICE STATE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns valuator device state.
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
*     INP IDNR    valuator device number
*     INP MLDR    dimension of data record
*     OUT IER     error indicator
*     OUT IMODE   operating mode
*     OUT IESW    echo switch
*     OUT VALUE   initial value
*     OUT IPET    prompt and echo type
*     OUT EAREA   echo area
*     OUT VALMN   minimum value
*     OUT VALMX   maximum value
*     OUT LDR     length of data record
*     OUT DATREC  data record
*
      INTEGER IWKID, IDNR, MLDR, IER, IMODE, IESW, IPET, LDR
      REAL VALUE, EAREA(4), VALMN, VALMX
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
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

        CALL GKQXXD (IWKID,KQVLS,IDNR,1,1,MLDR,0,IER,IMODE,IESW,
     :                  IPET,EAREA,NID,KDAT,NRD,QDAT,QDAT,LDR,DATREC,
     :                  I,I,I,VALUE,VALMN,VALMX)

      ELSE
         IER = KERROR
      ENDIF

      END
