C# IL>=b, OL>=0
      SUBROUTINE GQSKS (IWKID,IDNR,ITYPE,N,MLDR,IER,IMODE,IESW,ITNR,
     :                     NP,PX,PY,IPET,EAREA,IBFLEN,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE STROKE DEVICE STATE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns stroke device state.
*
*  MAINTENANCE LOG
*  ---------------
*     03/10/83  AS    Original version stabilized
*     10/02/84  JL    Call to GKPRLG inserted (I89)
*     10/02/84  JL    Change parameters to GKQXXD to make
*                     NID a distinct variable (I114)
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP IDNR    stroke device number
*     INP ITYPE   type of returned values
*     INP N       dimension of points arrays
*     INP MLDR    dimension of data record
*     OUT IER     error indicator
*     OUT IMODE   operating mode
*     OUT IESW    echo switch
*     OUT ITNR    initial normalization transformation number
*     OUT NP      number of points
*     OUT PX      initial points in stroke
*     OUT PY      initial points in stroke
*     OUT IPET    prompt and echo type
*     OUT EAREA   echo area
*     OUT IBFLEN  buffer length for stroke
*     OUT LDR     length of data record
*     OUT DATREC  data record
*
      INTEGER IWKID, IDNR, ITYPE, N, MLDR, IER, IMODE, IESW, ITNR, NP,
     :        IPET, IBFLEN, LDR
      REAL PX(*), PY(*), EAREA(4)
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
      INTEGER I, NID
      REAL R
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)
      IF (KERROR.EQ.0) THEN

        CALL GKQXXD (IWKID,KQSKS,IDNR,1,N,MLDR,ITYPE,IER,IMODE,IESW,
     :                  IPET,EAREA,NID,KDAT,NP,PX,PY,LDR,DATREC,ITNR,
     :                  IBFLEN,I,R,R,R)

      ELSE
         IER = KERROR
      ENDIF

      END
