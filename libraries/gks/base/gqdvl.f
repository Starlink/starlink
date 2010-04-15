C# IL>=b, OL>=0
      SUBROUTINE GQDVL (IWTYPE,IDCNR,NTH,MLDR,IER,DVAL,NPETL,
     :                     IPET,EAREA,VALMN,VALMX,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE DEFAULT VALUATOR DEVICE DATA
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns default valuator device data.
*
*  MAINTENANCE LOG
*  ---------------
*     04/10/83  AS    Original version stabilized
*     10/02/84  JL    Call to GKPRLG inserted (I89)
*     10/02/84  JL    Change parameters to GKQXXD to make
*                     NID & NRD distinct variables (I114)
*     23/12/88  NMH   Change position of EAREA in parameters list for
*                     call to GKQXXD to correspond to order returned by
*                     GKQXXD (S352).
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE  workstation type
*     INP IDCNR   logical input device number
*     INP NTH     list element requested
*     INP MLDR    dimension of data record
*     OUT IER     error indicator
*     OUT DVAL    default initial value
*     OUT NPETL   number of available prompt and echo types
*     OUT IPET    Nth element of list of available prompt and echo types
*     OUT EAREA   default echo area
*     OUT VALMN   minimal value
*     OUT VALMX   maximal value
*     OUT LDR     length of data record
*     OUT DATREC  default data record

      INTEGER IWTYPE, IDCNR, NTH, MLDR, IER, NPETL, IPET, LDR
      REAL DVAL, VALMN, VALMX, EAREA(4)
      CHARACTER*(*) DATREC(*)
*
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


      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IF (KERROR.EQ.0) THEN

         CALL GKQXXD(IWTYPE,KQDVL,IDCNR,1,1,MLDR,NTH,IER,I,NPETL,IPET,
     :                EAREA,NID,KDAT,NRD,QDAT,QDAT,LDR,DATREC,I,I,I,
     :                DVAL,VALMN,VALMX)

      ELSE
         IER = KERROR
      ENDIF

      END
