C# IL>=b, OL>=1
      SUBROUTINE GQDPK (IWTYPE,IDCNR,NTH,MLDR,IER,NPETL,
     :                     IPET,EAREA,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE DEFAULT PICK DEVICE DATA
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns default pick device data
*
*  MAINTENANCE LOG
*  ---------------
*     04/10/83  AS    Original version stabilized
*     10/02/84  JL    Call to GKPRLG inserted (I89)
*     10/02/84  JL    Change parameters to GKQXXD to make
*                     NID & NRD distinct variables (I114)
*     18/05/87  DCS   Correct valid states passed to GKPRLG (S262).
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE  workstation type
*     INP IDCNR   logical input device number
*     INP NTH     list element requested
*     INP MLDR    dimension of data record
*     OUT IER     error indicator
*     OUT NPETL   number of available prompt and echo types
*     OUT IPET    Nth element of list of available prompt and echo types
*     OUT EAREA   default echo area
*     OUT LDR     length of data record
*     OUT DATREC  default data record
*
      INTEGER IWTYPE, IDCNR, NTH, MLDR, IER, NPETL, IPET, LDR
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
      INTEGER I, NID, NRD
      REAL R
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IF (KERROR.EQ.0) THEN

        CALL GKQXXD(IWTYPE,KQDPK,IDCNR,1,1,MLDR,NTH,IER,I,NPETL,
     :                 IPET,EAREA,NID,KDAT,NRD,QDAT,QDAT,LDR,DATREC,
     :                 I,I,I,R,R,R)

      ELSE
         IER = KERROR
      ENDIF

      END
