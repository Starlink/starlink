C# IL>=b, OL>=0
      SUBROUTINE GQDLC (IWTYPE,IDCNR,NTH,MLDR,IER,DPX,DPY,NPETL,
     :                     IPET,EAREA,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE DEFAULT LOCATOR DEVICE DATA
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns default locator device data.
*
*  MAINTENANCE LOG
*  ---------------
*     04/10/83  AS    Original version stabilized
*     10/02/84  JL    Call to GKPRLG inserted (I89)
*     10/02/84  JL    Change parameters to GKQXXD to make
*                     NID & NRD distinct variables (I114)
*     11/04/91  KEVP  Get default initial locator point from
*                     the correct arguments of GKQXXD (C70).
*
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE  workstation type
*     INP IDCNR   logical input device number
*     INP NTH     list element requested
*     INP MLDR    dimension of data record
*     OUT IER     error indicator
*     OUT DPX     default initial locator position
*     OUT DPY     default initial locator position
*     OUT NPETL   number of available prompt and echo types
*     OUT IPET    Nth element of list of available prompt and echo types
*     OUT EAREA   default echo area
*     OUT LDR     length of data record
*     OUT DATREC  default data record
*
      INTEGER IWTYPE, IDCNR, NTH, MLDR, IER, NPETL, IPET, LDR
      REAL DPX, DPY, EAREA(4)
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
*     Dummy output arguments for GKQXXD
      INTEGER I, NID, NRD
      REAL R, XWC(1), YWC(1)
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IF (KERROR.EQ.0) THEN

        CALL GKQXXD(IWTYPE,KQDLC,IDCNR,1,1,MLDR,NTH,IER,I,NPETL,
     :                 IPET,EAREA,NID,KDAT,NRD,XWC,YWC,LDR,DATREC,
     :                 I,I,I,DPX,DPY,R)

      ELSE
         IER = KERROR
      ENDIF

      END
