C# IL>=a, OL>=0
      SUBROUTINE GKMTX(NCD,SDR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             DSG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Metafile interpreter Text output primitive routine
*
*  MAINTENANCE LOG
*  ---------------
*     21/07/83  DSG   Original version stabilized
*     10/01/84  DSG   LENGTH set (GUPKC documentation error)
*     17/01/84  DSG   Local character array replaced by CSTR
*     18/01/84  DSG   Changed 'include...' to upper case
*     21/02/86  DCS   Replace call to GKSCTM by its contents.
*                     Remove unused local J.
*     18/03/86  DSG   I246 - Error check done after GKSTAL call.
*     19/01/87  DCS   IS conversion. Remove metafile index and get
*                     text attributes from GKS State List.
*     08/03/91  DCS   Corrected updating of the normalisation
*                     transformation and clipping information -
*                     introduced section of code to set the
*                     data as in GKS State List (S475).
*
*  ARGUMENTS
*  ---------
*     INP   NCD    Length of item data record
*     INP   SDR    Item data record
*
      INTEGER NCD
      CHARACTER*80 SDR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WCA/    Set up vals for wkstn entry
*                     Replace local array by CSTR
*     Modify /GKYSL/  KSTRWK,KSTXWK
*
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkpid.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NOCHS       Number of chars in string
*
      INTEGER NOCHS, IAT, ISET, LENGTH
      PARAMETER (LENGTH =1)

* Local arrays
      INTEGER ILA(1)
      REAL RLA(2)

*
*  STACK USAGE
*  -----------
*     NOCHS    INTEGER  used to expand text to 1 char/INTEGER
*
*
*---------------------------------------------------------------------

      IAT=1
      CALL GKUPR(IAT,SDR,2,RLA)
      IAT=IAT+2*KPDRSZ
      CALL GKUPSI(IAT,SDR,1,ILA)
      IAT=IAT+KPDSSZ
*
*     Bring text attributes up to date
*
      IF (KSTXWK.NE.KGKSFN) THEN
*        Set transformation and clipping information as in GKS state list.
         IF(KSTRWK.NE.KGKSFN)THEN
           CALL GKCCTG
           IF(KERROR.NE.0) GOTO 990
           CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
           IF(KERROR.NE.0) GOTO 990
*          Set flag
           KSTRWK=KGKSFN
         ENDIF
*        Update attributes.
         CALL GKCTXG
         IF(KERROR.NE.0) GOTO 990
         CALL GKSACW(KSTXA,1,KDAT,1,QDAT,QDAT,1,CH)
         IF (KERROR .NE. 0) GOTO 990
*        Set flag
         KSTXWK=KGKSFN
      ENDIF

*
* Set transformation and clip information for metafile interpretation.
*
      IF (KSTRWK.NE.KMI) THEN
         CALL GKCCTM
         IF(KERROR.NE.0) GOTO 990
         CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
         IF(KERROR.NE.0) GOTO 990
*        Set flag
         KSTRWK=KMI
      ENDIF


      NOCHS=ILA(1)
* Allow for possible truncation having occured at input

      IF(NOCHS.GT.(NCD*80-2*KPDRSZ-KPDSSZ)/KPDCSZ) THEN
        KERROR=165
        GOTO 990
      ENDIF

* call workstation entry point

      IF(NOCHS.GT.KWCST) THEN
        KERROR=301
        GOTO 990
      ENDIF

      CALL GKUPC(IAT,SDR,LENGTH,NOCHS,CSTR)
      CALL GKSTAL(KINTGS,NOCHS,ISET)
      IF (KERROR.NE.0) GOTO 990
      CALL GKNTOA(NOCHS,CSTR(1),KSTACK(ISET))
      IF(KERROR.NE.0) GOTO 980
      QWR1=RLA(1)
      QWR2=RLA(2)

      CALL GKSACW(KTX,NOCHS,KSTACK(ISET),1,QDAT,QDAT,1,CH)
      IF (KERROR .NE. 0) GOTO 980

* regenerate if needed

      IF (KRGN) CALL GKRGN

  980 CALL GKSTDA(KINTGS,ISET)

  990 CONTINUE

      END
