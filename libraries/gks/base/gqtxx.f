C# IL>=a, OL>=0
      SUBROUTINE GQTXX(IWK,XO,YO,STR,IER,XP,YP,TXEXPX,TXEXPY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE TEXT EXTENT
*  Author:             FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     inquire text extent and concatenation point
*
*  MAINTENANCE LOG
*  ---------------
*     09/03/83    FY  Original version stabilized
*     05/05/83    FY  replace routine name by blank when calling gkprlg.
*                     change wkid,errind,cpx,cpy,erstk,
*                     ptr to iwk,ier,xp,yp,ierstk,iptr.
*     05/05/83    FY  call gkctxg instead of gkstxg
*     13/07/83    CDO prevent incest
*     28/09/83    AS  Change subroutine name
*     10/05/84    RSK Fix bug S51 ( Fails for Non-Active workstations)
*     19/02/86    DCS Correct valid states passed to GKPRLG (S89).
*                     Send transformation and attribute data to this
*                     workstation only and restore values afterwards
*                     (S175).
*     20/02/86    DCS Remove unused local IERSTK.
*     22/01/87  DCS   IS conversion. Remove metafile index and Metafile
*                     Input Attribute List and use GKS State List.
*     22/01/87  JCS   IS conversion. Remove error comment - none
*                     detected in this routine.
*
*  ARGUMENTS
*  ---------
*     INP   IWK    workstation id.
*     INP   XO     x-text position
*     INP   YO     y-text position
*     INP   STR    text string
*     OUT   IER    error code
*     OUT   XP     x-concatenation point
*     OUT   YP     y-concatenation point
*     OUT   TXEXPX x-text extent
*     OUT   TXEXPY y-text extent
*
      INTEGER IWK,IER
      CHARACTER*(*) STR
      REAL XP,YP,XO,YO,TXEXPX(4),TXEXPY(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKWKE/  KQTXX
*     Read   /gks/    gkcl,sgop..(par)
*     Read   /gksl/  kgksfn..(par)
*     Read   /GKSL/   KSTRWK,KSTXWK,QCCHUX,QCCHUY,QCCHH
*     Read   /GKWCA/  KDAT,QDAT,QDAT
*     Modify /GKWCA/  QWR(1-8),KERROR
*     Modify /GKSTK/  place text on stack
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NCH     number of chars in string
*     IPTR    pointer to stack
*     RL      length of upvector
*
      INTEGER NCH,IPTR
      REAL    RL
*
*  STACK USAGE
*  -----------
*     NCH      INTEGER  to hold integer code of text
*
*---------------------------------------------------------------------



      CALL GKPRLG (KNIL,GWSOP,GSGOP)
      IER = KERROR
      IF (IER .NE. 0) RETURN

* Send transformation data

      CALL GKCCTG
      CALL GKSONW(IWK, KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
      IER = KERROR
      IF (IER .NE. 0) RETURN

* Send text attributes

      CALL GKCTXG
      CALL GKSONW(IWK,KSTXA,1,KDAT,1,QDAT,QDAT,1,CH)
      IER = KERROR
      IF (IER .NE. 0) RETURN

      NCH = LEN(STR)
      CALL GKSTAL(KINTGS,NCH,IPTR)
      IER = KERROR
      IF (IER .NE. 0) RETURN
      CALL GKNTOA(NCH,STR,KSTACK(IPTR))
      IER = KERROR
      IF (IER .NE. 0) GOTO 990
      QWR1 = XO
      QWR2 = YO
* calculate height and width vectors
* QWR3:wd(x), QWR4:wd(y), QWR5:ht(x), QWR6:ht(y)
      RL = SQRT(QCCHUX*QCCHUX + QCCHUY*QCCHUY)
      QWR3 = (QCCHUY/RL)*QCCHH
      QWR5 = (QCCHUX/RL)*QCCHH
      QWR4 = -QWR5
      QWR6 = QWR3
      CALL GKSONW(IWK,KQTXX,NCH,KSTACK(IPTR),4,TXEXPX,TXEXPY,1,CH)
      IER = KERROR
      IF (IER .NE. 0) GOTO 990
      XP = QWR7
      YP = QWR8

  990 CONTINUE
      CALL GKSTDA(KINTGS,IPTR)

* Send transformation data and text attributes to correspond to 'last
* source' flags if necessary. (Correspond to KGKSFN at present and so
* only send when value of KSTRWK is KMI).
      IF (KSTRWK.EQ.KMI) THEN
        CALL GKCCTM
        CALL GKSONW(IWK,KNT,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KSTXWK.EQ.KGKSFN) THEN
          CALL GKCTXG
          CALL GKSONW(IWK,KSTXA,1,KDAT,1,QDAT,QDAT,1,CH)
        ENDIF
      ENDIF

      END
