C# IL>=a, OL>=0
      SUBROUTINE GTX(X,Y,STRING)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  TEXT
*  Author:             PMB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level text output primitive routine
*
*  MAINTENANCE LOG
*  ---------------
*     18/12/82  PMB   Original version stabilized
*     17/02/83  PGLS  Call to PRLG changed.
*     09/03/83  FY    test transformation and source flags
*     23/03/83  FY    add comments on COMMON block usage
*     05/05/83  FY    call gkctxg instead of gkstxg
*     13/07/83  CDO   prevent incest
*     11/11/83  AS    Get it to work
*     19/02/86  DCS   Update 'last source' flag here rather than GKCTXG.
*     20/02/86  DCS   Replace call to GKSCTG by its contents.
*     19/01/87  DCS   IS conversion. Update COMMON block usage.
*
*  ARGUMENTS
*  ---------
*     INP   X,Y     Text coordinates
*     INP   STRING  String to be displayed
*
      REAL X,Y
      CHARACTER*(*) STRING
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/ KSTRWK, KSTXWK
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NOCHS  Number of chars in string
*     IOFF   Allocated stack offset
*
      INTEGER NOCHS, IOFF
*
*  STACK USAGE
*  -----------
*     NOCHS    INTEGER  used to expand text to 1 char/INTEGER
*
*---------------------------------------------------------------------

* check GKS state

      IOFF = KNIL
      CALL GKPRLG (ETX,GWSAC,GSGOP)
      IF (KERROR.NE.0) GOTO 888
      IF (KSTRWK .NE. KGKSFN) THEN
        CALL GKCCTG
        CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
        IF (KERROR .NE. 0) GOTO 888
        KSTRWK=KGKSFN
      ENDIF
      IF (KSTXWK .NE. KGKSFN) THEN
        CALL GKCTXG
        CALL GKSACW(KSTXA,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KERROR .NE. 0) GOTO 888
        KSTXWK=KGKSFN
      ENDIF

* get length of string and allocate space on stack for conversion
* to internal ascii representation

      NOCHS=LEN(STRING)
      CALL GKSTAL(KINTGS,NOCHS,IOFF)
      IF (KERROR.NE.0) GOTO 888

* if room then convert

      CALL GKNTOA(NOCHS,STRING,KSTACK(IOFF))
      IF (KERROR .NE. 0) GOTO 888

* call workstation entry point

      QWR1=X
      QWR2=Y
      CALL GKSACW(KTX,NOCHS,KSTACK(IOFF),1,QDAT,QDAT,1,CH)
      IF (KERROR .NE. 0) GOTO 888

* regenerate if needed

      IF (KRGN) THEN
        CALL GKRGN
        IF (KERROR .NE. 0) GOTO 888
      ENDIF
      GOTO 999


  888 CONTINUE
      CALL GKERR(KERROR)

* release space from stack
  999 CONTINUE
      CALL GKSTDA(KINTGS,IOFF)

      END
