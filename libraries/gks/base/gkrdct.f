C# IL>=a, OL>=0
      SUBROUTINE GKRDCT( IWKTY, IWKIX)
*
* (C) COPYRIGHT ICL & SERC  1986
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             DLT
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Read predefined colour table into heap
*
*  MAINTENANCE LOG
*  ---------------
*     29/04/86  DLT   Original version stabilized.
*     22/01/87  JCS   IS conversion. Remove comment reporting error.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     30/05/90  KEVP  Read colour table only if there is at least
*                     one colour index (S268).
*                     Tidied up heap allocation/deallocation (S259).
*
*  ARGUMENTS
*  ---------
*    IWKTY  INP   Workstation type
*    IWKIX  INP   Workstation index
*
      INTEGER IWKTY, IWKIX
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkplb.cmn'
      INCLUDE '../include/gkpmb.cmn'
      INCLUDE '../include/gktxb.cmn'
      INCLUDE '../include/gkfab.cmn'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
*
*  LOCALS
*  ------
*
      INTEGER INTA(100), ITEM, NITEM, MORE, IISZ, IRSZ, NEXT
      INTEGER I, J, ITEMS
      REAL REALA(300)
*
*---------------------------------------------------------------------


      KCTBPT(1,IWKIX) = KNIL
      KCTBPT(2,IWKIX) = KNIL
      KCTBPT(3,IWKIX) = KNIL

*   Check number of colour indices - quit if not positive
      IF(KPCI(IWKIX) .LT. 1) GOTO 999

*   Allocate heap
      CALL GKHPAL(KPCI(IWKIX),KREALS,KCTBPT(1,IWKIX))
      IF (KERROR.NE.0) GOTO 999
      CALL GKHPAL(KPCI(IWKIX),KREALS,KCTBPT(2,IWKIX))
      IF (KERROR.NE.0) GOTO 988
      CALL GKHPAL(KPCI(IWKIX),KREALS,KCTBPT(3,IWKIX))
      IF (KERROR.NE.0) GOTO 977

*   Find record number of CT WDT entry
      CALL GKLWDT( IWKTY, KPCTB, 20, REALA, ITEM)
      IF (KERROR.NE.0) GO TO 888

*   Read colour table
      ITEMS = 0
   60 CONTINUE
         READ (KWDFLU,REC=ITEM,IOSTAT=KERROR) NITEM,NEXT,MORE,
     :          (IISZ, IRSZ,
     :          (INTA(I+(J-1)), I = 1,IISZ),
     :          (REALA(I+ 3*(J-1)),I = 1,IRSZ), J = 1,NITEM)
         IF (KERROR.NE.0) GO TO 888

         ITEM = NEXT


*      Copy to heap
         DO 40 I = 1, NITEM
            QHP(KHPXR(KCTBPT(1,IWKIX))+INTA(I)) = REALA(I*3 - 2)
            QHP(KHPXR(KCTBPT(2,IWKIX))+INTA(I)) = REALA(I*3 - 1)
            QHP(KHPXR(KCTBPT(3,IWKIX))+INTA(I)) = REALA(I*3)
   40    CONTINUE

         ITEMS = ITEMS + NITEM
         IF (ITEMS.LT.KPCI(IWKIX)) GO TO 60

      GOTO 999

  888 CONTINUE

* Deallocations if error occured
      CALL GKHPDA(KCTBPT(3,IWKIX),KREALS)
  977 CALL GKHPDA(KCTBPT(2,IWKIX),KREALS)
  988 CALL GKHPDA(KCTBPT(1,IWKIX),KREALS)

  999 CONTINUE
      END
