C# IL>=a, OL>=0
      SUBROUTINE GKIWSB(IWKIX,IWKTY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Allocate and predefine bundles
*
*  MAINTENANCE LOG
*  ---------------
*     01/03/83  AS    Original version stabilized
*     18/03/83  AS    Sort out colour tables
*     12/04/83  AS    Change argument list for call to GKQWDT
*     14/06/83  AS    Use heap for colour table
*     24/06/83  AS    Change errors, do patterns
*     18/07/83  AS    Fix call to HPDA, and pattern handling
*     15/10/84  CJW   Fix bug I226 - only deallocate heap for patterns
*                     when necessary.
*     09/04/86  DLT   Use routine GKRDCT to read all of colour table.
*     20/01/87  ARG   IS conversion. Error comment removed.
*     03/06/87  RMK   Merged ICL and RAL versions of this routine.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     26/10/89  RMK   Fix S380 from NMH: set KPABPT to KNIL if
*                     workstation has no patterns.
*
*  ARGUMENTS
*  ---------
*     INP IWKIX - workstation index
*     INP IWKTY - workstation type
*
      INTEGER IWKIX,IWKTY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkplb.cmn'
      INCLUDE '../include/gkpmb.cmn'
      INCLUDE '../include/gktxb.cmn'
      INCLUDE '../include/gkfab.cmn'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER INTA(20), I, J, IOFF, IPATD, IERROR
      REAL REALA(20)
*
*  ERRORS
*  ------
*
*---------------------------------------------------------------------


* Initialise predefined polyline bundles

      DO 10 I=1,KPPLI(IWKIX)
        CALL GKQWDT(IWKTY,KPPLB,I,3,1,INTA,REALA)
        IF (KERROR.NE.0) GOTO 999
        KPLI(I,IWKIX)  = INTA(1)
        KLNTY(I,IWKIX) = INTA(2)
        QLNWD(I,IWKIX) = REALA(1)
        KPLCI(I,IWKIX) = INTA(3)
   10 CONTINUE

* Initialise rest of polyline bundles to undefined

      DO 15 I=KPPLI(IWKIX)+1,KMXPLB
        KPLI(I,IWKIX) = KNIL
   15 CONTINUE



* Initialise predefined polymarker bundles

      DO 20 I=1,KPPMI(IWKIX)
        CALL GKQWDT(IWKTY,KPPMB,I,3,1,INTA,REALA)
        IF (KERROR.NE.0) GOTO 999
        KPMI(I,IWKIX)  = INTA(1)
        KMKTY(I,IWKIX) = INTA(2)
        QMKSZ(I,IWKIX) = REALA(1)
        KPMCI(I,IWKIX) = INTA(3)
   20 CONTINUE

* Initialise rest of polymarker bundles to undefined

      DO 25 I=KPPMI(IWKIX)+1,KMXPMB
        KPMI(I,IWKIX) = KNIL
   25 CONTINUE



* Initialise predefined text bundles

      DO 30 I=1,KPTXI(IWKIX)
        CALL GKQWDT(IWKTY,KPTXB,I,4,2,INTA,REALA)
        IF (KERROR.NE.0) GOTO 999
        KTXI(I,IWKIX)  = INTA(1)
        KTXFN(I,IWKIX) = INTA(2)
        KTXPR(I,IWKIX) = INTA(3)
        QCHXP(I,IWKIX) = REALA(1)
        QCHSP(I,IWKIX) = REALA(2)
        KTXCI(I,IWKIX) = INTA(4)
   30 CONTINUE

* Initialise rest of text bundles to undefined

      DO 35 I=KPTXI(IWKIX)+1,KMXTXB
        KTXI(I,IWKIX) = KNIL
   35 CONTINUE



* Initialise predefined fill area bundles

      DO 40 I=1,KPFAI(IWKIX)
        CALL GKQWDT(IWKTY,KPFAB,I,4,0,INTA,REALA)
        IF (KERROR.NE.0) GOTO 999
        KFAI(I,IWKIX)  = INTA(1)
        KIS(I,IWKIX)   = INTA(2)
        KSI(I,IWKIX)   = INTA(3)
        KFACI(I,IWKIX) = INTA(4)
   40 CONTINUE

* Initialise rest of fill area bundles to undefined

      DO 45 I=KPFAI(IWKIX)+1,KMXFAB
        KFAI(I,IWKIX) = KNIL
   45 CONTINUE



* Initialise patterns

      IF(KMXPAB(IWKIX).GT.0) THEN
* Initialise heap and stack pointer to KNIL so that if error can deallocate
        KPABPT(IWKIX) = KNIL
        IOFF = KNIL
* Create heap directory for patterns
        CALL GKDRCR(KMXPAB(IWKIX),3,0,KPABPT(IWKIX))
        IF (KERROR.NE.0) GOTO 999

        DO 52 I=1,KPPAI(IWKIX)
* Get pattern dimensions
          CALL GKQWDT(IWKTY,KPPASZ,I,3,0,INTA,REALA)
          IF (KERROR.NE.0) GOTO 888
          IPATD = INTA(2)*INTA(3)
* Allocate space for pattern in heap with index in INTA(4)
          CALL GKHPAL(IPATD,KINTGS,INTA(4))
          IF (KERROR.NE.0) GOTO 888
* Fill directory for this pattern
          CALL GKDRPU(KPABPT(IWKIX),INTA(1),3,0,INTA(2),REALA)
          IF (KERROR.NE.0) GOTO 888
* Allocate stack for pattern since cannot be sure heap will not change
          CALL GKSTAL(KINTGS,IPATD,IOFF)
          IF (KERROR.NE.0) GOTO 888
* Get pattern
          CALL GKQWDT(IWKTY,KPPAB,I,IPATD,0,KSTACK(IOFF),REALA)
          IF (KERROR.NE.0) GOTO 888
* Copy pattern from stack to heap
          DO 54 J=1,IPATD
            KHP(KHPXI(INTA(4))+J-1) = KSTACK(IOFF+J-1)
   54     CONTINUE
* Deallocate stack
          CALL GKSTDA(KINTGS,IOFF)
          IF (KERROR.NE.0) GOTO 888
          IOFF = -1
   52   CONTINUE
      ELSE
* If no patterns, set heap pointer to KNIL
        KPABPT(IWKIX) = KNIL
      ENDIF

* Initialize predefined colour table

      CALL GKRDCT(IWKTY, IWKIX)
      GOTO 999

  888 CONTINUE
* Deallocations if error occurs for pattern
* Must save error flag and reinstate before exit
      IERROR = KERROR
* Only bother deallocating patterns if there are any.
* Only deallocate heap storage if DRGE works
      IF(KMXPAB(IWKIX).GT.0) THEN
        CALL GKSTDA(KINTGS,IOFF)
        DO 889 I=1,KPPAI(IWKIX)
          KERROR = 0
          CALL GKDRGE(KPABPT(IWKIX),I,3,0,INTA,REALA)
          IF (KERROR .EQ. 0) CALL GKHPDA(INTA(3),KINTGS)
  889   CONTINUE
        CALL GKDRDL(KPABPT(IWKIX))
      ENDIF
      KERROR = IERROR

  999 CONTINUE
      END
