C# IL>=a, OL>=0
      SUBROUTINE GKCWSB(IWKIX)
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
*     Deallocate heaps used for bundles - colour and patterns.
*
*  MAINTENANCE LOG
*  ---------------
*     25/07/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKIX - workstation index
*
      INTEGER IWKIX
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkwdt.cmn'
*
*  LOCALS
*  ------
*
      INTEGER INTA(3), I
      REAL REALA(1)
*
*---------------------------------------------------------------------


* Deallocate colour tables from heap
      CALL GKHPDA(KCTBPT(1,IWKIX),KREALS)
      CALL GKHPDA(KCTBPT(2,IWKIX),KREALS)
      CALL GKHPDA(KCTBPT(3,IWKIX),KREALS)

* Only bother deallocating patterns if there any
      IF(KMXPAB(IWKIX).GT.0) THEN
* For each pattern bundle, deallocate heap for actual pattern
        DO 10 I=1,KPPAI(IWKIX)
          CALL GKDRGE(KPABPT(IWKIX),I,3,0,INTA,REALA)
          CALL GKHPDA(INTA(3),KINTGS)
   10   CONTINUE
* Delete pattern directory for this workstation
        CALL GKDRDL(KPABPT(IWKIX))
      ENDIF

      END
