C# IL>=a, OL>=0
      SUBROUTINE GKIWSO(IWKIX,IWKTY)
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
*     Initialise part of workstation description table and
*     workstation state list
*
*  MAINTENANCE LOG
*  ---------------
*     01/03/83  AS    Original version stabilized
*     12/04/83  AS    Change argument list for call to GKQWDT
*     19/04/83  AS    Initialise KMXCTB
*     25/04/83  AS    Change INTA for KWDT
*     18/05/83  AS    Correct viewport (-1.0)
*     24/06/83  AS    Change errors
*     09/11/83  AS    NIPDEV goes into common
*     22/11/83  AS    Take out references to segment stuff
*     11/01/84  AS    Initialise segment pointer
*     26/07/88  KEVP  Correct workstation viewport (QDSDX by QDSDY)
*                     as in GKS standard (ISO-7942-1985(E) section 6.5)
*                     (bug C18).
*     24/07/90  PLP   Removed unused local variable I.
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
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER INTA(19)
      REAL REALA(12)
*
*---------------------------------------------------------------------


* Initialise workstation description table

      CALL GKQWDT(IWKTY,KWDT,KNONE,19,12,INTA,REALA)
      IF (KERROR.EQ.0) THEN

        QDSDX(IWKIX)  = REALA(1)
        QDSDY(IWKIX)  = REALA(2)
        KDSRX(IWKIX)  = INTA(1)
        KDSRY(IWKIX)  = INTA(2)
        KLNWD(IWKIX)  = INTA(3)
        QNMLNW(IWKIX) = REALA(3)
        QMNLNW(IWKIX) = REALA(4)
        QMXLNW(IWKIX) = REALA(5)
        KMKSZ(IWKIX)  = INTA(4)
        QNMMKS(IWKIX) = REALA(6)
        QMNMKS(IWKIX) = REALA(7)
        QMXMKS(IWKIX) = REALA(8)
        KCHH(IWKIX)   = INTA(5)
        QMNCHH(IWKIX) = REALA(9)
        QMXCHH(IWKIX) = REALA(10)
        KCHXPF(IWKIX) = INTA(6)
        QMNCHX(IWKIX) = REALA(11)
        QMXCHX(IWKIX) = REALA(12)
        KMXPAB(IWKIX) = INTA(7)
        KPPLI(IWKIX)  = INTA(8)
        KPPMI(IWKIX)  = INTA(9)
        KPTXI(IWKIX)  = INTA(10)
        KPFAI(IWKIX)  = INTA(11)
        KPPAI(IWKIX)  = INTA(12)
        KPCI(IWKIX)   = INTA(13)
        KIPDEV(1,IWKIX) = INTA(14)
        KIPDEV(2,IWKIX) = INTA(15)
        KIPDEV(3,IWKIX) = INTA(16)
        KIPDEV(4,IWKIX) = INTA(17)
        KIPDEV(5,IWKIX) = INTA(18)
        KIPDEV(6,IWKIX) = INTA(19)

* Initialise workstation state list

        KWKTUP(IWKIX)= GNPEND
        KDSMT(IWKIX) = GEMPTY
        QRWWXL(IWKIX)= 0.0
        QRWWXR(IWKIX)= 1.0
        QRWWYB(IWKIX)= 0.0
        QRWWYT(IWKIX)= 1.0
        QCWWXL(IWKIX)= 0.0
        QCWWXR(IWKIX)= 1.0
        QCWWYB(IWKIX)= 0.0
        QCWWYT(IWKIX)= 1.0
        QRWVXL(IWKIX)= 0.0
        QRWVXR(IWKIX)= QDSDX(IWKIX)
        QRWVYB(IWKIX)= 0.0
        QRWVYT(IWKIX)= QDSDY(IWKIX)
        QCWVXL(IWKIX)= 0.0
        QCWVXR(IWKIX)= QDSDX(IWKIX)
        QCWVYB(IWKIX)= 0.0
        QCWVYT(IWKIX)= QDSDY(IWKIX)
        KNFAUP(IWKIX)= GNO
        KSSGPT(IWKIX)= KNIL

      ENDIF

  999 CONTINUE
      END
