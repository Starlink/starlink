C# IL>=a, OL>=0
      SUBROUTINE GKCWSI(IWKIX)
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
*     Deallocate heaps used for input
*
*  MAINTENANCE LOG
*  ---------------
*     25/07/83  AS    Original version stabilized
*     10/12/83  AS    Use delete data record utility GKWDDL
*
*  ARGUMENTS
*  ---------
*     INP IWKIX   workstation index
*
      INTEGER IWKIX
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I, J, NUMDEV, NI,NR, INTA(10)
      REAL REALA(10)
*
*---------------------------------------------------------------------


* For each input class
      DO 30 I=1,6
        IF (KIPDPT(I,IWKIX).NE.KNIL) THEN
* Get number of devices for this class
          CALL GKDRQ(KIPDPT(I,IWKIX),NUMDEV,NI,NR)
* For each device (assume device numbers 1,2,3...)
          DO 20 J=1,NUMDEV
* Get device information
            CALL GKDRGE(KIPDPT(I,IWKIX),J,10,10,INTA,REALA)
* Delete data record
            CALL GKWDDL(INTA(KIPD))
   20     CONTINUE
* Delete directory for this input class
          CALL GKDRDL(KIPDPT(I,IWKIX))
        ENDIF
   30 CONTINUE

      END
