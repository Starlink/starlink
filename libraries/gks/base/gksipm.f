C# IL>=b, OL>=0
      SUBROUTINE GKSIPM
*
* (C) COPYRIGHT ICL & SERC  1984
*
*-----------------------------------------------------------------------
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
*     Set input mode.
*
*  MAINTENANCE LOG
*  ---------------
*     18/07/83  AS    Original version stabilized
*     08/07/86  RMK   Change from PJWR 07/01/86 - changed value of
*                     NINT(2) from 9 to 10 in DATA statement,
*                     preventing incorrect setting of error 2017 in
*                     GKDRGE when called from this routine.  Also
*                     changed value of NINT(6) from 7 to 9 for same
*                     reason.
*     30/07/87  PJWR  Changed array name NINT to NINTS,  as the former
*                     is a FORTRAN 77 intrinsic.  Updated NINTS(5), as
*                     pick device has 9 integer data in it's entry.
*                     Increased dimension of INTA to 10,  as it was
*                     too small for stroke data.
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER NINTS(6), NREA(6), INTA(10)
      REAL REALA(7)
      DATA NINTS/5,10,4,6,9,9/
      DATA NREA/6,4,7,4,4,4/
*
*  ERRORS
*  ------
*     140   Specified input device doesn't exist
*
*-----------------------------------------------------------------------


      CALL GKDRGE(KIPDPT(KWI1,KWKIX),KWI2,NINTS(KWI1),NREA(KWI1),
     :              INTA,REALA)
      IF (KERROR.EQ.0) THEN
        INTA(KIPOPM) = KWI3
        INTA(KIPE)   = KWI4
        CALL GKDRPU(KIPDPT(KWI1,KWKIX),KWI2,NINTS(KWI1),NREA(KWI1),
     :                INTA,REALA)
      ELSE
        KERROR = 140
      ENDIF

      END
