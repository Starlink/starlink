C# IL>=a, OL>=0
      SUBROUTINE GKXCPU(STRING,IDEN,ISTAT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             PB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Put character string onto the Cache
*
*  MAINTENANCE LOG
*  ---------------
*     14/11/83  PB    Original version stabilized
*     16/04/86  RMK   Changed to use GKAN1 instead of CHAR (S103).
*     13/02/87  PKY   Changes to accomodate the fact that CACHE
*                     has changed from CHARACTER*20000 CACHE to
*                     CHARACTER*1 CACHE(20000) (see GKXCA.CMN)
*     11/06/87  RMK   Merged GKS-UK and RAL versions of this routine.
*
*  ARGUMENTS
*  ---------
*     INP   STRING Character string
*     OUT   IDEN   Position string was stored in Cache
*     OUT   ISTAT  Status
*
      INTEGER IDEN, ISTAT
      CHARACTER*(*) STRING
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /TCA/    Cache
*
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkxca.cmn'
*
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
*
      CHARACTER*1 GKAN1
*
*  LOCALS
*  ------
*     LENGTH  Length of string
*
      INTEGER LENGTH, I
*
*---------------------------------------------------------------------

*     if not room then empty Cache and give failure reply
*     (as simplest option)

      LENGTH = LEN(STRING)
      IF (KFREE+LENGTH+1.GT.KMXTXZ) THEN

* Empty cache
        DO 1 I = 1,KFNTMX
          KFNAMS(I) = 0
    1   CONTINUE
        DO 2 I = 1,KMXICN
          KCHNAM(I) = 0
    2   CONTINUE
        KFREE = 1

        ISTAT = KMXTXZ
      ELSE

*     else put string length onto Cache followed by the string itself

        ISTAT = 0
        IDEN = KFREE
        CACHE(KFREE) = GKAN1(LENGTH/128)
        CACHE(KFREE+1) = GKAN1(MOD(LENGTH,128))
        KFREE = KFREE+2
        DO 10 I=1,LENGTH
          CACHE(KFREE+I-1) = STRING(I:I)
   10   CONTINUE
        KFREE = KFREE+LENGTH
      ENDIF

      END
