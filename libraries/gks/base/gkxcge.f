C# IL>=a, OL>=0
      SUBROUTINE GKXCGE(IDEN,STRING)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             PM
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Get character string from the Cache
*
*  MAINTENANCE LOG
*  ---------------
*     08/11/83  PM    Original version stabilized
*     16/04/86  RMK   Changed to use GKNA1 instead of ICHAR (S103).
*     13/02/87  PKY   Changes to accomodate the fact that CACHE
*                     has changed from CHARACTER*20000 CACHE to
*                     CHARACTER*1 CACHE(20000) (see GKXCA.CMN)
*     11/06/87  RMK   Merged GKS-UK and RAL versions of this routine.
*
*  ARGUMENTS
*  ---------
*     INP   IDEN   Pointer into Cache
*     OUT   STRING Character string
*
      INTEGER IDEN
      CHARACTER*(*) STRING
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkxca.cmn'
*
*  EXTERNAL FUNCTION DEFINITIONS
*  -----------------------------
*
      INTEGER GKNA1
*
*  LOCALS
*  ------
*     LENGTH Length of string
*     I     Loop variable
*
      INTEGER LENGTH, I
*
*---------------------------------------------------------------------

*     find length of string

      LENGTH = GKNA1(CACHE(IDEN))*128
     :         +GKNA1(CACHE(IDEN+1))

*     get string itself

      DO 10 I=1,LENGTH
         STRING(I:I) = CACHE(IDEN+I+1)
   10 CONTINUE

      END
