      SUBROUTINE GKUREI ( STR, N )
*
* (C) COPYRIGHT ICL & SERC  1987
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             CJC
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Unpack a string back into an integer for the data record utility
*     GUREC. This is the inverse of GKPREI.
*
*  MAINTENANCE LOG
*  ---------------
*     22/01/87  CJC   IS conversion. GUREC rewritten to IS specification
*                        - required an unpack utility
*     10/03/87  CJC  Stabilisation of new version.
*
      INCLUDE '../include/check.inc'
*
*  EXTERNAL FUNCTION DEFINITION
*  ----------------------------
      INTEGER GKNA1
*
*  ARGUMENTS
*  ---------
*     INP   STR    The string to be upacked
*     OUT   N      The integer to return it in
      CHARACTER STR(5)
      INTEGER N
*
*  COMMENT
*  -------
*     This routine is only used by GUREC.
*
*---------------------------------------------------------------------
      N = (((MOD(GKNA1(STR(5)),8)*128 + GKNA1(STR(4)))*128
     :     + GKNA1(STR(3)))*128 + GKNA1(STR(2)))*128 + GKNA1(STR(1))
      IF ( GKNA1(STR(5)).GE.8 ) N = -N
      END
