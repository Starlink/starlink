      SUBROUTINE GKPREI ( N, STR )
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
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Pack on integer into a character string for the data record
*     utility GPREC. GKUREI is an inverse.
*
*  MAINTENANCE LOG
*  ---------------
*     22/01/87  CJC   IS conversion. GPREC rewritten to IS specification
*                     - required a pack utility
*     10/03/87  CJC   Stabilisation of new version.
*
*  EXTERNAL FUNCTION DEFINITION
*  ----------------------------

      CHARACTER*1 GKAN1

*
*  ARGUMENTS
*  ---------
*     INP   N      The integer to be packed
*     OUT   STR    The 5 characters to pack N in
      INTEGER N
      CHARACTER STR(5)
*
*  LOCALS
*  ------
*     IP     Copy  of N with sign removed which is manipulated
*     IT     Intermediate value
*     IS     sign of N - 0 positive, 8 negative
      INTEGER IP, IT, IS
*
*  ALGORITHM
*  ---------
*     The integer is assumed to be of 31 bits significance. It
*     is packed 7 bits at a time into STR - least significant part first.
*     Only 3 bits of the last character are used: 31-7*4 =3. This
*     Leaves the 4th bit (8=2**3) to be used for the sign.
*
*  COMMENT
*  -------
*     This routine is only used by OPREC. Other packing
*     routines are used elsewhere.
*
*---------------------------------------------------------------------
*  Detach the sign
      IF (N.LT.0) THEN
        IP = -N
        IS = 8
      ELSE
        IP = N
        IS = 0
      ENDIF
*  Pull the integer apart - least significant part first
      IT = IP
      IP = IT / 128
      STR(1) = GKAN1(IT-IP*128)
      IT = IP
      IP = IT / 128
      STR(2) = GKAN1(IT-IP*128)
      IT = IP
      IP = IT / 128
      STR(3) = GKAN1(IT-IP*128)
      IT = IP
      IP = IT / 128
      STR(4) = GKAN1(IT-IP*128)
      STR(5) = GKAN1(IP+IS)
      END
