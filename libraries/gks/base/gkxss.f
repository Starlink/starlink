C# IL>=a, OL>=0
      SUBROUTINE GKXSS(ICH,ISX,ISY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             FY
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Gets hershey strokes.
*     If necessary makes font index current font.
*     Puts characters onto the Cache if not already on there.
*
*  MAINTENANCE LOG
*  ---------------
*     08/11/83  FY    Original version stabilized
*     16/04/86  RMK   Changed to use GKNA1 instead of ICHAR (S103).
*     13/02/87  PKY   Changes to accomodate the fact that CINDEX has
*                     changed from CHARACTER*(KCXISZ) CINDEX to
*                     CHARACTER*1 CINDEX(KCXISZ) (see GKXFD.CMN)
*     11/06/87  RMK   Merged GKS-UK and RAL versions of this routine.
*
*  ARGUMENTS
*  ---------
*     INP   ICH    Character
*     OUT   ISX  X coordinates of stroke
*     OUT   ISY  Y coordinates of stroke
*
      INTEGER ICH,ISX(*),ISY(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkxfd.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  EXTERNAL FUNCTION DEFINITION
*  ----------------------------
*
      INTEGER GKNA1
*
*  LOCALS
*  ------
*     I
*     J
*     K
*     NUMREC Record number of block
*     ICN    Character number reduced by KBEGIN (index into font)
*     ICNDEF First Character defined in block
*     NCHARS Number of characters in block
*     ITOP   Top half of byte
*     ISTAT  indicating success of request to GKTCPU
*
      INTEGER I, J, K, NUMREC, ICN, ICNDEF, NCHARS, ITOP, ISTAT
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     KDBFLU   disk         Input character
*
*---------------------------------------------------------------------


* get characters ICN from font index

      ICN = KFONT(ICH-KBEGIN)

* if character not on the Cache read the record containing that
* character, copy all characters from that record onto the Cache

      IF (KCHNAM(ICN).EQ.0) THEN
        NUMREC = GKNA1(CINDEX(ICN*2-1))*128
     :           +GKNA1(CINDEX(ICN*2))
        READ(UNIT=KDBFLU, REC=NUMREC) CHARBF
        NCHARS = GKNA1(CHARBF(1:1))*128+GKNA1(CHARBF(2:2))
        ICNDEF = GKNA1(CHARBF(3:3))*128+GKNA1(CHARBF(4:4))
  40    I = 5
        DO 100 J = ICNDEF,ICNDEF+NCHARS-1
          K = I-2
  50      K = K+2
          ITOP = GKNA1(CHARBF(K+1:K+1)) + KFMARK
          IF (ITOP.NE.KFMARK) GOTO 50
          CALL GKXCPU(CHARBF(I:K+1),KCHNAM(J),ISTAT)
          IF (ISTAT.NE.0) GOTO 40
          I = K+2
 100    CONTINUE
      ENDIF

* read character from the Cache

      CALL GKXCGE(KCHNAM(ICN),CHARBF)
      I = 0
 200  CONTINUE
      I = I+1
      ISX(I) = GKNA1(CHARBF(I*2-1:I*2-1)) + KFMARK
      ISY(I) = GKNA1(CHARBF(I*2:I*2)) + KFMARK
      IF (ISY(I).NE.KFMARK) GOTO 200

      END
