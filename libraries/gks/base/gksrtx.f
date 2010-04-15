C# IL>=a, OL>=0
      SUBROUTINE GKSRTX(N,LFONTS,LPRECS,COLOUR)
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
*     Set test representation
*
*  MAINTENANCE LOG
*  ---------------
*     02/12/83  AS    Original version stabilized
*     01/03/84  RSK   Changed loop variable for Font/Precision check
*                     to J, to stop corruption of matched Text bundle
*                     index. Also changed KWI3 to KWI4 in Text Colour
*                     index validity check
*     02/07/86  DCS   Change local I to IENT for consistency with other
*                     GKSRxx routines.
*     21/01/87  ARG   IS conversion. Error numbers changed.
*     23/01/91  PLP   Precision check changed so that precisions lower
*                     than those stored are accepted (S438). Do not use
*                     this fix unless C27 is fixed so that RAL GKS and
*                     not NCC philosophy of font precision pairs is
*                     accepted.
*
*  ARGUMENTS
*  ---------
*     INP  N       Number of linetypes in list
*     INP  LFONTS  List of text fonts
*     INP  LPRECS  List of text precisions
*     INP  COLOUR  Colour regeneration
*
      INTEGER N, LFONTS(*), LPRECS(*)
      LOGICAL COLOUR
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gktxb.cmn'
      INCLUDE '../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*     IENT    Entry in text bundle table corresponding to text index in
*             KWI1
*
      INTEGER IENT, J
*
*  ERRORS
*  ------
*        76  Text font and precision not supported
*        93  Colour index invalid
*     -1004  No room for another bundle
*
*---------------------------------------------------------------------



* Find out if index exists or there is room for another bundle
      DO 10 IENT=1,KMXTXB
       IF(KWI1.EQ.KTXI(IENT,KWKIX) .OR. KTXI(IENT,KWKIX).EQ.KNIL)GOTO 20
   10 CONTINUE

* No room
      KERROR = -1004
      GOTO 999

   20 CONTINUE
* Check if text font and precision valid. Note the .LE. This is
* so that for each font the drivers can store only the highest
* precision supported.
      DO 40 J=1,N
        IF (KWI2.EQ.LFONTS(J) .AND. KWI3.LE.LPRECS(J)) GOTO 50
   40 CONTINUE
* Text font and precision not supported
      KERROR = 76
      GOTO 999

   50 CONTINUE
* Check if colour index valid
      IF (KWI4.GE.0 .AND. KWI4.LT.KPCI(KWKIX)) THEN
* See if regeneration will be necessary
      IF (KDSMT(KWKIX).EQ.GNEMPT .AND.
     :    (KTXFN(IENT,KWKIX).NE.KWI2 .OR. KTXPR(IENT,KWKIX).NE.KWI3 .OR.
     :     QCHXP(IENT,KWKIX).NE.QWR1 .OR. QCHSP(IENT,KWKIX).NE.QWR2 .OR.
     :     (KTXCI(IENT,KWKIX).NE.KWI4 .AND. COLOUR)) ) THEN
          IF (KIMRGM(KWKIX).EQ.GALLOW) THEN
            KRGN = .TRUE.
            KWRGN(KWKIX) = .TRUE.
          ELSE
            KNFAUP(KWKIX) = GYES
          ENDIF
        ENDIF
* Now set representation
          KTXI(IENT,KWKIX) = KWI1
          KTXFN(IENT,KWKIX) = KWI2
          KTXPR(IENT,KWKIX) = KWI3
          QCHXP(IENT,KWKIX) = QWR1
          QCHSP(IENT,KWKIX) = QWR2
          KTXCI(IENT,KWKIX) = KWI4
      ELSE
* Colour index invalid
        KERROR = 93
      ENDIF


  999 CONTINUE
      END
