C# IL>=a, OL>=0
      SUBROUTINE GKSRPL(N,LTYPES,COLOUR)
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
*     Set polyline representation
*
*  MAINTENANCE LOG
*  ---------------
*     02/12/83  AS    Original version stabilized
*     02/07/86  DCS   Name of local variable for bundle table entry
*                     changed to prevent corruption by linetype check
*                     (S85).
*     22/01/87  JCS   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     INP  N       Number of linetypes in list
*     INP  LTYPES  List of linetypes
*     INP  COLOUR  Colour regeneration
*
      INTEGER N, LTYPES(*)
      LOGICAL COLOUR
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkplb.cmn'
      INCLUDE '../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*     IENT    Entry in polyline bundle table corresponding to polyline
*             index in KWI1
*
      INTEGER IENT, I
*
*  ERRORS
*  ------
*        64 Linetype not supported
*        93 Colour index invalid
*     -1004 No room for another bundle
*
*---------------------------------------------------------------------



* Find out if index exists or there is room for another bundle
      DO 10 IENT=1,KMXPLB
       IF(KWI1.EQ.KPLI(IENT,KWKIX) .OR. KPLI(IENT,KWKIX).EQ.KNIL)GOTO 20
   10 CONTINUE

* No room
      KERROR = -1004
      GOTO 999

   20 CONTINUE
* Check if linetype supported
      IF (KWI2.GE.1 .AND. KWI2.LE.4) GOTO 50
      DO 40 I=1,N
        IF (KWI2.EQ.LTYPES(I)) GOTO 50
   40 CONTINUE
* Linetype not supported
      KERROR = 64
      GOTO 999

   50 CONTINUE
* Check if colour index valid
      IF (KWI3.GE.0 .AND. KWI3.LT.KPCI(KWKIX)) THEN
* See if regeneration will be necessary
        IF (KDSMT(KWKIX).EQ.GNEMPT .AND.
     :      (KLNTY(IENT,KWKIX).NE.KWI2 .OR.
     :      (KPLCI(IENT,KWKIX).NE.KWI3 .AND. COLOUR))) THEN
          IF (KIMRGM(KWKIX).EQ.GALLOW) THEN
            KRGN = .TRUE.
            KWRGN(KWKIX) = .TRUE.
          ELSE
            KNFAUP(KWKIX) = GYES
          ENDIF
        ENDIF
* Now set representation
        KPLI(IENT,KWKIX) = KWI1
        KLNTY(IENT,KWKIX) = KWI2
        QLNWD(IENT,KWKIX) = QWR1
        KPLCI(IENT,KWKIX) = KWI3
      ELSE
* Colour index invalid
        KERROR = 93
      ENDIF


  999 CONTINUE
      END
