C# IL>=a, OL>=0
      SUBROUTINE GKSRFA(COLOUR)
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
*     Set fill area representation
*
*  MAINTENANCE LOG
*  ---------------
*     02/12/83  AS    Original version stabilized
*     02/07/86  DCS   Change local I to IENT for consistency with other
*                     GKSRxx routines.
*     22/01/87  JCS   IS conversion. Error number changes.
*     22/01/87  RMK   IS conversion. Change to hatch style check.
*     23/08/89  RMK   Corrected check on pattern index (S347) and quit if
*                     it is invalid (S206). Removed unused local variables.
*
*  ARGUMENTS
*  ---------
*     INP  COLOUR  Colour regeneration
*
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
      INCLUDE '../include/gkfab.cmn'
      INCLUDE '../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*     IENT    Entry in fill area bundle table corresponding to fill area
*             index in KWI1
*
      INTEGER IENT
*
*  ERRORS
*  ------
*       85  Pattern index is invalid
*       86  Hatch style not supported
*       93  Colour index invalid
*    -1004  No room for another bundle
*
*---------------------------------------------------------------------



* Find out if index exists or there is room for another bundle
      DO 10 IENT=1,KMXFAB
       IF(KWI1.EQ.KFAI(IENT,KWKIX) .OR. KFAI(IENT,KWKIX).EQ.KNIL)GOTO 20
   10 CONTINUE

* No room
      KERROR = -1004
      GOTO 999

   20 CONTINUE
      IF (KWI2.EQ.GHATCH) THEN
* Check if hatch style valid
        IF (KWI3 .GE. -10 .AND. KWI3 .LE. -1) GOTO 30
        KERROR = 86
        GOTO 999
      ELSEIF (KWI2.EQ.GPATTR) THEN
* Check if pattern index valid i.e. > 0
        IF (KWI3 .GT. 0) GOTO 30
        KERROR = 85
        GOTO 999
      ENDIF
  30  CONTINUE

* Check if colour index valid
      IF (KWI4.GE.0 .OR. KWI4.LT.KPCI(KWKIX)) THEN
* See if regeneration will be necessary
        IF (KDSMT(KWKIX).EQ.GNEMPT .AND.
     :      (KIS(IENT,KWKIX).NE.KWI2 .OR. KSI(IENT,KWKIX).NE.KWI3 .OR.
     :       KFACI(IENT,KWKIX).NE.KWI4 .AND. COLOUR) ) THEN
          IF (KIMRGM(KWKIX).EQ.GALLOW) THEN
            KRGN = .TRUE.
            KWRGN(KWKIX) = .TRUE.
          ELSE
            KNFAUP(KWKIX) = GYES
          ENDIF
        ENDIF
* Now set representation
        KFAI(IENT,KWKIX) = KWI1
        KIS(IENT,KWKIX) = KWI2
        KSI(IENT,KWKIX) = KWI3
        KFACI(IENT,KWKIX) = KWI4
      ELSE
* Colour index invalid
        KERROR = 93
      ENDIF


  999 CONTINUE
      END
