C# IL>=a, OL>=0
      SUBROUTINE GKSRPM(N,MTYPES,COLOUR)
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
*     Set polymarker representation
*
*  MAINTENANCE LOG
*  ---------------
*     02/12/83  AS    Original version stabilized
*     02/07/86  DCS   Name of local variable for bundle table entry
*                     changed to prevent corruption by marker type check
*                     (S85).
*     22/01/87  JCS   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     INP  N       Number of markertypes in list
*     INP  MTYPES  List of markertypes
*     INP  COLOUR  Colour regeneration
*
      INTEGER N, MTYPES(*)
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
      INCLUDE '../include/gkpmb.cmn'
      INCLUDE '../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*     IENT    Entry in polymarker bundle table corresponding to
*             polymarker index in KWI1
*
      INTEGER IENT, I
*
*  ERRORS
*  ------
*       70  Marker type not supported
*       93  Colour index invalid
*    -1004  No room for another bundle
*
*---------------------------------------------------------------------



* Find out if index exists or there is room for another bundle
      DO 10 IENT=1,KMXPMB
       IF(KWI1.EQ.KPMI(IENT,KWKIX) .OR. KPMI(IENT,KWKIX).EQ.KNIL)GOTO 20
   10 CONTINUE

* No room
      KERROR = -1004
      GOTO 999

   20 CONTINUE
* Check if marker type valid
      IF (KWI2.GE.1 .AND. KWI2.LE.5) GOTO 50
      DO 40 I=1,N
        IF (KWI2.EQ.MTYPES(I)) GOTO 50
   40 CONTINUE
* Markertype not supported
      KERROR = 70
      GOTO 999

   50 CONTINUE
* Check if colour index valid
      IF (KWI3.GE.0 .AND. KWI3.LT.KPCI(KWKIX)) THEN
* See if regeneration will be necessary
        IF (KDSMT(KWKIX).EQ.GNEMPT .AND.
     :      (KMKTY(IENT,KWKIX).NE.KWI2 .OR.
     :      (KPMCI(IENT,KWKIX).NE.KWI3 .AND. COLOUR))) THEN
          IF (KIMRGM(KWKIX).EQ.GALLOW) THEN
            KRGN = .TRUE.
            KWRGN(KWKIX) = .TRUE.
          ELSE
            KNFAUP(KWKIX) = GYES
          ENDIF
        ENDIF
* Now set representation
        KPMI(IENT,KWKIX) = KWI1
        KMKTY(IENT,KWKIX) = KWI2
        QMKSZ(IENT,KWKIX) = QWR1
        KPMCI(IENT,KWKIX) = KWI3
      ELSE
* Colour index invalid
        KERROR = 93
      ENDIF


  999 CONTINUE
      END
