

*----------------------------------------------------------------------
      SUBROUTINE GK0BSP(INDEC,AHXRED,AHXGRN,AHXBLU)
*
* (C) COPYRIGHT ICL & SERC  1986
*

*----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of workstation driver
*  Author:             DRJF
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     BENSON Select pen according to specification
*
*  MAINTENANCE LOG
*  ---------------
*     24/01/86  DRJF  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP INDEC    Colour index
*     INP AHXRED   Colour table bundle index RED
*     INP AHXGRN                             GREEN
*     INP AHXBLU                             BLUE
*
      INTEGER INDEC
      INTEGER AHXRED,AHXGRN,AHXBLU
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER    KORDER,   KCPEN
      PARAMETER (KORDER=5, KCPEN=11)
      INTEGER INWPEN,IDUMR,IDUMG,IDUMB
*
*  COMMENTS
*  --------
*     PEN   RGB   COLOUR(logical)
*     0     000   Black
*     1     100   Red
*     2     001   Blue
*     3     010   Green
*
*----------------------------------------------------------------------


      CALL GK0BPN (QHP(KHPXR(AHXRED)+INDEC),
     :             QHP(KHPXR(AHXGRN)+INDEC),
     :             QHP(KHPXR(AHXBLU)+INDEC),
     :             INWPEN,IDUMR,IDUMG,IDUMB)
      IF (INWPEN.NE.KWKDAT(KCPEN,KWKIX)) THEN
        KWKDAT(KCPEN,KWKIX)=INWPEN
        KWKDAT(KORDER,KWKIX)=0
      END IF
      RETURN
*
      END
