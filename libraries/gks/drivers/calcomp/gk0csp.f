

* --------------------------------------------------------------
      SUBROUTINE GK0CSP(INDEC,AHXRED,AHXGRN,AHXBLU)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81 Select pen according to specification
*
*  MAINTENANCE LOG
*  ---------------
*     00/99/83  MGC  Original version stabilized
*     19/03/84  MGC  Always select a pen
*     11/05/84  MGC  Swap black,white pen table entries (I212)
*      4/12/84  RMK  Removed pen 8 - white.
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
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkhp.cmn'
*
*  LOCALS
*  ------
*
      INTEGER    ILSP
      PARAMETER (ILSP=3)
      INTEGER ICSP(ILSP),IPENS(7)
      INTEGER IRGB
      DATA ICSP/70,0,32/
      DATA IPENS/1,4,3,6,2,7,5/
*
*  ALGORITHM
*  ---------
*     .SELECT PEN : F<'0'+IPENS(IRGB)>
*     .END        : <SP>
*
*  COMMENTS
*  --------
*     PEN   RGB   COLOUR(logical)
*     1     000   Black
*     2     100   Red
*     3     010   Green
*     4     001   Blue
*     5     110   Yellow
*     6     011   Cyan
*     7     101   Magenta
*    (8     111   White - removed)
*
*  COMMENTS
*  --------
*  Pen 8 was defined to be white, which is the same as pen 0 the
*  background colour. But if colour index 8 was chosen, pen 8 was
*  selected and the primitive drawn! So pen 8 was removed meantime.
*  The routine needs to be recoded to allow the use of a real 8th
*  pen - probably brown.
*
* --------------------------------------------------------------

      IRGB = 1
      IF(QHP(KHPXR(AHXRED)+INDEC).GT.0.5) IRGB = IRGB + 4
      IF(QHP(KHPXR(AHXGRN)+INDEC).GT.0.5) IRGB = IRGB + 2
      IF(QHP(KHPXR(AHXBLU)+INDEC).GT.0.5) IRGB = IRGB + 1
      ICSP(2)=48+IPENS(IRGB)
      CALL GK0CPA(KIOPB,ILSP,ICSP)
      RETURN
      END
