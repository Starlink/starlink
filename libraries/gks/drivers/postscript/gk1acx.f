      SUBROUTINE GK1ACX (INDEX,NBC,HXCODE)
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           KEVP  (based on GK1AGL by PLP)
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Given a colour index, derive the appropriate Hexcode for
*     its RGB value.
*
*  ARGUMENTS
*  ---------------
*     INP  INDEX  - A colour index.
*     INP  NBC    - Number of bytes in colour code
*     OUT  HXCODE - Hex Code for Colour RGB Value
*
      INTEGER INDEX, NBC
      CHARACTER*(*) HXCODE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
*  MINDEX            - Colour Index, modified if nec' to be in range
*  GREY              - Grey value (0.0 to 1.0)
*  RED, GREEN, BLUE  - RGB values (0.0 to 1.0)
*

      INTEGER MINDEX
      REAL GREY, RED, GREEN, BLUE

*
*  ALGORITHM
*  ---------
*     If the number of bytes in the colour code is 1, the workstation
*     is assumed to be monochrome.
*     If it is 3, it is assumed to be colour
*     If it is something else, nothing is done (may be CMYK for 4).
*
*     The RGB value is obtained from the colour table.
*     If the workstation is assumed to be colour,
*     each of the 3 RGB values is converted into a HEX code
*     at 8 bits per value.
*     If the workstation is assumed to be monochrome,
*     the RGB value is first converted into an equivalent grey
*     level and then converted into a single HEX code of 8 bits.
*     The conversion to grey level conforms to the NTSC standard.
*
*
*       NTSC formula:
*         intensity = 0.3*red + 0.59*green + 0.11*blue
*
*       Tests show that this formula yields grey levels which differ in value
*       from those of PostScript's conversion, but this difference is so small
*       that it can only be detected numerically and is certainly not
*       discernible by human eye.
*
*     The (color)image operator expects values that are integers within
*     the range 0 to 2**N-1 (where N is number of bits per value).
*     This routine assumes N = 8. To ensure this, such that near equal
*     portions of the interval 0.0 to 1.0 are mapped on to each HEX-code
*     the value is multiplied by 255.9999 and rounded DOWN before
*     conversion into HEX.
*
*--------------------------------------------------------------------------
*

*     Ensure that the colour index is inside the workstation colour table
      MINDEX = MOD(MOD(INDEX, KPCI(KWKIX))+KPCI(KWKIX),KPCI(KWKIX))

*     Get the RGB components of colour with colour index MINDEX.
      RED    = QHP(KHPXR(KCTBPT(1,KWKIX))+MINDEX)
      GREEN  = QHP(KHPXR(KCTBPT(2,KWKIX))+MINDEX)
      BLUE   = QHP(KHPXR(KCTBPT(3,KWKIX))+MINDEX)

      IF(NBC .EQ. 1)THEN
*     Monochrome Workstation
*
*       Now employ the NTSC formula:
        GREY = 0.3 * RED   +  0.59 * GREEN  +  0.11 * BLUE
*
*       Convert the grey level to HEX
        CALL GKDHEX(INT(GREY*255.9999),HXCODE(1:2))
*
      ELSEIF(NBC .EQ. 3)THEN
*     Colour Workstation
*
*       Convert each RGB value to HEX
        CALL GKDHEX(INT(  RED*255.9999),HXCODE(1:2))
        CALL GKDHEX(INT(GREEN*255.9999),HXCODE(3:4))
        CALL GKDHEX(INT( BLUE*255.9999),HXCODE(5:6))
*
      ELSEIF(NBC .EQ. 4)THEN
*     Possibly for CMYK colorimage
      ENDIF
*
      END
