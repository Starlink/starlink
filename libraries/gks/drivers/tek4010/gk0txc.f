*-----------------------------------------------------------------------



      SUBROUTINE GK0TXC(IFID,ICHAR,X,Y)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Tektronix storage tube, T4014 and emulators+ Single Character Output
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine.
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     INP ICHAR    Character value (ASCII)
*     INP X,Y      Centre position for cell
*
      INTEGER IFID,ICHAR
      REAL X,Y
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gks.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*     ICHSIZ Offset in KWKDAT for whether number of hardware character
*            sizes is 1 (GLOWER) or greater (GHIGHR)
*     ICHSZD Offset for identifier of Tek 4014 & some of the emulators+
*            character height. For T4014 1 is smallest and 4 is largest.
*            Set up by entry 'Set Text Attributes".
*     ICHHT  Offset in QWKDAT for current hardware character height on
*            Tek 4014 or one of the (GHIGHR) emulators+. Also set up by
*            entry 'Set Text Attributes'.
*     ICHWD  Similar, but for character width.
*     RX,RY  Position of character (D.C)
*     IBUFF  Tek codes to output a character
*     NLEFT  Amount of space left in output buffer
*     RCHH   Character height in D.C.
*     RCHW   Character width in D.C.
*     HARD   .TRUE. iff hardware ch size is being altered
*     BOT801 Baseline to bottom line distance for T5 fonts
*
      INTEGER ICHSIZ,ICHSZD
      PARAMETER (ICHSIZ=2, ICHSZD=5)
      INTEGER ICHHT, ICHWD
      PARAMETER (ICHHT=1, ICHWD=2)
      INTEGER IBUFF(2),NLEFT
      REAL RCHH,RCHW,RX(1),RY(1),BOT801(12)
      LOGICAL HARD

      DATA IBUFF /31,0/
*                 US
      DATA BOT801/ 3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
     :             6.0, 8.0,11.0,14.0,16.0,19.0/

*  COMMENTS
*  --------
*     The Tektronix 4010 has only one character size which is
*     14 in x (allowing for spacing) by 14 in y (not allowing
*     for vertical spacing). Tek 4014 and some emulators+ have
*     range of heights.
*
*     Pen position is unknown after text output.
*
* --------------------------------------------------------------------

*  Obtain the height and width. Tek 4010 and the like have one font only.
      IF( KWKDAT(ICHSIZ,KWKIX).EQ.GHIGHR ) THEN
        RCHH=QWKDAT(ICHHT,KWKIX)
        RCHW=QWKDAT(ICHWD,KWKIX)
      ELSE
        RCHH=14.0
        RCHW=14.0
      ENDIF
*
*   X,Y are centre position of character. Point needs to be displaced
*   to find a suitable starting position (bottom left) for character.

      IF(KWKTYP.EQ.801)THEN
*   Cifer T5
*   X and Y are centre of the Character BODY (see comment for Pericom).
*   Cifer, however, takes the starting position thus derived, increases
*   it by 1 pixel ( 2 or 3 DC units) and understands this as character's
*   bottom line. Therefore, we make adjustments for the Y coordinate:
*   half the height, bottom to base distance and 3.0 DC ( not 2.0, so as
*   to maximise) are deducted.
         RX(1)=X-RCHW/2.0
         RY(1)=Y-RCHH/2.0-BOT801(KWKDAT(ICHSZD,KWKIX))-3.0

      ELSEIF(KWKTYP.EQ.820.OR.KWKTYP.EQ.821)THEN
*   Standard/RAL mods Pericom Monterey shared entry
*   X and Y are centre of the character BODY, by which we here mean
*   a rectangle drawn around the font' widest uppercase character.
*   Height of this rectangle equals that of font' base to bottom line
*   distance. Width of it is font' maximum width, including the
*   intercharacter spacing. Therefore, we have to move down by half
*   the height and left by half the width.
         RX(1)=X-RCHW/2.0
         RY(1)=Y-RCHH/2.0

      ELSE
*   Tek 4010, Tek 4014 and other emulators+
*   Move down by half the height and move left by half the width, which
*   itself is (roughly) 2/3 of the width between 2 starting positions
*   which is what RCHW is.
         RX(1)=X-RCHW/3.0
         RY(1)=Y-RCHH/2.0
      ENDIF

*   Before buffering anything, ensure that there's enough space to
*   send in the same buffer, the move (8 bytes) and the character (2).
*   In addition, we need to ensure 'normal' character size at end of
*   buffer (2 more bytes).
      HARD = KWKDAT(ICHSIZ,KWKIX).EQ.GHIGHR .AND.
     :       KWKDAT(ICHSZD,KWKIX).NE.1
      CALL GKIOBO(KIOQS,1,IBUFF,NLEFT)
      IF( NLEFT.LT.12 ) THEN
          IF( HARD ) CALL GK0TSH(1)
          CALL GKIOBO(KIOSN,1,IBUFF,NLEFT)
          IF( HARD ) CALL GK0TSH(KWKDAT(ICHSZD,KWKIX))
      ENDIF
*
*   Move to the required position
      CALL GK0TLN(1, RX, RY)
*
*   Stuff the character and display it
      IBUFF(2)= ICHAR
      CALL GKIOBO(KIOPB,2,IBUFF,NLEFT)
      END
