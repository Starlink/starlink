      SUBROUTINE GK0TGH
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Get the hardware text height closest to the requested character
*     height for devices that have a range of hardware characters.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine.
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     .... Start of offsets and constants in KWKDAT & QWKDAT workspace ....
*     ICHSZD Offset for identifier of  the hardware character height.
*            1 is smallest & NSnnn, where nnn is Workstation Type, is
*            largest.
*     ICHHT  Offset in QWKDAT for current hardware character height
*     ICHWD  Similar, but for character width
*     .... End of offsets and constants in KWKDAT & QWKDAT workspace .....
*     NSnnn  Number of character sizes for workstation nnn
*     HTEnnn Heights excluding interline      spacing for workstation nnn
*     WDSnnn Widths  including intercharacter spacing for workstation nnn
*     NS82X  Set of font constants, as those above, but shared by
*     HTE82X Standard/RAL mods Pericom Monterey terminals...
*     WDS82X ....
*
*     .... Start of offsets and constants in KWKDAT & QWKDAT workspace ....
      INTEGER ICHSZD
      PARAMETER (ICHSZD=5)
      INTEGER ICHHT, ICHWD
      PARAMETER (ICHHT=1, ICHWD=2)
*     .... End of offsets and constants in KWKDAT & QWKDAT workspace .....
      INTEGER NS203, NS801, NS82X
      PARAMETER (NS203=4, NS801=12, NS82X=16)
      REAL HTE203(NS203),WDS203(NS203)
      REAL HTE801(NS801),WDS801(NS801)
      REAL HTE82X(NS82X),WDS82X(NS82X)
*
      DATA HTE203/31.6, 32.8, 50.0, 56.0/,
     :     WDS203/31.0, 34.0, 51.0, 56.0/

*  Cifer T5 boasts a vertical resolution of 782, though in reality
*  it has no more than 300 pixels. The uncertainty about the actual
*  resolution springs from the fact that mapping from DC to pixels
*  is done via an algorhitm of unknown description. This makes it
*  virtually impossible to off screen measure font geometry correctly.
*  Thus, for each font 3 separate measurements were taken (test string
*  being displayed from different Y coordinates) , then the bigest
*  figure for height adopted. Note, that since first six T5 fonts are
*  all 14.0 pixels high, to make the separation possible corresponding
*  HTE entries had to be altered. This is rectified later.
      DATA HTE801/  12.0,  13.0,  16.0,  20.0,  22.0,  26.0,
     :              29.0,  45.0,  60.0,  75.0,  92.0, 107.0/
      DATA WDS801/   8.0,   9.0,   8.0,  13.0,  14.0,  15.0,
     :              28.0,  42.0,  56.0,  70.0,  84.0,  98.0/

*   The data for the Pericoms has been obtained through direct off
*   screen measurement. Note that character body height in GKS is
*   defined as centre of the base to centre of the cap line di-
*   stance. This is not so for the width. Therefore, HTE82X lists
*   number of pixels as measured of the screen minus 1 and WDS82X
*   lists full number of pixels in a character box. Note, also, that
*   to distinguish between sizes 8 & 9 which are identical in height,
*   HTE entry for the size 9 is increased by 1. This is taken into
*   account later.
      DATA HTE82X/ 6.0,7.0,11.0,12.0,13.0,15.0,20.0,23.0,24.0,
     :            25.0,27.0,31.0,35.0,38.0,47.0,51.0/
      DATA WDS82X/ 8.0,9.0,13.0,14.0,15.0,17.0,23.0,25.0,25.0,
     :            25.0,31.0,34.0,38.0,39.0,51.0,51.0/

*-----------------------------------------------------------------------

*  Find hardware character height that is nearest to magnitude of
*  height vector. Keep the resulting index and h/ware ht & wdth.

      IF (KWKTYP.EQ.203) THEN
* Tek 4014
         CALL GKNRL(SQRT(QWCHHX(KWKIX)**2+QWCHHY(KWKIX)**2),
     :                         NS203,HTE203,KWKDAT(ICHSZD,KWKIX))

         QWKDAT(ICHHT,KWKIX)=HTE203(KWKDAT(ICHSZD,KWKIX))
         QWKDAT(ICHWD,KWKIX)=WDS203(KWKDAT(ICHSZD,KWKIX))

      ELSEIF (KWKTYP.EQ.801) THEN
* Cifer T5
* Find the font with fictional height closest to requested...
         CALL GKNRL(SQRT(QWCHHX(KWKIX)**2+QWCHHY(KWKIX)**2),
     :                         NS801,HTE801,KWKDAT(ICHSZD,KWKIX))

         QWKDAT(ICHHT,KWKIX)=HTE801(KWKDAT(ICHSZD,KWKIX))
* ...but store the real values (see note above).
         IF(KWKDAT(ICHSZD,KWKIX).LE.6)
     :   QWKDAT(ICHHT,KWKIX)= 14.0
         QWKDAT(ICHWD,KWKIX)=WDS801(KWKDAT(ICHSZD,KWKIX))
      ELSEIF (KWKTYP.EQ.820.OR.KWKTYP.EQ.821) THEN
* Standard/RAL mods Pericom Monterey shared entry
         CALL GKNRL(SQRT(QWCHHX(KWKIX)**2+QWCHHY(KWKIX)**2),
     :                         NS82X,HTE82X,KWKDAT(ICHSZD,KWKIX))

         QWKDAT(ICHHT,KWKIX)=HTE82X(KWKDAT(ICHSZD,KWKIX))
* If size 9, correct the height (see note above).
         IF(KWKDAT(ICHSZD,KWKIX).EQ.9)
     :   QWKDAT(ICHHT,KWKIX)=QWKDAT(ICHHT,KWKIX)-1.0
         QWKDAT(ICHWD,KWKIX)=WDS82X(KWKDAT(ICHSZD,KWKIX))
      ENDIF

      END
