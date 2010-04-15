*-----------------------------------------------------------------
      SUBROUTINE GK1AXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
*-----------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Supply PostScript Hardware Font Details
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier
*     OUT RHT      Height from base to cap line
*     OUT RMAXWD   Width of widest character
*     OUT RBOT     Distance from base to bottom line
*     OUT RTOP     Distance from cap to top line
*     OUT RWD      Character widths array
*
      INTEGER IFID
      REAL RHT,RMAXWD,RBOT,RTOP,RWD(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkxfd.cmn'

*  LOCALS
*  ------
*
*  I      - Temporary count variable
*  IFTMAX - Number of fonts for which data is held in RCHW
*  INDTXT - Local pointer into the RCHW array
*  RBBHGT - Font's Bounding Box Height, where bounding box is the "smallest
*           rectangle enclosing the shape that would result if all of the
*           characters of the font were placed with their origins coincident
*           and painted" (Adobe's Red Book, page 91).
*           In PostScript all font data is defined in relation to this value.
*           In GKS all font data is defined in relation to the Character Body
*           Height (RHT), i.e. base to cap line distance.
*  RCHW   - Array which holds PostScript Hardware Fonts Character Widths as
*           multiples of each character's Bounding Box Height (RBBHGT). Also
*           held is auxiliary font data: maximum width, base  to bottom line,
*           base to cap line and cap to top line distances. All RCHW data is
*           obtainable from PostScript. However, where an Adobe implementation
*           of PostScript is mounted (LaserWriter, LaserWriter+, Linotron) font
*           data can be taken from the so called "font metrics" files.

*           NOTE: Number and ordering of the device's built-in (hardware) fonts
*           in the  WDT table can be changed at will, but the following must be
*           observed:
*                     a). MFxxxx arrays which map the WDT ordering to that of
*                         RCHW array here and fnam array in GK1APS have to be
*                         changed accordingly both in GK1AXF and in GK1AXS.
*                         Also, for every added font IFTMAX must be increased
*                         and an entry must exist in RCHW and fnam.
*                     b)  IFxxxx, IPxxxx and INxxxx in GK1AWD have to be made
*                         consistent with the WDT changes.
*
*

*     Integer workspace offset parameters
      INTEGER    IFTINT,   IFTMAP
      PARAMETER (IFTINT=8, IFTMAP=11)
*
      INTEGER I
      INTEGER INDTXT
*
      REAL RBBHGT, RCHW(1:99,13)
*
*     PostScript's built-in (sometimes called standard) fonts.
*     Courier:
      DATA  (RCHW(I,1),I=1,99)/
     :  95 * 0.600,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  0.600, 0.208, 0.584, 0.042/
*     Courier-Bold:
      DATA  (RCHW(I,2),I=1,99)/
     :  95 * 0.600,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  0.600, 0.258, 0.634, 0.032/
*     Courier-Oblique:
      DATA  (RCHW(I,3),I=1,99)/
     :  95 * 0.600,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  0.600, 0.208, 0.584, 0.042/
*     Courier-BoldOblique:
      DATA  (RCHW(I,4),I=1,99)/
     :  95 * 0.600,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  0.600, 0.258, 0.634, 0.032/
*     Times-Roman:
      DATA  (RCHW(I,5),I=1,99)/
     :  0.250, 0.333, 0.408, 0.500, 0.500, 0.833, 0.778, 0.333,
     :  0.333, 0.333, 0.500, 0.564, 0.250, 0.333, 0.250, 0.278,
     :  0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500,
     :  0.500, 0.500, 0.278, 0.278, 0.564, 0.564, 0.564, 0.444,
     :  0.921, 0.722, 0.667, 0.667, 0.722, 0.611, 0.556, 0.722,
     :  0.722, 0.333, 0.389, 0.722, 0.611, 0.889, 0.722, 0.722,
     :  0.556, 0.722, 0.667, 0.556, 0.611, 0.722, 0.722, 0.944,
     :  0.722, 0.722, 0.611, 0.333, 0.278, 0.333, 0.469, 0.500,
     :  0.333, 0.444, 0.500, 0.444, 0.500, 0.444, 0.333, 0.500,
     :  0.500, 0.278, 0.278, 0.500, 0.278, 0.778, 0.500, 0.500,
     :  0.500, 0.500, 0.333, 0.389, 0.278, 0.500, 0.500, 0.722,
     :  0.500, 0.500, 0.444, 0.480, 0.200, 0.480, 0.541,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  0.944, 0.220, 0.674, 0.014/
*     Times-Bold:
      DATA  (RCHW(I,6),I=1,99)/
     :  0.250, 0.333, 0.555, 0.500, 0.500, 1.000, 0.833, 0.333,
     :  0.333, 0.333, 0.500, 0.570, 0.250, 0.333, 0.250, 0.278,
     :  0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500,
     :  0.500, 0.500, 0.333, 0.333, 0.570, 0.570, 0.570, 0.500,
     :  0.930, 0.722, 0.667, 0.722, 0.722, 0.667, 0.611, 0.778,
     :  0.778, 0.389, 0.500, 0.778, 0.667, 0.944, 0.722, 0.778,
     :  0.611, 0.778, 0.722, 0.556, 0.667, 0.722, 0.722, 1.000,
     :  0.722, 0.722, 0.667, 0.333, 0.278, 0.333, 0.581, 0.500,
     :  0.333, 0.500, 0.556, 0.444, 0.556, 0.444, 0.333, 0.500,
     :  0.556, 0.278, 0.333, 0.556, 0.278, 0.833, 0.556, 0.500,
     :  0.556, 0.556, 0.444, 0.389, 0.333, 0.556, 0.500, 0.722,
     :  0.500, 0.500, 0.444, 0.394, 0.220, 0.394, 0.520,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  1.000, 0.211, 0.682, 0.0/
*     Times-Italic:
      DATA  (RCHW(I,7),I=1,99)/
     :  0.250, 0.333, 0.420, 0.500, 0.500, 0.833, 0.778, 0.333,
     :  0.333, 0.333, 0.500, 0.675, 0.250, 0.333, 0.250, 0.278,
     :  0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500,
     :  0.500, 0.500, 0.333, 0.333, 0.675, 0.675, 0.675, 0.500,
     :  0.920, 0.611, 0.611, 0.667, 0.722, 0.611, 0.611, 0.722,
     :  0.722, 0.333, 0.444, 0.667, 0.556, 0.833, 0.667, 0.722,
     :  0.611, 0.722, 0.611, 0.500, 0.556, 0.722, 0.611, 0.833,
     :  0.611, 0.556, 0.556, 0.389, 0.278, 0.389, 0.422, 0.500,
     :  0.333, 0.500, 0.500, 0.444, 0.500, 0.444, 0.278, 0.500,
     :  0.500, 0.278, 0.278, 0.444, 0.278, 0.722, 0.500, 0.500,
     :  0.500, 0.500, 0.389, 0.389, 0.278, 0.500, 0.444, 0.667,
     :  0.444, 0.444, 0.389, 0.400, 0.275, 0.400, 0.541,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  0.920, 0.207, 0.661, 0.025/
*     Times-BoldItalic:
      DATA  (RCHW(I,8),I=1,99)/
     :  0.250, 0.389, 0.555, 0.500, 0.500, 0.833, 0.778, 0.333,
     :  0.333, 0.333, 0.500, 0.570, 0.250, 0.333, 0.250, 0.278,
     :  0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500,
     :  0.500, 0.500, 0.333, 0.333, 0.570, 0.570, 0.570, 0.500,
     :  0.832, 0.667, 0.667, 0.667, 0.722, 0.667, 0.667, 0.722,
     :  0.778, 0.389, 0.500, 0.667, 0.611, 0.889, 0.722, 0.722,
     :  0.611, 0.722, 0.667, 0.556, 0.611, 0.722, 0.667, 0.889,
     :  0.667, 0.611, 0.611, 0.333, 0.278, 0.333, 0.570, 0.500,
     :  0.333, 0.500, 0.500, 0.444, 0.500, 0.444, 0.333, 0.500,
     :  0.556, 0.278, 0.278, 0.500, 0.278, 0.778, 0.556, 0.500,
     :  0.500, 0.500, 0.389, 0.389, 0.278, 0.556, 0.444, 0.667,
     :  0.500, 0.444, 0.389, 0.348, 0.220, 0.348, 0.570,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  0.889, 0.211, 0.682, 0.011/
*     Helvetica:
      DATA  (RCHW(I,9),I=1,99)/
     :  0.278, 0.278, 0.355, 0.556, 0.556, 0.889, 0.667, 0.222,
     :  0.333, 0.333, 0.389, 0.584, 0.278, 0.333, 0.278, 0.278,
     :  0.556, 0.556, 0.556, 0.556, 0.556, 0.556, 0.556, 0.556,
     :  0.556, 0.556, 0.278, 0.278, 0.584, 0.584, 0.584, 0.556,
     :  1.015, 0.667, 0.667, 0.722, 0.722, 0.667, 0.611, 0.778,
     :  0.722, 0.278, 0.500, 0.667, 0.556, 0.833, 0.722, 0.778,
     :  0.667, 0.778, 0.722, 0.667, 0.611, 0.722, 0.667, 0.944,
     :  0.667, 0.667, 0.611, 0.278, 0.278, 0.278, 0.469, 0.556,
     :  0.222, 0.556, 0.556, 0.500, 0.556, 0.556, 0.278, 0.556,
     :  0.556, 0.222, 0.222, 0.500, 0.222, 0.833, 0.556, 0.556,
     :  0.556, 0.556, 0.333, 0.500, 0.278, 0.556, 0.500, 0.722,
     :  0.500, 0.500, 0.500, 0.334, 0.260, 0.334, 0.584,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  1.015, 0.215, 0.730, 0.0/
*     Helvetica-Bold:
      DATA  (RCHW(I,10),I=1,99)/
     :  0.278, 0.333, 0.474, 0.556, 0.556, 0.889, 0.722, 0.278,
     :  0.333, 0.333, 0.389, 0.584, 0.278, 0.333, 0.278, 0.278,
     :  0.556, 0.556, 0.556, 0.556, 0.556, 0.556, 0.556, 0.556,
     :  0.556, 0.556, 0.333, 0.333, 0.584, 0.584, 0.584, 0.611,
     :  0.975, 0.722, 0.722, 0.722, 0.722, 0.667, 0.611, 0.778,
     :  0.722, 0.278, 0.556, 0.722, 0.611, 0.833, 0.722, 0.778,
     :  0.667, 0.778, 0.722, 0.667, 0.611, 0.722, 0.667, 0.944,
     :  0.667, 0.667, 0.611, 0.333, 0.278, 0.333, 0.584, 0.556,
     :  0.278, 0.556, 0.611, 0.556, 0.611, 0.556, 0.333, 0.611,
     :  0.611, 0.278, 0.278, 0.556, 0.278, 0.889, 0.611, 0.611,
     :  0.611, 0.611, 0.389, 0.556, 0.333, 0.611, 0.556, 0.778,
     :  0.556, 0.556, 0.500, 0.389, 0.280, 0.389, 0.584,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  0.975, 0.221, 0.730, 0.0/
*     Helvetica-Oblique:
      DATA  (RCHW(I,11),I=1,99)/
     :  0.278, 0.278, 0.355, 0.556, 0.556, 0.889, 0.667, 0.222,
     :  0.333, 0.333, 0.389, 0.584, 0.278, 0.333, 0.278, 0.278,
     :  0.556, 0.556, 0.556, 0.556, 0.556, 0.556, 0.556, 0.556,
     :  0.556, 0.556, 0.278, 0.278, 0.584, 0.584, 0.584, 0.556,
     :  1.015, 0.667, 0.667, 0.722, 0.722, 0.667, 0.611, 0.778,
     :  0.722, 0.278, 0.500, 0.667, 0.556, 0.833, 0.722, 0.778,
     :  0.667, 0.778, 0.722, 0.667, 0.611, 0.722, 0.667, 0.944,
     :  0.667, 0.667, 0.611, 0.278, 0.278, 0.278, 0.469, 0.556,
     :  0.222, 0.556, 0.556, 0.500, 0.556, 0.556, 0.278, 0.556,
     :  0.556, 0.222, 0.222, 0.500, 0.222, 0.833, 0.556, 0.556,
     :  0.556, 0.556, 0.333, 0.500, 0.278, 0.556, 0.500, 0.722,
     :  0.500, 0.500, 0.500, 0.334, 0.260, 0.334, 0.584,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  1.015, 0.215, 0.730, 0.0/
*     Helvetica-BoldOblique:
      DATA  (RCHW(I,12),I=1,99)/
     :  0.278, 0.333, 0.474, 0.556, 0.556, 0.889, 0.722, 0.278,
     :  0.333, 0.333, 0.389, 0.584, 0.278, 0.333, 0.278, 0.278,
     :  0.556, 0.556, 0.556, 0.556, 0.556, 0.556, 0.556, 0.556,
     :  0.556, 0.556, 0.333, 0.333, 0.584, 0.584, 0.584, 0.611,
     :  0.975, 0.722, 0.722, 0.722, 0.722, 0.667, 0.611, 0.778,
     :  0.722, 0.278, 0.556, 0.722, 0.611, 0.833, 0.722, 0.778,
     :  0.667, 0.778, 0.722, 0.667, 0.611, 0.722, 0.667, 0.944,
     :  0.667, 0.667, 0.611, 0.333, 0.278, 0.333, 0.584, 0.556,
     :  0.278, 0.556, 0.611, 0.556, 0.611, 0.556, 0.333, 0.611,
     :  0.611, 0.278, 0.278, 0.556, 0.278, 0.889, 0.611, 0.611,
     :  0.611, 0.611, 0.389, 0.556, 0.333, 0.611, 0.556, 0.778,
     :  0.556, 0.556, 0.500, 0.389, 0.280, 0.389, 0.584,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  0.975, 0.221, 0.730, 0.0/
*     Symbol:
*     NOTE: Due to the nature of this font Adobe metrics file provides no data
*           on descendants. A value given below is for compatibility only and
*           was derived by comparing bound box information of various symbols
*           containing what could loosely be defined as a descendant.
      DATA  (RCHW(I,13),I=1,99)/
     :  0.250, 0.333, 0.713, 0.500, 0.549, 0.833, 0.778, 0.439,
     :  0.333, 0.333, 0.500, 0.549, 0.250, 0.549, 0.250, 0.278,
     :  0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.500,
     :  0.500, 0.500, 0.278, 0.278, 0.549, 0.549, 0.549, 0.444,
     :  0.549, 0.696, 0.660, 0.710, 0.612, 0.652, 0.763, 0.603,
     :  0.765, 0.351, 0.631, 0.724, 0.686, 0.918, 0.739, 0.750,
     :  0.768, 0.741, 0.580, 0.592, 0.632, 0.690, 0.439, 0.768,
     :  0.645, 0.795, 0.650, 0.333, 0.863, 0.333, 0.658, 0.500,
     :  0.500, 0.631, 0.549, 0.549, 0.494, 0.439, 0.521, 0.411,
     :  0.603, 0.329, 0.603, 0.549, 0.549, 0.576, 0.521, 0.549,
     :  0.549, 0.521, 0.549, 0.603, 0.439, 0.576, 0.713, 0.686,
     :  0.493, 0.686, 0.494, 0.480, 0.200, 0.480, 0.549,
*       maximum width, base to bottom line, base to cap line, cap to top line:
     :  0.918, 0.225, 0.723, 0.269/
*
*  ALGORITHM
*  ---------
*     Return font data in points. Don't forget about the difference
*     between PostScript and GKS definitions of character heights(see
*     comments above for explanation). First thing we must do is find
*     the PostScript Bounding Box Height.
*
* --------------------------------------------------------------------
*

*
*  Obtain and validate Character Body Height.
*
      RHT=SQRT( QWCHHX(KWKIX)*QWCHHX(KWKIX) +
     :          QWCHHY(KWKIX)*QWCHHY(KWKIX) )
      RHT=AMAX1(QMNCHH(KWKIX),RHT)
      RHT=AMIN1(QMXCHH(KWKIX),RHT)

*
*     Map the WDT font index to RCHW entry via heap array:
*
      INDTXT = KHP(KHPXI(KWKDAT(IFTMAP,KWKIX))-KWTXFN(KWKIX)-1)

*
*     If the font is known provide its details.
*
      IF(INDTXT.LE.KWKDAT(IFTINT,KWKIX)) THEN
*        The font is locally known - obtain its Bounding Box Height first.
         RBBHGT = RHT/RCHW(98,INDTXT)
*        Font's data is defined as a multiple of RBBHGT:
         RMAXWD = RCHW(96,INDTXT)*RBBHGT
         RBOT   = RCHW(97,INDTXT)*RBBHGT
         RTOP   = RCHW(99,INDTXT)*RBBHGT
*        widths for chars [32..126]
         DO 10 I=1,95
            RWD(I)= RCHW(I,INDTXT)*RBBHGT
   10    CONTINUE
      ELSE
*        The font is not known locally - signal error.
         KERROR = -2004
      ENDIF



      END
