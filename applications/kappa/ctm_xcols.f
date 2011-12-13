      BLOCK DATA CTM_XCOLS
*+
*  Name:
*     CTM_XCOLS

*  Purpose:
*     Defines the RGBs and names of colours in the colour set.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     BLOCK DATA

*  Description:
*     Defines the KAPPA standard colour set.

*  Notes:
*     -  The arrays are broken into two because of the restriction of
*     19 continuation lines.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 18 (MJC):
*        Original version.
*     1992 March 16 (MJC):
*        Divided the RGB declarations into two arrays in order to
*        reduce the number of continuations in the DATA statements to
*        below that permitted by the Fortran 77 standard.  Therefore
*        used the new split common blocks CTM_COLNM and CTM_COLIN
*        verbatim rather than including the new CTM_COM.
*     1992 March 27 (MJC):
*        Used the full MIT colour set.  Previous set had errors.
*        Common-block variables have facility prefix.  Also longest name
*        is now 20 from 18.  Renamed from the too-long CTM_COLSET.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'CTM_PAR'          ! Colour-table management

*  Global Variables:

*  Note due to the limit of 19 continuation lines, CTM_COM cannot be
*  included, a modified version with split RGB and name arrays is here
*  verbatim.
*
*  Common block /CTM_COLIN/
*
*        CTM_RGB1( 3, 48 ) = REAL (Write)
*        CTM_RGB2( 3, 48 ) = REAL (Write)
*        CTM_RGB3( 3, 48 ) = REAL (Write)
*        CTM_RGB4( 3, 48 ) = REAL (Write)
*        CTM_RGB5( 3, 48 ) = REAL (Write)
*        CTM_RGB6( 3, 48 ) = REAL (Write)
*        CTM_RGB7( 3, 48 ) = REAL (Write)
*        CTM_RGB8( 3, 48 ) = REAL (Write)
*        CTM_RGB9( 3, 48 ) = REAL (Write)
*        CTM_RGB10( 3, CTM__NAMED - 48 * 9 ) = REAL (Write)
*           The normalised RGB intensities of the named colours.
*
*  Common block /CTM_COLNM/
*
*        CTM_NAM1( 75 ) = CHARACTER * 20 (Write)
*        CTM_NAM2( 58 ) = CHARACTER * 20 (Write)
*        CTM_NAM3( 72 ) = CHARACTER * 20 (Write)
*        CTM_NAM4( 58 ) = CHARACTER * 20 (Write)
*        CTM_NAM5( 62 ) = CHARACTER * 20 (Write)
*        CTM_NAM6( 81 ) = CHARACTER * 20 (Write)
*        CTM_NAM7( CTM__NAMED - 406 ) = CHARACTER * 20 (Write)
*           The names of each of the predefined colours.

      CHARACTER * 20 CTM_NAM1( 75 )  ! First part of name set
      CHARACTER * 20 CTM_NAM2( 58 )  ! Second part of name set
      CHARACTER * 20 CTM_NAM3( 72 )  ! Third part of name set
      CHARACTER * 20 CTM_NAM4( 58 )  ! Fourth part of name set
      CHARACTER * 20 CTM_NAM5( 62 )  ! Fifth part of name set
      CHARACTER * 20 CTM_NAM6( 81 )  ! Sixth part of name set
      CHARACTER * 20 CTM_NAM7( CTM__NAMED-406 ) ! Last part of name set
      REAL CTM_RGB1( 3, 48 )         ! First part of RGB set
      REAL CTM_RGB2( 3, 48 )         ! Second part of RGB set
      REAL CTM_RGB3( 3, 48 )         ! Third part of RGB set
      REAL CTM_RGB4( 3, 48 )         ! Fourth part of RGB set
      REAL CTM_RGB5( 3, 48 )         ! Fifth part of RGB set
      REAL CTM_RGB6( 3, 48 )         ! Sixth part of RGB set
      REAL CTM_RGB7( 3, 48 )         ! Seventh part of RGB set
      REAL CTM_RGB8( 3, 48 )         ! Eighth part of RGB set
      REAL CTM_RGB9( 3, 48 )         ! Ninth part of RGB set
      REAL CTM_RGB10( 3, CTM__NAMED - 432 ) ! Last part of RGB set

      COMMON /CTM_COLIN/  CTM_RGB1, CTM_RGB2, CTM_RGB3, CTM_RGB4,
     :  CTM_RGB5, CTM_RGB6, CTM_RGB7, CTM_RGB8, CTM_RGB9, CTM_RGB10
      COMMON /CTM_COLNM/  CTM_NAM1, CTM_NAM2, CTM_NAM3, CTM_NAM4,
     :  CTM_NAM5, CTM_NAM6, CTM_NAM7

*  Global Data:
      DATA CTM_NAM1 /
     :  'ALICEBLUE', 'ANTIQUEWHITE', 'ANTIQUEWHITE1', 'ANTIQUEWHITE2',
     :  'ANTIQUEWHITE3', 'ANTIQUEWHITE4', 'AQUAMARINE', 'AQUAMARINE1',
     :  'AQUAMARINE2', 'AQUAMARINE3', 'AQUAMARINE4', 'AZURE', 'AZURE1',
     :  'AZURE2', 'AZURE3', 'AZURE4', 'BEIGE', 'BISQUE', 'BISQUE1',
     :  'BISQUE2', 'BISQUE3', 'BISQUE4', 'BLACK', 'BLANCHEDALMOND',
     :  'BLUE', 'BLUE1', 'BLUE2', 'BLUE3', 'BLUE4', 'BLUEVIOLET',
     :  'BROWN', 'BROWN1', 'BROWN2', 'BROWN3', 'BROWN4', 'BURLYWOOD',
     :  'BURLYWOOD1', 'BURLYWOOD2', 'BURLYWOOD3', 'BURLYWOOD4',
     :  'CADETBLUE', 'CADETBLUE1', 'CADETBLUE2', 'CADETBLUE3',
     :  'CADETBLUE4', 'CHARTREUSE', 'CHARTREUSE1', 'CHARTREUSE2',
     :  'CHARTREUSE3', 'CHARTREUSE4', 'CHOCOLATE', 'CHOCOLATE1',
     :  'CHOCOLATE2', 'CHOCOLATE3', 'CHOCOLATE4', 'CORAL', 'CORAL1',
     :  'CORAL2', 'CORAL3', 'CORAL4', 'CORNFLOWERBLUE', 'CORNSILK',
     :  'CORNSILK1', 'CORNSILK2', 'CORNSILK3', 'CORNSILK4', 'CYAN',
     :  'CYAN1', 'CYAN2', 'CYAN3', 'CYAN4', 'DARKGOLDENROD',
     :  'DARKGOLDENROD1', 'DARKGOLDENROD2', 'DARKGOLDENROD3' /

      DATA CTM_NAM2 /
     :  'DARKGOLDENROD4', 'DARKGREEN', 'DARKKHAKI', 'DARKOLIVEGREEN',
     :  'DARKOLIVEGREEN1', 'DARKOLIVEGREEN2', 'DARKOLIVEGREEN3',
     :  'DARKOLIVEGREEN4', 'DARKORANGE', 'DARKORANGE1', 'DARKORANGE2',
     :  'DARKORANGE3', 'DARKORANGE4', 'DARKORCHID', 'DARKORCHID1',
     :  'DARKORCHID2', 'DARKORCHID3', 'DARKORCHID4', 'DARKSALMON',
     :  'DARKSEAGREEN', 'DARKSEAGREEN1', 'DARKSEAGREEN2',
     :  'DARKSEAGREEN3', 'DARKSEAGREEN4', 'DARKSLATEBLUE',
     :  'DARKSLATEGRAY', 'DARKSLATEGRAY1', 'DARKSLATEGRAY2',
     :  'DARKSLATEGRAY3', 'DARKSLATEGRAY4', 'DARKSLATEGREY',
     :  'DARKSLATEGREY1', 'DARKSLATEGREY2', 'DARKSLATEGREY3',
     :  'DARKSLATEGREY4', 'DARKTURQUOISE', 'DARKVIOLET', 'DEEPPINK',
     :  'DEEPPINK1', 'DEEPPINK2', 'DEEPPINK3', 'DEEPPINK4',
     :  'DEEPSKYBLUE', 'DEEPSKYBLUE1', 'DEEPSKYBLUE2', 'DEEPSKYBLUE3',
     :  'DEEPSKYBLUE4', 'DIMGRAY', 'DIMGREY', 'DODGERBLUE',
     :  'DODGERBLUE1', 'DODGERBLUE2', 'DODGERBLUE3', 'DODGERBLUE4',
     :  'FIREBRICK', 'FIREBRICK1', 'FIREBRICK2', 'FIREBRICK3' /

      DATA CTM_NAM3 /
     :  'FIREBRICK4', 'FLORALWHITE', 'FORESTGREEN', 'GAINSBORO',
     :  'GHOSTWHITE', 'GOLD', 'GOLD1', 'GOLD2', 'GOLD3', 'GOLD4',
     :  'GOLDENROD', 'GOLDENROD1', 'GOLDENROD2', 'GOLDENROD3',
     :  'GOLDENROD4', 'GRAY', 'GREY', 'GREEN', 'GREEN1', 'GREEN2',
     :  'GREEN3', 'GREEN4', 'GREENYELLOW', 'HONEYDEW', 'HONEYDEW1',
     :  'HONEYDEW2', 'HONEYDEW3', 'HONEYDEW4', 'HOTPINK', 'HOTPINK1',
     :  'HOTPINK2', 'HOTPINK3', 'HOTPINK4', 'INDIANRED', 'INDIANRED1',
     :  'INDIANRED2', 'INDIANRED3', 'INDIANRED4', 'IVORY', 'IVORY2',
     :  'IVORY3', 'IVORY4', 'KHAKI', 'KHAKI1', 'KHAKI2', 'KHAKI3',
     :  'KHAKI4', 'LAVENDER', 'LAVENDERBLUSH', 'LAVENDERBLUSH1',
     :  'LAVENDERBLUSH2', 'LAVENDERBLUSH3', 'LAVENDERBLUSH4',
     :  'LAWNGREEN', 'LEMONCHIFFON', 'LEMONCHIFFON1', 'LEMONCHIFFON2',
     :  'LEMONCHIFFON3', 'LEMONCHIFFON4', 'LIGHTBLUE', 'LIGHTBLUE1',
     :  'LIGHTBLUE2', 'LIGHTBLUE3', 'LIGHTBLUE4', 'LIGHTCORAL',
     :  'LIGHTCYAN', 'LIGHTCYAN1', 'LIGHTCYAN2', 'LIGHTCYAN3',
     :  'LIGHTCYAN4', 'LIGHTGOLDENROD', 'LIGHTGOLDENROD1' /

      DATA CTM_NAM4 /
     :  'LIGHTGOLDENROD2', 'LIGHTGOLDENROD3', 'LIGHTGOLDENROD4',
     :  'LIGHTGOLDENRODYELLOW', 'LIGHTGRAY', 'LIGHTGREY', 'LIGHTPINK',
     :  'LIGHTPINK1', 'LIGHTPINK2', 'LIGHTPINK3', 'LIGHTPINK4',
     :  'LIGHTSALMON', 'LIGHTSALMON1', 'LIGHTSALMON2', 'LIGHTSALMON3',
     :  'LIGHTSALMON4', 'LIGHTSEAGREEN', 'LIGHTSKYBLUE',
     :  'LIGHTSKYBLUE1', 'LIGHTSKYBLUE2', 'LIGHTSKYBLUE3',
     :  'LIGHTSKYBLUE4', 'LIGHTSLATEBLUE', 'LIGHTSLATEGRAY',
     :  'LIGHTSLATEGREY', 'LIGHTSTEELBLUE', 'LIGHTSTEELBLUE1',
     :  'LIGHTSTEELBLUE2', 'LIGHTSTEELBLUE3', 'LIGHTSTEELBLUE4',
     :  'LIGHTYELLOW', 'LIGHTYELLOW1', 'LIGHTYELLOW2', 'LIGHTYELLOW3',
     :  'LIGHTYELLOW4', 'LIMEGREEN', 'LINEN', 'MAGENTA', 'MAGENTA1',
     :  'MAGENTA2', 'MAGENTA3', 'MAGENTA4', 'MAROON', 'MAROON1',
     :  'MAROON2', 'MAROON3', 'MAROON4', 'MEDIUMAQUAMARINE',
     :  'MEDIUMBLUE', 'MEDIUMORCHID', 'MEDIUMORCHID1', 'MEDIUMORCHID2',
     :  'MEDIUMORCHID3', 'MEDIUMORCHID4', 'MEDIUMPURPLE',
     :  'MEDIUMPURPLE1', 'MEDIUMPURPLE2', 'MEDIUMPURPLE3' /

      DATA CTM_NAM5 /
     :  'MEDIUMPURPLE4', 'MEDIUMSEAGREEN', 'MEDIUMSLATEBLUE',
     :  'MEDIUMSPRINGGREEN', 'MEDIUMTURQUOISE', 'MEDIUMVIOLETRED',
     :  'MIDNIGHTBLUE', 'MINTCREAM', 'MISTYROSE', 'MISTYROSE1',
     :  'MISTYROSE2', 'MISTYROSE3', 'MISTYROSE4', 'MOCCASIN',
     :  'NAVAJOWHITE', 'NAVAJOWHITE1', 'NAVAJOWHITE2', 'NAVAJOWHITE3',
     :  'NAVAJOWHITE4', 'NAVY', 'NAVYBLUE', 'OLDLACE', 'OLIVEDRAB',
     :  'OLIVEDRAB1', 'OLIVEDRAB2', 'OLIVEDRAB3', 'OLIVEDRAB4',
     :  'ORANGE', 'ORANGE1', 'ORANGE2', 'ORANGE3', 'ORANGE4',
     :  'ORANGERED', 'ORANGERED1', 'ORANGERED2', 'ORANGERED3',
     :  'ORANGERED4', 'ORCHID', 'ORCHID1', 'ORCHID2', 'ORCHID3',
     :  'ORCHID4', 'PALEGOLDENROD', 'PALEGREEN', 'PALEGREEN1',
     :  'PALEGREEN2', 'PALEGREEN3', 'PALEGREEN4', 'PALETURQUOISE',
     :  'PALETURQUOISE1', 'PALETURQUOISE2', 'PALETURQUOISE3',
     :  'PALETURQUOISE4', 'PALEVIOLETRED', 'PALEVIOLETRED1',
     :  'PALEVIOLETRED2', 'PALEVIOLETRED3', 'PALEVIOLETRED4',
     :  'PAPAYAWHIP', 'PEACHPUFF', 'PEACHPUFF1', 'PEACHPUFF2' /

      DATA CTM_NAM6 /
     :  'PEACHPUFF3', 'PEACHPUFF4', 'PERU', 'PLUM', 'PINK', 'PINK1',
     :  'PINK2', 'PINK3', 'PINK4', 'PLUM1', 'PLUM2', 'PLUM3', 'PLUM4',
     :  'POWDERBLUE', 'PURPLE', 'PURPLE1', 'PURPLE2', 'PURPLE3',
     :  'PURPLE4', 'RED', 'RED1', 'RED2', 'RED3', 'RED4', 'ROSYBROWN',
     :  'ROSYBROWN1', 'ROSYBROWN2', 'ROSYBROWN3', 'ROSYBROWN4',
     :  'ROYALBLUE', 'ROYALBLUE1', 'ROYALBLUE2', 'ROYALBLUE3',
     :  'ROYALBLUE4', 'SADDLEBROWN', 'SALMON', 'SALMON1', 'SALMON2',
     :  'SALMON3', 'SALMON4', 'SANDYBROWN', 'SEAGREEN', 'SEAGREEN1',
     :  'SEAGREEN2', 'SEAGREEN3', 'SEAGREEN4', 'SEASHELL', 'SEASHELL1',
     :  'SEASHELL2', 'SEASHELL3', 'SEASHELL4', 'SIENNA', 'SIENNA1',
     :  'SIENNA2', 'SIENNA3', 'SIENNA4', 'SKYBLUE', 'SKYBLUE1',
     :  'SKYBLUE2', 'SKYBLUE3', 'SKYBLUE4', 'SLATEBLUE', 'SLATEBLUE1',
     :  'SLATEBLUE2', 'SLATEBLUE3', 'SLATEBLUE4', 'SLATEGRAY',
     :  'SLATEGRAY1', 'SLATEGRAY2', 'SLATEGRAY3', 'SLATEGRAY4',
     :  'SLATEGREY', 'SLATEGREY1', 'SLATEGREY2', 'SLATEGREY3',
     :  'SLATEGREY4', 'SNOW', 'SNOW1', 'SNOW2', 'SNOW3', 'SNOW4' /

      DATA CTM_NAM7 /
     :  'SPRINGGREEN', 'SPRINGGREEN1', 'SPRINGGREEN2', 'SPRINGGREEN3',
     :  'SPRINGGREEN4', 'STEELBLUE', 'STEELBLUE1', 'STEELBLUE2',
     :  'STEELBLUE3', 'STEELBLUE4', 'TAN', 'TAN1', 'TAN2', 'TAN3',
     :  'TAN4', 'THISTLE', 'THISTLE1', 'THISTLE2', 'THISTLE3',
     :  'THISTLE4', 'TOMATO', 'TOMATO1', 'TOMATO2', 'TOMATO3',
     :  'TOMATO4', 'TURQUOISE', 'TURQUOISE1', 'TURQUOISE2',
     :  'TURQUOISE3', 'TURQUOISE4', 'VIOLET', 'VIOLETRED', 'VIOLETRED1',
     :  'VIOLETRED2', 'VIOLETRED3', 'VIOLETRED4', 'VORY1', 'WHEAT',
     :  'WHEAT1', 'WHEAT2', 'WHEAT3', 'WHEAT4', 'WHITE', 'WHITESMOKE',
     :  'YELLOW', 'YELLOW1', 'YELLOW2', 'YELLOW3', 'YELLOW4',
     :  'YELLOWGREEN' /

      DATA CTM_RGB1 /
     :  0.941, 0.973, 1.000, 0.980, 0.922, 0.843, 1.000, 0.937, 0.859,
*       AliceBlue            AntiqueWhite         AntiqueWhite1
     :  0.933, 0.875, 0.800, 0.804, 0.753, 0.690, 0.545, 0.514, 0.471,
*       AntiqueWhite2        AntiqueWhite3        AntiqueWhite4
     :  0.498, 1.000, 0.831, 0.498, 1.000, 0.831, 0.463, 0.933, 0.776,
*       Aquamarine           Aquamarine1          Aquamarine2
     :  0.400, 0.804, 0.667, 0.271, 0.545, 0.455, 0.941, 1.000, 1.000,
*       Aquamarine3          Aquamarine4          Azure
     :  0.941, 1.000, 1.000, 0.878, 0.933, 0.933, 0.757, 0.804, 0.804,
*       Azure1               Azure2               Azure3
     :  0.514, 0.545, 0.545, 0.961, 0.961, 0.863, 1.000, 0.894, 0.769,
*       Azure4               Beige                Bisque
     :  1.000, 0.894, 0.769, 0.933, 0.835, 0.718, 0.804, 0.718, 0.620,
*       Bisque1              Bisque2              Bisque3
     :  0.545, 0.490, 0.420, 0.000, 0.000, 0.000, 1.000, 0.922, 0.804,
*       Bisque4              Black                BlanchedAlmond
     :  0.000, 0.000, 1.000, 0.000, 0.000, 1.000, 0.000, 0.000, 0.933,
*       Blue                 Blue1                Blue2
     :  0.000, 0.000, 0.804, 0.000, 0.000, 0.545, 0.541, 0.169, 0.886,
*       Blue3                Blue4                BlueViolet
     :  0.647, 0.165, 0.165, 1.000, 0.251, 0.251, 0.933, 0.231, 0.231,
*       Brown                Brown1               Brown2
     :  0.804, 0.200, 0.200, 0.545, 0.137, 0.137, 0.871, 0.722, 0.529,
*       Brown3               Brown4               Burlywood
     :  1.000, 0.827, 0.608, 0.933, 0.773, 0.569, 0.804, 0.667, 0.490,
*       Burlywood1           Burlywood2           Burlywood3
     :  0.545, 0.451, 0.333, 0.373, 0.620, 0.627, 0.596, 0.961, 1.000,
*       Burlywood4           CadetBlue            CadetBlue1
     :  0.557, 0.898, 0.933, 0.478, 0.773, 0.804, 0.325, 0.525, 0.545,
*       CadetBlue2           CadetBlue3           CadetBlue4
     :  0.498, 1.000, 0.000, 0.498, 1.000, 0.000, 0.463, 0.933, 0.000 /
*       Chartreuse           Chartreuse1          Chartreuse2

      DATA CTM_RGB2 /
     :  0.400, 0.804, 0.000, 0.271, 0.545, 0.000, 0.824, 0.412, 0.118,
*       Chartreuse3          Chartreuse4          Chocolate
     :  1.000, 0.498, 0.141, 0.933, 0.463, 0.129, 0.804, 0.400, 0.114,
*       Chocolate1           Chocolate2           Chocolate3
     :  0.545, 0.271, 0.075, 1.000, 0.498, 0.314, 1.000, 0.447, 0.337,
*       Chocolate4           Coral                Coral1
     :  0.933, 0.416, 0.314, 0.804, 0.357, 0.271, 0.545, 0.243, 0.184,
*       Coral2               Coral3               Coral4
     :  0.392, 0.584, 0.929, 1.000, 0.973, 0.863, 1.000, 0.973, 0.863,
*       CornflowerBlue       Cornsilk             Cornsilk1
     :  0.933, 0.910, 0.804, 0.804, 0.784, 0.694, 0.545, 0.533, 0.471,
*       Cornsilk2            Cornsilk3            Cornsilk4
     :  0.000, 1.000, 1.000, 0.000, 1.000, 1.000, 0.000, 0.933, 0.933,
*       Cyan                 Cyan1                Cyan2
     :  0.000, 0.804, 0.804, 0.000, 0.545, 0.545, 0.722, 0.525, 0.043,
*       Cyan3                Cyan4                DarkGoldenrod
     :  1.000, 0.725, 0.059, 0.933, 0.678, 0.055, 0.804, 0.584, 0.047,
*       DarkGoldenrod1       DarkGoldenrod2       DarkGoldenrod3
     :  0.545, 0.396, 0.031, 0.000, 0.392, 0.000, 0.741, 0.718, 0.420,
*       DarkGoldenrod4       DarkGreen            DarkKhaki
     :  0.333, 0.420, 0.184, 0.792, 1.000, 0.439, 0.737, 0.933, 0.408,
*       DarkOliveGreen       DarkOliveGreen1      DarkOliveGreen2
     :  0.635, 0.804, 0.353, 0.431, 0.545, 0.239, 1.000, 0.549, 0.000,
*       DarkOliveGreen3      DarkOliveGreen4      DarkOrange
     :  1.000, 0.498, 0.000, 0.933, 0.463, 0.000, 0.804, 0.400, 0.000,
*       DarkOrange1          DarkOrange2          DarkOrange3
     :  0.545, 0.271, 0.000, 0.600, 0.196, 0.800, 0.749, 0.243, 1.000,
*       DarkOrange4          DarkOrchid           DarkOrchid1
     :  0.698, 0.227, 0.933, 0.604, 0.196, 0.804, 0.408, 0.133, 0.545,
*       DarkOrchid2          DarkOrchid3          DarkOrchid4
     :  0.914, 0.588, 0.478, 0.561, 0.737, 0.561, 0.757, 1.000, 0.757 /
*       DarkSalmon           DarkSeaGreen         DarkSeaGreen1

      DATA CTM_RGB3 /
     :  0.706, 0.933, 0.706, 0.608, 0.804, 0.608, 0.412, 0.545, 0.412,
*       DarkSeaGreen2        DarkSeaGreen3        DarkSeaGreen4
     :  0.282, 0.239, 0.545, 0.184, 0.310, 0.310, 0.592, 1.000, 1.000,
*       DarkSlateBlue        DarkSlateGray        DarkSlateGray1
     :  0.553, 0.933, 0.933, 0.475, 0.804, 0.804, 0.322, 0.545, 0.545,
*       DarkSlateGray2       DarkSlateGray3       DarkSlateGray4
     :  0.184, 0.310, 0.310, 0.592, 1.000, 1.000, 0.553, 0.933, 0.933,
*       DarkSlateGrey        DarkSlateGrey1       DarkSlateGrey2
     :  0.475, 0.804, 0.804, 0.322, 0.545, 0.545, 0.000, 0.808, 0.820,
*       DarkSlateGrey3       DarkSlateGrey4       DarkTurquoise
     :  0.580, 0.000, 0.827, 1.000, 0.078, 0.576, 1.000, 0.078, 0.576,
*       DarkViolet           DeepPink             DeepPink1
     :  0.933, 0.071, 0.537, 0.804, 0.063, 0.463, 0.545, 0.039, 0.314,
*       DeepPink2            DeepPink3            DeepPink4
     :  0.000, 0.749, 1.000, 0.000, 0.749, 1.000, 0.000, 0.698, 0.933,
*       DeepSkyBlue          DeepSkyBlue1         DeepSkyBlue2
     :  0.000, 0.604, 0.804, 0.000, 0.408, 0.545, 0.412, 0.412, 0.412,
*       DeepSkyBlue3         DeepSkyBlue4         DimGray
     :  0.412, 0.412, 0.412, 0.118, 0.565, 1.000, 0.118, 0.565, 1.000,
*       DimGrey              DodgerBlue           DodgerBlue1
     :  0.110, 0.525, 0.933, 0.094, 0.455, 0.804, 0.063, 0.306, 0.545,
*       DodgerBlue2          DodgerBlue3          DodgerBlue4
     :  0.698, 0.133, 0.133, 1.000, 0.188, 0.188, 0.933, 0.173, 0.173,
*       Firebrick            Firebrick1           Firebrick2
     :  0.804, 0.149, 0.149, 0.545, 0.102, 0.102, 1.000, 0.980, 0.941,
*       Firebrick3           Firebrick4           FloralWhite
     :  0.133, 0.545, 0.133, 0.863, 0.863, 0.863, 0.973, 0.973, 1.000,
*       ForestGreen          Gainsboro            GhostWhite
     :  1.000, 0.843, 0.000, 1.000, 0.843, 0.000, 0.933, 0.788, 0.000,
*       Gold                 Gold1                Gold2
     :  0.804, 0.678, 0.000, 0.545, 0.459, 0.000, 0.855, 0.647, 0.125 /
*       Gold3                Gold4                Goldenrod

      DATA CTM_RGB4 /
     :  1.000, 0.757, 0.145, 0.933, 0.706, 0.133, 0.804, 0.608, 0.114,
*       Goldenrod1           Goldenrod2           Goldenrod3
     :  0.545, 0.412, 0.078, 0.753, 0.753, 0.753, 0.753, 0.753, 0.753,
*       Goldenrod4           Gray                 Grey
     :  0.000, 1.000, 0.000, 0.000, 1.000, 0.000, 0.000, 0.933, 0.000,
*       Green                Green1               Green2
     :  0.000, 0.804, 0.000, 0.000, 0.545, 0.000, 0.678, 1.000, 0.184,
*       Green3               Green4               GreenYellow
     :  0.941, 1.000, 0.941, 0.941, 1.000, 0.941, 0.878, 0.933, 0.878,
*       Honeydew             Honeydew1            Honeydew2
     :  0.757, 0.804, 0.757, 0.514, 0.545, 0.514, 1.000, 0.412, 0.706,
*       Honeydew3            Honeydew4            HotPink
     :  1.000, 0.431, 0.706, 0.933, 0.416, 0.655, 0.804, 0.376, 0.565,
*       HotPink1             HotPink2             HotPink3
     :  0.545, 0.227, 0.384, 0.804, 0.361, 0.361, 1.000, 0.416, 0.416,
*       HotPink4             IndianRed            IndianRed1
     :  0.933, 0.388, 0.388, 0.804, 0.333, 0.333, 0.545, 0.227, 0.227,
*       IndianRed2           IndianRed3           IndianRed4
     :  1.000, 1.000, 0.941, 0.933, 0.933, 0.878, 0.804, 0.804, 0.757,
*       Ivory                Ivory2               Ivory3
     :  0.545, 0.545, 0.514, 0.941, 0.902, 0.549, 1.000, 0.965, 0.561,
*       Ivory4               Khaki                Khaki1
     :  0.933, 0.902, 0.522, 0.804, 0.776, 0.451, 0.545, 0.525, 0.306,
*       Khaki2               Khaki3               Khaki4
     :  0.902, 0.902, 0.980, 1.000, 0.941, 0.961, 1.000, 0.941, 0.961,
*       Lavender             LavenderBlush        LavenderBlush1
     :  0.933, 0.878, 0.898, 0.804, 0.757, 0.773, 0.545, 0.514, 0.525,
*       LavenderBlush2       LavenderBlush3       LavenderBlush4
     :  0.486, 0.988, 0.000, 1.000, 0.980, 0.804, 1.000, 0.980, 0.804,
*       LawnGreen            LemonChiffon         LemonChiffon1
     :  0.933, 0.914, 0.749, 0.804, 0.788, 0.647, 0.545, 0.537, 0.439 /
*       LemonChiffon2        LemonChiffon3        LemonChiffon4

      DATA CTM_RGB5 /
     :  0.678, 0.847, 0.902, 0.749, 0.937, 1.000, 0.698, 0.875, 0.933,
*       LightBlue            LightBlue1           LightBlue2
     :  0.604, 0.753, 0.804, 0.408, 0.514, 0.545, 0.941, 0.502, 0.502,
*       LightBlue3           LightBlue4           LightCoral
     :  0.878, 1.000, 1.000, 0.878, 1.000, 1.000, 0.820, 0.933, 0.933,
*       LightCyan            LightCyan1           LightCyan2
     :  0.706, 0.804, 0.804, 0.478, 0.545, 0.545, 0.933, 0.867, 0.510,
*       LightCyan3           LightCyan4           LightGoldenrod
     :  1.000, 0.925, 0.545, 0.933, 0.863, 0.510, 0.804, 0.745, 0.439,
*       LightGoldenrod1      LightGoldenrod2      LightGoldenrod3
     :  0.545, 0.506, 0.298, 0.980, 0.980, 0.824, 0.827, 0.827, 0.827,
*       LightGoldenrod4      LightGoldenrodYellow LightGray
     :  0.827, 0.827, 0.827, 1.000, 0.714, 0.757, 1.000, 0.682, 0.725,
*       LightGrey            LightPink            LightPink1
     :  0.933, 0.635, 0.678, 0.804, 0.549, 0.584, 0.545, 0.373, 0.396,
*       LightPink2           LightPink3           LightPink4
     :  1.000, 0.627, 0.478, 1.000, 0.627, 0.478, 0.933, 0.584, 0.447,
*       LightSalmon          LightSalmon1         LightSalmon2
     :  0.804, 0.506, 0.384, 0.545, 0.341, 0.259, 0.125, 0.698, 0.667,
*       LightSalmon3         LightSalmon4         LightSeaGreen
     :  0.529, 0.808, 0.980, 0.690, 0.886, 1.000, 0.643, 0.827, 0.933,
*       LightSkyBlue         LightSkyBlue1        LightSkyBlue2
     :  0.553, 0.714, 0.804, 0.376, 0.482, 0.545, 0.518, 0.439, 1.000,
*       LightSkyBlue3        LightSkyBlue4        LightSlateBlue
     :  0.467, 0.533, 0.600, 0.467, 0.533, 0.600, 0.690, 0.769, 0.871,
*       LightSlateGray       LightSlateGrey       LightSteelBlue
     :  0.792, 0.882, 1.000, 0.737, 0.824, 0.933, 0.635, 0.710, 0.804,
*       LightSteelBlue1      LightSteelBlue2      LightSteelBlue3
     :  0.431, 0.482, 0.545, 1.000, 1.000, 0.878, 1.000, 1.000, 0.878,
*       LightSteelBlue4      LightYellow          LightYellow1
     :  0.933, 0.933, 0.820, 0.804, 0.804, 0.706, 0.545, 0.545, 0.478 /
*       LightYellow2         LightYellow3         LightYellow4

      DATA CTM_RGB6 /
     :  0.196, 0.804, 0.196, 0.980, 0.941, 0.902, 1.000, 0.000, 1.000,
*       LimeGreen            Linen                Magenta
     :  1.000, 0.000, 1.000, 0.933, 0.000, 0.933, 0.804, 0.000, 0.804,
*       Magenta1             Magenta2             Magenta3
     :  0.545, 0.000, 0.545, 0.690, 0.188, 0.376, 1.000, 0.204, 0.702,
*       Magenta4             Maroon               Maroon1
     :  0.933, 0.188, 0.655, 0.804, 0.161, 0.565, 0.545, 0.110, 0.384,
*       Maroon2              Maroon3              Maroon4
     :  0.400, 0.804, 0.667, 0.000, 0.000, 0.804, 0.729, 0.333, 0.827,
*       MediumAquamarine     MediumBlue           MediumOrchid
     :  0.878, 0.400, 1.000, 0.820, 0.373, 0.933, 0.706, 0.322, 0.804,
*       MediumOrchid1        MediumOrchid2        MediumOrchid3
     :  0.478, 0.216, 0.545, 0.576, 0.439, 0.859, 0.671, 0.510, 1.000,
*       MediumOrchid4        MediumPurple         MediumPurple1
     :  0.624, 0.475, 0.933, 0.537, 0.408, 0.804, 0.365, 0.278, 0.545,
*       MediumPurple2        MediumPurple3        MediumPurple4
     :  0.235, 0.702, 0.443, 0.482, 0.408, 0.933, 0.000, 0.980, 0.604,
*       MediumSeaGreen       MediumSlateBlue      MediumSpringGreen
     :  0.282, 0.820, 0.800, 0.780, 0.082, 0.522, 0.098, 0.098, 0.439,
*       MediumTurquoise      MediumVioletRed      MidnightBlue
     :  0.961, 1.000, 0.980, 1.000, 0.894, 0.882, 1.000, 0.894, 0.882,
*       MintCream            MistyRose            MistyRose1
     :  0.933, 0.835, 0.824, 0.804, 0.718, 0.710, 0.545, 0.490, 0.482,
*       MistyRose2           MistyRose3           MistyRose4
     :  1.000, 0.894, 0.710, 1.000, 0.871, 0.678, 1.000, 0.871, 0.678,
*       Moccasin             NavajoWhite          NavajoWhite1
     :  0.933, 0.812, 0.631, 0.804, 0.702, 0.545, 0.545, 0.475, 0.369,
*       NavajoWhite2         NavajoWhite3         NavajoWhite4
     :  0.000, 0.000, 0.502, 0.000, 0.000, 0.502, 0.992, 0.961, 0.902,
*       Navy                 NavyBlue             OldLace
     :  0.420, 0.557, 0.137, 0.753, 1.000, 0.243, 0.702, 0.933, 0.227 /
*       OliveDrab            OliveDrab1           OliveDrab2

      DATA CTM_RGB7 /
     :  0.604, 0.804, 0.196, 0.412, 0.545, 0.133, 1.000, 0.647, 0.000,
*       OliveDrab3           OliveDrab4           Orange
     :  1.000, 0.647, 0.000, 0.933, 0.604, 0.000, 0.804, 0.522, 0.000,
*       Orange1              Orange2              Orange3
     :  0.545, 0.353, 0.000, 1.000, 0.271, 0.000, 1.000, 0.271, 0.000,
*       Orange4              OrangeRed            OrangeRed1
     :  0.933, 0.251, 0.000, 0.804, 0.216, 0.000, 0.545, 0.145, 0.000,
*       OrangeRed2           OrangeRed3           OrangeRed4
     :  0.855, 0.439, 0.839, 1.000, 0.514, 0.980, 0.933, 0.478, 0.914,
*       Orchid               Orchid1              Orchid2
     :  0.804, 0.412, 0.788, 0.545, 0.278, 0.537, 0.933, 0.910, 0.667,
*       Orchid3              Orchid4              PaleGoldenrod
     :  0.596, 0.984, 0.596, 0.604, 1.000, 0.604, 0.565, 0.933, 0.565,
*       PaleGreen            PaleGreen1           PaleGreen2
     :  0.486, 0.804, 0.486, 0.329, 0.545, 0.329, 0.686, 0.933, 0.933,
*       PaleGreen3           PaleGreen4           PaleTurquoise
     :  0.733, 1.000, 1.000, 0.682, 0.933, 0.933, 0.588, 0.804, 0.804,
*       PaleTurquoise1       PaleTurquoise2       PaleTurquoise3
     :  0.400, 0.545, 0.545, 0.859, 0.439, 0.576, 1.000, 0.510, 0.671,
*       PaleTurquoise4       PaleVioletRed        PaleVioletRed1
     :  0.933, 0.475, 0.624, 0.804, 0.408, 0.537, 0.545, 0.278, 0.365,
*       PaleVioletRed2       PaleVioletRed3       PaleVioletRed4
     :  1.000, 0.937, 0.835, 1.000, 0.855, 0.725, 1.000, 0.855, 0.725,
*       PapayaWhip           PeachPuff            PeachPuff1
     :  0.933, 0.796, 0.678, 0.804, 0.686, 0.584, 0.545, 0.467, 0.396,
*       PeachPuff2           PeachPuff3           PeachPuff4
     :  0.804, 0.522, 0.247, 0.867, 0.627, 0.867, 1.000, 0.753, 0.796,
*       Peru                 Plum                 Pink
     :  1.000, 0.710, 0.773, 0.933, 0.663, 0.722, 0.804, 0.569, 0.620,
*       Pink1                Pink2                Pink3
     :  0.545, 0.388, 0.424, 1.000, 0.733, 1.000, 0.933, 0.682, 0.933 /
*       Pink4                Plum1                Plum2

      DATA CTM_RGB8 /
     :  0.804, 0.588, 0.804, 0.545, 0.400, 0.545, 0.690, 0.878, 0.902,
*       Plum3                Plum4                PowderBlue
     :  0.627, 0.125, 0.941, 0.608, 0.188, 1.000, 0.569, 0.173, 0.933,
*       Purple               Purple1              Purple2
     :  0.490, 0.149, 0.804, 0.333, 0.102, 0.545, 1.000, 0.000, 0.000,
*       Purple3              Purple4              Red
     :  1.000, 0.000, 0.000, 0.933, 0.000, 0.000, 0.804, 0.000, 0.000,
*       Red1                 Red2                 Red3
     :  0.545, 0.000, 0.000, 0.737, 0.561, 0.561, 1.000, 0.757, 0.757,
*       Red4                 RosyBrown            RosyBrown1
     :  0.933, 0.706, 0.706, 0.804, 0.608, 0.608, 0.545, 0.412, 0.412,
*       RosyBrown2           RosyBrown3           RosyBrown4
     :  0.255, 0.412, 0.882, 0.282, 0.463, 1.000, 0.263, 0.431, 0.933,
*       RoyalBlue            RoyalBlue1           RoyalBlue2
     :  0.227, 0.373, 0.804, 0.153, 0.251, 0.545, 0.545, 0.271, 0.075,
*       RoyalBlue3           RoyalBlue4           SaddleBrown
     :  0.980, 0.502, 0.447, 1.000, 0.549, 0.412, 0.933, 0.510, 0.384,
*       Salmon               Salmon1              Salmon2
     :  0.804, 0.439, 0.329, 0.545, 0.298, 0.224, 0.957, 0.643, 0.376,
*       Salmon3              Salmon4              SandyBrown
     :  0.180, 0.545, 0.341, 0.329, 1.000, 0.624, 0.306, 0.933, 0.580,
*       SeaGreen             SeaGreen1            SeaGreen2
     :  0.263, 0.804, 0.502, 0.180, 0.545, 0.341, 1.000, 0.961, 0.933,
*       SeaGreen3            SeaGreen4            Seashell
     :  1.000, 0.961, 0.933, 0.933, 0.898, 0.871, 0.804, 0.773, 0.749,
*       Seashell1            Seashell2            Seashell3
     :  0.545, 0.525, 0.510, 0.627, 0.322, 0.176, 1.000, 0.510, 0.278,
*       Seashell4            Sienna               Sienna1
     :  0.933, 0.475, 0.259, 0.804, 0.408, 0.224, 0.545, 0.278, 0.149,
*       Sienna2              Sienna3              Sienna4
     :  0.529, 0.808, 0.922, 0.529, 0.808, 1.000, 0.494, 0.753, 0.933 /
*       SkyBlue              SkyBlue1             SkyBlue2

      DATA CTM_RGB9 /
     :  0.424, 0.651, 0.804, 0.290, 0.439, 0.545, 0.416, 0.353, 0.804,
*       SkyBlue3             SkyBlue4             SlateBlue
     :  0.514, 0.435, 1.000, 0.478, 0.404, 0.933, 0.412, 0.349, 0.804,
*       SlateBlue1           SlateBlue2           SlateBlue3
     :  0.278, 0.235, 0.545, 0.439, 0.502, 0.565, 0.776, 0.886, 1.000,
*       SlateBlue4           SlateGray            SlateGray1
     :  0.725, 0.827, 0.933, 0.624, 0.714, 0.804, 0.424, 0.482, 0.545,
*       SlateGray2           SlateGray3           SlateGray4
     :  0.439, 0.502, 0.565, 0.776, 0.886, 1.000, 0.725, 0.827, 0.933,
*       SlateGrey            SlateGrey1           SlateGrey2
     :  0.624, 0.714, 0.804, 0.424, 0.482, 0.545, 1.000, 0.980, 0.980,
*       SlateGrey3           SlateGrey4           Snow
     :  1.000, 0.980, 0.980, 0.933, 0.914, 0.914, 0.804, 0.788, 0.788,
*       Snow1                Snow2                Snow3
     :  0.545, 0.537, 0.537, 0.000, 1.000, 0.498, 0.000, 1.000, 0.498,
*       Snow4                SpringGreen          SpringGreen1
     :  0.000, 0.933, 0.463, 0.000, 0.804, 0.400, 0.000, 0.545, 0.271,
*       SpringGreen2         SpringGreen3         SpringGreen4
     :  0.275, 0.510, 0.706, 0.388, 0.722, 1.000, 0.361, 0.675, 0.933,
*       SteelBlue            SteelBlue1           SteelBlue2
     :  0.310, 0.580, 0.804, 0.212, 0.392, 0.545, 0.824, 0.706, 0.549,
*       SteelBlue3           SteelBlue4           Tan
     :  1.000, 0.647, 0.310, 0.933, 0.604, 0.286, 0.804, 0.522, 0.247,
*       Tan1                 Tan2                 Tan3
     :  0.545, 0.353, 0.169, 0.847, 0.749, 0.847, 1.000, 0.882, 1.000,
*       Tan4                 Thistle              Thistle1
     :  0.933, 0.824, 0.933, 0.804, 0.710, 0.804, 0.545, 0.482, 0.545,
*       Thistle2             Thistle3             Thistle4
     :  1.000, 0.388, 0.278, 1.000, 0.388, 0.278, 0.933, 0.361, 0.259,
*       Tomato               Tomato1              Tomato2
     :  0.804, 0.310, 0.224, 0.545, 0.212, 0.149, 0.251, 0.878, 0.816 /
*       Tomato3              Tomato4              Turquoise

      DATA CTM_RGB10 /
     :  0.000, 0.961, 1.000, 0.000, 0.898, 0.933, 0.000, 0.773, 0.804,
*       Turquoise1           Turquoise2           Turquoise3
     :  0.000, 0.525, 0.545, 0.933, 0.510, 0.933, 0.816, 0.125, 0.565,
*       Turquoise4           Violet               VioletRed
     :  1.000, 0.243, 0.588, 0.933, 0.227, 0.549, 0.804, 0.196, 0.471,
*       VioletRed1           VioletRed2           VioletRed3
     :  0.545, 0.133, 0.322, 1.000, 1.000, 0.941, 0.961, 0.871, 0.702,
*       VioletRed4           Vory1                Wheat
     :  1.000, 0.906, 0.729, 0.933, 0.847, 0.682, 0.804, 0.729, 0.588,
*       Wheat1               Wheat2               Wheat3
     :  0.545, 0.494, 0.400, 1.000, 1.000, 1.000, 0.961, 0.961, 0.961,
*       Wheat4               White                WhiteSmoke
     :  1.000, 1.000, 0.000, 1.000, 1.000, 0.000, 0.933, 0.933, 0.000,
*       Yellow               Yellow1              Yellow2
     :  0.804, 0.804, 0.000, 0.545, 0.545, 0.000, 0.604, 0.804, 0.196 /
*       Yellow3              Yellow4              YellowGreen

*.

      END
