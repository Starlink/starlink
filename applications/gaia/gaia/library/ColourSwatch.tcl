#+
#  Name:
#     ColourSwatch

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Dialog for choosing an RGB/HSB/CMY colour, either from the full
#     range or from a set of standard X11 colours.

#  Description:
#     The standard colours are chosen to closely match the "rgb.txt"
#     file. Normal use follows something like:
#
#     ColourSwatch .cs
#
#     if {[.cs activate]} {
#        puts stdout "OK >>==> [.fs get]"
#     } else {
#        puts stdout "Cancel"
#     }
#     .cs destroy
#
#     The object withdraws on accept and close.
#
#     The choice of colour space is decided using a file menu option.

#  Invocations:
#
#        ColourSwatch object_name [configuration options]
#
#     This creates an instance of a ColourSwatch object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:

#  Methods:

#  Inheritance:
#     This object inherits no other classes.

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     02-APR-2001 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual ColourSwatch {}

itcl::class gaia::ColourSwatch {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Withdraw this window immediately, it is restored by the get
      #  method.
      wm withdraw $w_

      #  Evaluate any options.
      eval itk_initialize $args
      wm title $w_ "Choose Colour"

      #  Frame for buttons.
      itk_component add butfrm {
         frame $w_.butfrm \
            -relief raised \
            -border 3 \
            -width 250 \
            -height 410
      }

      #  Frame for sliders.
      itk_component add sliders {
         frame $w_.butfrm.sliders
      }

      #  Scale for first component.
      itk_component add red {
         scale $itk_component(sliders).r \
            -label Red: \
            -from 0 \
            -to 255 \
            -showvalue 1 \
            -sliderrelief raised \
            -troughcolor red \
            -variable [scope redval_] \
            -command [code $this set_color] \
            -orient horizontal
      }

      #  Scale for green component.
      itk_component add green {
         scale $itk_component(sliders).g \
            -label Green: \
            -from 0 \
            -to 255 \
            -showvalue 1 \
            -sliderrelief raised \
            -troughcolor green \
            -variable [scope grnval_] \
            -command [code $this set_color] \
            -orient horizontal
      }

      #  Scale for blue component.
      itk_component add blue {
         scale $itk_component(sliders).b \
            -label Blue: \
            -from 0 \
            -to 255 \
            -showvalue 1 \
            -sliderrelief raised \
            -troughcolor blue \
            -variable [scope blueval_] \
            -command [code $this set_color] \
            -orient horizontal
      }

      #  Button and entry displaying the current colour.
      itk_component add visualfrm {
         frame $w_.butfrm.visual
      }
      itk_component add visualbutton {
         button $w_.butfrm.visual.button \
            -background black \
            -foreground white \
            -text "Color" \
            -height 6 \
            -width 10
      }
      itk_component add visualent {
         label $w_.butfrm.visual.ent \
            -width 7 \
            -textvariable [scope hexval_] \
            -relief raised
      }

      #  Packing for button frames.
      pack $itk_component(red) $itk_component(green) $itk_component(blue) \
         -expand 1 -side top

      pack $itk_component(visualbutton) \
         -anchor nw \
         -side top

      pack $itk_component(visualent) \
         -anchor n \
         -side bottom

      pack $itk_component(sliders) -side left -expand 1 -ipadx 5
      pack $itk_component(visualfrm) -side right -expand 1 -ipadx 5

      #  Canvas for displaying the colours.
      itk_component add canvas {
         scrolledcanvas $w_.canvas \
            -autoresize 1 \
            -height 200 \
            -hscrollmode none \
            -width 200 \
            -relief raised \
            -background black
      }

      #  Colours and names are added as regions of the canvas.
      set framer 0
      set frm $itk_component(canvas)
      set colnum [llength $collist]
      set i 0
      foreach colour $collist {
         incr i
         set tag [$frm create rect 4 [expr $framer * 30] \
                     200 [expr [expr $framer * 30] + 30]\
		     -fill "#$colour" \
		     -width 2]
         scan $colour "%02x%02x%02x" red green blue

         $frm bind $tag <ButtonPress-1> \
			  [code $this set_rgb_color $red $green $blue]

         set temp [expr $red + [expr $green + $blue]]
         set bming [expr abs([expr $green - $blue])]
         set rminb [expr abs([expr $red - $green])]
         set gminr [expr abs([expr $green - $red])]

         set sumodiff [expr $gminr + [expr $bming + $rminb]]

         set name [lindex $colnamelist $i]
         if {$sumodiff < 350 && $temp < 400} {
	    set tag [$frm create text 100 [expr [expr $framer *30] + 15] \
                        -text $name \
                        -fill white]
         } else {
	    set tag [$frm create text 100 [expr [expr $framer *30] + 15] \
                        -text $name \
                        -fill black]
         }
         $frm bind $tag <ButtonPress-1> \
			  [code $this set_rgb_color $red $green $blue]

         incr framer
      }

      #  Actions bar.
      itk_component add actionframe {
         frame $w_.actions
      }
      itk_component add accept {
         button $itk_component(actionframe).accept \
            -text {Accept} \
            -command [code $this accept]
      }
      itk_component add cancel {
         button $itk_component(actionframe).cancel \
            -text {Cancel} \
            -command [code $this cancel]
      }
      pack $itk_component(accept) $itk_component(cancel) \
         -side left -fill x -padx 5 -pady 5

      pack $itk_component(butfrm) -side top
      pack $itk_component(canvas) -side top
      pack $itk_component(actionframe) -side bottom
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------
   public method set_rgb_color {red green blue} {
      set redval_ $red
      set grnval_ $green
      set blueval_ $blue
      set_color
   }

   public method set_color {args} {
      set hexval_ "#[format "%02X%02X%02X" $redval_ $grnval_ $blueval_]"
      $itk_component(visualbutton) configure \
         -background "$hexval_" \
         -activebackground "$hexval_"
      set temp [expr [expr $blueval_ + $grnval_] + $redval_]
      set bming [expr abs([expr $grnval_ - $blueval_])]
      set rminb [expr abs([expr $redval_  - $grnval_])]
      set gminr [expr abs([expr $grnval_ - $redval_])]
      set sumodiff [expr $gminr + [expr $bming + $rminb]]
      if {$sumodiff < 350 && $temp < 400} {
         $itk_component(visualbutton) configure \
	    -foreground white \
	    -activeforeground white
      } else {
         $itk_component(visualbutton) configure \
	    -foreground black \
	    -activeforeground black
      }
   }

   #  Activate the chooser and allow the selection of a colour.
   #  Result is 1 or 0 according to whether accept or cancel is
   #  pressed.
   public method activate {} {
      wm deiconify $w_
      if { $itk_option(-modal) } {
         grab $w_
      }

      #  Wait until the window is closed.
      tkwait variable [scope status_]
      wm withdraw $w_
      return $status_
   }

   #  Return the current colour.
   public method get {} {
      return $hexval_
   }

   #  Close the window accepting the result.
   public method accept {} {
      close_window_
      set status_ 1
   }

   #  Close the windows not accepting the result.
   public method cancel {} {
      close_window_
      set status_ 0
   }

   #  Close the window.
   protected method close_window_ {} {
      wm withdraw $w_
      if {$itk_option(-modal)} {
         grab release $w_
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  flag: if true, grab the screen
   itk_option define -modal modal Modal 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  Selected red value.
   protected variable redval_

   #  Selected green value.
   protected variable grnval_

   #  Selected blue value.
   protected variable blueval_

   #  Selected colour as a hex value.
   protected variable hexval_

   #  Status of close window request.
   protected variable status_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  All pre-defined colours. These are from X11 "rgb.txt".
   common collist {
      FFFAFA F8F8FF F5F5F5 DCDCDC FFFAF0 FDF5E6 FAF0E6 FAEBD7
      FFEFD5 FFEBCD FFE4C4 FFDAB9 FFDEAD FFE4B5 FFF8DC FFFFF0
      FFFACD FFF5EE F0FFF0 F5FFFA F0FFFF F0F8FF E6E6FA FFF0F5
      FFE4E1 FFFFFF 000000 2F4F4F 696969 708090 778899 BEBEBE
      D3D3D3 191970 000080 6495ED 483D8B 6A5ACD 7B68EE 8470FF
      0000CD 4169E1 0000FF 1E90FF 00BFFF 87CEEB 87CEFA 4682B4
      B0C4DE ADD8E6 B0E0E6 AFEEEE 00CED1 48D1CC 40E0D0 00FFFF
      E0FFFF 5F9EA0 66CDAA 7FFFD4 006400 556B2F 8FBC8F 2E8B57
      3CB371 20B2AA 98FB98 00FF7F 7CFC00 00FF00 7FFF00 00FA9A
      ADFF2F 32CD32 9ACD32 228B22 6B8E23 BDB76B F0E68C EEE8AA
      FAFAD2 FFFFE0 FFFF00 FFD700 EEDD82 DAA520 B8860B BC8F8F CD5C5C
      8B4513 A0522D CD853F DEB887 F5F5DC F5DEB3 F4A460 D2B48C D2691E
      B22222 A52A2A E9967A FA8072 FFA07A FFA500 FF8C00 FF7F50 F08080
      FF6347 FF4500 FF0000 FF69B4 FF1493 FFC0CB FFB6C1 DB7093 B03060
      C71585 D02090 FF00FF EE82EE DDA0DD DA70D6 BA55D3 9932CC 9400D3
      8A2BE2 A020F0 9370DB D8BFD8 EEE9E9 CDC9C9 8B8989 EEE5DE CDC5BF
      8B8682 FFEFDB EEDFCC CDC0B0 8B8378 EED5B7 CDB79E 8B7D6B EECBAD
      CDAF95 8B7765 EECFA1 CDB38B 8B795E EEE9BF CDC9A5 8B8970 EEE8CD
      CDC8B1 8B8878 EEEEE0 CDCDC1 8B8B83 E0EEE0 C1CDC1 838B83 EEE0E5
      CDC1C5 8B8386 EED5D2 CDB7B5 8B7D7B E0EEEE C1CDCD 838B8B 836FFF
      7A67EE 6959CD 473C8B 4876FF 436EEE 3A5FCD 27408B 0000EE 00008B
      1C86EE 1874CD 104E8B 63B8FF 5CACEE 4F94CD 36648B 00B2EE 009ACD
      00688B 87CEFF 7EC0EE 6CA6CD 4A708B B0E2FF A4D3EE 8DB6CD 607B8B
      C6E2FF B9D3EE 9FB6CD 6C7B8B CAE1FF BCD2EE A2B5CD 6E7B8B BFEFFF
      B2DFEE 9AC0CD 68838B D1EEEE B4CDCD 7A8B8B BBFFFF AEEEEE 96CDCD
      668B8B 98F5FF 8EE5EE 7AC5CD 53868B 00F5FF 00E5EE 00C5CD 00868B
      00EEEE 00CDCD 008B8B 97FFFF 8DEEEE 79CDCD 528B8B 76EEC6 458B74
      C1FFC1 B4EEB4 9BCD9B 698B69 54FF9F 4EEE94 43CD80 9AFF9A 90EE90
      7CCD7C 548B54 00EE76 00CD66 008B45 00EE00 00CD00 008B00 76EE00
      66CD00 458B00 C0FF3E B3EE3A 698B22 CAFF70 BCEE68 A2CD5A 6E8B3D
      FFF68F EEE685 CDC673 8B864E FFEC8B EEDC82 CDBE70 8B814C EEEED1
      CDCDB4 8B8B7A EEEE00 CDCD00 8B8B00 EEC900 CDAD00 8B7500 FFC125
      EEB422 CD9B1D 8B6914 FFB90F EEAD0E CD950C 8B6508 FFC1C1 EEB4B4
      CD9B9B 8B6969 FF6A6A EE6363 CD5555 8B3A3A FF8247 EE7942 CD6839
      8B4726 FFD39B EEC591 CDAA7D 8B7355 FFE7BA EED8AE CDBA96 8B7E66
      FFA54F EE9A49 8B5A2B FF7F24 EE7621 CD661D FF3030 EE2C2C CD2626
      8B1A1A FF4040 EE3B3B CD3333 8B2323 FF8C69 EE8262 CD7054 8B4C39
      EE9572 CD8162 8B5742 EE9A00 CD8500 8B5A00 FF7F00 EE7600 CD6600
      8B4500 FF7256 EE6A50 CD5B45 8B3E2F EE5C42 CD4F39 8B3626 EE4000
      CD3700 8B2500 EE0000 CD0000 8B0000 EE1289 CD1076 8B0A50 FF6EB4
      EE6AA7 CD6090 8B3A62 FFB5C5 EEA9B8 CD919E 8B636C FFAEB9 EEA2AD
      CD8C95 8B5F65 FF82AB EE799F CD6889 8B475D FF34B3 EE30A7 CD2990
      8B1C62 FF3E96 EE3A8C CD3278 8B2252 EE00EE CD00CD 8B008B FF83FA
      EE7AE9 CD69C9 8B4789 FFBBFF EEAEEE CD96CD 8B668B E066FF D15FEE
      B452CD 7A378B BF3EFF B23AEE 9A32CD 68228B 9B30FF 912CEE 7D26CD
      551A8B AB82FF 9F79EE 8968CD 5D478B FFE1FF EED2EE CDB5CD 8B7B8B
      030303 050505 080808 0A0A0A 0D0D0D 0F0F0F 121212 141414 171717
      1A1A1A 1C1C1C 1F1F1F 212121 242424 262626 292929 2B2B2B 2E2E2E
      303030 333333 363636 383838 3B3B3B 3D3D3D 404040 424242 454545
      474747 4A4A4A 4D4D4D 4F4F4F 525252 545454 575757 595959 5C5C5C
      5E5E5E 616161 636363 666666 6B6B6B 6E6E6E 707070 737373 757575
      787878 7A7A7A 7D7D7D 7F7F7F 828282 858585 878787 8A8A8A 8C8C8C
      8F8F8F 919191 949494 969696 999999 9C9C9C 9E9E9E A1A1A1 A3A3A3
      A6A6A6 A8A8A8 ABABAB ADADAD B0B0B0 B3B3B3 B5B5B5 B8B8B8 BABABA
      BDBDBD BFBFBF C2C2C2 C4C4C4 C7C7C7 C9C9C9 CCCCCC CFCFCF D1D1D1
      D4D4D4 D6D6D6 D9D9D9 DBDBDB DEDEDE E0E0E0 E3E3E3 E5E5E5 E8E8E8
      EBEBEB EDEDED F0F0F0 F2F2F2 F7F7F7 FAFAFA FCFCFC }

   #  Names for the colours. These are from X11 "rgb.txt".
   common colnamelist {
      snow {ghost white} {white smoke} gainsboro {floral white}
      {old lace} linen {antique white} {papaya whip} {blanched almond}
      bisque {peach puff} {navajo white} moccasin cornsilk ivory
      {lemon chiffon} seashell honeydew {mint cream} azure
      {alice blue} lavender {lavender blush} {misty rose} white black
      {dark slate gray} {dim gray} {slate gray} {light slate gray}
      gray {light grey} {midnight blue} navy {cornflower blue}
      {dark slate blue} {slate blue} {medium slate blue}
      {light slate blue} {medium blue} {royal blue} blue {dodger blue}
      {deep sky blue} {sky blue} {light sky blue} {steel blue}
      {light steel blue} {light blue} {powder blue} {pale turquoise}
      {dark turquoise} {medium turquoise} turquoise cyan {light cyan}
      {cadet blue} {medium aquamarine} aquamarine {dark green}
      {dark olive green} {dark sea green} {sea green} {medium sea green}
      {light sea green} {pale green} {spring green} {lawn green} green
      chartreuse {medium spring green} {green yellow} {lime green}
      {yellow green} {forest green} {olive drab} {dark khaki} khaki
      {pale goldenrod} {light goldenrod yellow} {light yellow} yellow
      gold {light goldenrod} goldenrod {dark goldenrod} {rosy brown}
      {indian red} {saddle brown} sienna peru burlywood beige wheat
      {sandy brown} tan chocolate firebrick brown {dark salmon} salmon
      {light salmon} orange {dark orange} coral {light coral} tomato
      {orange red} red {hot pink} {deep pink} pink {light pink}
      {pale violet red} maroon {medium violet red} {violet red} magenta
      violet plum orchid {medium orchid} {dark orchid} {dark violet}
      {blue violet} purple {medium purple} thistle snow2 snow3 snow4
      seashell2 seashell3 seashell4 AntiqueWhite1 AntiqueWhite2
      AntiqueWhite3 AntiqueWhite4 bisque2 bisque3 bisque4 PeachPuff2
      PeachPuff3 PeachPuff4 NavajoWhite2 NavajoWhite3 NavajoWhite4
      LemonChiffon2 LemonChiffon3 LemonChiffon4 cornsilk2 cornsilk3
      cornsilk4 ivory2 ivory3 ivory4 honeydew2 honeydew3 honeydew4
      LavenderBlush2 LavenderBlush3 LavenderBlush4 MistyRose2
      MistyRose3 MistyRose4 azure2 azure3 azure4 SlateBlue1 SlateBlue2
      SlateBlue3 SlateBlue4 RoyalBlue1 RoyalBlue2 RoyalBlue3
      RoyalBlue4 blue2 blue4 DodgerBlue2 DodgerBlue3 DodgerBlue4
      SteelBlue1 SteelBlue2 SteelBlue3 SteelBlue4 DeepSkyBlue2
      DeepSkyBlue3 DeepSkyBlue4 SkyBlue1 SkyBlue2 SkyBlue3 SkyBlue4
      LightSkyBlue1 LightSkyBlue2 LightSkyBlue3 LightSkyBlue4
      SlateGray1 SlateGray2 SlateGray3 SlateGray4 LightSteelBlue1
      LightSteelBlue2 LightSteelBlue3 LightSteelBlue4 LightBlue1
      LightBlue2 LightBlue3 LightBlue4 LightCyan2 LightCyan3
      LightCyan4 PaleTurquoise1 PaleTurquoise2 PaleTurquoise3
      PaleTurquoise4 CadetBlue1 CadetBlue2 CadetBlue3 CadetBlue4
      turquoise1 turquoise2 turquoise3 turquoise4 cyan2 cyan3 cyan4
      DarkSlateGray1 DarkSlateGray2 DarkSlateGray3 DarkSlateGray4
      aquamarine2 aquamarine4 DarkSeaGreen1 DarkSeaGreen2
      DarkSeaGreen3 DarkSeaGreen4 SeaGreen1 SeaGreen2 SeaGreen3
      PaleGreen1 PaleGreen2 PaleGreen3 PaleGreen4 SpringGreen2
      SpringGreen3 SpringGreen4 green2 green3 green4 chartreuse2
      chartreuse3 chartreuse4 OliveDrab1 OliveDrab2 OliveDrab4
      DarkOliveGreen1 DarkOliveGreen2 DarkOliveGreen3 DarkOliveGreen4
      khaki1 khaki2 khaki3 khaki4 LightGoldenrod1 LightGoldenrod2
      LightGoldenrod3 LightGoldenrod4 LightYellow2 LightYellow3
      LightYellow4 yellow2 yellow3 yellow4 gold2 gold3 gold4
      goldenrod1 goldenrod2 goldenrod3 goldenrod4 DarkGoldenrod1
      DarkGoldenrod2 DarkGoldenrod3 DarkGoldenrod4 RosyBrown1
      RosyBrown2 RosyBrown3 RosyBrown4 IndianRed1 IndianRed2
      IndianRed3 IndianRed4 sienna1 sienna2 sienna3 sienna4 burlywood1
      burlywood2 burlywood3 burlywood4 wheat1 wheat2 wheat3 wheat4
      tan1 tan2 tan4 chocolate1 chocolate2 chocolate3 firebrick1
      firebrick2 firebrick3 firebrick4 brown1 brown2 brown3 brown4
      salmon1 salmon2 salmon3 salmon4 LightSalmon2 LightSalmon3
      LightSalmon4 orange2 orange3 orange4 DarkOrange1 DarkOrange2
      DarkOrange3 DarkOrange4 coral1 coral2 coral3 coral4 tomato2
      tomato3 tomato4 OrangeRed2 OrangeRed3 OrangeRed4 red2 red3 red4
      DeepPink2 DeepPink3 DeepPink4 HotPink1 HotPink2 HotPink3
      HotPink4 pink1 pink2 pink3 pink4 LightPink1 LightPink2
      LightPink3 LightPink4 PaleVioletRed1 PaleVioletRed2
      PaleVioletRed3 PaleVioletRed4 maroon1 maroon2 maroon3 maroon4
      VioletRed1 VioletRed2 VioletRed3 VioletRed4 magenta2 magenta3
      magenta4 orchid1 orchid2 orchid3 orchid4 plum1 plum2 plum3 plum4
      MediumOrchid1 MediumOrchid2 MediumOrchid3 MediumOrchid4
      DarkOrchid1 DarkOrchid2 DarkOrchid3 DarkOrchid4 purple1 purple2
      purple3 purple4 MediumPurple1 MediumPurple2 MediumPurple3
      MediumPurple4 thistle1 thistle2 thistle3 thistle4 gray1 gray2
      gray3 gray4 gray5 gray6 gray7 gray8 gray9 gray10 gray11 gray12
      gray13 gray14 gray15 gray16 gray17 gray18 gray19 gray20 gray21
      gray22 gray23 gray24 gray25 gray26 gray27 gray28 gray29 gray30
      gray31 gray32 gray33 gray34 gray35 gray36 gray37 gray38 gray39
      gray40 gray42 gray43 gray44 gray45 gray46 gray47 gray48 gray49
      gray50 gray51 gray52 gray53 gray54 gray55 gray56 gray57 gray58
      gray59 gray60 gray61 gray62 gray63 gray64 gray65 gray66 gray67
      gray68 gray69 gray70 gray71 gray72 gray73 gray74 gray75 gray76
      gray77 gray78 gray79 gray80 gray81 gray82 gray83 gray84 gray85
      gray86 gray87 gray88 gray89 gray90 gray91 gray92 gray93 gray94
      gray95 gray97 gray98 gray99 }

#  End of class definition.
}
