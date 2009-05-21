# E.S.O. - VLT project
# "@(#) $Id$"
#
#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.
#
#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 11 Oct 95   created
# P.W.Draper 21 Nov 96   added GAIA defaults
#            13 Feb 98   tuned for Solaris font rendering
#            04 Mar 99   removed repeats from other Xdefaults

proc gaia::setXdefaults {} {

    #  General widget defaults
    option add *foreground Black
    option add *background #B2B2B2
    option add *disabledForeground Gray90
    option add *highlightBackground #B2B2B2
    option add *selectBackground  lightblue2
    option add *selectForeground  black

    #  Overrides of other defaults (cat/skycat/rtd/util).
    option add *LabelMenu.relief raised

    #  Use three main fonts for the UI. One for labels, monospaced text
    #  and the special symbols.
    if { [info exists ::gaia_fonts(labelfont)] } {
       set labelFont $::gaia_fonts(labelfont)
    } else {
       set labelFont variable
    }
    if { [info exists ::gaia_fonts(textfont)] } {
       set textFont $::gaia_fonts(textfont)
    } else {
       set textFont fixed
    }
    if { [info exists ::gaia_fonts(wcsfont)] } {
       set wcsFont $::gaia_fonts(wcsfont)
    } else {
       set wcsFont "-*-symbol-*-*-*-*-*-140-*-*-*-*-*-*"
    }

    option add *Font        $labelFont
    option add *labelFont   $labelFont
    option add *textFont    $textFont
    option add *valueFont   $textFont
    option add *wcsFont     $wcsFont
    option add *titleFont   $labelFont

    option add *Button.Font              $labelFont
    option add *Checkbutton.Font         $labelFont
    option add *Chooser.Font             $labelFont
    option add *DialogWidget.messageFont $labelFont
    option add *InputDialog.messageFont  $labelFont
    option add *Label.Font               $labelFont
    option add *LabelCheck.Font          $labelFont
    option add *LabelChoice.Font         $labelFont
    option add *LabelWidget.labelFont    $labelFont
    option add *ListboxWidget.titleFont  $labelFont
    option add *Menu.Font                $labelFont
    option add *Menubutton.Font          $labelFont
    option add *Message.Font             $labelFont
    option add *ProgressBar.font         $labelFont
    option add *Radiobutton.Font         $labelFont
    option add *Scale.Font               $labelFont
    option add *TableList.titleFont      $labelFont
    option add *TextDialog.messageFont   $labelFont

    option add *CheckEntry.Font          $textFont
    option add *Chooser.font             $textFont
    option add *Listbox.font             $textFont
    option add *Entry.font               $textFont
    option add *Text.Font                $textFont
    option add *shelp*Font               $textFont
    option add *ListboxWidget.font       $textFont
    option add *TableList.font           $textFont
    option add *TableList.headingFont    $textFont

    option add *QueryResult.font           $textFont
    option add *QueryResult.headingFont    $textFont
    option add *QueryResult.titleFont      $labelFont
    option add *SkyQueryResult.font        $textFont
    option add *SkyQueryResult.headingFont $textFont
    option add *SkyQueryResult.titleFont   $labelFont

    #  GAIA defaults:
    option add *StarLabelCheck.anchor          w
    option add *GaiaImageCtrl.canvasBackground black
    option add *GaiaImageCtrl.canvasWidth      512
    option add *GaiaImageCtrl.canvasHeight     512

    option add *GaiaSpectralPlot.canvasBackground black
    option add *GaiaSpectralPlot.background       black

    option add *LabelCommandMenu.anchor      w
    option add *LabelCommandMenu.relief      groove
    option add *LabelCommandMenu.indicatorOn 1

    option add *LabelEntryMenu.anchor       w
    option add *LabelEntryMenu.buttonrelief raised
    option add *LabelEntryMenu.indicatorOn  1

    option add *LabelFileChooser.anchor w
    option add *LabelFileChooser.buttonRelief raised
    option add *LabelFontChooser.anchor w

    option add *LabelCubeFileChooser.anchor w
    option add *LabelCubeFileChooser.buttonRelief raised
    option add *LabelCubeFontChooser.anchor w

    option add *GaiaQueryResult.relief       sunken
    option add *GaiaQueryResult.borderwidth  3
    option add *GaiaQueryResult.font         $textFont
    option add *GaiaQueryResult.headingFont  $textFont
    option add *GaiaQueryResult.headingLines 1
    option add *GaiaQueryResult.titleFont    $labelFont

    option add *GaiaTabStops.textFont        $textFont

    option add *FITSLabelEntry.anchor w 

    #  Stop ugly tearoff menus and post using "flush", as in Tk4.2.
    option add *TearOff  0 userDefault
    option add *Menubutton*direction flush

    #  Fonts for canvas draw (used here as work around to modifying
    #  code, these are the Solaris Xsun optimised names).
    option add *Fonts {
       -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*
       -*-times-medium-r-*-*-*-120-*-*-*-*-*-*
       -adobe-courier-medium-o-*-*-*-120-*-*-*-*-*-*
       -*-times-medium-o-*-*-*-120-*-*-*-*-*-*
       -adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*
       -*-times-bold-r-*-*-*-120-*-*-*-*-*-*
       -adobe-courier-medium-r-*-*-*-140-*-*-*-*-*-*
       -*-times-medium-r-*-*-*-140-*-*-*-*-*-*
       -adobe-courier-medium-o-*-*-*-140-*-*-*-*-*-*
       -*-times-medium-o-*-*-*-140-*-*-*-*-*-*
       -adobe-courier-bold-r-*-*-*-140-*-*-*-*-*-*
       -*-times-bold-r-*-*-*-140-*-*-*-*-*-*
       -adobe-courier-medium-r-*-*-*-180-*-*-*-*-*-*
       -*-times-medium-r-*-*-*-180-*-*-*-*-*-*
       -adobe-courier-medium-o-*-*-*-180-*-*-*-*-*-*
       -*-times-medium-o-*-*-*-180-*-*-*-*-*-*
       -adobe-courier-bold-r-*-*-*-180-*-*-*-*-*-*
       -*-times-bold-r-*-*-*-180-*-*-*-*-*-*
       -adobe-courier-medium-r-*-*-*-240-*-*-*-*-*-*
       -*-times-medium-r-*-*-*-240-*-*-*-*-*-*
       -adobe-courier-medium-o-*-*-*-240-*-*-*-*-*-*
       -*-times-medium-o-*-*-*-240-*-*-*-*-*-*
       -adobe-courier-bold-r-*-*-*-240-*-*-*-*-*-*
       -*-times-bold-r-*-*-*-240-*-*-*-*-*-*
    }

    #  Stop fixed font being used in on Canvas (this doesn't print).
    option add *StarCanvasDraw.textFont -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*
    option add *CanvasDraw.textFont -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*

    #  OptionDialog
    option add *OptionDialog.messageWidth 5i 
    option add *OptionDialog.messageFont $labelFont

    #  DialogWidget
    option add *DialogWidget.messageWidth 6i
    option add *DialogWidget.messageFont $labelFont

    #  Mousewheel support for various classes. These are mapped to higher
    #  buttons. Note no 6 or 7 support (horizontal scroll). See GaiaImageCtrl
    #  if want to do that.
    bind Canvas <4> "%W yview scroll -1 units"
    bind Canvas <5> "%W yview scroll 1 units"
    bind Listbox <4> "%W yview scroll -1 units"
    bind Listbox <5> "%W yview scroll 1 units"

}
