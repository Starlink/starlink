# E.S.O. - VLT project
# "@(#) $Id$"
#
#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2009 Science and Technology Facilities Council.
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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA
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

    #  Use three main fonts for the UI. One for labels and monospaced text.
    #  Special symbols are now handled using unicode symbols, so use
    #  same font as labels. Still separate in UI.
    if { [info exists ::gaia_fonts(labelfont)] } {
       set labelFont $::gaia_fonts(labelfont)
    } else {
       set labelFont TkDefaultFont
    }
    if { [info exists ::gaia_fonts(textfont)] } {
       set textFont $::gaia_fonts(textfont)
    } else {
       set textFont TkFixedFont
    }
    if { [info exists ::gaia_fonts(labelfont)] } {
       set wcsFont $::gaia_fonts(labelfont)
    } else {
       set wcsFont TkDefaultFont
    }

    option add *Font        $labelFont
    option add *labelFont   $labelFont
    option add *textFont    $textFont
    option add *valueFont   $textFont
    option add *wcsFont     $wcsFont
    option add *titleFont   $labelFont

    #  For BLT graph axes, nothing else works.
    option add *Axis.tickFont  TkHeadingFont
    option add *Axis.titleFont TkHeadingFont
    option add *Graph.font     TkHeadingFont

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

    option add *LabelFileChooser.anchor       w
    option add *LabelFileChooser.buttonRelief raised

    option add *LabelFontChooser.anchor w

    option add *LabelCubeFileChooser.anchor       w
    option add *LabelCubeFileChooser.buttonRelief raised

    option add *LabelCubeFontChooser.anchor w

    option add *LabelScale.anchor w

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

    #  Fonts for canvas draw.
    option add *Fonts {
       "Helvetica 8 normal"
       "Helvetica 8 italic"
       "Helvetica 8 bold"
       "Helvetica 8 bold italic"
       "Helvetica 14 normal"
       "Helvetica 14 italic"
       "Helvetica 14 bold"
       "Helvetica 14 bold italic"
       "Times 10 normal"
       "Times 10 italic"
       "Times 10 bold"
       "Times 10 bold italic"
       "Courier 10 normal"
       "Courier 10 italic"
       "Courier 10 bold"
       "Courier 10 bold italic"
       "Helvetica 18 normal"
       "Helvetica 18 italic"
       "Helvetica 18 bold"
       "Helvetica 18 bold italic"
    }

    option add *StarCanvasDraw.textFont $textFont
    option add *CanvasDraw.textFont $textFont

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

    #  Fonts available in the AST interface. These should match those
    #  defined in grf_tkcan.c.
    global ::gaia::astfontmap
    set ::gaia::astfontmap {
       0  "Helvetica 8 normal"       "normal"
       1  "Helvetica 8 italic"       "italic"
       2  "Helvetica 8 bold"         "bold"
       3  "Helvetica 8 bold italic"  "bold italic"
       4  "Helvetica 10 normal"      "normal"
       5  "Helvetica 10 italic"      "italic"
       6  "Helvetica 10 bold"        "bold"
       7  "Helvetica 10 bold italic" "bold italic"
       8  "Times 8 normal"           "normal"
       9  "Times 8 italic"           "italic"
       10 "Times 8 bold"             "bold"
       11 "Times 8 bold italic"      "bold italic"
       12 "Courier 8 normal"         "normal"
       13 "Courier 8 italic"         "italic"
       14 "Courier 8 bold"           "bold"
       15 "Courier 8 bold italic"    "bold italic"
       16 "Helvetica 18 bold"        "bold"
    }
}
