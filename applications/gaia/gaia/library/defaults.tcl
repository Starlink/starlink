# E.S.O. - VLT project
# "@(#) $Id$"
#
# Rtd.tcl - real-time image display application class
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 11 Oct 95   created
# P.W.Draper 21 Nov 96   added GAIA defaults
#            13 Feb 98   tuned for Solaris font rendering


proc gaia::setXdefaults {} {
    #  General widget defaults
    option add *foreground Black
    option add *background #B2B2B2
    option add *DisabledForeground Gray90

    #  Define fonts that are generally available and work well on
    #  Solaris. Other systems seems to handle these OK.
    set labelFont variable
    set textFont fixed
    set wcsFont -*-symbol-medium-r-normal-*-*-120-*-*-*-*-*-*
    option add *Font        $labelFont
    option add *labelFont   $labelFont
    option add *textFont    $textFont
    option add *valueFont   $textFont
#    option add *wcsFont $wcsFont

    option add *Radiobutton.Font $labelFont
    option add *Checkbutton.Font $labelFont
    option add *Button.Font      $labelFont
    option add *Label.Font       $labelFont
    option add *Menu.Font        $labelFont
    option add *Menubutton.Font  $labelFont
    option add *Message.Font     $labelFont
    option add *Scale.Font       $labelFont
    option add *LabelCheck.Font  $labelFont
    option add *LabelChoice.Font $labelFont
    option add *Chooser.Font     $labelFont
    option add *titleFont        $labelFont

    option add *CheckEntry.Font  $textFont
    option add *Chooser.font     $textFont
    option add *Listbox.font     $textFont
    option add *Entry.font       $textFont
    option add *Text.Font        $textFont

    option add *Text.foreground Black
    option add *Listbox.foreground Black
    option add *Entry.foreground Black

    option add *selectBackground  lightblue2
    option add *selectForeground  black

    option add *highlightThickness 0
    option add *Text.highlightThickness 2
    option add *Entry.highlightThickness 2
    option add *highlightBackground #B2B2B2
    option add *highlightColor Black

    option add *shelp*Background  #B2B2B2
    option add *shelp*Font $textFont

    option add *plotBackground #C0C0C0

    # general bindings

    # make delete and backspace work like they should
    bind Entry <Delete> {tkEntryBackspace %W}
    bind Entry <BackSpace> {tkEntryBackspace %W}
    bind Text <Delete> {tkEntryBackspace %W}
    bind Text <BackSpace> {tkEntryBackspace %W}

    bind Button <Return> { tkButtonInvoke %W }
    bind Radiobutton <Return> { tkCheckRadioInvoke %W }
    bind Checkbutton <Return> { tkCheckRadioInvoke %W }
    bind Listbox <1> {+ focus %W }
    bind Listbox <3> { %W selection clear 0 end }
    bind Scale <ButtonPress-1> {+ focus %W }

    # itk widget defaults (normally you'd put these in the itk sources, but
    # putting them here makes it easier to "pre-load" the classes for the single
    # binary version...

    option add *DialogWidget.messageWidth 3i
    option add *DialogWidget.justify left
    option add *DialogWidget.messageFont $labelFont

    option add *InputDialog.messageWidth 3i
    option add *InputDialog.justify left
    option add *InputDialog.messageFont $labelFont

    option add *LabelCheck.anchor w
    option add *LabelChoice.anchor w
    option add *LabelEntry.anchor w
    option add *LabelFileChooser.anchor w

    option add *LabelEntryMenu.anchor w
    option add *LabelEntryMenu.buttonrelief raised
    option add *LabelEntryMenu.indicatorOn 1

    option add *LabelMenu.anchor w
    option add *LabelMenu.relief raised
    option add *LabelMenu.indicatorOn 1

    option add *LabelMessage.anchor w
    option add *LabelMessage.justify left
    option add *LabelMessage.aspect 2000

    option add *LabelNumber.anchor w
    option add *LabelNumber.groove ridge
    option add *LabelNumber.justify right

    option add *LabelValue.anchor w
    option add *LabelValue.relief ridge

    option add *LabelWidget.anchor w
    option add *LabelWidget.labelFont $labelFont

    option add *ListboxWidget.relief sunken
    option add *ListboxWidget.borderwidth 3
    option add *ListboxWidget.setgrid 1
    option add *ListboxWidget.font $textFont
    option add *ListboxWidget.titleFont $labelFont

    option add *ProgressBar.font $labelFont
    option add *ProgressBar.width 8
    option add *ProgressBar.length 6c

    option add *RtdImage.canvasBackground black

    option add *RtdImageCtrl.canvasBackground black
    option add *RtdImageCtrl.canvasWidth 520
    option add *RtdImageCtrl.canvasHeight 520

    option add *RtdImagePan.background black

    option add *RtdImageTrans.relief flat
    #option add *RtdImageZoomView.background black

    option add *TableList.relief sunken
    option add *TableList.borderwidth  3
    option add *TableList.font         $textFont

    option add *SkyQueryResult.relief sunken 
    option add *SkyQueryResult.borderwidth 3 
    option add *SkyQueryResult.font $textFont 
    option add *SkyQueryResult.headingFont $textFont 
    option add *SkyQueryResult.headingLines 1 
    option add *SkyQueryResult.titleFont $labelFont

    option add *GaiaQueryResult.relief sunken 
    option add *GaiaQueryResult.borderwidth 3 
    option add *GaiaQueryResult.font $textFont 
    option add *GaiaQueryResult.headingFont $textFont 
    option add *GaiaQueryResult.headingLines 1 
    option add *GaiaQueryResult.titleFont $labelFont

    option add *TableList.headingFont  $textFont
    option add *TableList.headingLines 1
    option add *TableList.titleFont    $labelFont

    option add *TextDialog.textWidth 80
    option add *TextDialog.textHeight 40
    option add *TextDialog.messageWidth 3i
    option add *TextDialog.justify left
    option add *TextDialog.messageFont $labelFont
    option add *TextDialog.textState disabled

    option add *RtdImageColorRamp.cursor exchange

    #  GAIA defaults:
    option add *GaiaImage.canvasBackground     black
    option add *GaiaImageCtrl.canvasBackground black
    option add *GaiaImageCtrl.canvasWidth      512
    option add *GaiaImageCtrl.canvasHeight     512
    option add *StarLabelCheck.anchor          w

    #  Stop ugly tearoff menus.
    option add *TearOff  0 userDefault

    #  Fonts for canvas draw (used here as work around to modifying
    #  code, these are the Solaris XSun optimised names).
    option add *Fonts {
       -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*
       -adobe-courier-medium-o-*-*-*-120-*-*-*-*-*-*
       -adobe-courier-bold-r-*-*-*-120-*-*-*-*-*-*
       -adobe-courier-medium-r-*-*-*-140-*-*-*-*-*-*
       -adobe-courier-medium-o-*-*-*-140-*-*-*-*-*-*
       -adobe-courier-bold-r-*-*-*-140-*-*-*-*-*-*
       -adobe-courier-medium-r-*-*-*-180-*-*-*-*-*-*
       -adobe-courier-medium-o-*-*-*-180-*-*-*-*-*-*
       -adobe-courier-bold-r-*-*-*-180-*-*-*-*-*-*
       -adobe-courier-medium-r-*-*-*-240-*-*-*-*-*-*
       -adobe-courier-medium-o-*-*-*-240-*-*-*-*-*-*
       -adobe-courier-bold-r-*-*-*-240-*-*-*-*-*-*
    }

    #  Stop fixed font being used in on Canvas (this doesn't print).
    option add *StarCanvasDraw.textFont -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*
    option add *CanvasDraw.textFont -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*
}

