# E.S.O. - VLT project/ESO Archive
# @(#) $Id: catdefaults.tcl,v 1.13 1998/12/15 12:38:07 abrighto Exp $
#
# catdefaults.tcl - X defaults for itk catalog widgets
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 20 May 96   created

# itk widget defaults (normally you'd put these in the itk sources, but
# putting them here makes it easier to "pre-load" the classes for the single
# binary version...

proc cat::setXdefaults {} {
    #set menuBg DeepSkyBlue3
    set menuBg #009acd
    set menuFg White

    option add *foreground Black 
    option add *background Gray77
    option add *DisabledForeground Gray90

    option add *Button*foreground NavyBlue
    option add *Button*background Gray77

    option add *Canvas*insertBackground white
    option add *Text.foreground Black 
    option add *Listbox.foreground Black 
    option add *Entry.foreground Black 
    option add *Scrollbar.foreground Gray77
    option add *Scrollbar.Width 14 

    option add *selectForeground Gray77
    option add *selectBackground Black
    
    option add *highlightThickness 0 
    option add *Text.highlightThickness 2  
    option add *Entry.highlightThickness 2 
    option add *highlightBackground Gray77
    option add *highlightColor Black 

    #option add *shelp*Background  FloralWhite
    #option add *plotBackground gray80

    set labelFont -Adobe-Helvetica-Bold-R-Normal--*-120-*-*-*-*-*-*

    option add *Listbox.font -*-courier-medium-r-*-*-*-140-*-*-*-*-iso8859-* 
    option add *Entry.font -*-courier-medium-r-*-*-*-140-*-*-*-*-iso8859-* 
    option add *titleFont -*-courier-Bold-r-*-*-*-140-*-*-*-*-iso8859-* 

    option add *Button.Font $labelFont
    option add *Label.Font $labelFont
    option add *Menu.Font $labelFont
    option add *Menubutton.Font $labelFont
    option add *Message.Font $labelFont
    option add *Scale.Font $labelFont
    option add *Text.Font "-*-Courier-Medium-R-Normal--*-120-*-*-*-*-*-*"

    option add *QueryResult.relief sunken 
    option add *QueryResult.borderwidth 3 
    option add *QueryResult.font -*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-* 
    option add *QueryResult.headingFont -*-courier-bold-r-*-*-*-120-*-*-*-*-iso8859-* 
    option add *QueryResult.headingLines 1 
    option add *QueryResult.titleFont -Adobe-helvetica-bold-r-normal-*-12* 

    option add *Menu*background $menuBg
    option add *Menu*foreground $menuFg
    option add *Menubutton*background $menuBg
    option add *menubar*background $menuBg
    option add *Menubutton*foreground $menuFg

    # use smaller default fonts fo Tix widgets
    #catch {tix configure -fontset "12Point"}
    # option add *TixFontSet TK
    # catch {tix configure -fontset "TK"}
}
