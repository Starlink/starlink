#
# itk.tcl
# ----------------------------------------------------------------------
# Invoked automatically upon startup to customize the interpreter
# for [incr Tk].
# ----------------------------------------------------------------------
#   AUTHOR:  Michael J. McLennan
#            Bell Labs Innovations for Lucent Technologies
#            mmclennan@lucent.com
#            http://www.tcltk.com/itcl
#
#      RCS:  $Id$
# ----------------------------------------------------------------------
#               Copyright (c) 1993-1996  Lucent Technologies
# ======================================================================
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted,
# provided that the above copyright notice appear in all copies and that
# both that the copyright notice and warranty disclaimer appear in
# supporting documentation, and that the names of Lucent Technologies
# any of their entities not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# Lucent Technologies disclaims all warranties with regard to this
# software, including all implied warranties of merchantability and
# fitness.  In no event shall Lucent be liable for any special, indirect
# or consequential damages or any damages whatsoever resulting from loss
# of use, data or profits, whether in an action of contract, negligence
# or other tortuous action, arising out of or in connection with the use
# or performance of this software.
# ======================================================================

#
# Provide transparent access to all [incr Tk] commands
#
import add ::itk
if {$tcl_platform(os) == "MacOS"} {
    source -rsrc itk:tclIndex
} else {
    lappend auto_path ${itk::library}
}

#
# If the [incr Widgets] are available, then require them by default.
#
catch {package require Iwidgets}

#
# Define "usual" option-handling code for the Tk widgets:
#
itk::usual Button {
    keep -background -cursor -foreground -font
    keep -activebackground -activeforeground -disabledforeground
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Canvas {
    keep -background -cursor
    keep -insertbackground -insertborderwidth -insertwidth
    keep -insertontime -insertofftime
    keep -selectbackground -selectborderwidth -selectforeground
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Checkbutton {
    keep -background -cursor -foreground -font
    keep -activebackground -activeforeground -disabledforeground
    keep -selectcolor
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Entry {
    keep -background -cursor -foreground -font
    keep -insertbackground -insertborderwidth -insertwidth
    keep -insertontime -insertofftime
    keep -selectbackground -selectborderwidth -selectforeground
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Frame {
    keep -background -cursor
}

itk::usual Label {
    keep -background -cursor -foreground -font
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Listbox {
    keep -background -cursor -foreground -font
    keep -selectbackground -selectborderwidth -selectforeground
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Menu {
    keep -background -cursor -foreground -font
    keep -activebackground -activeforeground -disabledforeground
    keep -selectcolor -tearoff
}

itk::usual Menubutton {
    keep -background -cursor -foreground -font
    keep -activebackground -activeforeground -disabledforeground
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Message {
    keep -background -cursor -foreground -font
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Radiobutton {
    keep -background -cursor -foreground -font
    keep -activebackground -activeforeground -disabledforeground
    keep -selectcolor
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Scale {
    keep -background -cursor -foreground -font -troughcolor
    keep -activebackground
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Scrollbar {
    keep -background -cursor -troughcolor
    keep -activebackground -activerelief
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Text {
    keep -background -cursor -foreground -font
    keep -insertbackground -insertborderwidth -insertwidth
    keep -insertontime -insertofftime
    keep -selectbackground -selectborderwidth -selectforeground
    keep -highlightcolor -highlightthickness
    rename -highlightbackground -background background Background
}

itk::usual Toplevel {
    keep -background -cursor
}
