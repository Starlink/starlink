proc Action action {

#+ Action

#  Send an action to the ADAM application.

#  action (Given)
#    Name of the action in the ADAM application to be invoked.

#  Method:
#   Return if there is no catalogue open.
#   Create the top-level window.
#   Withdraw the top-level window.
#   Write text into it.
#   Grab the focus.
#   Assemble the parameters for the action.
#   If ok then
#     If a list is to be captured from the application then
#       Initialise the list.
#     end if
#     Send the given action to the ADAM application.
#   end if
#   Release the focus.

#  Author:
#   ACD: A C Davenhall (Edinburgh)

#  History:
#   8/9/99   (ACD): Rewritten for STARTCL.
#   17/11/99 (ACD): First stable version.
#   10/3/00  (ACD): Fixed a bug in setting the parameters for the
#      SETFILE action.
#   11/4/01  (ACD): Added the QUIET parameter.
#   1/11/01  (ACD): Truncated the lists of columns set in SETCMP and
#      SETSTAT to avoid a segmentation fault.
#-

#
#  Return if there is no catalogue open.

    global catalogueName
    global quietMode

    if {$catalogueName == ""} then {
       Error "Currently no catalogue is open."
       return
    }

#
#  Create the top-level window and lower its stacking order.

    toplevel     .action   -class Dialog   -bd 10

    wm withdraw  .action

    wm title     .action   "Action"
    wm iconname  .action   Action
    wm transient .action   .

#  Write text into it.

    label .action.text -text "Please Wait..."
    pack  .action.text

#
#  Withdraw the window, then update all the geometry information
#  to determine how big the window wants to be, then centre the window
#  in parent and de-iconify it.

    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .action]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .action]/2 + \
      [winfo y .]]
    wm geom .action +$x+$y

#
#  Set a grab and claim the focus.

    set oldFocus [focus]
    grab  .action
    focus .action

#
#  Initialise the flag to indicate whether the output is sent to the
#  output screen or appended to a list.  The default is that it is
#  sent to the screen.  If it is to be appended to the list then the
#  flag will be set in the case statement for individual actions, below.

    global captureList
    set captureList 0

#
#  Assemble the parameters for the action.  Additional operations
#  include clearing the display window if required and redrawing the
#  .action widget for actions which are likely to take a significant
#  length of time.

    set unknownAction 0

    case $action in {

    "OPEN" {
       set parameters "GUI=YES CNAME=$catalogueName QUIET=$quietMode"
    }

    "SHOWCOL" {
       set parameters "GUI=YES QUIET=$quietMode"

       wm deiconify .action

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "DETCOL" {
       set parameters "GUI=YES QUIET=$quietMode"

       wm deiconify .action

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "SHOWPAR" {
       set parameters "GUI=YES QUIET=$quietMode"

       wm deiconify .action

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "DETPAR" {
       set parameters "GUI=YES QUIET=$quietMode"

       wm deiconify .action

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "SHOWTXT" {
       set parameters "GUI=YES QUIET=$quietMode"

       wm deiconify .action

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "SHOWROWS" {
       set parameters "GUI=YES QUIET=$quietMode"

       set captureList 1
    }

    "SETCMP" {
       global chosenColumns

       set chosenLength [string length $chosenColumns]

       if {$chosenLength > 375} then {
          set rightPos [string last ";" [string range $chosenColumns 0 375]]
          set chosenColumnsSend [string range $chosenColumns 0 $rightPos]
          Error "(Warning) List of columns truncated."
       } else {
          set chosenColumnsSend $chosenColumns
       }

       set parameters "GUI=YES CMPLST=$chosenColumnsSend QUIET=$quietMode"

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "SHOWSEL" {
       set parameters "GUI=YES QUIET=$quietMode"

       set captureList 1
    }

    "CHOSEL" {
       global selectionNumber
       set parameters "GUI=YES SELNO=$selectionNumber QUIET=$quietMode"

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "SETSEL" {
       wm deiconify .action

       global selectionExpression
       set parameters "GUI=YES EXPR='$selectionExpression'"
       append parameters " QUIET=$quietMode"

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "SHOWRNG" {
       set parameters "GUI=YES QUIET=$quietMode"

       set captureList 1
    }

    "SETRNG" {
       global rangeColumn
       global rangeMin
       global rangeMax

       set parameters "GUI=YES PNAME=$rangeColumn"
       append parameters " QUIET=$quietMode"
       append parameters " MINRNG=$rangeMin MAXRNG=$rangeMax"
    }

    "SETROW" {
       global currentRow
       set parameters "GUI=YES ROWNO=$currentRow QUIET=$quietMode"
    }

    "LIST" {
       set parameters "GUI=YES QUIET=$quietMode"

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "PREV" {
       set parameters "GUI=YES QUIET=$quietMode"

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "SETSTAT" {
       global  statsColumns

       set statsLength [string length $statsColumns]

       if {$statsLength > 375} then {
          set rightPos [string last ";" [string range $statsColumns 0 375]]
          set statsColumnsSend [string range $statsColumns 0 $rightPos]
          Error "(Warning) List of columns truncated."
       } else {
          set statsColumnsSend $statsColumns
       }

       set parameters "GUI=YES CMPSTT=$statsColumnsSend QUIET=$quietMode"
    }

    "SETDECPL" {
       global  statsDecPl
       set parameters "GUI=YES DECPL=$statsDecPl QUIET=$quietMode"
    }

    "STATS" {
       wm deiconify .action

       global  statsFile
       set parameters "GUI=YES SFNAME=$statsFile QUIET=$quietMode"

       .display.output configure -state normal
       .display.output delete 1.0 end
       .display.output configure -state disabled
    }

    "SCOPEN" {
       wm deiconify .action

       global scatterDevice
       global scatterTitle
       global scatterXaxis
       global scatterYaxis

       set parameters "GUI=YES GRPHDV=$scatterDevice"
       append parameters " QUIET=$quietMode"
       append parameters " TITLE=$scatterTitle"
       append parameters " XEXPR=$scatterXaxis YEXPR=$scatterYaxis"
    }

    "SCRANGE" {
       global scatterAutoScale
       global scatterXmin
       global scatterXmax
       global scatterYmin
       global scatterYmax

       set parameters "GUI=YES AUTOSCL=$scatterAutoScale"
       append parameters " QUIET=$quietMode"
       append parameters " CXMIN=$scatterXmin CXMAX=$scatterXmax"
       append parameters " CYMIN=$scatterYmin CYMAX=$scatterYmax"
    }

    "SCPLOT" {
       wm deiconify .action

       global scatterPlotSymbol
       global scatterSymbolColour

       set parameters "GUI=YES PLTSYM=$scatterPlotSymbol"
       append parameters " QUIET=$quietMode"
       append parameters " COLOUR=$scatterSymbolColour"
    }

    "SCSHRNG" {
       set parameters "GUI=YES QUIET=$quietMode"

       set captureList 1
    }

    "SCLOSE" {
       set parameters "GUI=YES QUIET=$quietMode"
    }

    "HSOPEN" {
       wm deiconify .action

       global histDevice
       global histTitle
       global histXaxis

       set parameters "GUI=YES GRPHDV=$histDevice"
       append parameters " QUIET=$quietMode"
       append parameters " TITLE=$histTitle"
       append parameters " XEXPR=$histXaxis"
    }

    "HSRANGE" {
       global histAutoScale
       global histXmin
       global histXmax
       global histBinSpec
       global histBinDet
       global histNormal

       set parameters "GUI=YES AUTOSCL=$histAutoScale"
       append parameters " QUIET=$quietMode"
       append parameters " CXMIN=$histXmin CXMAX=$histXmax"
       append parameters " BINSP=$histBinSpec BINDET=$histBinDet"
       append parameters " NORML=$histNormal"
    }

    "HSPLOT" {
       wm deiconify .action

       global histLineColour

       set parameters "GUI=YES COLOUR=$histLineColour"
       append parameters " QUIET=$quietMode"
    }

    "HSSHRNG" {
       set parameters "GUI=YES QUIET=$quietMode"

       set captureList 1
    }

    "HSCLOSE" {
       set parameters "GUI=YES QUIET=$quietMode"
    }

    "FILE" {
       wm deiconify .action

       global textfileFirstRow
       global textfileLastRow
       global textfileName

       set parameters "GUI=YES FIRSTR=$textfileFirstRow"
       append parameters " QUIET=$quietMode"
       append parameters " LASTR=$textfileLastRow FLNAME=$textfileName"
    }

    "SAVECAT" {
       wm deiconify .action

       global saveCatName
       global saveCatColumns
       global saveCatText
       global saveCatComm

       set parameters "GUI=YES CATOUT=$saveCatName"
       append parameters " QUIET=$quietMode"
       append parameters " CFLAG=$saveCatColumns TFLAG=$saveCatText"
       append parameters " COMM=$saveCatComm"
    }

    "SHOWFMT" {
       global columnName

       set parameters "GUI=YES PNAME=$columnName"
       append parameters " QUIET=$quietMode"

       set captureList 1
    }

    "SETFMT" {
       global columnName
       global columnUnits
       global columnFormat

       set parameters "GUI=YES PNAME=$columnName"
       append parameters " QUIET=$quietMode"
       append parameters " UNITS=$columnUnits EXFMT=$columnFormat"
    }

    "SETCONF" {
       global screenHeight
       global sequenceNumber
       global numberListed
       global angleRepn
       global angleReformat

       set parameters "GUI=YES SWID=299 SHT=$screenHeight"
       append parameters " QUIET=$quietMode"
       append parameters " SEQNO=$sequenceNumber NLIST=$numberListed"
       append parameters " ANGRPN=$angleRepn ANGRF=$angleReformat"
    }

    "SETFILE" {
       global textfilePage
       global textfileWidth
       global textfileSummary
       global textfileColumns
       global textfileParameters
       global textfileText
       global textfileTable

       set parameters "GUI=YES FPRINT=$textfilePage"
       append parameters " QUIET=$quietMode"
       append parameters " FPGSZE=$textfilePage FWID=$textfileWidth"
       append parameters " FSUMM=$textfileSummary FCOL=$textfileColumns"
       append parameters " FPAR=$textfileParameters FTXT=$textfileText"
       append parameters " FTABL=$textfileTable"
    }

    "COLNAME" {
       set parameters "GUI=YES QUIET=$quietMode"

       set captureList 1
    }

    "EXIT" {
       set parameters "GUI=YES QUIET=$quietMode"

    }

    default {
       set unknownAction 1

       Error "Unrecognised action:"
       Error $action
    }
    }

#
#  Proceed if ok.

    if {$unknownAction == 0} then {

#
#     If the list is to be produced then initialise the list of items.

       if {$captureList != 0} then {
          global catalogueList
          set    catalogueList ""
       }

#
#     Send the given action to the ADAM application.

       global echoCommand

       if {$echoCommand != 0} then {
          set actionTxt "Action: "
          append actionTxt $action

          set parametersTxt "Parameters: "
          append parametersTxt $parameters

          .messages.output configure -state normal
          .messages.output insert end $actionTxt
          .messages.output insert end "\n"
          .messages.output insert end $parametersTxt
          .messages.output insert end "\n"
          .messages.output configure -state disabled
        }

       global catviewFinished
       set catviewFinished 0

#      puts stdout "Before Action: $action"
#      puts stdout "Parameters: $parameters\n\n"

       catview  obey  $action  $parameters \
         -endmsg {global catviewFinished; set catviewFinished 1}

#      puts stdout "After Action: $action"

       tkwait variable catviewFinished
    }

#
#  Destroy the dialogue box and restore the focus.

    destroy  .action
    focus    $oldFocus

}
