#+ IsysCursa procedures
#
#  Procedures to invoke catremote to access remote catalogues.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   19/5/97 (ACD): The original prototype replaced with a version
#      hacked from the corresponding xcatview procedures.

proc IsysCursaInit {} {

#
#  This initialisation procedure initialises various global variables.

#
#  The catremote execution module.

    global CURSA_DIR
    global catremoteExe
    set    catremoteExe  $CURSA_DIR/catremote



#
#  Details of query submitted to a remote database.  Note that the
#  initial values for the query centre and radius are obtained from
#  the details of the underlying image.  The values are, however,
#  reformatted into the form required by catremote.  Briefly, these
#  are:
#
#  remoteQueryRA:
#    Sexagesimal hours, with a colon as separator and an integer
#    number of seconds.
#
#  remoteQueryDec:
#    Sexagesimal degrees, with a colon as separator and an integer
#    number of seconds.
#
#  remoteQueryRadius:
#    An integer number of minutes of arc.

    global remoteQueryCat
    set    remoteQueryCat     ""

    global Ra
    global Dec
    global nbid

    set lx [nbs get $nbid.regxmin]
    set hx [nbs get $nbid.regxmax]
    set wtodeg [nbs get $nbid.wtodeg]
    set wid [expr abs($hx - $lx)*1.4* $wtodeg *60.0]

    global remoteQueryRA
    regsub -all  h|d|m  $Ra  ":"  temp
    regsub {\.[1234567890]*}  $temp  ""  remoteQueryRA

    global remoteQueryDec
    regsub -all  h|d|m  $Dec  ":"  temp
    regsub {\.[1234567890]*}  $temp  ""  remoteQueryDec

    global remoteQueryRadius
    regsub {\.[1234567890]*$}  $wid  ""  remoteQueryRadius
    set remoteQueryRadius [expr $remoteQueryRadius + 1]

    global remoteQueryMaxRows
    set    remoteQueryMaxRows 100

#
#  Names of optional files specifying details of the subset extracted
#  from a remote database.

    global remoteQueryDetails
    set    remoteQueryDetails "none"

    global remoteQueryCols
    set    remoteQueryCols    "none"

#
#  Name of the catalogue returned by catremote.

    global catalogueName  ""

}

proc IsysCursa {w} {


#+ IsysCursa
#
#  Extract and open a subset selected from a remote database.
#
#  Given:
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   26:3/97 (ACD): Original version.
#   19/5/97 (ACD): Hacked for Isys.
#-

     toplevel $w -class Dialog
     wm withdraw $w
     wm title $w "Cursa Interface"
     wm iconname $w Cursa

#
#   Declare the global variables.
#   ----------------------------

     global remoteCatsShort
     global remoteQueryCat

     global catalogueName

     global MarkPositions NumberPositions AppendPositions

     global FilePos
#
#   Attempt to determine the list of databases available on the remote
#   server and proceed if ok.  Note that the remote server is only
#   queried for the list if it is not already available.

     if {$remoteCatsShort == ""} then {
        RemoteList
     }

     if {$remoteCatsShort != ""} then {

#
#      Attempt to define the query and proceed if ok.

        GetRemoteQuery

        if {$remoteQueryCat != ""} then {
           RemoteQuery

           if {$catalogueName != ""} then {
	       if {$MarkPositions == "Y"} {
                ImgExecWait imark "list=$catalogueName  NUMBER=$NumberPositions  accept"
               }
               if {$AppendPositions == "Y"} {
                ImgExecWait iposit "mode=ENT list=$catalogueName  accept"
                ImgExecWaitNoMsg iposit "mode=SAV file=$FilePos accept"
               }
           } else {
              Show_Error .error "Failure querying the remote catalogue."
           }
        }

     } else {
        Show_Error .error "Failed to obtain list of catalogues on the remote server."
        set catalogueName  ""
     }

  destroy $w

}

# -------------------------------------------------------------------------

proc RemoteList { } {

#+ RemoteList
#
#  Interrogate a remote server to obtain a list of the databasees
#  available on it.
#
#  Given.
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   25/3/97 (ACD): Original version.
#   19/5/97 (ACD): xcatview version converted to Isys; replaced expect
#      with standard tcl exec.
#-

#
#  Declare global variables.
#  ------------------------

    global catremoteExe

    global remoteCatsShort
    global remoteCatsLong


    global AST_ETC

#
#  Create the top-level window and lower its stacking order.

    toplevel     .remotelist   -class Dialog   -bd 10

    wm withdraw .remotelist

    wm title     .remotelist   "Remote List"
    wm iconname  .remotelist   Remote
    wm transient .remotelist   .

#  Write text into it.

    message .remotelist.text -width 8c -text \
    "Please wait (obtaining list of catalogues from remote server)..."
    pack  .remotelist.text

#
#  Withdraw the window, then update all the geometry information
#  to determine how big the window wants to be, then centre the window
#  in parent and de-iconify it.

    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .remotelist]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .remotelist]/2 + \
      [winfo y .]]
    wm geom .remotelist +$x+$y
    wm deiconify .remotelist

#
#  Set a grab and claim the focus.

    set oldFocus [focus]
    grab  .remotelist
    focus .remotelist
    update idletasks

#
#  Run catremote to generate the list of catalogues accessible and
#  write the list to a file.

    set remoteStatus [catch {exec $catremoteExe GUI  \
      <$AST_ETC/Isys.catlist  >CATSLIST.LIS}]

    if {$remoteStatus == 0} then {

#
#     Attempt to open the file containing the list of remote catalogues.

       set f [open CATSLIST.LIS r]

#
#     Loop reading back all the remote catalogues.

       while {[gets $f current_line] >= 0} {

#
#        Remove carriage returns and new line characters from the output.

          set current_line [string trim $current_line \n]
          set current_line [string trim $current_line \r]

#
#        Check whether the current line starts with a vertical bar ('|').
#        If it does then it contains genuine output.  If it does not
#        it contains either an error or warning message or an echo of
#        commands sent to the ADAM application.

          set vbar_pos [string first | $current_line]

          if {$vbar_pos == 0} then {

#
#           Remove the leading vertical bar and append it to the list
#           of catalogues.

             set firstSpace [string first " " $current_line]

             lappend remoteCatsShort  \
               [string range $current_line 1 $firstSpace]

             set currentLong [string range $current_line $firstSpace end]
             lappend remoteCatsLong  [string trimleft $currentLong]

          } else {

#
#           Display genuine error messages.

             set excl_pos [string first ! $current_line]

             if {$excl_pos == 0} then {
                Show_Error .error $current_line
             }
          }
       }

#
#     Close and delete the file holding the list of catalogues.

       close $f
#       exec rm CATSLIST.LIS

    } else {
       Show_Error .error \
         "Failed to obtain list of catalogues from configuration file."
    }

#
#  Destroy the dialogue box and restore the focus.

    destroy  .remotelist
    focus    $oldFocus

}

# -------------------------------------------------------------------------

proc GetRemoteQuery { } {

#+ GetRemoteQuery
#
#  Procedure to obtain the details of a query to be submitted to a
#  database on a remote server.
#
#  Given:
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   26/3/97  (ACD): Original version.
#   9/4/97   (ACD): Modified comments for Declination and bound the
#     return key to the global "ok" button.
#   20/5/97  (ACD): Modified for Isys.
#-
#
#   Declare the global variables.
#   ----------------------------

#
#   The state returned by clicking on the "OK" or "Cancel" buttons.

     global getRemoteCatButton

#
#   Lists of the catalogues in the remote database

     global remoteCatsShort
     global remoteCatsLong

#
#   Details of query submitted to a remote database.

     global remoteQueryCat
     global remoteQueryRA
     global remoteQueryDec
     global remoteQueryRadius
     global remoteQueryMaxRows


#
#   Create the top level window.

     toplevel     .getremotequery  -class Dialog   -bd 10
     wm title     .getremotequery  "Selection from remote database"
     wm iconname  .getremotequery  Remote
     wm transient .getremotequery  .

#
#   Bind the return key to set global variable button to "ok".  That is, 
#   hitting return will have the same effect as clicking on the "ok"
#   button.

     bind  .getremotequery <Return> {set getRemoteCatButton "ok" }

#
#   Create a frame to hold the listbox, create the listbox and its
#   associated scroll bars.

     frame  .getremotequery.cats

     scrollbar .getremotequery.cats.xscroll  -orient horizontal \
       -command {.getremotequery.cats.list  xview} \
       -relief sunken  -bd 2

     scrollbar .getremotequery.cats.yscroll  -orient vertical \
       -command {.getremotequery.cats.list  yview} \
       -relief sunken  -bd 2

     listbox .getremotequery.cats.list  -relief groove  -bd 2  \
       -width 30 -height 15  \
       -xscroll {.getremotequery.cats.xscroll  set}  \
       -yscroll {.getremotequery.cats.yscroll  set}

#
#   Pack the listbox and scroll bars into their frame and pack the
#   frame into the window.

     pack .getremotequery.cats.yscroll  -side left    -fill y
     pack .getremotequery.cats.xscroll  -side bottom  -fill x
     pack .getremotequery.cats.list     -expand yes   -fill both

     pack .getremotequery.cats  -side left  -padx 3m  -fill y

#
#   Insert the list of catalogues on the remote server into the list box.

     set numCats [llength $remoteCatsLong]

     set counter 0

     while {$counter < $numCats} {
        set currentCat [lrange $remoteCatsLong $counter $counter]
        set currentCat [string trimleft $currentCat "{"]
        set currentCat [string trimright $currentCat "}"]
        .getremotequery.cats.list insert end $currentCat
        set counter [expr $counter + 1]
     }

#
#   Create a frame to hold the various entry boxes and buttons etc.

     frame .getremotequery.in

#
#   Create a sub-frame for the name of the chosen remote catalogue.
#   Both the full description and the short form name are displayed.

     frame .getremotequery.in.name

     label .getremotequery.in.name.title  \
       -text "Remote Catalogue:"  \
       -anchor w  -width 60
     pack  .getremotequery.in.name.title -side top  -anchor w

     entry .getremotequery.in.name.value -width 60  -relief flat  \
       -highlightthickness 0
     pack  .getremotequery.in.name.value -side top  -anchor w
     .getremotequery.in.name.value  configure  -state disabled

    label .getremotequery.in.name.title2 \
       -text "Abbreviation:"  \
       -anchor w  -width 60
     pack  .getremotequery.in.name.title2 -side top  -anchor w

     entry .getremotequery.in.name.abbrvn -width 60  -relief flat  \
       -highlightthickness 0
     pack  .getremotequery.in.name.abbrvn -side top  -anchor w
     .getremotequery.in.name.abbrvn  configure  -state disabled

     pack  .getremotequery.in.name -side top -pady 0m

#
#   Create a sub-frame for the query details.  Create and pack into
#   it the menu buttons and value boxes for the query details.

     frame .getremotequery.in.query

#   ... Right Ascension.

     frame .getremotequery.in.query.ra

     label .getremotequery.in.query.ra.label  \
       -text "Central Right Ascension (hh:mm:ss):"  \
       -anchor w  -width 40
     pack  .getremotequery.in.query.ra.label  -side left

     entry .getremotequery.in.query.ra.value -width 15  \
       -relief sunken  -bd 2
     pack  .getremotequery.in.query.ra.value -side left -padx 2m
     .getremotequery.in.query.ra.value insert 0 $remoteQueryRA

     pack  .getremotequery.in.query.ra -side top -pady 2m

#   ... Declination.

     frame .getremotequery.in.query.dec

     label .getremotequery.in.query.dec.label  \
       -text "Central Declination (sdd:mm:ss):"  \
       -anchor w  -width 40
     pack  .getremotequery.in.query.dec.label  -side left

     entry .getremotequery.in.query.dec.value -width 15  \
       -relief sunken  -bd 2
     pack  .getremotequery.in.query.dec.value -side left -padx 2m
     .getremotequery.in.query.dec.value insert 0 $remoteQueryDec

     pack  .getremotequery.in.query.dec -side top -pady 2m

#   ... Radius

     frame .getremotequery.in.query.radius

     label .getremotequery.in.query.radius.label  \
       -text "Radius (minutes of arc):"  \
       -anchor w  -width 40
     pack  .getremotequery.in.query.radius.label  -side left

     entry .getremotequery.in.query.radius.value -width 15  \
       -relief sunken  -bd 2
     pack  .getremotequery.in.query.radius.value -side left -padx 2m
     .getremotequery.in.query.radius.value insert 0 $remoteQueryRadius

     pack  .getremotequery.in.query.radius -side top -pady 2m

#   ... Maximum number of rows.

     frame .getremotequery.in.query.maxrows

     label .getremotequery.in.query.maxrows.label  \
       -text "Maximum permitted number of rows:"  \
       -anchor w  -width 40
     pack  .getremotequery.in.query.maxrows.label  -side left

     entry .getremotequery.in.query.maxrows.value -width 15  \
       -relief sunken  -bd 2
     pack  .getremotequery.in.query.maxrows.value -side left -padx 2m
     .getremotequery.in.query.maxrows.value insert 0 $remoteQueryMaxRows

     pack  .getremotequery.in.query.maxrows -side top -pady 2m

#
#   Create a text box to hold a message about the equinox of the
#   coordinates.

     text .getremotequery.in.query.equinox -height 1 -width 60 -relief flat  \
       -highlightthickness 0
     pack .getremotequery.in.query.equinox  -side top  -anchor w
     .getremotequery.in.query.equinox  insert 1.0  \
       "The coordinates should be for equinox J2000."

#
#   Pack the query frame.

     pack  .getremotequery.in.query -side top -pady 10m


#   create a frame for radiobuttons controling what happens to returned positions

     frame .getremotequery.in.control1
     frame .getremotequery.in.control2
     frame .getremotequery.in.control3

     global MarkPositions NumberPositions AppendPositions
     set MarkPositions Y
     set NumberPositions N
     set AppendPositions N

     label .getremotequery.in.control1.lbl -width 15 -text "Mark points"
     radiobutton .getremotequery.in.control1.on -text on -variable MarkPositions -value Y \
                     -anchor w
     radiobutton .getremotequery.in.control1.off -text off -variable MarkPositions -value N \
                     -anchor w
     pack .getremotequery.in.control1.lbl .getremotequery.in.control1.on \
              .getremotequery.in.control1.off -side left

     label .getremotequery.in.control2.lbl -width 15 -text "Number points"
     radiobutton .getremotequery.in.control2.on -text on -variable NumberPositions -value Y \
                     -anchor w
     radiobutton .getremotequery.in.control2.off -text off -variable NumberPositions -value N \
                     -anchor w
     pack .getremotequery.in.control2.lbl .getremotequery.in.control2.on \
              .getremotequery.in.control2.off -side left

     label .getremotequery.in.control3.lbl -width 15 -text "Append to list"
     radiobutton .getremotequery.in.control3.on -text on -variable AppendPositions -value Y \
                     -anchor w
     radiobutton .getremotequery.in.control3.off -text off -variable AppendPositions -value N \
                     -anchor w
     pack .getremotequery.in.control3.lbl .getremotequery.in.control3.on \
              .getremotequery.in.control3.off -side left

     pack .getremotequery.in.control1 .getremotequery.in.control2 .getremotequery.in.control3 \
                -side top

#
#   Create a frame to hold the the control buttons, then create and add
#   these buttons.

     frame .getremotequery.in.button

#   ... OK.

     button .getremotequery.in.button.ok \
       -text OK \
       -width 6 \
       -command {set getRemoteCatButton "ok"}

#   ... Cancel.

     button .getremotequery.in.button.can \
       -text Cancel \
       -width 6 \
       -command {set getRemoteCatButton "can"}

#
#   Pack the buttons into their frame, then the button frame into its
#   frame.

     frame .getremotequery.in.button.default -relief sunken -bd 1
     raise .getremotequery.in.button.ok .getremotequery.in.button.default

     pack  .getremotequery.in.button.ok -in .getremotequery.in.button.default \
       -side left  -padx 1m -pady 1m -ipadx 1m

     pack .getremotequery.in.button.default  \
       -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getremotequery.in.button.can   \
       -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getremotequery.in.button  -side top -pady 3m

#
#   Pack the frame holding the various entry boxes and buttons etc.

     pack .getremotequery.in  -side left  -pady 10m


#
#   Bind an action to a mouse click in the list box.  The action
#   required is to copy the selected long database name to the
#   name box and then get the short database name.

     bind .getremotequery.cats.list  <ButtonRelease-1>  {
        set numCat [.getremotequery.cats.list curselect]
        if {$numCat > -1} then {
           set chosenCat [.getremotequery.cats.list get $numCat]

           .getremotequery.in.name.value  configure -state normal
           .getremotequery.in.name.value  delete 0 end
           .getremotequery.in.name.value  insert end $chosenCat
           .getremotequery.in.name.value  configure -state disabled

           set remoteQueryCat [lrange $remoteCatsShort $numCat $numCat]
           set remoteQueryCat [string trimleft $remoteQueryCat "{"]
           set remoteQueryCat [string trimright $remoteQueryCat "}"]

           .getremotequery.in.name.abbrvn  configure -state normal
           .getremotequery.in.name.abbrvn  delete 0 end
           .getremotequery.in.name.abbrvn  insert end $remoteQueryCat
           .getremotequery.in.name.abbrvn  configure -state disabled

           focus -force .getremotequery
        }
     }

     bindtags .getremotequery.cats.list {.getremotequery.cats.list Listbox}

#
#   Withdraw the window, then update all the geometry information
#   to determine how big the window wants to be, then centre the window
#   in parent and de-iconify it.

     wm withdraw .getremotequery
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .getremotequery]/2 + \
       [winfo x .]]
     set y [expr [winfo height .]/2 - [winfo reqheight .getremotequery]/2 + \
       [winfo y .]]
     wm geom .getremotequery +$x+$y
     wm deiconify .getremotequery

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab  .getremotequery
     focus .getremotequery

     tkwait variable getRemoteCatButton

     if {$getRemoteCatButton == "ok"} then {
        set    remoteQueryRA      ""
        append remoteQueryRA      [.getremotequery.in.query.ra.value  get]

        set    remoteQueryDec     ""
        append remoteQueryDec     [.getremotequery.in.query.dec.value get]

        set remoteQueryRadius  [.getremotequery.in.query.radius.value get]
        set remoteQueryMaxRows [.getremotequery.in.query.maxrows.value get]


#
#      Check that values have been supplied for all the variables which
#      define the query.

        if {$remoteQueryCat == ""} then {
           Show_Error .error "No remote catalogue specified."
        }

        if {$remoteQueryRA == ""} then {
           Show_Error .error "No Central Right Ascension specified."
           set remoteQueryCat ""
        }

        if {$remoteQueryDec == ""} then {
           Show_Error .error "No Central Declination specified."
           set remoteQueryCat ""
        }

        if {$remoteQueryRadius  < 1} then {
           Show_Error .error "Invalid value for the selection radius."
           set remoteQueryCat ""
        }

        if {$remoteQueryMaxRows < 1} then {
           Show_Error .error "Invalid value for the maximum number of rows."
           set remoteQueryCat ""
        }
     }

     if {$getRemoteCatButton == "can"} then {
        set remoteQueryCat     ""
     }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .getremotequery
     focus    $oldFocus

}

# -------------------------------------------------------------------------

proc RemoteQuery { } {

#+ RemoteQuery
#
#  Perform a selection on a catalogue available via a remote server.
#  Note that the only type of selection available is a 'cone search'
#  to choose the objects which lie within a given circular area of sky.
#
#  Given.
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   26/3/97 (ACD): Original version.
#   19/5/97 (ACD): Modified for Isys.
#-

#
#  Declare global variables.
#  ------------------------

    global catremoteExe

    global remoteQueryCat
    global remoteQueryRA
    global remoteQueryDec
    global remoteQueryRadius
    global remoteQueryMaxRows

    global remoteQueryDetails
    global remoteQueryCols

    global catalogueName

#
#  Create the top-level window and lower its stacking order.

    toplevel     .remotequery   -class Dialog   -bd 10

    wm withdraw .remotequery

    wm title     .remotequery   "Remote List"
    wm iconname  .remotequery   Remote
    wm transient .remotequery   .

#  Write text into it.

    message .remotequery.text -width 8c -text  \
      "Please wait (querying remote catalogue)..."
    pack  .remotequery.text

#
#  Withdraw the window, then update all the geometry information
#  to determine how big the window wants to be, then centre the window
#  in parent and de-iconify it.

    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .remotequery]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .remotequery]/2 + \
      [winfo y .]]
    wm geom .remotequery +$x+$y
    wm deiconify .remotequery
    update idletasks

#
#  Set a grab and claim the focus.

    set oldFocus [focus]
    grab  .remotequery
    focus .remotequery
    update idletasks

#
#  Write the query details to a local file.

    set f [open QUERY.LIS w]

    puts $f $remoteQueryCat\r
    puts $f $remoteQueryMaxRows\r
    puts $f $remoteQueryRA\r
    puts $f $remoteQueryDec\r
    puts $f $remoteQueryRadius\r
    puts $f $remoteQueryDetails\r
    puts $f $remoteQueryCols\r

    close $f

#
#  Run catremote to access the remote catalogue and write its output
#  to a file.

    set remoteStatus [catch {exec $catremoteExe GUI  \
       <QUERY.LIS  >CATQUERY.LIS}]

    if {$remoteStatus == 0} then {

#
#     Loop reading back the file returned by catremote.

       set catalogueName ""

       set f [open CATQUERY.LIS r]
       while {[gets $f current_line] >= 0} {

#
#        Remove carriage returns and new line characters from the output.

          set current_line [string trim $current_line \n]
          set current_line [string trim $current_line \r]

#
#        Check whether the current line starts with a vertical bar ('|').
#        If it does then it contains genuine output.  If it does not
#        it contains either an error or warning message or an echo of
#        commands sent to the ADAM application.

          set vbar_pos [string first | $current_line]

          if {$vbar_pos == 0} then {

#
#           Remove the leading vertical bar and copy the value
#           to become the name of the new catalogue.

             set firstSpace [string first " " $current_line]

             set catalogueName [string range $current_line 1 end]

          } else {

#
#           Report genuine errors.

             set excl_pos [string first ! $current_line]

             if {$excl_pos == 0} then {
                Show_Error .error $current_line
             }
          }
       }

#
#     Close the catremote output file and delete the temporary files.

       close $f
       exec rm CATQUERY.LIS
#       exec rm QUERY.LIS

    } else {
        Show_Error .error \
          "Failed to access the remote catalogue."
        set catalogueName ""
    }

#
#  Destroy the dialogue box and restore the focus.

    destroy  .remotequery
    focus    $oldFocus
}
