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

#
#   Create a frame to hold the configuration button and the control
#   buttons, then create and add these buttons.

     frame .getremotequery.in.button

#   ... Configuration.

     button .getremotequery.in.button.config \
       -text Config. \
       -width 6 \
       -command {GetRemoteDetails}

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

#   ... Help

     button .getremotequery.in.button.help \
       -text Help \
       -width 6 \
       -command {HelpText GetRemoteQuery_help}

#
#   Pack the buttons into their frame, then the button frame into its
#   frame.

     pack .getremotequery.in.button.config  \
       -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     frame .getremotequery.in.button.default -relief sunken -bd 1
     raise .getremotequery.in.button.ok .getremotequery.in.button.default

     pack  .getremotequery.in.button.ok -in .getremotequery.in.button.default \
       -side left  -padx 1m -pady 1m -ipadx 1m

     pack .getremotequery.in.button.default  \
       -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getremotequery.in.button.can   \
       -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getremotequery.in.button.help  \
       -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getremotequery.in.button  -side top

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
        set    remoteQueryRA      :
        append remoteQueryRA      [.getremotequery.in.query.ra.value  get]

        set    remoteQueryDec     :
        append remoteQueryDec     [.getremotequery.in.query.dec.value get]

        set remoteQueryRadius  [.getremotequery.in.query.radius.value get]
        set remoteQueryMaxRows [.getremotequery.in.query.maxrows.value get]

#
#      Check that values have been supplied for all the variables which
#      define the query.

        if {$remoteQueryCat == ""} then {
           Error "No remote catalogue specified."
        }

        if {$remoteQueryRA == ""} then {
           Error "No Central Right Ascension specified."
           set remoteQueryCat ""
        }

        if {$remoteQueryDec == ""} then {
           Error "No Central Declination specified."
           set remoteQueryCat ""
        }

        if {$remoteQueryRadius  < 1} then {
           Error "Invalid value for the selection radius."
           set remoteQueryCat ""
        }

        if {$remoteQueryMaxRows < 1} then {
           Error "Invalid value for the maximum number of rows."
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
