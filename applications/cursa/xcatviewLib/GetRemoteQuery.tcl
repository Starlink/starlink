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
#   26/11/99 (ACD): Truncated Right Ascensions and Declinations by
#     removing decimal points.
#   13/4/01  (ACD): Modified for the new version of catremote written
#      Perl.
#   17/4/01  (ACD): First stable version for Perl version of catremote.
#   9/5/01   (ACD): Added Stripping of embedded spaces from object names.
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

#
#   Name given for the object to be at the centre of the query of the
#   remote database.

     global remoteQueryName

#
#  List of directories of remote catalogues which have been accessed.

    global env
    global catDirLevel
    global catDirList


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
       -width 60 -height 15  \
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

#   ... Object name.

     frame .getremotequery.in.query.name

     label .getremotequery.in.query.name.label  \
       -text "Object name:"  \
       -anchor w  -width 40
     pack  .getremotequery.in.query.name.label  -side left

     entry .getremotequery.in.query.name.value -width 15  \
       -relief sunken  -bd 2
     pack  .getremotequery.in.query.name.value -side left -padx 2m

     pack  .getremotequery.in.query.name -side top -pady 2m

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

#   ... Adopt previous list of catalogues.

     button .getremotequery.in.button.config \
       -text Previous \
       -width 6 \
       -command {

#
#         Check that not already at the first list of catalogues.

           if {$catDirLevel > 1} then {

#
#            Decrement the index for the list of catalogues.

              set  catDirLevel [expr $catDirLevel - 1]

#
#            Get the new list of catalogues.

              set env(CATREM_CONFIG) $catDirList($catDirLevel)
              RemoteList

#
#            Delete the previous list of catalogues from the list box
#            and copy in the new one.

              .getremotequery.cats.list delete 0 end
              set numCats [llength $remoteCatsLong]

              set counter 0

              while {$counter < $numCats} {
                 set currentCat [lrange $remoteCatsLong $counter $counter]
                 set currentCat [string trimleft $currentCat "{"]
                 set currentCat [string trimright $currentCat "}"]
                 .getremotequery.cats.list insert end $currentCat
                 set counter [expr $counter + 1]
              }
           } else {
              Error "Already at the first list of catalogues."
           }
        }

#   ... OK.

     button .getremotequery.in.button.ok \
       -text OK \
       -width 6 \
       -command {

#
#         Check if any object name has been specified.  If so then remove
#         any embadded spaces and attempt to determine its R.A. and Dec.
#         Otherwise attempt to read the R.A. and Dec. from the entry boxes.

           set remoteQueryName [.getremotequery.in.query.name.value  get]

           if {$remoteQueryName != ""} then {
              regsub -all " " $remoteQueryName "" remoteQueryName

              RemoteName

              if {$remoteQueryRA == ""} then {
                 Error "Enter a different name or enter the Right Ascension and Declination directly."

                 .getremotequery.in.query.name.value  configure -state normal
                 .getremotequery.in.query.name.value  delete 0 end
                 .getremotequery.in.query.name.value  configure -state disabled
              }

           } else {
              set  remoteQueryRA   [.getremotequery.in.query.ra.value  get]
              set  remoteQueryDec  [.getremotequery.in.query.dec.value get]

              if {$remoteQueryRA == ""} then {
                 Error "No Central Right Ascension specified."
              }

              if {$remoteQueryDec == ""} then {
                 Error "No Central Declination specified."
              }

           }

#
#         Get the query radius from the entry box.

           set remoteQueryRadius  [.getremotequery.in.query.radius.value get]

#
#         Check that values have been supplied for all the variables which
#         define the query.

           set gotValues 1

           if {$remoteQueryCat == ""} then {
              Error "No remote catalogue specified."
              set gotValues 0
           }

           if {$remoteQueryRA == ""} then {
              set gotValues 0
           }

           if {$remoteQueryDec == ""} then {
              set gotValues 0
           }

           if {$remoteQueryRadius  < 1} then {
              Error "Invalid value for the selection radius."
              set gotValues 0
           }

#
#         Set the "ok" button if all the required values have been set.

           if {$gotValues == 1} then {
              set getRemoteCatButton "ok"
           }
        }

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
#   required is:-
#
#   - if the entry is not a directory then copy the selected long database
#     name to the name box and then get the short database name,
#
#   - if the entry is a directory then make the directory the current
#     list of catalogues and display the new list of catalogues.

     bind .getremotequery.cats.list  <ButtonRelease-1>  {
        set numCat [.getremotequery.cats.list curselect]
        if {$numCat > -1} then {
           set chosenCat [.getremotequery.cats.list get $numCat]

#
#         Check whether the entry chosen is a directory or not.

           set dirPos [string first "(directory)" $chosenCat]

           if {$dirPos < 0} then {

#
#            Not a directory.

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

           } else {

#
#            A directory.  Get its URL and then get the list of catalogues
#            that it contains.

              global remoteDir
              global remoteUrl

              set remoteDir [lrange $remoteCatsShort $numCat $numCat]
              set remoteDir [string trimleft $remoteDir "{"]
              set remoteDir [string trimright $remoteDir "}"]

              RemoteUrl

              set env(CATREM_CONFIG) $remoteUrl

              RemoteList

#
#            Delete the previous list of catalogues from the list box
#            and copy in the new one.

              .getremotequery.cats.list delete 0 end

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
#            Update the list of directories.

              set  catDirLevel [expr $catDirLevel + 1]
              set  catDirList($catDirLevel) $remoteUrl
           }
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

#
#      If a decimal fraction was included in the Right Ascension then
#      remove it and report a warning.

        set dotpos [string first "." $remoteQueryRA ]

        if {$dotpos > 0} then {
           set    finish          [expr $dotpos - 1]
           set    remoteQueryRA   [string range $remoteQueryRA 0 $finish]
           set    errorQueryRA    $remoteQueryRA
           set    errorQueryRA    [string trimleft $errorQueryRA ":"]
           Error "(Warning) Truncated Right Ascension to $errorQueryRA"
        }

#
#      If a decimal fraction was included in the Declination then
#      remove it and report a warning.

        set dotpos [string first "." $remoteQueryDec ]

        if {$dotpos > 0} then {
           set    finish          [expr $dotpos - 1]
           set    remoteQueryDec   [string range $remoteQueryDec 0 $finish]
           set    errorQueryDec $remoteQueryDec
           set    errorQueryDec    [string trimleft $errorQueryDec ":"]
           Error "(Warning) Truncated Declination to $errorQueryDec"
        }

#
#      Double-check that values have been supplied for all the variables
#      which define the query.

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

#        puts stdout "exiting from GetRemoteQuery"
#        puts stdout "remoteQueryRA: $remoteQueryRA"
#        puts stdout "remoteQueryDec: $remoteQueryDec"
     }


     if {$getRemoteCatButton == "can"} then {
        set remoteQueryCat     ""
     }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .getremotequery
     focus    $oldFocus
}
