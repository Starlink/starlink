# IsysSkyviewDialog.tcl

# IsysSkyviewDialog
#

# Arguments:
#
# w -           Window to use for dialog top-level.
#

proc IsysSkyviewDialog {w RA1950 DEC1950 WidthInDeg} {

    global isyssky_priv
    global env
    global SKYVIEW
    global AST_BIN

# First time through?
    if [catch {set SKYVIEW(first)}] {
      set SKYVIEW(db) "digitized sky survey"
      set SKYVIEW(file) dss.fits
      set SKYVIEW(first) 1
      set SKYVIEW(xpix)  300
      set SKYVIEW(ypix)  300
      set SKYVIEW(eqnx)  2000
      set SKYVIEW(csys)  Equatorial
      set SKYVIEW(ra)    $RA1950
      set SKYVIEW(dec)   $DEC1950
      set SKYVIEW(wid)   $WidthInDeg
      }

# Mark as not finished
    set SKYVIEW(finished) 0

# Create the top level window.
    toplevel $w -class Dialog -bd 10
    wm title $w "Image Processing SKYVIEW Interface"
    wm iconname $w Isys.SKYVIEW
    wm transient $w .

# Set the name of the database client
    set client $AST_BIN/getsky
    set doit 0
    if { $client != "" } {
      if { [file exists $client] } {
        if { [file executable $client] } {
          set doit 1
          }
        }
      }


    if { $doit } { 
      frame $w.db

        frame $w.db.lhs

        label $w.db.lab -text "Database Selection"
        entry $w.db.dat -width 30 -fg blue -relief sunken -bd 2 \
                -textvariable SKYVIEW(db)

        frame $w.db.rhs
        listbox $w.db.rhs.lb -relief raised -bd 2 \
  	         -yscrollcommand "$w.db.rhs.scroll set" \
		 -font -adobe-courier-medium-r-normal--*-100-*-*-*-*-*-*
        pack $w.db.rhs.lb -side left -fill x
        scrollbar $w.db.rhs.scroll -command "$w.db.rhs.lb yview"
        pack $w.db.rhs.scroll -side right -fill y
        foreach i {"EGRET (3D)" "EGRET >100 MeV" "EGRET <100 MeV" COMPTEL" \
                   "HEAO 1 A-2" "RASS 1/4 keV" "RASS 1/4 keV" "RASS 1.5 keV" \
                   "Old PSPC (2 deg)" "ROSAT WFC F1" "ROSAT WFC F2" \
                   "EUVE 83 A" "EUVE 171 A" "EUVE 405 A" "EUVE 555 A" \
                   "Digitized Sky Survey" "COBE DIRBE" \
                   "IRAS 12 micron" "IRAS 25 micron" \
                   "IRAS 60 micron" "IRAS 100 micron" \
                   "4850MHz" "4850MHz(OLD)" "nH" \
                   "VLA FIRST (1.4GHz)" "VLA NVSS (1.4GHz)" \
                   "1420 MHz (Bonn)" "0408MHz" "0035MHz"} {
          $w.db.rhs.lb insert end $i
          }
        bind $w.db.rhs.lb <ButtonRelease-1> { \
       set SKYVIEW(db) [%W get [%W curselection]]}
        pack $w.db.lab $w.db.dat $w.db.rhs -side top -anchor e -fill x

      frame $w.ser
        frame $w.ser.ra
         label $w.ser.ra.lab -text "RA 1950: "
         entry $w.ser.ra.dat -width 20 -fg blue -relief sunken -bd 2 \
              -textvariable SKYVIEW(ra)
         pack $w.ser.ra.lab -side left
         pack $w.ser.ra.dat -side right
        frame $w.ser.dec
         label $w.ser.dec.lab -text "DEC 1950: "
         entry $w.ser.dec.dat -width 20 -fg blue -relief sunken -bd 2 \
              -textvariable SKYVIEW(dec)
         pack $w.ser.dec.lab -side left
         pack $w.ser.dec.dat -side right
        frame $w.ser.wid
         label $w.ser.wid.lab -text "Search box width (degrees): "
         entry $w.ser.wid.dat -width 20 -fg blue -relief sunken -bd 2 \
              -textvariable SKYVIEW(wid)
         pack $w.ser.wid.lab -side left
         pack $w.ser.wid.dat -side right
        pack $w.ser.ra $w.ser.dec $w.ser.wid -side top -anchor nw \
                -fill x

      frame $w.op
        frame $w.op.pix
          label $w.op.pix.lab -text "Image size (pixels): "
          entry $w.op.pix.xpixdat -width 5 -fg blue -relief sunken -bd 2 \
                -textvariable SKYVIEW(xpix)
          label $w.op.pix.times -text " x "
          entry $w.op.pix.ypixdat -width 5 -fg blue -relief sunken -bd 2 \
                -textvariable SKYVIEW(ypix)
          pack $w.op.pix.lab -side left -fill x
          pack $w.op.pix.ypixdat $w.op.pix.times $w.op.pix.xpixdat \
		-side right 
                
        frame $w.op.eqnx
          label $w.op.eqnx.lab -text "Coordinate equinox : "
          checkbutton $w.op.eqnx.new -text "2000" \
            -onvalue "2000" -offvalue "1950" -variable SKYVIEW(eqnx)
          checkbutton $w.op.eqnx.old -text "1950" \
            -onvalue "1950" -offvalue "2000" -variable SKYVIEW(eqnx)
          pack $w.op.eqnx.lab -side left -fill x
          pack $w.op.eqnx.old $w.op.eqnx.new -side right

        frame $w.op.csys
          label $w.op.csys.lab -text "Coordinate system : "
          radiobutton $w.op.csys.equ -text Equatorial \
 		-variable SKYVIEW(csys) -value Equatorial
          radiobutton $w.op.csys.ecl -text Ecliptic \
 		-variable SKYVIEW(csys) -value Ecliptic -state disabled
          radiobutton $w.op.csys.gal -text Galactic \
 		-variable SKYVIEW(csys) -value Galactic -state disabled
          pack $w.op.csys.lab -side left -fill x
          pack $w.op.csys.gal $w.op.csys.ecl $w.op.csys.equ -side right

        pack $w.op.pix $w.op.eqnx $w.op.csys -side top -fill x 

      frame $w.files
        label $w.files.lab -text "Output filename: "
        entry $w.files.dat -width 30 -fg blue -relief sunken -bd 2 \
              -textvariable SKYVIEW(file)
        pack $w.files.lab -side left 
        pack $w.files.dat -side right

#   Create an "OK" and a "Cancel" button.
      frame $w.ctrl
      button $w.ctrl.ok -text "Retrieve data" -width 12 -command {set isyssky_priv(button) "ok"}
      button $w.ctrl.can -text Cancel -width 6 \
  	-command {set isyssky_priv(button) "can"}

# Pack them into the bottom frame with a default border around the OK
# button.
      frame $w.ctrl.default -relief sunken -bd 1
      raise $w.ctrl.ok $w.ctrl.default
      pack $w.ctrl.default -side left -expand 1 -padx 3m -pady 2m
      pack $w.ctrl.ok -in $w.ctrl.default -padx 1m -pady 1m -ipadx 1m

      pack $w.ctrl.can -side left -expand 1 -padx 3m -pady 2m -ipadx 1m

      pack $w.db $w.ser $w.op $w.files $w.ctrl \
		-side top -fill x -anchor nw -ipadx 5 -ipady 5

# Bind the return key to the OK button.
      bind $w <Return> "$w.ctrl.ok flash; set isyssky_priv(button) ok"

# Centre the window
      WinCentre $w

#  Set a grab and claim the focus.
      set oldFocus [focus]
      grab $w
      focus $w

# Wait for the user to respond.
      for {} {1} {} {
	tkwait variable isyssky_priv(button)

	if ![string compare $isyssky_priv(button) ok] {

          set db [$w.db.dat get]
          set db [string tolower $db]
          if {[lsearch {xray optical radio} $db] > -1 } {
            set quantities "RA,DEC,NAME,DATABASE,CLASS"
          } elseif {"$db" == "gsc"} {
            set quantities "RA,DEC,EXT,QV_MAG,QB_MAG"
          } else {
            set quantities "RA,DEC,NAME,CLASS"
            }

        set file [$w.files.dat get]
 
        $w configure -cursor watch
        update idletasks

        set pid [pid]
        set tfile "/tmp/getsky_${pid}"
        if [file exists $tfile] {
          exec rm -f $tfile
          }

        set q ""

        set cmdstr "$client $SKYVIEW(ra) $SKYVIEW(dec) \
                        survey=$q$SKYVIEW(db)$q xpixel=$SKYVIEW(xpix) \
                        ypixel=$SKYVIEW(ypix) image_size=$SKYVIEW(wid) \
                        projection=Gnomonic coordinate=$SKYVIEW(csys) \
                        equinox=$SKYVIEW(eqnx) file=$SKYVIEW(file)"
        IsysHdbDoing $w $cmdstr

#    Re-centre the window
#        WinCentre $w

        set edata [catch {exec $client $SKYVIEW(ra) $SKYVIEW(dec) \
                        survey="$SKYVIEW(db)" xpixel=$SKYVIEW(xpix) \
                        ypixel=$SKYVIEW(ypix) image_size=$SKYVIEW(wid) \
                        projection=Gnomonic coordinate=$SKYVIEW(csys) \
                        equinox=$SKYVIEW(eqnx) file=$SKYVIEW(file) >&$tfile}]

#        set edata [catch {[eval exec $cmdstr >&$tfile]}]

        $w configure -cursor xterm

        IsysHdbTfile $w $tfile
        set SKYVIEW(finished) 1
        focus $oldFocus
	}
      destroy $w
      break
      }
  } else {
    destroy $w
    }    
  }
