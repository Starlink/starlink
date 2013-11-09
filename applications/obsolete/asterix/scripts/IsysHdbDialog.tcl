# IsysHdbDialog.tcl

# IsysHdbDialog
#
# This procedure creates a modal dialog box with controls for setting the
# the extent and spacing of the radial samples in CLPLOT.
#
# Arguments:
#
# w -           Window to use for dialog top-level.
#

proc IsysHdbError {w txt} {
  frame $w.hdb
  message $w.hdb.msg -width 8c -justify left -relief raised -bd 2 \
          -text $txt
  set done 0
  button $w.hdb.ok -text Ok -command {set done 1}
  pack $w.hdb.msg $w.hdb.ok -side top -fill x
  pack $w.hdb
  tkwait variable done
  }


global XANADU
set XANADU ""
if { [info exists env(XANADU)]} {
  set XANADU $env(XANADU)
  }

#
# Centre a window over its parent
#
proc WinCentre {w} {
# Withdraw the window, then update all the geometry information
# so we know how big it wants to be, then center the window in
# parent and de-iconify it.
  wm withdraw $w
  update idletasks
  set parent [winfo parent $w]
  set x [expr [winfo width $parent]/2 - [winfo reqwidth $w]/2 \
          + [winfo x $parent]]
  set y [expr [winfo height $parent]/2 - [winfo reqheight $w]/2 \
          + [winfo y $parent]]
  wm geom $w +$x+$y
  wm deiconify $w

  update idletasks
  }



proc IsysGetXanProg {w executable descrip} {
  global XANADU env exec xanprog_priv

  set XanaduEquivs(alpha_OSF1) osf
  set XanaduEquivs(sun4_Solaris) sol

# Get machine architecture
  set astmach $env(AST_MACHINE)
  set mach $XanaduEquivs($astmach)

# Default return value
  set exec ""
  set xanprog_priv 0

# Check for XANADU environment variable
  if { $XANADU != "" } {

#   Check version of ximage is suitable
    if [file exists $XANADU/$mach/bin/$executable] {
      set exec $XANADU/$mach/bin/$executable
      set mode 0
      set xanprog_priv 1

    } else {
      set txt "The $descrip program is not\
              present in your XANADU installation. This probably\
              means your version of ximage is out of date, or \
              its installation did not finish successfully. Enter \
		the name of the executable if you have another copy,\
		or get your XANADU expert to fix your installation!"
      set mode 1
      }
 
  } else {
    set txt "The XANADU environment variable is not defined. This\
        means you haven't executed the command which defines XANADU commands \
        before starting Isys. Either restart Isys or type the value of the \
	XANADU environment variable into the box below. You can obtain this \
	by typing your site's XANADU start up command followed by \
	echo \$XANADU."
    set mode 2
    }

# Issue warning
  if { ! $xanprog_priv } {

# Centre the window
    WinCentre $w 

    frame $w.hdb
    message $w.hdb.msg -width 10c -justify left -relief raised -bd 2 \
           -text $txt
    entry $w.hdb.dat -width 50 -textvariable exec -fg blue
    bind $w.hdb.dat <Return> "$w.hdb.buts.ok flash; set xanprog_priv 1"
    frame $w.hdb.buts
    button $w.hdb.buts.ok -bd 2 -text Ok -command {set xanprog_priv 1}
    button $w.hdb.buts.can -bd 2 -text Cancel -command {set xanprog_priv 0}
    pack $w.hdb.buts.ok $w.hdb.buts.can -side left -fill x
    pack $w.hdb.msg $w.hdb.dat $w.hdb.buts -side top -fill x
    pack $w.hdb

    set oldfocus [focus]
    focus $w.hdb.dat

    tkwait variable xanprog_priv

    focus $oldfocus

    destroy $w.hdb

    if { ($xanprog_priv > 0) && "$exec" != "" } {
      if { $mode == 1 } {
      } elseif { $mode == 2 } {
        set XANADU $exec
        set exec $XANADU/$mach/bin/$executable
        }
    } else {
      set exec ""
      }
    }

  return $exec
  }



proc IsysHdbDoing {w cmd} {
  pack forget $w.ctrl
  frame $w.hdbcmd
  label $w.hdbcmd.lab -text "Executing: " 
  text $w.hdbcmd.msg -relief raised -bd 2 -height 3
  $w.hdbcmd.msg insert end $cmd
  pack $w.hdbcmd.lab -side top -anchor nw
  pack $w.hdbcmd.msg -ipadx 10 -side top -fill x
  pack $w.hdbcmd
  update idletasks
  }

proc IsysHdbTfile {w tfile} {
  frame $w.hdb
  label $w.hdb.lab -text "Output: " 
  text $w.hdb.msg -relief raised -bd 2 -height 12
  set f [open $tfile]
  while {![eof $f]} {
     $w.hdb.msg insert end [read $f 1000]
     }
  close $f
  set done 0
  button $w.hdb.ok -text Ok -command {set done 1}
  pack $w.hdb.lab -anchor nw
  pack $w.hdb.msg -ipadx 10 -side top -fill x
  pack $w.hdb.ok -side top -fill x
  pack $w.hdb
  tkwait variable done
  }

proc SetCatName {descrip} {
  global HDB

  set fsp [string first " " $descrip]

  set cn [string range $descrip 0 [expr $fsp - 1]]

  set HDB(db) $cn
  set cn [string tolower $cn]
  set HDB(file) "${cn}.txt"
  }


proc IsysGetXanProgDialog {w prog descrip} {

# Create the top level window.
  toplevel $w -class Dialog -bd 10
  wm title $w "Isys XANADU Interface"
  wm transient $w .

# Get the name of the database client
  set client [IsysGetXanProg $w $prog $descrip]

# Destroy this window
  destroy $w

# Returnn client name
  return $client
  }




proc IsysHdbDialog {w RA1950 DEC1950 WidthInArcmin} {

    global isyshdb_priv
    global env
    global HDB

# First time through?
    if [catch {set HDB(first)}] {
      set HDB(db) XRAY
      set HDB(file) xray.txt
      set HDB(first) 1
      set HDB(mark)  1
      set HDB(mark_n) n
      set HDB(ra) $RA1950
      set HDB(dec) $DEC1950
      set HDB(wid) $WidthInArcmin
      }

# Mark as not finished
    set HDB(finished) 0

# Create the top level window.
    toplevel $w -class Dialog -bd 10
    wm title $w "Image Processing HEASARC Database Access"
    wm iconname $w Isys.HDB
    wm transient $w .

# Get the name of the database client
    set client [IsysGetXanProg $w hdbcone "HEASARC Database Client"]
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
        entry $w.db.dat -width 15 -fg blue -relief sunken -bd 2 \
                -textvariable HDB(db)

        frame $w.db.rhs
        listbox $w.db.rhs.lb -relief raised -bd 2 -width 45 \
  	         -yscrollcommand "$w.db.rhs.scroll set" \
		 -font -adobe-courier-medium-r-normal--*-100-*-*-*-*-*-*
        pack $w.db.rhs.lb -side left -fill x
        scrollbar $w.db.rhs.scroll -command "$w.db.rhs.lb yview"
        pack $w.db.rhs.scroll -side right -fill y

        foreach mcat {
		"ALLDATA    Archival Data*"
		"OPTICAL    Optical Catalogs*"
		"RADIO      Radio Catalogs*"
		"XCOLL      X-ray Collim. Catalogs*"
		"XRAY       X-ray Catalogs*"
          } {
          $w.db.rhs.lb insert end $mcat
          }

        foreach bcat {
"ABELL      Abell Clusters                "
"ACRS       Astrographic Cat of Ref Stars "
"BD         Bonner Durchmusterung         "
"BESTARS    Be Stars                      "
"BSC5P      Bright Stars                  "
"CABSCAT    Chromospherically Active Binary"
"CARBONSTAR Cool Carbon Stars Catalog     "
"CNS3       Third Catalog Of Nearby Stars "
"CPSTARS    Gen Catalog of Ap & Am Stars  "
"CVCAT      Cataclysmic Variables Catalog "
"DIXON      Dixon Radio Sources           "
"DUERBECK   Galactic Novae Ref Catalog    "
"ESOUPPSALA ESO-Uppsala ESO(B) Atlas Survey"
"GLOBCLUST  Galactic Globular Clusters    "
"GSC        Guide Star Catalog            "
"HBC        3d Emission-Line Star Catalog "
"HD         Henry Draper Catalog          "
"HIC        Hipparcos Input Main Catalog  "
"HIIREGION  Sharpless H II Region Catalog "
"KUEHR      Extragal. Radio Sources       "
"LBN        Lynds Bright Nebulae Catalog  "
"LDN        Lynds Dark Nebulae Catalog    "
"M31STARS   Field of M31 Bright Stars     "
"MARKARIAN  Markarian Galaxies Catalog    "
"MCG        Morphological Galaxy          "
"MCKSION    McCook & Sion WD Catalog      "
"MESSIER    Messier Catalog               "
"MRC        Molonglo Radio Src Catalog    "
"NGC2000    NGC 2000.0 Catalog            "
"NLTT       NLTT Catalog & 1st Supplement "
"NORTH20CM  20cm Radio Catalog            "
"NORTH6CM   6cm Radio Catalog             "
"OPENCLUST  Catalog of Open Clusters      "
"OSTARS     Galactic O-Stars Catalog      "
"PKSCAT90   Parkes Southern Radio Catalog "
"PLNEBULAE  Strasbourg Gal Planet Nebulae "
"PMN        PMN Surveys                   "
"PPM        Positions and Proper Motions  "
"PULSAR     Lyne Pulsar Catalog           "
"QSO        Hewitt & Burbidge QSO Catalog "
"RC3        3rd Ref Catalog of Gal's      "
"RITTER     Ritter CVs & LMXRBs Catalog   "
"SAO        SAO Stars Catalog             "
"SNRGREEN   Green SNR Catalog             "
"VERON89    Veron Quasars & AGN Catalog   "
"VERON91    Veron Quasars & AGN           "
"VERON93    Veron Quasars & AGN           "
"VLANEP     1.5 GHz VLA-NEP Survey        "
"VSTARS     4th Variable Stars Catalog    "
"VSTARSUSP  Suspected Variables Catalog   "
"WOOLLEY    Stars <25 pc from Sun         "
"XRBCAT     X-Ray Binaries                "
"ZCAT       Huchra CfA Redshift Catalog   "
"ZWCLUSTERS Zwicky Clusters of Galaxies Catalog"
          } {

          $w.db.rhs.lb insert end $bcat
          }
         
        bind $w.db.rhs.lb <ButtonRelease-1> {
           SetCatName [%W get [%W curselection]]]
           }

        pack $w.db.lab $w.db.dat $w.db.rhs -side top -anchor e -fill x


        bind $w.db.dat <Return> {SetCatName [%W get]]}


      frame $w.info
        frame $w.info.ra
         label $w.info.ra.lab -text "RA 1950: "
         entry $w.info.ra.dat -width 20 -fg blue -relief sunken -bd 2 \
              -textvariable HDB(ra)
         pack $w.info.ra.lab -side left 
         pack $w.info.ra.dat -side right
        frame $w.info.dec
         label $w.info.dec.lab -text "DEC 1950: "
         entry $w.info.dec.dat -width 20 -fg blue -relief sunken -bd 2 \
              -textvariable HDB(dec)
         pack $w.info.dec.lab -side left 
         pack $w.info.dec.dat -side right
        frame $w.info.wid
         label $w.info.wid.lab -text "Search box width (arcmin): "
         entry $w.info.wid.dat -width 20 -fg blue -relief sunken -bd 2 \
              -textvariable HDB(wid)
         pack $w.info.wid.lab -side left 
         pack $w.info.wid.dat -side right
        pack $w.info.ra $w.info.dec $w.info.wid -side top -anchor nw \
		-fill x

      frame $w.files
        label $w.files.lab -text "Output filename: "
        entry $w.files.dat -width 30 -fg blue -relief sunken -bd 2 \
              -textvariable HDB(file)
        pack $w.files.lab -side left 
        pack $w.files.dat -side right

      frame $w.mark
        frame $w.mark.onoff
          label $w.mark.onoff.lab -text "Source marking: "
          checkbutton $w.mark.onoff.on -text "On" -variable HDB(mark) \
              -onvalue 1
          checkbutton $w.mark.onoff.off -text "Off" -variable HDB(mark) \
              -onvalue 0
          pack $w.mark.onoff.lab -side left
          pack $w.mark.onoff.off $w.mark.onoff.on -side right

        frame $w.mark.num
          label $w.mark.num.lab -text "Numbering: "
          checkbutton $w.mark.num.on -text "On" -variable HDB(mark_n) \
              -onvalue y
          checkbutton $w.mark.num.off -text "Off" -variable HDB(mark_n) \
              -onvalue n
          pack $w.mark.num.lab -side left
          pack $w.mark.num.off $w.mark.num.on -side right

        pack $w.mark.onoff $w.mark.num -side top -anchor nw -fill x

#   Create an "OK" and a "Cancel" button.
      frame $w.ctrl
      button $w.ctrl.ok -text "Retrieve data" -width 12 -command {set isyshdb_priv(button) "ok"}
      button $w.ctrl.can -text Cancel -width 6 \
  	-command {set isyshdb_priv(button) "can"}

# Pack them into the bottom frame with a default border around the OK
# button.
      frame $w.ctrl.default -relief sunken -bd 1
      raise $w.ctrl.ok $w.ctrl.default
      pack $w.ctrl.default -side left -expand 1 -padx 3m -pady 2m
      pack $w.ctrl.ok -in $w.ctrl.default -padx 1m -pady 1m -ipadx 1m

      pack $w.ctrl.can -side left -expand 1 -padx 3m -pady 2m -ipadx 1m

      pack $w.db $w.info $w.files $w.mark $w.ctrl \
		-side top -fill x -anchor nw -ipadx 5 -ipady 5

# Bind the return key to the OK button.
      bind $w <Return> "$w.ctrl.ok flash; set isyshdb_priv(button) ok"

# Centre the window
      WinCentre $w 

#  Set a grab and claim the focus.
      set oldFocus [focus]
      grab $w
      focus $w

# Wait for the user to respond.
      for {} {1} {} {
	tkwait variable isyshdb_priv(button)

	if ![string compare $isyshdb_priv(button) ok] {

          $w.db.dat configure -state disabled
          $w.info.ra.dat configure -state disabled
          $w.info.dec.dat configure -state disabled
          $w.info.wid.dat configure -state disabled
          $w.files.dat configure -state disabled

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

        set pid [pid]
        set tfile "/tmp/hdbcone_${pid}"
        if [file exists $tfile] {
          exec rm -f $tfile
          }

        set cmdstr "$client $db $HDB(ra) $HDB(dec) $HDB(wid) $quantities outfile=$file"
        IsysHdbDoing $w "$cmdstr"

#    Re-centre the window
#        WinCentre $w 

        set edata [catch {exec $client $db $HDB(ra) $HDB(dec) \
                        $HDB(wid) $quantities outfile=$file >&$tfile}]
#        set edata [catch [eval exec $cmdstr >&$tfile]]

        $w configure -cursor xterm

        IsysHdbTfile $w $tfile
        set HDB(finished) 1
        focus $oldFocus
	}
      destroy $w
      break
      }
  } else {
    destroy $w
    }    
  }
