# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: FileSelect.tcl,v 1.14 2005/11/25 12:06:27 pdraper Exp $"
#
# FileSelect.tcl - OSF/Motif standard file selection dialog
#
# ----------------------------------------------------------------------
# Implements a version of the OSF/Motif standard file selection dialog
# box using primitive widgets as the building blocks.  
# 
#
#   PUBLIC ATTRIBUTES:
#
#     -filterlabel ... label for filter entry, default "Filter"
#     -dirlabel ...... label for directory list, default "Directories"
#     -filelabel ..... label for file list, default "Files"
#     -selectlabel ... label for selection entry, default "Selection"
#     -dispfilter .... display filter, yes or no, default yes
#     -dispdir ....... display directory list, yes or no, default yes
#     -dispfile ...... display file list, yes or no, default yes
#     -dispselect .... display selection, yes or no, default yes
#     -filter ........ file list filter, defaults to "*"
#     -dir ........... initial directory, default to [pwd]
#     -title ......... window title text, default "Select File"
#     -full .......... display full file names, yes or no, default no
#
#     -width ......... width of filter/selection entry widgets in chars
#     -height ........ height of displayed list in lines
#
#     -button_1 ...... Name of the first button, default ok
#     -button_2 ...... Name of the second button, default filter
#     -button_3 ...... Name of the third button, default cancel
#
#   METHODS:
#
#     config ......... used to change public attributes
#     get ............ return the selection
#     activate ....... perform a grab, upon selection of ok(1) or cancel(0), 
#                      return result.
#
#   USAGE:
#     
#     FileSelect .fs -title "Test File Select" -full 0
#
#     if {[.fs activate]} {
#        puts stdout "OK >>==> [.fs get]"
#     } else {
#        puts stdout "Cancel"
#     }
#
#     .fs destroy
#
#   X11 OPTION DATABASE ATTRIBUTES
#
#     ...and the rest of the usual widget attributes
#
# ----------------------------------------------------------------------
#   AUTHOR:  Mark L. Ulferts          Phone: (214) 519-3947
#            DSC Communications Corp  E-mail: mulferts@spd.dsccc.com
# ----------------------------------------------------------------------
#
#   Contributions:  Shawn Ellis       E-mail: ellis@sctc.com
#                   o Simple Emacs key bindings for entry widgets              
#                   o Can now paste a filename or directory into entry
#                     widgets
#                   o Directories are checked on whether they exist before
#                     they are cd'd into.
#                   o Filenames displayed in alphabetical order
#                   o Multiple FileSelect widgets can be used.
#                   o Get method returns _selection only if the file
#                     exists (XXX allan: no, not good when specifying a new file)
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  20 May 96  added quick hack to get this to work in itk2.0. 
#                            (New version in iwidgets2.0.1 was too slow)
#
# Peter W. Draper 27 Jan 97  added -filter_types option to allow caller
#                            to see a list of "known" file extensions. 
#                 30 Mar 00  added changes to add a file filter
#                            when user just types in a directory
#                            (which doesn't then show any files).
#                 05 Jun 00  added catch to directory list expansion,
#                            this blows up if a file like "~noname" is
#                            found (a failed attempt to seek
#                            $env(HOME)/noname is made). 
#                 16 Oct 00  Also now skips $filename and
#                            ~$filename. These cannot be handled by
#                            NDF (user can still enter these names by hand)
#                 26 Mar 01  Added panedwindow to control how the
#                            space is divided between the directory
#                            and files views.
#                 07 Jul 03  Added horizontal scrollbars.
#                 22 Nov 05  Removed panedwindow as this seems to be stopping
#                            the delivery of double clicks.


itk::usual FileSelect {}

# This class implements a version of the OSF/Motif standard file selection 
# dialog box using primitive widgets as the building blocks. 
# This is a modified version of an old widget written by Mark Ulferts
# that is still used because of poor performance in the iwidgets2.x version.
# This should probably be replaced when we move to tcl8.x.
 
itcl::class util::FileSelect {
    inherit util::TopLevelWidget

    #  create new scrolled text
    constructor {args} {

	eval itk_initialize $args

	#
	# Set the minimum size to 0,0 to allow both shrinkage and expansion.
	# Also, set the title if one has been given.
	#
	wm minsize $w_ 250 250
	wm title $w_ "$itk_option(-title)"

        #
	# Create an overall frame and separate frames for the filter, 
	# lists, selection, and buttons.
	#
	set f [frame $w_.fs]

	set fs(filterf) [frame $f.filterf]
	set fs(listf) [frame $f.listf]
	set fs(self) [frame $f.self]
	set fs(btnf) [frame $f.btnf -height 30]

	#
	# Create the label and entry widgets for the filter.  Turn off
	# the selection capability, at least from the visual aspect.
	#
	label $fs(filterf).label -text "$itk_option(-filterlabel)"
	set fs(filter) [entry $fs(filterf).entry -relief sunken]

	#$fs(filter) configure -selectbackground \
	#	[lindex [$fs(filter) configure -background] 4] -selectborderwidth 0

	pack $fs(filterf).label -side top -anchor w
        # PWD: modification here, pack to top not bottom.
        pack $fs(filterf).entry -side top -fill x -expand yes -ipady 1m

	#
	# Create directory list, scrollbar, and label for the directory 
	# frame.  Make the list single select.
	#
	set fs(dirf) [frame $fs(listf).dirf]
	label $fs(dirf).label -text "$itk_option(-dirlabel)"
	set fs(dirs) [listbox $fs(dirf).list -relief sunken \
		-yscrollcommand "$fs(dirf).vscroll set" \
		-xscrollcommand "$fs(dirf).hscroll set" \
		-selectmode single \
		-exportselection no]

	scrollbar $fs(dirf).vscroll -orient vertical -relief sunken \
		-command "$fs(dirf).list yview"

	scrollbar $fs(dirf).hscroll -orient horizontal -relief sunken \
		-command "$fs(dirf).list xview"

	pack $fs(dirf).label -side top -anchor w
	pack $fs(dirf).vscroll -side right -fill y
	pack $fs(dirf).hscroll -side bottom -fill x
	pack $fs(dirf).list -side left -expand yes -fill both

	#
	# Create file list, scrollbar, and label for the file frame.
	# Again, make the list single select.
	#
        set fs(filef) [frame $fs(listf).filef]
	label $fs(filef).label -text "$itk_option(-filelabel)"
	set fs(files) [listbox $fs(filef).list -relief sunken \
		-yscrollcommand "$fs(filef).vscroll set" \
		-xscrollcommand "$fs(filef).hscroll set" \
                -selectmode single \
		-exportselection no]

	scrollbar $fs(filef).vscroll -orient vertical -relief sunken \
		-command "$fs(filef).list yview"

	scrollbar $fs(filef).hscroll -orient horizontal -relief sunken \
		-command "$fs(filef).list xview"

	pack $fs(filef).label -side top -anchor w
	pack $fs(filef).vscroll -side right -fill y
	pack $fs(filef).hscroll -side bottom -fill x
	pack $fs(filef).list -side left -expand yes -fill both

        #
        # Pack the directory and file lists based on the attributes
	# for displaying each list.
        #
        frame $fs(listf).buf -width $_margin -borderwidth 0

        if {$itk_option(-dispdir)} {
           pack $fs(dirf) -side left -fill both -expand yes
        }
	if {$itk_option(-dispfile)} {
           pack $fs(filef) -side right -fill both -expand yes
        }

	#
	# Create the label and entry widgets for the selection frame. Turn
	# off the selection capability, at least from the visual aspect
	#
	label $fs(self).label -text "$itk_option(-selectlabel)"
	set fs(select) [entry $fs(self).entry -relief sunken]
	set _selection "$itk_option(-dir)/"

	pack $fs(self).label -side top -anchor w
	pack $fs(self).entry -side bottom -fill x -expand yes -ipady 1m
	
	#
	# Add the separator and create the buttons in the button frame.
	# Each button is within a frame used to display as default. 
	# The placer is used to locate the three buttons at relative
	# locations.
	#
	frame $f.line -height 2 -width 2 -borderwidth 1 -relief sunken
	
	frame $fs(btnf).okf -borderwidth 1
	set fs(okbtn) [button $fs(btnf).okf.ok -text $itk_option(-button_1) -width 8]
	pack $fs(btnf).okf.ok -padx 2 -pady 2
	raise $fs(btnf).okf.ok

	frame $fs(btnf).ff -borderwidth 1
	set fs(filterbtn) [button $fs(btnf).ff.f -text $itk_option(-button_2) -width 8 \
		-command [code $this _filtercmd]]
	pack $fs(btnf).ff.f -padx 2 -pady 2
	raise $fs(btnf).ff.f

	frame $fs(btnf).cf -borderwidth 1
	set fs(cancelbtn) [button $fs(btnf).cf.c -text $itk_option(-button_3) -width 8]
	pack $fs(btnf).cf.c -padx 2 -pady 2
	raise $fs(btnf).cf.c

	place $fs(btnf).okf -relx 0 -rely 0.5 -anchor w
	place $fs(btnf).ff -relx 0.5 -rely 0.5 -anchor center
	place $fs(btnf).cf -relx 1 -rely 0.5 -anchor e

	#
	# Pack all the components of the file selection box.  The filter
	# and selection frames are packed based on the display attributes.
	#

        if {$itk_option(-dispfilter)} {pack $fs(filterf) -fill x -padx $_margin -pady 5}

	pack $fs(listf) -fill both -padx $_margin -pady 5 -expand yes 

	if {$itk_option(-dispselect)} {pack $fs(self) -fill x -padx $_margin -pady 5}

	pack $f.line -fill x -pady 5
	pack $fs(btnf) -fill x -padx $_margin -pady 5 

	pack $f -fill both -expand yes

	#
	# Set up the bindings for the list widgets. Single click in either 
	# list executes a select method.  Double click for the both lists
	# selects the entry and then invokes the button callback.  Focus
	# events for the filter and select entry widgets control the default
	# button display, and return is mapped to the default button as well.
	#
        bind $fs(dirs) <1> [code $this _selectdir %y]
        bind $fs(files) <1> [code $this _selectfile %y]
        bind $fs(dirs) <Double-1> [code $this _dclickdir %y]
        bind $fs(files) <Double-1> [code $this _dclickfile %y]

	bind $fs(filter) <FocusIn> [code $this _defaultbtn filter]
	bind $fs(select) <FocusIn> [code $this _defaultbtn ok]
	bind $fs(filter) <Return> "$fs(filterbtn) invoke"
	bind $fs(select) <Return> "$fs(okbtn) invoke"

	#
	# Explicitly handle configs that may have been ignored earlier.
	# Also, check to see if the user has specified, width, or height.  
	# If not, use the default and config.
	#
	set _initialized 1
	eval itk_initialize $args

	# 
	# Construction is complete.  Now set up the initial text for the
	# filter, selection, and both lists.  Finally, set the focus for
	# either the filter or select widget based on the display attribute.
	#
	_setfilter $itk_option(-dir) $itk_option(-filter)
	_setselection

	_filldirlist
	_fillfilelist

	
	if {$itk_option(-dispselect)} {
	    focus $fs(select)
	} elseif {$itk_option(-dispfilter)} {
	    focus $fs(filter)
	}
    }

    # Perform a grab operation, install the button
    # callbacks, and wait for the result.  Make sure
    # to reset the working directory back to the
    # original before returning the result.

    public method activate {} {
	global ::result

	set curwd "[pwd]"

	wm deiconify $w_
	if {$itk_option(-modal)} {
	    grab $w_
	}

	$fs(okbtn) configure -command [code $this _okcmd]
	$fs(cancelbtn) configure -command [code $this _cancelcmd]

	set seldir [file dirname [$fs(filter) get]]	
	cd $seldir
	configure -dir "[pwd]"
	configure -filter "[file tail [$fs(filter) get]]"
	set _selection "$itk_option(-dir)/"

	_filldirlist
	_fillfilelist

	tkwait variable result($this)
	wm withdraw $w_
	cd $curwd
	return $result($this)
    }

    #  Return the selection.

    public method get {} {
	return $_selection
    }

    #  called for double click on a filename 

    protected method _dclickfile {y} {
	_selectfile $y
	update idletasks
	$fs(okbtn) invoke
    }

    #  called for double click on a dir name 

    protected method _dclickdir {y} {
	_selectdir $y
	update idletasks
	$fs(filterbtn) invoke
    }


    # Select the directory, set the filter to
    # the new directory.  Set the selection to the
    # new directory if the file list is not 
    # displayed.  Mark the filter button as the 
    # default.

    protected method _selectdir {y} {
	$fs(dirs) selection clear 0 end
	$fs(dirs) selection set [$fs(dirs) nearest $y]

	set curwd "[pwd]"
	set seldir [$fs(dirs) get [$fs(dirs) curselection]]

	if {$seldir == "."} {
	    cd .
	} elseif {$seldir == ".."} {
	    cd ..
	} else {
	    cd $seldir
	}

	_setfilter "[pwd]"

	if {! $itk_option(-dispfile)} {
	    _setselection "[pwd]"
	}

	cd $curwd

	_defaultbtn filter
    }

    # Select the file, set the selection to
    # the new file.  Mark the ok button as the
    # default.
    protected method _selectfile {y} {
	$fs(files) selection clear 0 end
	$fs(files) selection set [$fs(files) nearest $y]

	set sel [$fs(files) curselection]
	if {[llength $sel]} {
	    set _selection $itk_option(-dir)/[$fs(files) get $sel]
	    if {[file exists $_selection]} {
		_setselection
		_defaultbtn ok
	    }
	}
    }

    # Update the filter based on the parameters.
    # If the directory 'd' parameter is null, use 
    # the 'dir' attribute.  If the file 'f' 
    # parameter is null use the tail of the filter
    # entry text.

    protected method _setfilter {{d ""} {f ""}} {
       if { $d == "" } { 
          set filt [$fs(filter) get]
          if { [file isdirectory $filt] } {
             set d $filt
          } else {
             set d [file dirname $filt]
          }
       }
       if {$f == ""} {
          set filt [$fs(filter) get]
          if { [file isdirectory $filt] } {
             set f $last_filter_type_
          } else {
             set f [file tail $filt]
          }
       }
       $fs(filter) delete 0 end
       $fs(filter) insert 0 "$d/$f"
       set last_filter_type_ $f
    }

    # Update the selection based on the 
    # parameter.  If the file 'f' parameter is
    # null, use the 'selection' attribute.

    protected method _setselection {{f ""}} {
	if {$f == ""} {set f $_selection}

	$fs(select) delete 0 end
	$fs(select) insert end "$f"
    }

    # Ok button callback.  Set the default button to
    # OK, and set the selection attribute to the
    # current entry widget text.  Undo the callbacks
    # for both the ok and cancel buttons.  Release
    # the grab and set the global ::result, which frees
    # the wait.

    protected method _okcmd {} {
	global ::result

	set _selection [$fs(select) get]

	if {[file isdirectory $_selection]} {
	    warning_dialog "Please select a file"
	    return
	}

	_defaultbtn ok

	$fs(okbtn) configure -command {}
	$fs(cancelbtn) configure -command {}

	if {$itk_option(-modal)} {
	    grab release $w_
	}
	set result($this) 1
    }

    # Filter button callback.  Change directories
    # as needed, and set the dir, filter, and 
    # selection attributes.  Change the filter and
    # selection text along with the list contents.
    # Mark the default button as filter.

    protected method _filtercmd {} {
       set filt [$fs(filter) get]
       if { [file isdirectory $filt] } { 
          set seldir $filt
       } else {
          set seldir [file dirname $filt]
       }
       if {![file exists $seldir]} {
          return
       }
       
       cd $seldir
       set seldir [pwd]
       _setfilter $seldir
       configure -dir $seldir
       configure -filter "[file tail [$fs(filter) get]]"
       set _selection "$itk_option(-dir)/"
       
       _setselection
       _filldirlist
       _fillfilelist
       
       _defaultbtn filter
    }

    # ------------------------------------------------------------------
    #  METHOD:  _cancelcmd - Cancel button callback.  Set the default
    #                        button to cancel, undo the callbacks, and
    #                        release the grab and wait via the global
    #                        result variable.
    # ------------------------------------------------------------------
    protected method _cancelcmd {} {
	global ::result
	_defaultbtn cancel

	$fs(okbtn) configure -command {}
	$fs(cancelbtn) configure -command {}

	if {$itk_option(-modal)} {
	    grab release $w_
	}
	set result($this) 0
    }

    # Clear the directory list filling with the
    # results of an 'ls'.  Use the full attribute
    # to determine full file name insertion.
    # Select the first element if it exists.

    protected method _filldirlist {} {
	$fs(dirs) delete 0 end
	foreach i [exec /bin/ls -a $itk_option(-dir)] {
           catch {
              if {[file isdirectory $i]} {
                 if {$itk_option(-full)} {
		    $fs(dirs) insert end "$itk_option(-dir)/$i"
                 } else {
		    $fs(dirs) insert end [file tail $i]
                 }
              }
           }
	}

	if {[$fs(dirs) size]} {
	    $fs(dirs) selection clear 0 end
	    $fs(dirs) selection set 0
	}
    }

    # Clear the file list filling with the results of an 'glob'.  
    # Use the full attribute to determine full file name insertion.
    # Select the first element if it exists.  PWD: modification. NDF's
    # cannot start with "$" or "~$", so leave these files alone.

    protected method _fillfilelist {} {
       $fs(files) delete 0 end
       
       set file_temp [glob -nocomplain $itk_option(-dir)/$itk_option(-filter)]
       set filefiller [lsort $file_temp]
       foreach i $filefiller {
          set tail [file tail $i]
          if { [string match {$*} $tail] || [string match {./~$*} $tail] } {
             continue
          }
          if {[file isfile $i] } {
             if {$itk_option(-full)} {
                $fs(files) insert end $i
             } else {
                $fs(files) insert end $tail
             }
          }
       }
    }

    # Sets the default button, either ok, filter
    # or cancel.  The focus is also adjusted.

    protected method _defaultbtn {btn} {
	if {$btn == "ok"} {
	    $fs(btnf).okf configure -relief sunken
	    $fs(btnf).ff configure -relief flat
	    $fs(btnf).cf configure -relief flat

	    focus $fs(select)
	} elseif {$btn == "filter"} {
	    $fs(btnf).okf configure -relief flat
	    $fs(btnf).ff configure -relief sunken
	    $fs(btnf).cf configure -relief flat

	    focus $fs(filter)
	} elseif {$btn == "cancel"} {
	    $fs(btnf).okf configure -relief flat
	    $fs(btnf).ff configure -relief flat
	    $fs(btnf).cf configure -relief sunken
	}
    }

    # Method:  Get the selection from an xterm and display it if
    # it in one of the entry widgets.  A catch has to be used
    # because Tk will issue an error that the selection doesn't
    # exist.  If it doesn't exist, this will still insert it
    # into the entry widget.  I don't know of a workaround

    protected method get_selection {entry_widget} {
	set cmd "selection get"
	catch $cmd string
	$entry_widget insert insert $string

    }


    # Method:  Get the position of the cursor and move it one way or another

    protected method entry_adjust_pos {entry_widget offset} {
	set pos [$entry_widget index insert]
	incr pos $offset
	if {$pos < 0} {
	    set pos 0
	}
	$entry_widget icursor $pos
    }

    #
    # Method: Set the filter type to a known value.
    #
    method set_filter_type {type} {
       if { $type == "" } {
          set type "*"
       }
       set dirname [file dirname [$fs(filter) get]]
       $fs(filter) delete 0 end
       $fs(filter) insert 0 $dirname/$type
       set last_filter_type_ $type
       update
       _filtercmd
    }


    # Set the window title.
    itk_option define -title title Title "Select File" {
	if {$_initialized} {
	    wm title $w_ $itk_option(-title)
	}
    }

    # The label string above the filter widget.
    itk_option define -filterlabel filterLabel FilterLabel "Filter" {
	if {$_initialized} {
	    $fs(filterf).label configure -text $itk_option(-filterlabel)
	}
    }

    #  File list filter, defaults to "*".
    itk_option define -filter filter Filter "*" {
	if {$_initialized} {
	    _setfilter $itk_option(-dir) $itk_option(-filter)
	}
    }

    # Initial directory, default to [pwd].
    itk_option define -dir dir Dir "." {
	cd $itk_option(-dir)
    }

    # Label for directory list, default "Directories"
    itk_option define -dirlabel dirLabel DirLabel "Directories" {
	if {$_initialized} {
	    $fs(dirf).label configure -text $itk_option(-dirlabel)
	}
    }

    # Label for file list, default "Files".
    itk_option define -filelabel fileLabel FileLabel "Files" {
	if {$_initialized} {
	    $fs(filef).label configure -text $itk_option(-filelabel)
	}
    }

    # Label for selection entry, default 
    # "Selection".
    itk_option define -selectlabel selectLabel SelectLabel "Selection" {
	if {$_initialized} {
	    $fs(self).label configure -text $itk_option(-selectlabel)
	}
    }

    # Display full file names, yes/no, default no.
    itk_option define -full full Full 0 {}

    #  Display filter, yes or no, default yes
    itk_option define -dispfilter dispFilter DispFilter 1 {}

    #  Display directory list, yes or no, default yes
    itk_option define -dispdir dispDir DispDir 1 {}

    #  Display file list, yes or no, default yes
    itk_option define -dispfile dispFile DispFile 1 {}

    #  Display selection, yes or no, default yes
    itk_option define -dispselect dispSelect DispSelect 1 {}

    # Set the width of the selection and filter entry
    # widgets.
    itk_option define -width width Width 50 {
	if {$_initialized} {
	    $fs(filter) configure -width $itk_option(-width)
	    $fs(select) configure -width $itk_option(-width)
	}
    }

    #  Set the height of the directory and file lists.
    itk_option define -height height Height 10 {
	if {$_initialized} {
	    $fs(dirs) configure -height $itk_option(-height)
	    $fs(files) configure -height $itk_option(-height)
	}
    }

    #  The button label for button 1
    itk_option define -button_1 button_1 Button_1 "OK" {}

    #  The button label for button 2
    itk_option define -button_2 button_2 Button_2 "Filter" {}

    #  The button label for button 3
    itk_option define -button_3 button_3 Button_3 "Cancel" {}

    #  Add an optional menu of file suffixes (preset file filters).
    itk_option define -filter_types filter_types Filter_Types {} {
       if {$_initialized} {
          if { [info exists fs(filter_types)] } { 
             catch {destroy $fs(filter_types)}
          }
          if { $itk_option(-filter_types) != {} } { 
             set fs(filter_types) [LabelMenu $fs(filterf).types \
                                      -text {Type Filter:}]
             foreach pair "$itk_option(-filter_types)" {
                set name [lindex $pair 0]
                set type [lindex $pair 1]
                $fs(filter_types) add -label $name \
                   -command [code $this set_filter_type $type]
             }
             pack $fs(filterf).types -side bottom -ipady 1m -anchor w
          }
       }
    }

    #  flag: if true, grab the screen
    itk_option define -modal modal Modal 0

    #  Margin distance used in construction
    protected variable _margin 10

    # Current selection.
    protected variable _selection "./"
    protected variable fs
    protected variable _initialized 0

    # Last filter type.
    protected variable last_filter_type_ "*.*"

}



