#+
#  Name:
#    GaiaEsp
#
#  Type of module:
#    [incr Tk] class
#
#  Purpose:
#    Defines a class for controlling an ESP process.
#
#  Description:
#    This class creates a toplevel widget for controlling an ESP
#    process.  It is closely based on Peter Draper's SExtractor GAIA
#    plugin.
#
#  Copyright:
#    Copyright 1999 Central Laboratory of the Research Councils
#
#  Authors:
#    NG: Norman Gray (Starlink, Glasgow)
#    {enter_new_authors_here}
#
#  History:
#    October 1999 (NG):
#      Original version, closely based on Peter Draper's SExtractor
#      GAIA plugin.
#    {enter_further_changes_here}
#
#  Weaknesses:
#    The following aren't really bugs, but things it would be nice to
#    improve, given the opportunity:
#
#      - Supporting some of the other ESP tools (see notes below).
#      - Setting the colour of the ellipse drawn by the ellprofou
#        tool.
#      - Graphing the output of the ellprofou tool.
#
#  Notes:
#
#    There is a fair amount of redundant code in here.  In an early phase
#    of this interface's development, I (NG) implemented _all_ the ESP
#    tools, in a fairly rough fashion.  Subsequently, I substantially
#    rewrote the ellprofou tool, without having time to rewrite the
#    other tools in the same way.  The code supporting the other tools
#    can't be reused trivially, but it should still be of some use
#    when and if support for those other tools is added, so it makes
#    sense to leave the un-called code lying around.
#
#  RCS id:
#    $Id$
#
#-

itk::usual GaiaEsp {}

itcl::class gaia::GaiaEsp {

    # Inheritances:
    inherit util::TopLevelWidget

    # Constructor
    constructor {args} {

	# Evaluate any options
	eval itk_initialize $args

	# Set the top-level window title
	wm title $w_ "GAIA: Extended Surface Photometry ($itk_option(-number))"

	# Add a short help window
	make_short_help

	# Add the file menu
	add_menubar
	set File [add_menubutton "File" left]
	configure_menubutton File -underline 0
	set Options [add_menubutton "Options" left]
	configure_menubutton Options -underline 0
	set Colours [add_menubutton "Colours" left]
	configure_menubutton Colours -underline 0

	# Add window help
	global gaia_dir
	add_help_button $gaia_dir/GaiaEsp.hlp "On Window..."
	add_short_help $itk_component(menubar).help \
		{Help menu: get some help about this window}

	# Create a new toolbox window
	$File add command -label {New window} \
		-command [code $this clone_me_] \
		-accelerator {Control-n}
	bind $w_ <Control-n> [code $this clone_me_]
	add_menu_short_help $File {New window} {Create a new toolbox}
	# Dummy command menu item
	$File add command -label {Show ESP command} \
		-command [code $this run 0 1]
	# Exit menu item
	$File add command -label Exit \
		-command [code $this close] \
		-accelerator {Control-c}
	bind $w_ <Control-c> [code $this close]

	#  Control of options
	$Options add checkbutton \
		-label {Warn if overwriting files?} \
		-variable [scope itk_option(-warn-if-overwrite)]
	$Options add checkbutton \
		-label {Automatically fit background if necessary?} \
		-variable [scope itk_option(-auto-fit-background)]

	#  Control of colours of selections and result ellipses.
	$Colours add cascade -label {Selections} \
		-menu [menu $Colours.selcol]
	$Colours add cascade -label {Result ellipses} \
		-menu [menu $Colours.resellcol]
	foreach {menu name} {\
		selcol selection_colour \
		resellcol ellipse_colour} {
	    foreach c $itk_option(-colors) {
		$Colours.$menu add radiobutton \
			-value $c \
			-background $c \
			-command [code $this configure -$name $c] \
			-variable [scope itk_option(-$name)]
	    }
	    configure -$name $itk_option(-$name)
	}

	#  Establish the defaults
	set_defaults_

	###
	#  Start building the panel we use to interact with the plugin

	# Add the notebook, which we use to deal with the different tools
	itk_component add notebook {
	    ::iwidgets::tabnotebook $w_.notebook \
               -tabpos n \
               -angle 0 \
               -width $itk_option(-width) \
               -height $itk_option(-height)
	}
	pack $itk_component(notebook) -side top -fill both -expand 1 \
		-ipadx 1m -ipady 1m

	set thisnum [get_panel_number_]
	#  OBJECT SELECTION
	$itk_component(notebook) add -label {Objects} \
		-command [code $this reveal_ $thisnum]
	set pages_($thisnum) objectselection
	set revealed_($thisnum) 0
	set notebook_characteristics_(disable,objectselection) {run}
	set notebook_characteristics_(reqsrc,objectselection) 0

	#  CORR
	#$itk_component(notebook) add -label Corr \
	#	-command [code $this reveal_ 0]
	#set pages_(0) corr
	#set revealed_(0) 0
	##set notebook_characteristics_(disable,corr) {outtextname inardname}
	#set notebook_characteristics_(reqsrc,corr) 0

	set thisnum [get_panel_number_]
	#  ELLPRO/ELLFOU
	$itk_component(notebook) add -label {Ellipse fit} \
		-command [code $this reveal_ $thisnum]
	set pages_($thisnum) ellprofou
	set revealed_($thisnum) 0
	set notebook_characteristics_(files,ellprofou) \
		{sourcefile outputtextfile outputstlfile}
	#set notebook_characteristics_(disable,ellprofou) {outndfname nsigma}
	set notebook_characteristics_(reqsrc,ellprofou) 1

	#  FASTMED
	#set thisnum [get_panel_number_]
	#$itk_component(notebook) add -label Fastmed \
	#	-command [code $this reveal_ $thisnum]
	#set pages_($thisnum) fastmed
	#set revealed_($thisnum) 0
	#set notebook_characteristics_(disable,fastmed) {outtextname inardname  nsigma}
	#set notebook_characteristics_(reqsrc,fastmed) 0

	#  GAUFIT
	#set thisnum [get_panel_number_]
	#$itk_component(notebook) add -label Gaufit \
	#	-command [code $this reveal_ $thisnum]
	#set pages_($thisnum) gaufit
	#set revealed_($thisnum) 0
	#set notebook_characteristics_(disable,gaufit) {inardname}
	#set notebook_characteristics_(reqsrc,gaufit) 1

	#  HSUB
	#set thisnum [get_panel_number_]
	#$itk_component(notebook) add -label Histpeak \
	#	-command [code $this reveal_ $thisnum]
	#set pages_($thisnum) hsub
	#set revealed_($thisnum) 0
	#set notebook_characteristics_(disable,hsub) {inardname outndfname  back nsigma}
	#set notebook_characteristics_(reqsrc,hsub) 0

	#  LOBACK
	#set thisnum [get_panel_number_]
	#$itk_component(notebook) add -label Loback \
	#	-command [code $this reveal_ $thisnum]
	#set pages_($thisnum) loback
	#set revealed_($thisnum) 0
	#set notebook_characteristics_(disable,loback) \
	#	{inardname outndfname back nsigma}
	#set notebook_characteristics_(reqsrc,loback) 1
	#set notebook_characteristics_(sourceconfig,loback) {-sourceshape square -maxobjects 1}

	#  MASK
	#set thisnum [get_panel_number_]
	#$itk_component(notebook) add -label Mask \
	#	-command [code $this reveal_ $thisnum]
	#set pages_($thisnum) mask
	#set revealed_($thisnum) 0
	#set notebook_characteristics_(disable,mask) \
	#	{outtextname  back nsigma}
	#set notebook_characteristics_(reqsrc,mask) 0
	
	#  MIXUP
	#set thisnum [get_panel_number_]
	#$itk_component(notebook) add -label Mixup \
	#	-command [code $this reveal_ $thisnum]
	#set pages_($thisnum) mixup
	#set revealed_($thisnum) 0
	#set notebook_characteristics_(disable,mixup) \
	#	{outtextname inardname  back nsigma}
	#set notebook_characteristics_(reqsrc,mixup) 0

	#  SECTOR
	#set thisnum [get_panel_number_]
	#$itk_component(notebook) add -label Sector \
	#	-command [code $this reveal_ $thisnum]
	#set pages_($thisnum) sector
	#set revealed_($thisnum) 0
	#set notebook_characteristics_(disable,sector) \
	#	{nsigma outndfname}
	## We do require a source selection, which is why selobj
	## isn't disabled, but we do _not_ require those to be saved,
	## because that's done through the command line in sector.
	#set notebook_characteristics_(reqsrc,sector) 0
	#set notebook_characteristics_(sourceconfig,sector) \
	#	{-sourceshape sector -maxobjects 1}

	#  SELFC
	#set thisnum [get_panel_number_]
	#$itk_component(notebook) add -label Selfc(w) \
	#	-command [code $this reveal_ $thisnum]
	#set pages_($thisnum) selfc
	#set revealed_($thisnum) 0
	#set notebook_characteristics_(disable,selfc) \
	#	{outtextname }
	#set notebook_characteristics_(reqsrc,selfc) 0
	#set notebook_characteristics_(sourceconfig,selfc) {-maxobjects 1}

	#  SKEW
	#set thisnum [get_panel_number_]
	#$itk_component(notebook) add -label Skew \
	#	-command [code $this reveal_ $thisnum]
	#set pages_($thisnum) skew
	#set revealed_($thisnum) 0
	#set notebook_characteristics_(disable,skew) \
	#	{outtextname }
	#set notebook_characteristics_(reqsrc,skew) 0

	# TOPPED
	#set thisnum [get_panel_number_]
	#$itk_component(notebook) add -label Topped \
	#	-command [code $this reveal_ $thisnum]
	#set pages_($thisnum) topped
	#set revealed_($thisnum) 0
	#set notebook_characteristics_(disable,topped) {outtextname }
	#set notebook_characteristics_(reqsrc,topped) 0

	#  RESULTS
	set thisnum [get_panel_number_]
	$itk_component(notebook) add -label Results \
		-command [code $this reveal_ $thisnum]
	set pages_($thisnum) results
	set revealed_($thisnum) 0

	# create the inverse of pages_
	foreach num [array names pages_] {
	    set indexes_($pages_($num)) $num
	}

	#  Create the button bar at the bottom
	itk_component add actionframe {frame $w_.action}

	#  Button: close window
	itk_component add close {
	    button $itk_component(actionframe).close -text Close \
		    -command [code $this close]
	}
	add_short_help $itk_component(close) {Close window}

	#  Button: reset the current page
	itk_component add resetpage {
	    button $itk_component(actionframe).resetpage -text {Reset} \
		    -command [code $this reset_page_ 0]
	}
	add_short_help $itk_component(resetpage) \
		{Reset current page to defaults}

	#  Button: reset all pages
	itk_component add resetall {
	    button $itk_component(actionframe).resetall \
		    -text {Reset all} \
		    -command [code $this reset_page_ 1]
	}
	add_short_help $itk_component(resetall) \
		{Reset all pages to defaults}

	#  Button: run ESP
	itk_component add run {
	    button $itk_component(actionframe).run -text {Fit} \
		    -command [code $this run 1 0]
	}
	add_short_help $itk_component(run) {Fit the selected galaxies}

	#  Add a status area for monitoring the output of the program
	itk_component add status {
	    Scrollbox $w_.status
	}
	$w_.status configure -height 5
	add_short_help $itk_component(status) \
		{Displays output from ESP}

	#  Pack all the components into place
	pack $itk_component(notebook) \
		-side top -fill both -expand 1 -pady 5 -padx 5
	pack $itk_component(status) \
		-side bottom -fill both -expand 1 -pady 5 -padx 5
	pack $itk_component(actionframe) \
		-side bottom -fill x -pady 5 -padx 5

	# buttons
	#pack $itk_component(selobj) -side right -expand 1 -pady 3 -padx 3
	pack $itk_component(run) \
		$itk_component(resetpage) \
		$itk_component(resetall) \
		$itk_component(close) \
		-side left -fill x -expand 1 -pady 3 -padx 3

	#  Enable all the common widgets, prior to notebook pages
	#  selectively disabling these.
	disable_common_widgets_ {}

	#  Create the initial, empty, source list
	set objectlist_ [gaia::GaiaEspSelectList #auto \
                            -canvas     $itk_option(-canvas) \
                            -canvasdraw $itk_option(-canvasdraw) \
                            -rtdimage   $itk_option(-rtdimage) \
                            -short_help $short_help_win_
                        ]

	#  Select a default page of the notebook
	$itk_component(notebook) select 0

	#  Create an object for dealing with image names
	set namer_ [GaiaImageName \#auto]
    }

    # Destructor
    destructor {
	puts "GaiaEsp destructor: delete files..."
	foreach f $temporary_files_ {
	    puts "deleting file $f..."
	    exec rm $f
	}
	if { $star_app_ != {} } {
	    catch {$star_app_ delete_sometime}
	    set star_app_ {}
	}
    }

    # Methods

    #  Create a new instance of this object.
    protected method clone_me_ {} {
	if { $itk_option(-clone_cmd) != {} } {
	    eval $itk_option(-clone_cmd)
	}
    }

    # Close this window.  Kill it if needed, otherwise withdraw
    public method close {} {
	delete_canvas_graphics_ [list esp_sel esp_out$w_]
	if { $itk_option(-really_die) } {
	    delete object $this
	} else {
	    wm withdraw $w_
	}
    }

    #  Actually run the command
    public method run {execit showit} {
	# delete the results of any previous run from this toolbox
	delete_canvas_graphics_ esp_out$w_

	set image [$itk_option(-rtdimage) cget -file]
	if {$image != {}} {
	    $namer_ configure -imagename $image
	    set image [$namer_ ndfname]
	}

	set whichpage $pages_([$itk_component(notebook) index select])

	if {[info exists notebook_characteristics_(files,$whichpage)]} {
	    set fileexists ""
	    foreach ftype $notebook_characteristics_(files,$whichpage) {
		set fname $values_($this,$ftype)
		if {$fname == {}} {
		    if {$ftype == "outputstlfile"} {
			set values_($this,$ftype) "GaiaEsp_catalogue.txt"
		    } else {
			set values_($this,$ftype) [format "GaiaEsp_%s" $ftype]
		    }
		    set fname $values_($this,$ftype)
		}
		# Check that none of these output files already exist.
		if {[file exists $fname]} {
		    set fileexists [format "%s%s " $fileexists $fname]
		}
	    }
	    if {$fileexists != ""} {
		if {$itk_option(-warn-if-overwrite)} {
		    set w [DialogWidget $w_.dialog \
			    -text [format "I'll overwrite output files\n%s.\nIs that OK?" $fileexists] \
			    -bitmap warning \
			    -buttons {OK Cancel} \
			    -default 1 \
			    -modal 1]
		    set answer [$w activate]
		    if {$answer == 0} {
			eval exec rm $fileexists
		    } else {
			return {}
		    }
		} else {
		    # Delete silently
		    eval exec rm $fileexists
		}
	    }
	}

	set arglist [make_${whichpage}_command_ $image]

	set allok 1
	if {$arglist == {}} {
	    set allok 0
	    # ...but don't put out another error message, since the 
	    # make_..._command_ should have done that
	} elseif {$image == {}} {
	    set allok 0
	    error_dialog "No image available"
	} elseif {$notebook_characteristics_(reqsrc,$whichpage) && [$objectlist_ get_sourcelist] == {}} {
	    set allok 0
	    error_dialog "Can't run ESP: you haven't defined any sources"
	} elseif {$values_($this,sourcefile) == {}} {
	    set allok 0
	    error_dialog "Can't save sources"
	}

	#if {[info exists notebook_characteristics_(disable,$whichpage)]} {
	#    set disable_chars $notebook_characteristics_(disable,$whichpage)
	#} else {
	#    set disable_chars {}
	#}
	#if {[lsearch $disable_chars {outndfname}] < 0} {
	#    if {[file exists $values_($this,outputndffile)]} {
	#	set allok 0
	#	error_dialog "Output NDF file $values_($this,outputndffile) already exists"
	#    }
	#}
	#if {[lsearch $disable_chars {outtextname}] < 0} {
	#    if {[file exists $values_($this,outputtextfile)]} {
	#	set allok 0
	#	error_dialog "Output text file $values_($this,outputtextfile) already exists"
	#    }
	#}

	if {$allok} {

	    # Save the source information to a file
	    if {$notebook_characteristics_(reqsrc,$whichpage)} {
		save_${whichpage}_sourcefile_ $values_($this,sourcefile)
	    }

	    lappend arglist "reset accept"

	    # Normalise/check the argument list by running through it
	    # looking for trailing equals signs.
	    set norm_arglist {}
	    foreach setting $arglist {
		if {[regexp {^(.*)=$} $setting fullstring parameter]} {
		    if {[info exists defaults_($parameter)]} {
			lappend norm_arglist "$parameter=$defaults_($parameter)"
		    } else {
			puts "ERROR: no value for parameter '$parameter' and no default"
		    }
		} else {
		    lappend norm_arglist $setting
		}
	    }

	    set invoke_cmd [lindex $norm_arglist 0]
	    set invoke_args [join [lrange $norm_arglist 1 end]]

	    if {$showit} {
		puts "invocation: $invoke_cmd $invoke_args"
	    }
	    if {$execit} {
		# Establish a control object for this task, if not already done
		# (dunno what this does!, but GaiaSextractor does it)
		blt::busy hold $w_

		if {$star_app_ == {} || $star_app_name_ != $invoke_cmd} {
		    set star_app_ [StarApp #auto \
			    -show_output $itk_component(status) \
			    -notify [code $this completed_${whichpage}_] \
			    -application $invoke_cmd
		    ]
		    set star_app_name_ $invoke_cmd
		}

		# Clear the log window
		$itk_component(status) clear 0 end

		# RUN ESP!!!
		$star_app_ runwiths $invoke_args
	    }
	}
    }

    # Any cleanup when task is completed
    private method completed_ {} {
	blt::busy release $w_

	set whichpage $pages_([$itk_component(notebook) index select])

	if {[info exists notebook_characteristics_(disable,$whichpage)]} {
	    set disable_chars $notebook_characteristics_(disable,$whichpage)
	} else {
	    set disable_chars {}
	}

	set s "[string toupper $whichpage] output:\n"
	if {[lsearch $disable_chars {outndfname}] < 0} {
	    set s "${s}NDF in file $values_($this,outputndffile)\n"
	}
	if {[lsearch $disable_chars {outtextname}] < 0} {
	    set s "${s}Text output in file $values_($this,outputtextfile)\n"
	}
	
	info_dialog $s
    }

    private method completed_ellprofou_ {} {
	blt::busy release $w_

	# Get rid of the scrawls on the canvas,
	# and replace them with the result ellipses.
	#$itk_option(-canvasdraw) clear

	if {! $revealed_($indexes_(results))} {
	    # This has to be revealed (ie, constructed) before we can
	    # add material to the results-menu component below.
	    reveal_ $indexes_(results)
	}

	set stl [gaia::StarSTLFile #auto $values_($this,outputstlfile)]
	if {[$stl status]} {
	    set nellipses [$stl parameter _nrows]
	    set ESP_results_ {}
	    $itk_component(results-menu) clear
	    set sourcen 0
	    for {set elln 1} {$elln <= $nellipses} {incr elln} {
		$stl reset_isvalid
		set ell_src [$stl table $elln sourcen]
		set ell_x   [$stl table $elln x]
		set ell_y   [$stl table $elln y]
		set ell_sma [$stl table $elln semimajor]
		set ell_pos [$stl table $elln pa]
		# invell is 1/Ellipticity
		# semiminor axis is semimajor axis / ellipticity
		set ell_invell [$stl table $elln 1/ellipt]

		if {[$stl isvalid]} {
		    # These coordinates and sizes are in image units,
		    # rather than in canvas units.  Convert them
		    # before use.  ESP position angles are clockwise
		    # from vertical, but rtd_ellipse angles are
		    # clockwise from the positive x-axis
		    # (aggravatingly, this is different from the angle
		    # convention for canvas sectors, for example).
		    $itk_option(-rtdimage) convert coords \
			    $ell_x $ell_y image canvx canvy canvas
		    $itk_option(-rtdimage) convert dist \
			    $ell_sma 0 image canvd1 canvd2 canvas
		    set canvsma [expr $canvd1+$canvd2]
		    set canvpa [expr $ell_pos - 90]
		    $itk_option(-canvas) \
			    create rtd_ellipse \
			    $canvx $canvy \
			    -semimajor $canvsma \
			    -semiminor [expr $canvsma * $ell_invell] \
			    -angle $canvpa \
			    -outline $itk_option(-ellipse_colour) \
			    -tags esp_out$w_

		    if {$ell_src != $sourcen} {
			incr sourcen
			$itk_component(results-menu) add \
				-label [format "Galaxy %d: (%.1f,%.1f)" $sourcen $ell_x $ell_y] \
                                -value $sourcen \
				-command [code $this show_ellprofou_results_ $sourcen]
		    }
		} else {
		    puts "can't read details of ellipse $elln"
		}

		lappend ESP_results_ \
		    [list $sourcen $ell_x $ell_y $ell_sma $ell_pos [expr 1/$ell_invell]]
	    }
	    show_ellprofou_results_ 1
            $itk_component(results-menu) configure -value 1
	} else {
	    error_dialog "Error reading ELLPRO output file:\n[$stl error_msg]"
	}
    }

    private method show_ellprofou_results_ {sourcen} {
	if {! $revealed_($indexes_(results))} {
	    reveal_ $indexes_(results)
	}
	$itk_component(results) clear
	foreach row $ESP_results_ {
	    if {[lindex $row 0] == $sourcen} {
		$itk_component(results) append_row $row
	    }
	}
	$itk_component(results) new_info
    }

    # return `true' or `false' depending on whether $value_($this,$var) is
    # true or false
    private method bool_value {var} {
	if {$values_($this,$var)} {
	    return "true"
	} else {
	    return "false"
	}
    }

    #  Reveal a page of widgets if not already done (these are deferred
    #  to give a better interactive response).
    protected method reveal_ {index {all 0}} {
	set npages [$itk_component(notebook) index end]
	if { ! $all } {
	    set pagename $pages_($index)
	    if { !$revealed_($index) } {
		set revealed_($index) 1
		set child [$itk_component(notebook) childsite $index]
		eval add_${pagename}_selections_ $child
	    }
	    if {[info exists notebook_characteristics_(disable,$pagename)]} {
		set disabledset $notebook_characteristics_(disable,$pagename)
		if {$disabledset != {}} {
		    disable_common_widgets_ $disabledset
		}
	    } else {
		# enable everything
		disable_common_widgets_ {}
	    }
	    if {[info exists notebook_characteristics_(sourceconfig,$pagename)]} {
		eval $objectlist_ configure \
			$notebook_characteristics_(sourceconfig,$pagename)
	    } else {
		$objectlist_ default_config		;# default
	    }
	} else {
	    #  Reveal all pages.
	    for {set i 0} {$i <= $npages} {incr i} {
		if { !$revealed_($i) } {
		    set revealed_($i) 1
		    set child [$itk_component(notebook) childsite $i]
		    eval add_$pages_($i)_selections_ $child
		}
	    }
	}
    }





    #  Reset the current page to its default values.
    protected method reset_page_ {{all 0}} {
	set npages [$itk_component(notebook) index end]

	# reset the list of objects
	$objectlist_ reset

	# Delete any scrawls on the canvas
	delete_canvas_graphics_ esp_out$w_

	if { ! $all } {
	    #  Get the index of the current page.
	    set current [$itk_component(notebook) index select]
	    eval reset_$pages_($current)_

	    if {$current != "results"} {
		eval reset_results_
	    }
	} else {
	    #  Reset all pages.
	    reveal_ 0 1
	    for {set i 0} {$i <= $npages} {incr i} {
		eval reset_$pages_($i)_
	    }
	}
    }

    #  Select one or more objects
    #protected method select_objects_ {} {
    #$objectlist_ edit_sourcelist
    #}

    ### OBJECT SELECTION...
    # Following three functions should exists for consistency with
    # the other panels of the notebook, but should never be called.
    private method make_objectselection_command_ {image} {
	info_dialog "Select a tool from the tabs\n(only `Ellipse fit' available at present)"
	return {}
    }
    private method save_objectselection_sourcefile_ {filename} {
	return {}
    }
    protected method reset_objectselection_ {} {
	return {}
    }

    protected method add_objectselection_selections_ {parent} {
	itk_component add objsel {
	    $objectlist_ edit_sourcelist $parent
	}
	pack $itk_component(objsel) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(objsel) \
		{Select objects to be fitted}
    }

    ### CORR...
    private method make_corr_command_ {image} {
	# insist that back and sigma be specified, otherwise return ""
	if {$values_($this,back) <= 0 || $values_($this,sigma) <= 0} {
	    error_dialog "Need to have BACK and SIGMA specified"
	    return {}
	}
	
	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/corr"

	lappend arglist "in=$image"
	lappend arglist "mult=$values_($this,mult)"
	lappend arglist "back=$values_($this,back)"
	lappend arglist "sigma=$values_($this,sigma)"
	lappend arglist "nsigma=$values_($this,nsigma)"
	lappend arglist "out=$values_($this,outputndffile)"
	lappend arglist "psize=$values_($this,psize)"
	lappend arglist "scale=$values_($this,scale)"
	lappend arglist "useall=[bool_value useall]"

	return $arglist
    }

    private method reset_corr_ {} {
	$itk_component(back) configure -value $defaults_(back)
	$itk_component(mult-corr) configure -value $defaults_(mult)
	$itk_component(nsigma) configure -value $defaults_(nsigma)
	$itk_component(psize-corr) configure -value $defaults_(psize)
	#$itk_component(sigma) configure -value $defaults_(sigma)
	$itk_component(useall-corr) configure -value $defaults_(useall)
	$itk_component(scale) configure -value $defaults_(scale)
    }

    private method add_corr_selections_ {parent} {
	set lwidth 16
	set vwidth 5

	itk_component add mult-corr {
	    LabelEntry $parent.mult \
		    -text "Multiplication factor" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,mult)]
	}
	pack $itk_component(mult-corr) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(mult-corr) \
		{A multiplying factor applied to each of the results}

	itk_component add psize-corr {
	    LabelEntry $parent.psize \
		    -text "Pixel size/as." \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,psize)]
	}
	pack $itk_component(psize-corr) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(psize-corr) \
		{The size of each image pixel, in units of arc-seconds}

	itk_component add useall-corr {
	    StarLabelCheck $parent.useall \
		    -text "Use pixel threshold" \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,useall)]
	}
	pack $itk_component(useall-corr) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(useall-corr) \
		{Is a pixel count threshold use when calculation the correlation?}

	itk_component add scale {
	    LabelEntry $parent.scale \
		    -text "Scale length" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,scale)]
	}
	pack $itk_component(scale) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(scale) \
		{The scale length of the galaxies to be highlighted in the output image (units as.)}
    }

    ### ELLPRO...
    # Save the ELLPRO/FOU source selections to a file
    private method save_ellprofou_sourcefile_ {filename} {
	set olist [$objectlist_ get_sourcelist]
	if {[llength $olist] > 0 && $filename != {}} {
	    busy {
		set fid [::open $filename w]
		puts $fid "# GAIA ESP ELLPRO sources file"
		puts $fid "# Columns:  X, Y Background (ignored) rlim"
		puts $fid "## wcsdomain = grid"

		foreach o $olist {
		    set xyr [$o coords]
		    puts $fid "[lindex $xyr 0] [lindex $xyr 1] -1 [lindex $xyr 2]"
		}
		::close $fid
	    }
	}
    }

    private method make_ellprofou_command_ {image} {
	# insist that back and sigma be specified, otherwise return ""
	if {$values_($this,sigma) <= 0} {
	    # If Sigma is null, that's an error, because ELLPRO/FOU
	    # refuse to deal with that.  If -auto-fit-background is
	    # true, then automatically fit the background, if false,
	    # then ask for permission, in case this is an error.
	    if {$itk_option(-auto-fit-background)} {
		set answer 0
	    } else {
		set w [DialogWidget $w_.dialog \
			-text "No background or standard deviation\nspecified.  Should I estimate them?" \
			-bitmap warning \
			-buttons {OK Cancel} \
			-default 1 \
			-modal 1]
		set answer [$w activate]
	    }
	    if {$answer == 0} {
		bg_from_hsub_ 1
	    } else {
		return {}
	    }
	}
	if {$values_($this,back) <= 0} {
	    # Zero background is legitimate but worth warning about.
	    set w [DialogWidget $w_.dialog \
		    -text "Zero background.  Is that OK?" \
		    -bitmap warning \
		    -buttons {OK Cancel} \
		    -default 1 \
		    -modal 1]
	    set answer [$w activate]
	    if {$answer != 0} {
		return {}
	    }
	}

	set arglist {}

	#set meth [string range $values_($this,ellprofoumethod) 0 2]
	#set is_ellpro [expr {$meth == "pro"}]

	set fullmeth $values_($this,ellprofoumethod)
	set meth [string range $fullmeth 0 2]
	set is_ellpro [expr {$meth == "pro"}]

	if {$is_ellpro} {
	    lappend arglist "$itk_option(-esp_dir)/ellpro"
	} else {
	    lappend arglist "$itk_option(-esp_dir)/ellfou"
	}

	lappend arglist "infile=$values_($this,sourcefile)"

	set ofname [string trim $values_($this,outputtextfile)]
	if {$ofname != {}} {
	    lappend arglist "out=$values_($this,outputtextfile)"
	}

	set ofname [string trim $values_($this,outputstlfile)]
	if {! [regexp {\.txt$} $ofname]} {
	    error_dialog "STL file must end in .txt"
	    return {}
	}
	# Catalogue name (after path, and before ".txt") may contain
	# only alphanumerics plus underscore.  This isn't obviously documented
	# anywhere, but can be discovered from an inspection of 
	# cat1_cnmpr.f in the CAT library.
	if {! [regexp {^(.*/)?[A-Za-z0-9_]+\.txt$} $ofname]} {
	    error_dialog "STL catalogue name may include only\nalphanumeric characters plus underscore"
	    return {}
	}
	lappend arglist "outcat=$ofname"

	lappend arglist "in=$image"
	lappend arglist "mode=false"
	lappend arglist "angcon=[bool_value angcon]"
	lappend arglist "angoff=$values_($this,angoff)"
	if {$values_($this,ardfile) != {}} {
	    lappend arglist "ardfil=^$values_($this,ardfile)"
	} else {
	    lappend arglist "ardfil=!"
	}
	#lappend arglist "autol=[bool_value autol]"
	lappend arglist "back=$values_($this,back)"
	lappend arglist "sigma=$values_($this,sigma)"
	if {$is_ellpro} {
	    # fast is true if ellprofoumethod is pro, false if it's proslow
	    lappend arglist "fast=[expr {($fullmeth == "pro") ? "true" : "false"}]"
	    #lappend arglist "fast=[expr {($values_($this,ellprofoumethod) == "pro") ? true : false}]"
	}
	if {[$itk_component(fine) is_enabled]} {
	    lappend arglist "fine=$values_($this,fine)"
	}
	#lappend arglist "frzori=[bool_value frzori]"

	set sourcepos $values_($this,ellprofouoriginflag)
	if {[string index $sourcepos 0] == {y}} {
	    lappend arglist autol
	    lappend arglist "autolt=[bool_value autolt]"
	} else {
	    lappend arglist noautol
	}
	#lappend arglist [expr {[string index $sourcepos 0]=={y} ? "autol" : "noautol"}]
	lappend arglist [expr {[string index $sourcepos 1]=={y} ? "nofrzori" : "frzori"}]

	if {$is_ellpro && [$itk_component(fract) is_enabled]} {
	    lappend arglist "fract=$values_($this,fract)"
	}
	if {[$itk_component(lim1) is_enabled]} {
	    lappend arglist "lim1=$values_($this,lim1)"
	}
	if {[$itk_component(lim2) is_enabled]} {
	    lappend arglist "lim2=$values_($this,lim2)"
	}
	if {$is_ellpro && [$itk_component(lim3) is_enabled]} {
	    if {$values_($this,lim3flag)} {
		# don't impose limit -- give negative argument
		lappend arglist "lim3=-1"
	    } else {
		lappend arglist "lim3=$values_($this,lim3)"
	    }
	}
	lappend arglist "psize=$values_($this,psize)"
	# Set default rlim.  This is never used, because we specify individual
	# radius limits in the input sourcelist file, but we need to specify it
	# or else we get an error from the parameter system.
	lappend arglist "rlim=0"
	if {[$itk_component(zerop) is_enabled]} {
	    lappend arglist "zerop=$values_($this,zerop)"
	}
	if {$is_ellpro && $values_($this,minmod) >= 0} {
	    lappend arglist "minmod=$values_($this,minmod)"
	}

	return $arglist
    }

    protected method reset_ellprofou_ {} {
	#$itk_component(angcon) configure -value $defaults_(angcon)
	#$itk_component(angoff) configure -value $defaults_(angoff)
	#set values_($this,angoff) $defaults_(angoff)
	#set values_($this,angcon) $defaults_(angcon)

	#set values_($this,autol) $defaults_(autol)
	#$itk_component(back-ellprofou)   configure -value $defaults_(back)
	#$itk_component(sigma)  configure -value $defaults_(sigma)

	#$itk_component(expert) configure -value $defaults_(expert)
	set values_($this,expert) $defaults_(expert)

	#$itk_component(autolt) configure -value $defaults_(autolt)
	#$itk_component(fast)   configure -value $defaults_(fast)
	#$itk_component(fine)   configure -value $defaults_(fine)
	$itk_component(fine) reset
	#$itk_component(fract)  configure -value $defaults_(fract)
	$itk_component(fract) reset
	#$itk_component(frzori) configure -value $defaults_(frzori)
	#$itk_component(lim1)   configure -value $defaults_(lim1)
	#$itk_component(lim2)   configure -value $defaults_(lim2)
	#$itk_component(lim3)   configure -value $defaults_(lim3)
	$itk_component(lim1) reset
	$itk_component(lim2) reset
	$itk_component(lim3) reset
	$itk_component(zerop) reset
	#$itk_component(rlim)   configure -value $defaults_(rlim)
	#$itk_component(zerop)  configure -value $defaults_(zerop)
	$itk_component(minmod) configure -value $defaults_(minmod)
    }

    protected method add_ellprofou_selections_ {parent} {
	set lwidth $labelwidth_
	set vwidth 5

	add_common_widgets_ $parent {outtextname outstlname inardname back} ellprofou

	itk_component add method-ellprofou {
	    LabelMenu $parent.method \
		    -text "Method:" \
		    -labelwidth $lwidth \
		    -variable [scope values_($this,ellprofoumethod)]
	}
	foreach {str val} \
		{"Using isophote contours" fou "Using intensity analysis" pro "Using intensity analysis (slow variant)" proslow} {
	    $itk_component(method-ellprofou) add -label $str \
		    -value $val \
		    -command [code $this toggle_expert_]
	}
	$itk_component(method-ellprofou) configure \
		-value $values_($this,ellprofoumethod)
	pack $itk_component(method-ellprofou) \
		-side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(method-ellprofou) \
		{How should the ellipses be fitted?}

	# origin settings
	itk_component add originflag-ellprofou {
	    LabelMenu $parent.originflag \
		    -text "Source:" \
		    -labelwidth $lwidth \
		    -variable [scope values_($this,ellprofouoriginflag)]
	}
	foreach {str val} \
		{"Source position fixed" nn "Refine initial position, then fix" yn "Refine position during processing" ny "Continuously refine position" yy} {
	    $itk_component(originflag-ellprofou) add \
		    -label $str -value $val \
		    -command [code $this toggle_originflag_]
	}
	$itk_component(originflag-ellprofou) configure \
		-value $values_($this,ellprofouoriginflag)
	pack $itk_component(originflag-ellprofou) \
		-side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(originflag-ellprofou) \
		{Should the source position be refined?}

	# autolt
	itk_component add autolt {
	    LabelMenu $parent.autolt \
		    -text "Auto-origin:" \
		    -labelwidth $lwidth \
		    -variable [scope values_($this,autolt)]
	}
	foreach {str val} {"Simple centroid" 0 "Weighted centroid" 1} {
	    $itk_component(autolt) add \
		    -label $str -value $val \
		    -command "set values_($this,autolt) $val"
	}
	$itk_component(autolt) configure \
		-value $values_($this,autolt)
	pack $itk_component(autolt) \
		-side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(autolt) \
		{How should the initial source position be refined?}

	# Angular offset
	itk_component add angoff {
	    gaia::StarLabelWidgetChild $parent.angoff \
		    -text "Angular offset:" \
		    -labelwidth $lwidth
	}
	set cs [$itk_component(angoff) childsite]
	entry $cs.b -textvariable [scope values_($this,angoff)] -width 10
	radiobutton $cs.a1 -text "clockwise" \
		-variable [scope values_($this,angcon)] \
		-value 1 -anchor w
	radiobutton $cs.a0 -text "anticlockwise" \
		-variable [scope values_($this,angcon)] \
		-value 0 -anchor w
	pack $cs.b $cs.a1 $cs.a0 -side left -fill x
	pack $itk_component(angoff) -side top -fill x -ipadx 1m -ipady 1m
  	add_short_help $itk_component(angoff) \
  		{Angular offset for position angles generated.  Units degrees}

	# Expert toggle
	itk_component add expert {
	    StarLabelCheck $parent.expert \
		    -text "Expert options:" \
		    -onvalue 1 -offvalue 0 \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,expert)] \
		    -command [code $this toggle_expert_]
	}
	pack $itk_component(expert) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(expert) \
		{Enable expert commands.  See SUN/180, for further assistance}

	itk_component add fine {
	    gaia::StarLabelWidgetChild $parent.fine \
		    -text {Sep. factor:} \
		    -labelwidth $lwidth \
		    -enablebutton 1 \
		    -initialstate 0 \
		    -childtype scale \
		    -childopts "-orient horizontal -showvalue 0 -resolution 0.05 -from 0.0 -to 2.0 -variable [scope values_($this,fine)]"
	}
	set cs [$itk_component(fine) childsite]
	label $cs.tracelabel -textvariable [scope values_($this,fine)]
	pack $cs.tracelabel -side left -fill x -ipadx 1m
	pack $itk_component(fine) -side top -fill x -ipadx 1m
	add_short_help $itk_component(fine) \
		{fine: Factor modifying the default separation of profiled ellipses}

	itk_component add fract {
	    gaia::StarLabelWidgetChild $parent.fract \
		    -text {Min. points:} \
		    -labelwidth $lwidth \
		    -enablebutton 1 \
		    -initialstate 0 \
		    -childtype scale \
		    -childopts "-orient horizontal -showvalue 0 -resolution 1 -from 0 -to 100 -variable [scope values_($this,fract)]"
	}
	set cs [$itk_component(fract) childsite]
	label $cs.tracelabel -textvariable [scope values_($this,fract)]
	label $cs.units -text "%"
	pack $cs.tracelabel $cs.units -side left -fill x -ipadx 1m
	pack $itk_component(fract) -side top -fill x -ipadx 1m
	add_short_help $itk_component(fract) \
		{fract: minimum good points on an ellipse}

	itk_component add lim1 {
	    gaia::StarLabelWidgetChild $parent.lim1 \
		    -text {Max. ratio:} \
		    -labelwidth $lwidth \
		    -enablebutton 1 \
		    -initialstate 0 \
		    -childtype scale \
		    -childopts "-orient horizontal -showvalue 0 -resolution 0.05 -from 0 -to 2 -variable [scope values_($this,lim1)]"
	}
	set cs [$itk_component(lim1) childsite]
	label $cs.tracelabel -textvariable [scope values_($this,lim1)]
	pack $cs.tracelabel -side left -fill x -ipadx 1m
	pack $itk_component(lim1) -side top -fill x -ipadx 1m
	add_short_help $itk_component(lim1) \
		{lim1: Max. ratio of mean counts in successive profiles}

	itk_component add lim2 {
	    gaia::StarLabelWidgetChild $parent.lim2 \
		    -text {Min. count:} \
		    -labelwidth $lwidth \
		    -enablebutton 1 \
		    -initialstate 0 \
		    -childtype scale \
		    -childopts "-orient horizontal -showvalue 0 -resolution 0.1 -from 0 -to 5 -variable [scope values_($this,lim2)]"
	}
	set cs [$itk_component(lim2) childsite]
	label $cs.tracelabel -textvariable [scope values_($this,lim2)]
	label $cs.units -text {std.dev.}
	pack $cs.tracelabel $cs.units -side left -fill x -ipadx 1m
	pack $itk_component(lim2) -side top -fill x -ipadx 1m
	add_short_help $itk_component(lim2) \
		{lim2: Minimum mean profile count}

	itk_component add lim3 {
	    gaia::StarLabelWidgetChild $parent.lim3 \
		    -text {Freeze ell. at:} \
		    -labelwidth $lwidth \
		    -enablebutton 1 \
		    -initialstate 0 \
		    -childtype scale \
		    -childopts "-orient horizontal -showvalue 0 -resolution 1 -from 0 -to 100 -variable [scope values_($this,lim3)]"
	}
	set cs [$itk_component(lim3) childsite]
	label $cs.tracelabel -textvariable [scope values_($this,lim3)]
	label $cs.units -text px
	checkbutton $cs.cb -text {No freeze} \
		-variable [scope values_($this,lim3flag)]
	pack $cs.tracelabel $cs.units $cs.cb \
		-side left -fill x -ipadx 1m
	pack $itk_component(lim3) -side top -fill x -ipadx 1m
	add_short_help $itk_component(lim3) \
		{lim3: radius at which ellipse parameters are frozen}

	itk_component add zerop {
	    gaia::StarLabelWidgetChild $parent.zerop \
		    -text {Zero point:} \
		    -labelwidth $lwidth \
		    -enablebutton 1 \
		    -initialstate 0 \
		    -childtype scale \
		    -childopts "-orient horizontal -showvalue 0 -resolution .25 -from 0 -to 40 -variable [scope values_($this,zerop)]"
	}
	set cs [$itk_component(zerop) childsite]
	label $cs.tracelabel -textvariable [scope values_($this,zerop)]
	label $cs.units -text {mags/sq.as}
	pack $cs.tracelabel $cs.units -side left -fill x -ipadx 1m
	pack $itk_component(zerop) -side top -fill x -ipadx 1m
	add_short_help $itk_component(zerop) \
		{zerop: Zero point on surface brightness scale}

	#  Minimisation mode
	itk_component add minmod {
	    LabelMenu $parent.minmod \
		    -text "Min'n mode:" \
		    -labelwidth $lwidth \
		    -variable [scope values_($this,minmod)]
	}
	foreach {name value} \
		"Default -1 Mean 0 Median 1 Least-squares 2" {
	    $itk_component(minmod) add \
		    -label $name -value $value \
		    -command "set values_($this,minmod) $value"
	}
	pack $itk_component(minmod) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(minmod) \
		{minmod: Which minimisation mode should be used?}

	# Set up for initial values of enabled/disabled
	#toggle_seellpro_
	toggle_originflag_
	toggle_expert_
    }

    ### FASTMED...

    private method make_fastmed_command_ {image} {
	# insist that back and sigma be specified, otherwise return ""
	if {$values_($this,back) <= 0 || $values_($this,sigma) <= 0} {
	    error_dialog "Need to have BACK and SIGMA specified"
	    return {}
	}
	
	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/fastmed"

	lappend arglist "in=$image"
	lappend arglist "out=$values_($this,outputndffile)"
	lappend arglist "back=$values_($this,back)"
	lappend arglist "sigma=$values_($this,sigma)"
	lappend arglist "width=$values_($this,width)"	

	return $arglist
    }

    private method reset_fastmed_ {} {
	$itk_component(back) configure -value $defaults_(back)
	$itk_component(width) configure -value $defaults_(width)
    }

    private method add_fastmed_selections_ {parent} {
	set lwidth 16
	set vwidth 5

	itk_component add width {
	    LabelEntryScale $parent.width \
		    -text "Width" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -increment 2 \
		    -resolution 2 \
		    -fix_range 1 \
		    -validate real \
		    -from 2 \
		    -to 200 \
		    -value $values_($this,width) \
		    -command [code $this set_values_ width]
	}
	pack $itk_component(width) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(width) \
		{The width of the median filter to be employed.  Units pixels}
    }


    ###  GAUFIT...
    # Save the GAUFIT source selections to a file
    private method save_gaufit_sourcefile_ {filename} {
	set olist [$objectlist_ get_sourcelist]
	if {[llength $olist] > 0 && $filename != {}} {
	    busy {
		set fid [::open $filename w]
		puts $fid "# GAIA ESP GAUFIT sources file"
		puts $fid "# Columns:  X, Y, Radius"

		foreach o $olist {
		    puts $fid [$o coords]
		}
		::close $fid
	    }
	}
    }

    protected method reset_gaufit_ {} {
	$itk_component(angcon)  configure -value $defaults_(angcon)
	$itk_component(angoff)  configure -value $defaults_(angoff)

	$itk_component(fwhm)    configure -value $defaults_(fwhm)
	$itk_component(psize)   configure -value $defaults_(psize)
	$itk_component(calcsd)  configure -value $defaults_(calcsd)
	$itk_component(maxiter) configure -value $defaults_(maxiter)

	set values_($this,fitback) $defaults_(fitback)
	$itk_component(back)    configure -value $defaults_(back)
	#$itk_component(sigma)   configure -value $defaults_(sigma)
	$itk_component(nsigma)  configure -value $defaults_(nsigma)
	toggle_fitback_

	#$itk_component(lsqfit)  configure -value $defaults_(lsqfit)
	set values_($this,lsqfit) $defaults_(lsqfit)
	#$itk_component(autol)   configure -value $defaults_(autol)
	set values_($this,autol)  $defaults_(autol)
	$itk_component(anginc)  configure -value $defaults_(anginc)
	$itk_component(pinc)    configure -value $defaults_(pinc)
	$itk_component(sainc)   configure -value $defaults_(sainc)
	$itk_component(sbinc)   configure -value $defaults_(sbinc)
	$itk_component(xinc)    configure -value $defaults_(xinc)
	$itk_component(yinc)    configure -value $defaults_(yinc)
	toggle_lsqfit_
    }

    # Add controls for the gaufit parameter
    protected method add_gaufit_selections_ {parent} {
	set lwidth 16
	set vwidth 5

	itk_component add angoff {
	    gaia::StarLabelWidgetChild $parent.angoff \
		    -text "Angular offset" \
		    -labelwidth $lwidth
	}
	set cs [$itk_component(angoff) childsite]
	entry $cs.b -textvariable [scope values_($this,angoff)] -width 10
	radiobutton $cs.a1 -text "clockwise" \
		-variable [scope values_($this,angcon)] \
		-value 1 -anchor w
	radiobutton $cs.a0 -text "anticlockwise" \
		-variable [scope values_($this,angcon)] \
		-value 0 -anchor w
	pack $cs.b $cs.a1 $cs.a0 -side left -fill x
	pack $itk_component(angoff) -side top -fill x -ipadx 1m -ipady 1m
  	add_short_help $itk_component(angoff) \
  		{Angular offset for position angles generated.  Units degrees}

	#  Perhaps put the fitting parameters in another notebook, to
	#  avoid them sprawling all over the screen.
	#itk_component add gaufitnotebook {
	#    ::iwidgets::tabnotebook #auto \
	#	    -tabpos n -angle 15
	#}
	#pack $itk_component(gaufitnotebook) -side top -fill both -expand 1 \
	#	-ipadx 1m -ipady 1m
	    
	#  Do we use the least-squares fit or not?
	#  (it might be better to use radiobuttons for
	#  least-squares/search)
	itk_component add lsqfit {
	    StarLabelCheck $parent.lsqfit \
		    -text "Least-squares fit" \
		    -onvalue 1 \
		    -offvalue 0 \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,lsqfit)] \
		    -command [code $this toggle_lsqfit_]
	}
	pack $itk_component(lsqfit) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(lsqfit) \
		{Use a least-squares fit (newer, more robust)}

	#  Now add the autol toggle and the six *inc fields
	itk_component add autol-gaufit {
	    StarLabelCheck $parent.autol-gaufit \
		    -text "Auto-locate origin" \
		    -onvalue 1 \
		    -offvalue 0 \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,autol)]
	}
	pack $itk_component(autol-gaufit) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(autol-gaufit) \
		{Is the source origin provided to be refined?}		    

	itk_component add anginc {
	    LabelEntryScale $parent.anginc \
		    -text "Angular increment" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -increment 0.05  \
		    -resolution 0.05 \
		    -show_arrows 1 \
		    -fix_range 1 \
		    -validate real \
		    -anchor w \
		    -value $values_($this,anginc) \
		    -from 0.0 \
		    -to 1.0 \
		    -command [code $this set_values_ anginc]
	}
	pack $itk_component(anginc) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(anginc) \
		{The amount by which the angle of a source may vary, from 0 (fixed) to 1 (completely free)}

	itk_component add pinc {
	    LabelEntryScale $parent.pinc \
		    -text "Peak increment" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -increment 0.05  \
		    -resolution 0.05 \
		    -show_arrows 1 \
		    -fix_range 1 \
		    -validate real \
		    -anchor w \
		    -value $values_($this,pinc) \
		    -from 0.0 \
		    -to 1.0 \
		    -command [code $this set_values_ pinc]
	}
	pack $itk_component(pinc) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(pinc) \
		{The amount by which the peak of a source may vary, from 0 (fixed) to 1 (completely free)}

	itk_component add sainc {
	    LabelEntryScale $parent.sainc \
		    -text "sigma_a increment" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -increment 0.05  \
		    -resolution 0.05 \
		    -show_arrows 1 \
		    -fix_range 1 \
		    -validate real \
		    -anchor w \
		    -value $values_($this,sainc) \
		    -from 0.0 \
		    -to 1.0 \
		    -command [code $this set_values_ sainc]
	}
	pack $itk_component(sainc) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(sainc) \
		{The amount by which the width (larger axis) of a source may vary, from 0 (fixed) to 1 (completely free)}

	itk_component add sbinc {
	    LabelEntryScale $parent.sbinc \
		    -text "sigma_b increment" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -increment 0.05  \
		    -resolution 0.05 \
		    -show_arrows 1 \
		    -fix_range 1 \
		    -validate real \
		    -anchor w \
		    -value $values_($this,sbinc) \
		    -from 0.0 \
		    -to 1.0 \
		    -command [code $this set_values_ sbinc]
	}
	pack $itk_component(sbinc) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(sbinc) \
		{The amount by which the width (smaller axis) of a source may vary, from 0 (fixed) to 1 (completely free)}

	itk_component add xinc {
	    LabelEntryScale $parent.xinc \
		    -text "X increment" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -increment 0.05  \
		    -resolution 0.05 \
		    -show_arrows 1 \
		    -fix_range 1 \
		    -validate real \
		    -anchor w \
		    -value $values_($this,xinc) \
		    -from 0.0 \
		    -to 1.0 \
		    -command [code $this set_values_ xinc]
	}
	pack $itk_component(xinc) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(xinc) \
		{The amount by which the X-coordinate of a source may vary, from 0 (fixed) to 1 (completely free)}

	itk_component add yinc {
	    LabelEntryScale $parent.yinc \
		    -text "Y increment" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -increment 0.05  \
		    -resolution 0.05 \
		    -show_arrows 1 \
		    -fix_range 1 \
		    -validate real \
		    -anchor w \
		    -value $values_($this,yinc) \
		    -from 0.0 \
		    -to 1.0 \
		    -command [code $this set_values_ yinc]
	}
	pack $itk_component(yinc) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(yinc) \
		{The amount by which the Y-coordinate of a source may vary, from 0 (fixed) to 1 (completely free)}

	
	#  Fit background?
	#  If not, then also back, sigma, nsigma

	itk_component add maxiter {
	    LabelEntry $parent.maxiter \
		    -text "Maximum iterations" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,maxiter)]
	}
	pack $itk_component(maxiter) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(maxiter) \
		{Upper-bound on iteration count}

	itk_component add psize-gaufit {
	    LabelEntry $parent.psize \
		    -text "Pixel size/as." \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,psize)]
	}
	pack $itk_component(psize-gaufit) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(psize-gaufit) \
		{The size of each image pixel, in units of arc-seconds}

	#  Should we fit the background or specify it
	itk_component add fitback {
	    StarLabelCheck $parent.fitback \
		    -text "Fit background " \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,fitback)] \
		    -command [code $this toggle_fitback_]
	}
	pack $itk_component(fitback) -side left -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(fitback) \
		{Fit the background}

	itk_component add calcsd {
	    StarLabelCheck $parent.calcsd \
		    -text "Calculate SD" \
		    -anchor w \
		    -variable [scope values_($this,calcsd)]
	}
	pack $itk_component(calcsd) -side left -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(calcsd) \
		{Calculate fit errors}

	itk_component add fwhm {
	    StarLabelCheck $parent.fwhm \
		    -text "FWHM" \
		    -anchor w \
		    -variable [scope values_($this,fwhm)]
	}
	pack $itk_component(fwhm) -side left -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(fwhm) \
		{Display results in FWHM rather than sigma?}

	#  Set fitback to the default
	toggle_fitback_
	#  Finally set the lsqfit toggle to the default
	toggle_lsqfit_

	#  I/O: in, infile, model, modtype, out, device?

	#  Ignore: colour, mode
    }

    private method make_gaufit_command_ {image} {
	# insist that either fitback is true, or else 
	# back, sigma and nsigma have values.
	if {! $values_($this,fitback)
	&& ($values_($this,back) < 0
	|| $values_($this,sigma) < 0
	|| $values_($this,nsigma) < 0)} {
	    error_dialog "If fitback is not selected, back, sigma and nsigma must have values"
	    return {}
	}

	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/gaufit"

	lappend arglist "infile=$values_($this,sourcefile)"
	lappend arglist "in=$image"
	lappend arglist "mode=false"
	lappend arglist "angcon=[bool_value angcon]"
	lappend arglist "angoff=$values_($this,angoff)"
	lappend arglist "fwhm=[bool_value fwhm]"
	lappend arglist "psize=$values_($this,psize)"
	lappend arglist "calcsd=[bool_value calcsd]"

	if {$values_($this,lsqfit)} {
	    lappend arglist "lsqfit=true"
	    if {$values_($this,maxiter) > 0.0} {
		lappend arglist "maxiter=$values_($this,maxiter)"
	    }
	    if {$values_($this,fitback)} {
		lappend arglist "back=-1"
	    } else {
		# XXX What do I do when these have no value???
		lappend arglist "back=$values_($this,back)"
		lappend arglist "sigma=$values_($this,sigma)"
		lappend arglist "nsigma=$values_($this,nsigma)"
	    }
	} else {
	    lappend arglist "lsqfit=false"
	    lappend arglist "autol=[bool_value autol]"
	    lappend arglist "anginc=$values_($this,anginc)"
	    lappend arglist "pinc=$values_($this,pinc)"
	    lappend arglist "sainc=$values_($this,sainc)"
	    lappend arglist "sbinc=$values_($this,sbinc)"
	    lappend arglist "xinc=$values_($this,xinc)"
	    lappend arglist "yinc=$values_($this,yinc)"
	    lappend arglist "back=$values_($this,back)"
	    lappend arglist "sigma=$values_($this,sigma)"
	    lappend arglist "nsigma=$values_($this,nsigma)"
	    if {$values_($this,maxiter) > 0.0} {
		lappend arglist "niter=$values_($this,maxiter)"
	    }
	}

	# Set the model type, and model and output text files.
	lappend arglist "model=$values_($this,outputndffile)"
	lappend arglist "out=$values_($this,outputtextfile)"
	lappend arglist "modtyp=w"

	return $arglist
    }

    ### HSUB...
    private method make_hsub_command_ {image} {
	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/hsub"

	lappend arglist "in=$image"
	if {$values_($this,sfactflag) <= 0} {
	    lappend arglist "sfact=$values_($this,sfactflag)"
	} else {
	    lappend arglist "sfact=$values_($this,sfact)"
	}
	lappend arglist "type=$values_($this,hsubtype)"
	lappend arglist "out=$values_($this,outputtextfile)"
	lappend arglist "formatted=[bool_value hsubformatted]"

	return $arglist
    }

    private method reset_hsub_ {} {
	$itk_component(back)      configure -value $defaults_(back)
	$itk_component(sfact-hsub)     configure -value $defaults_(sfact)
	set values_($this,sfactflag) $defaults_(sfactflag)
	$itk_component(hsubtype)  configure -value $defaults_(hsubtype)
	$itk_component(hsubformatted) configure -value $defaults_(hsubformatted)
    }

    private method add_hsub_selections_ {parent} {
	set lwidth 16
	set vwidth 5

	itk_component add sfact-hsub {
	    gaia::StarLabelWidgetChild $parent.sfact \
		    -text "Smoothing" \
		    -labelwidth $lwidth
	}
	set cs [$itk_component(sfact-hsub) childsite]
	frame $cs.f1
	frame $cs.f2
	radiobutton $cs.rb1 \
		-text {Automatic} \
		-variable [scope values_($this,sfactflag)] \
		-value -1 -anchor w
	radiobutton $cs.rb2 \
		-text {None} \
		-variable [scope values_($this,sfactflag)] \
		-value 0 -anchor w
	radiobutton $cs.rb3 \
		-text {Set} \
		-variable [scope values_($this,sfactflag)] \
		-value 1 -anchor w
	spinint $cs.e \
		-textvariable [scope values_($this,sfact)] \
		-range {1 100} \
		-wrap 0
	pack $cs.f1 $cs.f2 -side top -anchor w
	pack $cs.rb1 $cs.rb2 -side top -anchor w -in $cs.f1 -ipady 1m
	pack $cs.rb3 $cs.e -side left -in $cs.f2 
	pack $itk_component(sfact-hsub) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(sfact-hsub) \
		{Type of Gaussian smoothing requested}

	itk_component add hsubtype {
	    LabelMenu $parent.hsubtype \
		    -text {Modal pixel calc.} \
		    -labelwidth $lwidth \
		    -variable [scope values_($this,hsubtype)]
	}
	pack $itk_component(hsubtype) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(hsubtype) \
		{How should the modal pixel be calculated?}
	foreach {name value} {
	    Automatic 0
	    {Raw histogram} 1
	    {Smoothed histogram} 2
	    {Extrapolated chords} 3
	    {Interpolation of data points} 4
	} {
	    $itk_component(hsubtype) add -label $name -value $value
	}

	itk_component add hsubformatted {
	    StarLabelCheck $parent.hsubformatted \
		    -text "Formatted output" \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,hsubformatted)]
	}
	pack $itk_component(hsubformatted) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(hsubformatted) \
		{Produce formatted output?}

	itk_component add hsubnote {
	    label $parent.note \
		    -justify left \
		    -relief raised \
		    -text \
"This panel provides the functionality of the ESP HISTPEAK
application, but without the graph-drawing capabilities
of that tool.  It is, however, actually implemented using
the ESP HSUB application, so it is that part of the ESP
documentation which you should refer to for further details"
	}
	pack $itk_component(hsubnote) -side top -fill x -ipadx 1m -ipady 1m

    }

    ### LOBACK...
    # Save source selections to a file.
    private method save_loback_sourcefile_ {filename} {
	set olist [$objectlist_ get_sourcelist]
	if {[llength $olist] > 0 && $filename != {}} {
	    busy {
		set fid [::open $filename w]
		puts $fid "# GAIA LOBACK sources file"
		foreach o $olist {
		    set xyr [$o coords]
		    set wid [lindex $xyr 2]
		    puts $fid "[lindex $xyr 0] [lindex $xyr 1] [expr $wid*$wid]"
		}
		::close $fid
	    }
	}
    }

    private method make_loback_command_ {image} {

	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/loback"

	lappend arglist "in=$image"
	lappend arglist "infile=$values_($this,sourcefile)"
	lappend arglist "out=$values_($this,outputtextfile)"
	if {$values_($this,sfactflag) <= 0} {
	    lappend arglist "sfact=$values_($this,sfactflag)"
	} else {
	    lappend arglist "sfact=$values_($this,sfact)"
	}
	lappend arglist "third=false"	;# support this loback mode only
	lappend arglist "width=32"	;# ignored, but needs to be there

	return $arglist
    }

    private method reset_loback_ {} {
	$itk_component(sfact) configure -value $defaults_(sfact)
	set values_($this,sfactflag) $defaults_(sfactflag)
    }

    private method add_loback_selections_ {parent} {
	set lwidth 16
	set vwidth 5

	itk_component add sfact-loback {
	    gaia::StarLabelWidgetChild $parent.sfact \
		    -text "Smoothing" \
		    -labelwidth $lwidth
	}
	set cs [$itk_component(sfact-loback) childsite]
	frame $cs.f1
	frame $cs.f2
	radiobutton $cs.rb1 \
		-text {Automatic} \
		-variable [scope values_($this,sfactflag)] \
		-value -1 -anchor w
	radiobutton $cs.rb2 \
		-text {None} \
		-variable [scope values_($this,sfactflag)] \
		-value 0 -anchor w
	radiobutton $cs.rb3 \
		-text {Set} \
		-variable [scope values_($this,sfactflag)] \
		-value 1 -anchor w
	spinint $cs.e \
		-textvariable [scope values_($this,sfact)] \
		-range {1 100} \
		-wrap 0
	pack $cs.f1 $cs.f2 -side top -anchor w
	pack $cs.rb1 $cs.rb2 -side top -anchor w -in $cs.f1 -ipady 1m
	pack $cs.rb3 $cs.e -side left -in $cs.f2 
	pack $itk_component(sfact-loback) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(sfact-loback) \
		{Type of Gaussian smoothing requested}
    }


    ### MASK...
    private method make_mask_command_ {image} {

	if {$values_($this,ardfile) == {} || $values_($this,outputndffile) == {}} {
	    error_dialog "ARD file and Output NDF must be specified"
	    return {}
	}

	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/mask"

	lappend arglist "in=$image"
	lappend arglist "ardfil=$values_($this,ardfile)"
	lappend arglist "out=$values_($this,outputndffile)"

	return $arglist
    }

    private method reset_mask_ {} {
	$itk_component(inardname)  configure -value $defaults_(inardname)
	$itk_component(outndfname) configure -value $defaults_(outndfname)
    }

    private method add_mask_selections_ {parent} {
	itk_component add masknote {
	    label $parent.note \
		    -justify left \
		    -relief raised \
		    -text \
"This panel provides the functionality of the ESP MASK
application.  It requires only the ARD file and output NDF
to be specified"
	}
	pack $itk_component(masknote) -side top -fill x -ipadx 1m -ipady 1m
    }

    ### MIXUP...
    private method make_mixup_command_ {image} {

	if {$values_($this,outputndffile) == {}} {
	    error_dialog "Output NDF must be specified"
	    return {}
	}

	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/mixup"

	lappend arglist "in=$image"
	lappend arglist "out=$values_($this,outputndffile)"

	return $arglist
    }

    private method reset_mixup_ {} {
	$itk_component(outndfname) configure -value $defaults_(outndfname)
    }

    private method add_mixup_selections_ {parent} {
	itk_component add mixupnote {
	    label $parent.note \
		    -justify left \
		    -relief raised \
		    -text \
"This panel provides the functionality of the ESP MIXUP
application.  It requires only the output NDF
to be specified"
	}
	pack $itk_component(mixupnote) -side top -fill x -ipadx 1m -ipady 1m
    }

    ### SECTOR...
    private method make_sector_command_ {image} {
	if {$values_($this,back) == 0 || $values_($this,sigma) == 0} {
	    error_dialog "Background must be specified"
	    return {}
	}
	set olist [$objectlist_ get_sourcelist]
	if {[llength $olist] != 1} {
	    error_dialog "Must specify exactly one source"
	    return {}
	}

	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/sector"

	lappend arglist "in=$image"
	lappend arglist "out=$values_($this,outputtextfile)"
	if {$values_($this,ardfile) == {}} {
	    lappend arglist "ardfil=!"
	} else {
	    lappend arglist "ardfil=^$values_($this,ardfile)"
	}
	lappend arglist "back=$values_($this,back)"
	lappend arglist "sigma=$values_($this,sigma)"

	lappend arglist "autol=[bool_value autol]"
	lappend arglist "mirror=[bool_value mirror]"
	lappend arglist "surf=[bool_value surf]"

	lappend arglist "psize=$values_($this,psize)"
	lappend arglist "radisp=$values_($this,radisp)"

	lappend arglist "fitlim=$values_($this,fitlim1),$values_($this,fitlim2)"
	if {$values_($this,zeropflag)} {
	    lappend arglist "zerop=$values_($this,zerop)"
	}

	# Specify position.  coords method gives posang in degrees
	# from x-axis, with anti-clockwise positive.  posang parameter
	# is degrees from y-axis, with clockwise positive.
	set source1 [lindex $olist 0]
	set oc [$source1 coords]
	lappend arglist "porigin=\[[lindex $oc 0],[lindex $oc 1]\]"
	lappend arglist "rlim=[lindex $oc 2]"
	set pang [expr 90-[lindex $oc 3]]
	while {$pang < 0} {
	    set pang [expr $pang+360]
	}
	lappend arglist "posang=$pang"
	lappend arglist "angwid=[lindex $oc 4]"

	lappend arglist "cursor=no"
	lappend arglist "again=no"

	return $arglist	
    }

    private method reset_sector_ {} {
	$itk_component(autol-sector) configure -value $defaults_(autol)
	$itk_component(fitlim)       configure -value $defaults_(fitlim)
	$itk_component(mirror)       configure -value $defaults_(mirror)
	$itk_component(psize-sector) configure -value $defaults_(psize)
	$itk_component(radisp)       configure -value $defaults_(radisp)
	$itk_component(surf)         configure -value $defaults_(surf)
    }

    private method add_sector_selections_ {parent} {
	set lwidth 12
	set vwidth 5

	#  Now add the autol toggle and the six *inc fields
	itk_component add autol-sector {
	    StarLabelCheck $parent.autol-sector \
		    -text "Auto-locate" \
		    -onvalue 1 \
		    -offvalue 0 \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,autol)]
	}
	pack $itk_component(autol-sector) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(autol-sector) \
		{Is the source origin provided to be refined?}

	itk_component add fitlim {
	    gaia::StarLabelWidgetChild $parent.fitlim \
		    -text "Fit limits" \
		    -labelwidth $lwidth
	}
	set cs [$itk_component(fitlim) childsite]
	scale $cs.s1 \
		-label "from" \
		-orient horizontal \
		-showvalue 1 \
		-resolution 0.5 \
		-from 0 \
		-to 40 \
		-command [code $this synch_fitlim_ 2] \
		-variable [scope values_($this,fitlim1)]
	scale $cs.s2 \
		-label "to" \
		-orient horizontal \
		-showvalue 1 \
		-resolution 0.5 \
		-from 0 \
		-to 40 \
		-command [code $this synch_fitlim_ 1] \
		-variable [scope values_($this,fitlim2)]
	pack $cs.s1 $cs.s2 -side left -padx 1m -fill x
	pack $itk_component(fitlim) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(fitlim) \
		{The range of fitted radius values}

	itk_component add mirror {
	    StarLabelCheck $parent.mirror \
		    -text "Use mirror" \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,mirror)]
	}
	pack $itk_component(mirror) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(mirror) \
		{Sum diametrically opposite sectors}

	itk_component add psize-sector {
	    LabelEntry $parent.psize \
		    -text "Pixel size/as." \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,psize)]
	}
	pack $itk_component(psize-sector) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(psize-sector) \
		{The size of each image pixel, in units of arc-seconds}

	itk_component add radisp {
	    LabelMenu $parent.radisp \
		    -text {Output display type} \
		    -labelwidth $lwidth \
		    -variable [scope values_($this,radisp)]
	}
	pack $itk_component(radisp) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(radisp) \
		{Display mode used for the radius axis of the graphs}
	foreach {name value} {
	    {Quarter power} q
	    {Logarithmic} l
	    {Square root} s
	    {Linear} r
	} {
	    $itk_component(radisp) add -label $name -value $value
	}

	itk_component add surf {
	    StarLabelCheck $parent.surf \
		    -text "Output surface brightness" \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,surf)]
	}
	pack $itk_component(surf) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(surf) \
		{Are the pixel values to be expressed as surface brightness?}
    }
    private method synch_fitlim_ {which newval} {
	if {$which == 1} {
	    # synch fitlim1
	    if {$values_($this,fitlim1) > $newval} {
		set values_($this,fitlim1) $newval
	    }
	} else {
	    if {$values_($this,fitlim2) < $newval} {
		set values_($this,fitlim2) $newval
	    }
	}
    }

    ### SELFC...
    private method make_selfc_command_ {image} {

	if {$values_($this,outputndffile) == {}} {
	    error_dialog "Output NDF must be specified"
	    return {}
	}
	if {$values_($this,back) == 0} {
	    error_dialog "Background must be specified"
	    return {}
	}
	set olist [$objectlist_ get_sourcelist]
	if {[llength $olist] != 1} {
	    error_dialog "Must specify exactly one 'source'"
	}

	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/selfc"

	lappend arglist "in=$image"
	lappend arglist "out=$values_($this,outputndffile)"
	lappend arglist "back=$values_($this,back)"
	lappend arglist "sigma=$values_($this,sigma)"
	lappend arglist "nsigma=$values_($this,nsigma)"
	if {$values_($this,psize) != 1} {
	    lappend arglist "psize=$values_($this,psize)"
	}
	lappend arglist "useall=[bool_value useall]"

	set source1 [lindex $olist 0]
	set xyr [$source1 coords]
	if {$values_($this,isselfc)} {
	    lappend arglist "diam=[lindex $xyr 2]"
	} else {
	    lappend arglist "mult=$values_($this,mult)"
	    lappend arglist "scale=[lindex $xyr 2]"
	}

	return $arglist
    }

    private method reset_selfc_ {} {
	$itk_component(psize-selfc) configure -value $defaults_(psize)
	$itk_component(useall-selfc) configure -value $defaults_(useall)
	$itk_component(mult-selfc) configure -value $defaults_(mult)
	set values_($this,selfcflag) $defaults_(selfcflag)
    }

    private method add_selfc_selections_ {parent} {
	set lwidth 16
	set vwidth 5

	itk_component add isselfc {
	    gaia::StarLabelWidgetChild $parent.isselfc \
		    -text "Mode" \
		    -labelwidth $lwidth
	}
	set cs [$itk_component(isselfc) childsite]
	radiobutton $cs.a1 -text SELFC \
		-variable [scope values_($this,selfcflag)] \
		-value 1 -anchor w \
		-command [code $this toggle_isselfc_]
	radiobutton $cs.a0 -text SELFCW \
		-variable [scope values_($this,selfcflag)] \
		-value 0 -anchor w \
		-command [code $this toggle_isselfc_]
	pack $cs.a1 $cs.a0 -side left -fill x
	pack $itk_component(isselfc) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(isselfc) \
		{Select between SELFC and SELFCW}

	itk_component add psize-selfc {
	    LabelEntry $parent.psize \
		    -text "Pixel size/as." \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,psize)]
	}
	pack $itk_component(psize-selfc) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(psize-selfc) \
		{The size of each image pixel, in units of arc-seconds}

	itk_component add mult-selfc {
	    LabelEntry $parent.mult \
		    -text "Multiplication factor" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,mult)]
	}
	pack $itk_component(mult-selfc) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(mult-selfc) \
		{A multiplying factor applied to each of the results}


	itk_component add useall-selfc {
	    StarLabelCheck $parent.useall \
		    -text "Use pixel threshold" \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,useall)]
	}
	pack $itk_component(useall-selfc) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(useall-selfc) \
		{Is a pixel count threshold use when calculation the correlation?}

	itk_component add diag-b {
	    button $parent.diag \
		    -text "Draw galaxy diameter" \
		    -command [code $this select_objects_]
	}
	pack $itk_component(diag-b) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(diag-b) \
		{draw circle}

	toggle_isselfc_
    }


    ### SKEW...
    private method make_skew_command_ {image} {
	# insist that back and sigma be specified, otherwise return ""
	if {$values_($this,back) <= 0 
		|| $values_($this,sigma) <= 0
    		|| $values_($this,nsigma) <= 0} {
	    error_dialog "Need to have BACK, SIGMA, NSIGMA specified"
	    return {}
	}
	
	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/skew"

	lappend arglist "in=$image"
	lappend arglist "out=$values_($this,outputndffile)"
	lappend arglist "back=$values_($this,back)"
	lappend arglist "sigma=$values_($this,sigma)"
	lappend arglist "nsigma=$values_($this,nsigma)"
	lappend arglist "width=$values_($this,width)"
	lappend arglist "psize=$values_($this,psize)"
	lappend arglist "mult=$values_($this,mult)"
	lappend arglist "useall=[bool_value useall]"
	lappend arglist "modet=[bool_value modet]"

	return $arglist
    }

    private method reset_skew_ {} {
	$itk_component(nsigma)      configure -value $defaults_($this,nsigma)
	$itk_component(psize-skew)  configure -value $defaults_($this,psize)
	$itk_component(useall-skew) configure -value $defaults_($this,useall)
	$itk_component(modet)       configure -value $defaults_($this,modet)
	$itk_component(mult-skew)   configure -value $defaults_($this,mult)
	$itk_component(width-skew)  configure -value $defaults_($this,width)
    }

    private method add_skew_selections_ {parent} {
	set lwidth 16
	set vwidth 5

	itk_component add psize-skew {
	    LabelEntry $parent.psize \
		    -text "Pixel size/as." \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,psize)]
	}
	pack $itk_component(psize-skew) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(psize-skew) \
		{The size of each image pixel, in units of arc-seconds}

	itk_component add useall-skew {
	    StarLabelCheck $parent.useall \
		    -text "Use pixel threshold" \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,useall)]
	}
	pack $itk_component(useall-skew) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(useall-skew) \
		{Is a pixel count threshold use when calculation the correlation?}
	itk_component add modet {
	    StarLabelCheck $parent.modet \
		    -text "Use global modal pixel value" \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,modet)]
	}
	pack $itk_component(modet) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(modet) \
		{Use a global modal pixel value when calculating skewness?}

	itk_component add mult-skew {
	    LabelEntry $parent.mult \
		    -text "Multiplication factor" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,mult)]
	}
	pack $itk_component(mult-skew) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(mult-skew) \
		{A multiplying factor applied to each of the results}

	itk_component add width-skew {
	    LabelEntry $parent.width \
		    -text "Sampling width" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,width)]
	}
	pack $itk_component(width-skew) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(width-skew) \
		{Width of the sampling area/filter (units as.)}

    }

    ### TOPPED...
    private method make_topped_command_ {image} {
	# insist that back and sigma be specified, otherwise return ""
	if {$values_($this,back) <= 0 
		|| $values_($this,sigma) <= 0
    		|| $values_($this,nsigma) <= 0} {
	    error_dialog "Need to have BACK, SIGMA, NSIGMA specified"
	    return {}
	}
	
	set arglist {}

	lappend arglist "$itk_option(-esp_dir)/topped"

	lappend arglist "in=$image"
	lappend arglist "out=$values_($this,outputndffile)"
	lappend arglist "back=$values_($this,back)"
	lappend arglist "sigma=$values_($this,sigma)"
	lappend arglist "nsigma=$values_($this,nsigma)"
	lappend arglist "width=$values_($this,width)"
	lappend arglist "psize=$values_($this,psize)"
	lappend arglist "noise=[bool_value noise]"

	return $arglist
    }

    private method reset_topped_ {} {
	$itk_component(nsigma)       configure -value $defaults_($this,nsigma)
	$itk_component(psize-topped) configure -value $defaults_($this,psize)
	$itk_component(noise)        configure -value $defaults_($this,noise)
	$itk_component(width-topped) configure -value $defaults_($this,width)
    }

    private method add_topped_selections_ {parent} {
	set lwidth 16
	set vwidth 5

	itk_component add noise {
	    StarLabelCheck $parent.noise \
		    -text "Noise" \
		    -labelwidth $lwidth \
		    -anchor w \
		    -variable [scope values_($this,noise)]
	}
	pack $itk_component(noise) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(noise) \
		{Set selected pixels to random value, or BAD}

	itk_component add psize-topped {
	    LabelEntry $parent.psize \
		    -text "Pixel size/as." \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,psize)]
	}
	pack $itk_component(psize-topped) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(psize-topped) \
		{The size of each image pixel, in units of arc-seconds}

	itk_component add width-topped {
	    LabelEntry $parent.width \
		    -text "Radius" \
		    -labelwidth $lwidth \
		    -valuewidth $vwidth \
		    -textvariable [scope values_($this,width)]
	}
	pack $itk_component(width-topped) -side top -fill x -ipadx 1m -ipady 1m
	add_short_help $itk_component(width-topped) \
		{Radius of the selected circle around bright pixels (units as.)}
    }

    ### Results...
    private method make_results_command_ {image} {
	# do nothing
    }

    private method reset_results_ {} {
	# Clear the page of results, if it's present
	if {$revealed_($indexes_(results))} {
	    $itk_component(results-menu) clear
	    $itk_component(results) clear
	}
    }

    private method add_results_selections_ {parent} {
	itk_component add results-menu {
	    LabelMenu $parent.results-menu \
		    -text "Source:"
	}
	$itk_component(results-menu) add -label "None" -value 0
	pack $itk_component(results-menu) -side top

	itk_component add results {
	    TableList $parent.results \
		    -title "Results" \
		    -headings {SourceN X Y SemiMajor PA Ellipt} \
		    -sizes {10 10 10 10 10 10}
	}
	$itk_component(results) set_option X Precision 1
	$itk_component(results) set_option Y Precision 1
	$itk_component(results) set_option SemiMajor Precision 1
	$itk_component(results) set_option PA Precision 3
	$itk_component(results) set_option Ellipt Precision 3
	$itk_component(results) set_option SourceN Show 0
	pack $itk_component(results) -fill both -expand 1
	# Now clear the table, even though there's nothing in it.
	# This repaints the table, so that the SourceN column will be
	# properly hidden as required.
	$itk_component(results) clear
    }

    #  Set an element of the values_ array
    protected method set_values_ {elem value} {
	set values_($this,$elem) $value
    }

    # Set defaults for _all_ of the parameters which are used in ESP
    protected method set_defaults_ {} {

	# Set local defaults
	set values_($this,angcon) 1	;# boolean
	set values_($this,angoff) 0.0
	set values_($this,autol) 0	;# boolean
	set values_($this,back) 0.0
	set values_($this,calcsd) 1	;# boolean
	set values_($this,expert) 0	;# doesn't correspond to parameter
	set values_($this,fwhm) 1	;# boolean
	set values_($this,maxiter) -1
	set values_($this,nsigma) 0
	set values_($this,psize) 1
	set values_($this,sigma) 0.0

	# corr-specific
	set values_($this,mult) 1000
	set values_($this,scale) 5	;# reasonable default?
	set values_($this,useall) 1	;# boolean

	# ellprofou-specific
	set values_($this,ellprofoumethod) pro
	set values_($this,autolt) 1	;# boolean
	set values_($this,fast) 1	;# boolean
	set values_($this,fine) 1.0
	set values_($this,fract) 40
	set values_($this,frzori) 1	;# boolean
	set values_($this,lim1) 1.25
	set values_($this,lim2) 0.5
	set values_($this,lim3) 20
	set values_($this,lim3flag) 0
	set values_($this,zerop) 27.5
	set values_($this,minmod) 0
	set values_($this,ellprofouoriginflag) yy

	# fastmed-specific
	set values_($this,width) 50

	# gaufit-specific
	set values_($this,lsqfit) 1
	set values_($this,anginc) 0.0
	set values_($this,pinc) 0.0
	set values_($this,sainc) 0.0
	set values_($this,sbinc) 0.0
	set values_($this,xinc) 0.0
	set values_($this,yinc) 0.0
	set values_($this,fitback) 1

	# hsub-specific
	set values_($this,sfact) 10
	set values_($this,sfactflag) -1
	set values_($this,hsubtype) 0
	set values_($this,hsubformatted) 1	;# boolean

	# selfc-specific
	set values_($this,selfcflag) 1		;# boolean

	# skew-specific
	set values_($this,modet) 1		;# boolean

	# sector-specific
	set values_($this,fitlim1) 0
	set values_($this,fitlim2) 20
	set values_($this,mirror) 0		;# boolean
	set values_($this,radisp) r
	set values_($this,surf) 1		;# boolean

	# File names
	set values_($this,sourcefile) {}
	set values_($this,outputndffile) {GaiaEsp_output}
	set values_($this,outputtextfile) {GaiaEsp_outputtext}
	set values_($this,outputstlfile) {GaiaEsp_outputstl.txt}
	set values_($this,ardfile) {}

	set valuenames_ {
angcon
angoff
autol
back
calcsd
expert
fwhm
maxiter
nsigma
psize
sigma
mult
scale
useall
ellprofoumethod
autolt
fast
fine
fract
frzori
lim1
lim2
lim3
lim3flag
zerop
minmod
ellprofouoriginflag
width
lsqfit
anginc
pinc
sainc
sbinc
xinc
yinc
fitback
fitlim1
fitlim2
mirror
radisp
surf
sfact
sfactflag
hsubtype
hsubformatted
selfcflag
modet
sourcefile
outputndffile
outputtextfile
outputstlfile
ardfile
	}

	#  Record defaults so they can be restored
	foreach name $valuenames_ {
	    set defaults_($name) $values_($this,$name)
	}

    }

    #  Helper methods
    private method toggle_angcon_ {value} {
	set values_($this,angcon) $value
	if { $value == 1 } {
	    set help {Positive angles denote clockwise rotation}
	} else {
	    set help {Positive angles denote anti-clockwise rotation}
	}
	add_short_help $itk_component(angcon) "$help"
    }

    private method toggle_lsqfit_ {} {
	set search_only {autol-gaufit anginc pinc sainc sbinc xinc yinc}
	set lsqfit_only {fitback calcsd}
	if { $values_($this,lsqfit) } {
	    foreach cpt $search_only {
		$itk_component($cpt) configure -state disabled
	    }
	    foreach cpt $lsqfit_only {
		$itk_component($cpt) configure -state normal
	    }
	    $itk_component(maxiter) configure -text "Maximum iterations"
	    add_short_help $itk_component(maxiter) \
		    {maxiter: Upper-bound on iteration count}
	} else {
	    foreach cpt $search_only {
		$itk_component($cpt) configure -state normal
	    }
	    set values_($this,fitback) 0
	    foreach cpt $lsqfit_only {
		$itk_component($cpt) configure -state disabled
	    }
	    $itk_component(maxiter) configure -text "Iteration count"
	    add_short_help $itk_component(maxiter) \
		    {niter: number of iterations}
	}
	toggle_fitback_
    }

    private method toggle_fitback_ {} {
	if { $values_($this,fitback) } {
	    foreach cpt "back nsigma" {
		$itk_component($cpt) configure -state disabled
	    }
	} else {
	    foreach cpt "back nsigma" {
		$itk_component($cpt) configure -state normal
	    }
	}
    }

    # If values_($this,expert) is now `true', then enable a set of
    # fields; if it is now `false',
    # switch that set off.  The set consists of $expert_parameter_list_ if 
    # `useellpromethod' is pro or proslow, but if that is false, then
    # the set consists of 
    # {$expert_parameter_list_ \ $ellpro_only_}, and the members of 
    # $ellpro_only_ should all be disabled.
    private method toggle_expert_ {} {
	if {$values_($this,expert)} {
	    set newval normal
	} else {
	    set newval disabled
	}

	# get current value from menu
	set method [$itk_component(method-ellprofou) get]
	set values_($this,ellprofoumethod) $method

	foreach i $expert_parameter_list_ {
	    if {$method == "fou" && [lsearch -exact $ellpro_only_ $i] >= 0} {
		set st disabled
	    } else {
		set st $newval
	    }
	    $itk_component($i) configure -state $st
	}
    }

    private method toggle_originflag_ {} {
	set values_($this,ellprofouoriginflag) \
		[$itk_component(originflag-ellprofou) get]
	if {[string index $values_($this,ellprofouoriginflag) 0] == {y}} {
	    $itk_component(autolt) configure -state normal
	} else {
	    $itk_component(autolt) configure -state disabled
	}
    }

    private method toggle_isselfc_ {} {
	if {$values_($this,selfcflag)} {
	    $itk_component(mult-selfc) configure -state disabled
	    add_short_help $itk_component(diag-b) \
		{Draw a circle: expected galaxy diameter}
	} else {
	    $itk_component(mult-selfc) configure -state normal
	    add_short_help $itk_component(diag-b) \
		{Draw a circle: expected galaxy scalelength}
	}
    }

    # Return the name of a temporary file.  Do this by trying ten filenames
    # starting with the PID.  Something is amiss if this doesn't work.
    #
    # Append the name to the list of temporary files.
    private method temp_file_name_ {prefix {suffix {}}} {
	set n [pid]
	for {set i 0} {$i < 10} {incr i} {
	    if {[file exists $prefix$n$suffix]} {
		incr n
	    } else {
		lappend temporary_files_ $prefix$n$suffix
		return $prefix$n$suffix
	    }
	}
	error_dialog "Can't generate temporary file name!"
	return {}
    }

    # Invoke HSUB to estimate the background.  If wait_for_it is
    # non-zero, then do not return until the command has completed.
    private method bg_from_hsub_ {{wait_for_it 0}} {
	if {$hsub_wfile_ == {}} {
	    set hsub_wfile_ [temp_file_name_ "/tmp/GaiaEsp-hsubworkfile"]
	} elseif {[file exists $hsub_wfile_]} {
	    exec rm $hsub_wfile_
	}

	set image [$itk_option(-rtdimage) cget -file]
	if {$image != {}} {
	    $namer_ configure -imagename $image
	    set image [$namer_ ndfname]
	}

	set invoke_cmd "$itk_option(-esp_dir)/hsub"
	set invoke_args "in=$image sfact=-1 type=0 noformatted out=$hsub_wfile_"

	# Establish a control object for this task, if not already done
	blt::busy hold $w_

	set hsub_star_app_ [StarApp #auto \
		-show_output $itk_component(status) \
		-notify [code $this completed_bg_from_hsub_] \
		-application $invoke_cmd
	]

	# Clear the log window
	$itk_component(status) clear 0 end

	# Run HSUB
	$hsub_star_app_ runwiths $invoke_args

	if {$wait_for_it} {
	    set hsub_semaphore_ 1
	    tkwait variable [scope hsub_semaphore_]
	}
    }

    private method completed_bg_from_hsub_ {} {
	blt::busy release $w_

	if {$hsub_wfile_ != {} && [file readable $hsub_wfile_]} {
	    set kw [gaia::StarSTLFile #auto $hsub_wfile_]
	    set kws [$kw status]
	    if {$kws} {
		set mode [$kw parameter mode]
		set sd   [$kw parameter sd]
		if {$mode != {} && $sd != {}} {
		    set values_($this,back) $mode
		    set values_($this,sigma) $sd
		} else {
		    error_dialog "Couldn't find BACK and SIGMA from HSUB"
		}
		if {$kws < 0} {
		    puts "Errors from $hsub_wfile_: [$kw error_msg]"
		}
	    } else {
		puts "Can't read $hsub_wfile_: [$kw error_msg]"
	    }

	} else {
	    error_dialog "Couldn't find HSUB temporary file"
	}

	# method bg_from_hsub_ may be tkwait-ing on this variable
	set hsub_semaphore_ 0
    }

    # Add a subset of the list of `common' widgets.
    private method add_common_widgets_ {parent panellist toolname} {

	if {[lsearch $panellist inardname] >= 0} {
	    itk_component add inardname-$toolname {
		LabelFileChooser $parent.inardname \
			-labelwidth $labelwidth_ \
			-text "ARD file:" \
			-textvariable [scope values_($this,ardfile)]
	    }
	    pack $itk_component(inardname-$toolname) \
		    -side top -fill x -ipadx 1m -ipady 1m
	    add_short_help $itk_component(inardname-$toolname) \
		    {Name of input ARD file}
	}
	    
	if {[lsearch $panellist outndfname] >= 0} {
	    itk_component add outndfname-$toolname {
		LabelFileChooser $parent.outndfname \
			-labelwidth $labelwidth_ \
			-text "NDF output:" \
			-textvariable [scope values_($this,outputndffile)]
	    }
	    pack $itk_component(outndfname-$toolname) \
		    -side top -fill x -ipadx 1m -ipady 1m
	    add_short_help $itk_component(outndfname-$toolname) \
		    {Name of output NDF file}
	}
	    
	if {[lsearch $panellist outtextname] >= 0} {
	    itk_component add outtextname-$toolname {
		LabelFileChooser $parent.outtextname \
			-labelwidth $labelwidth_ \
			-text "Text output:" \
			-textvariable [scope values_($this,outputtextfile)]
	    }
	    pack $itk_component(outtextname-$toolname) \
		    -side top -fill x -ipadx 1m -ipady 1m
	    add_short_help $itk_component(outtextname-$toolname) \
		    {Name of output text file}
	}

	if {[lsearch $panellist outstlname] >= 0} {
	    itk_component add outstlname-$toolname {
		LabelFileChooser $parent.outstltname \
			-labelwidth $labelwidth_ \
			-text "STL output:" \
			-textvariable [scope values_($this,outputstlfile)]
	    }
	    pack $itk_component(outstlname-$toolname) \
		    -side top -fill x -ipadx 1m -ipady 1m
	    add_short_help $itk_component(outstlname-$toolname) \
		    {Name of output STL file}
	}

	if {[lsearch $panellist back] >= 0} {

	    itk_component add back-$toolname {
		gaia::StarLabelWidgetChild $parent.back \
			-text "Background:" \
			-labelwidth $labelwidth_
	    }
	    set cs [$itk_component(back-$toolname) childsite]
	    entry $cs.b -textvariable [scope values_($this,back)]  -width 7
	    label $cs.l1 -text {+/-}
	    entry $cs.s -textvariable [scope values_($this,sigma)] -width 7
	    label $cs.l2 -text {counts}
	    button $cs.button \
		    -text {Estimate} \
		    -command [code $this bg_from_hsub_]
	    pack $cs.b -side left -padx 1m
	    pack $cs.l1 -side left -padx 1m
	    pack $cs.s -side left -padx 1m
	    pack $cs.l2 -side left -padx 1m
	    pack $cs.button -side right -padx 1m -fill x
	    pack $itk_component(back-$toolname) \
		    -side top -fill x -ipadx 1m -ipady 1m
	    add_short_help $itk_component(back-$toolname) \
		    {The background value for the image}
	}

	if {[lsearch $panellist nsigma] >= 0} {
	    itk_component add nsigma-$toolname {
		LabelEntry $parent.nsigma \
			-text "Sig. sigma:" \
			-labelwidth $labelwidth_ \
			-valuewidth 2 \
			-textvariable [scope values_($this,nsigma)]
	    }
	    pack $itk_component(nsigma-$toolname) \
		    -side top -ipadx 1m -ipady 1m
	    add_short_help $itk_component(nsigma-$toolname) \
		    {Number of sigma above background before a pixel should be regarded as significant}
	}
    }

      # Disable a subset of the common widgets.  Argument is a list of elements
      # in common_widgets_ which are to be disabled; others are enabled.
      # disable_common_widgets_{} enables all,
      # disable_common_widgets_{$common_widgets_} disables all.
      private method disable_common_widgets_ {subset} {
  	foreach w $common_widgets_ {
  	    if {[lsearch $subset $w] >= 0} {
  		$itk_component($w) configure -state disabled
  	    } else {
  		$itk_component($w) configure -state normal
  	    }
  	}
      }

    private method get_panel_number_ {} {
	if {$int_panel_number_ < 0} {
	    set int_panel_number_ 0
	} else {
	    incr int_panel_number_
	}
	return $int_panel_number_
    }

    private method delete_canvas_graphics_ {taglist} {
	foreach t $taglist {
	    $itk_option(-canvas) delete $t
	}
    }


    #  -- Configuration options (public variables)

    #  Name of a StarCanvasDraw widget to use to control objects.
    itk_option define -canvasdraw canvasdraw CanvasDraw {} {}

    #  Name of canvas.
    itk_option define -canvas canvas Canvas {} {}

    #  Name of rtdimage widget
    itk_option define -rtdimage rtdimage RtdImage {} {}
    
    #  Name of RtdImageCtrl widget or a derived class.
    itk_option define -image image Image {} {}

    #  Identifying number for toolbox (shown in () in window title).
    itk_option define -number number Number 0 {}

    #  Colours
    itk_option define -ellipse_colour ellipse_colour Ellipse_colour {white} {
	$itk_option(-canvas) itemconfigure esp_out$w_ \
		-outline $itk_option(-ellipse_colour)
    }
    itk_option define -selection_colour selection_colour Selection_colour {white} {
	$itk_option(-canvas) itemconfigure esp_sel \
		-outline $itk_option(-selection_colour)
	$itk_option(-canvasdraw) configure \
		-outlinecolor $itk_option(-selection_colour)
	#if { $objectlist_ != {} } {
	#    foreach o $objectlist_ {
	#	$o configure -selection_colour $itk_option(-selection_colour)
	#    }
	#}
    }

    #  Command to execute to create a new instance of this object.
    itk_option define -clone_cmd clone_cmd Clone_Cmd {}

    #  If this is a clone, then it should die rather than be withdrawn
    itk_option define -really_die really_die Really_Die 0

    #  How wide should the panel be (minimum)?
    itk_option define -width gaiaespwidth GaiaEspWidth 350
    itk_option define -height gaiaespheight GaiaEspHeight 500

    #  Possible colours.
    itk_option define -colors colors Colors {
	red green blue cyan magenta yellow white grey90 grey40 grey10 black
    }

    #  Should the application warn if output files are about to be overwritten?
    itk_option define -warn-if-overwrite warn-if-overwrite Warn-if-overwrite 1

    #  Should the application ask for permission before automatically
    #  fitting a background
    itk_option define -auto-fit-background \
	    auto-fit-background Auto-fit-background 0

    #  Define ESP directory
    itk_option define -esp_dir esp_dir Esp_Dir {} {
	if {$itk_option(-esp_dir) == {} } {
	    global env
	    if {[info exists env(ESP_DIR)]} {
		set itk_option(-esp_dir) $env(ESP_DIR)
	    } elseif {[info exists env(STARLINK)] && [file isdirectory $env(STARLINK)/bin/esp]} {
	        set itk_option(-esp_dir) $env(STARLINK)/bin/esp
	    } elseif {[file isdirectory /star/bin/esp]} {
		set itk_option(-esp_dir) /star/bin/esp
	    } else {
		info_dialog "Cannot locate ESP directory.  Define ESP_DIR and restart"
	    }
	}
    }

    #  --- Private variables (available to instance only)

    #  Symbolic names of pages (used to access names via index)
    private variable pages_
    private variable indexes_		;# inverse to pages_
    private variable revealed_

    #  Default values for all parameters
    private variable defaults_

    #  List of selected objects
    private variable objectlist_ {}

    #  Object to deal with image names
    private variable namer_ {}

    #  The interaction with the actual application
    private variable star_app_ {}
    #  The command the star_app_ was invoked with, if any
    private variable star_app_name_ {}

    #  The temporary file which bg_from_hsub_ uses
    private variable hsub_wfile_ {}

    #  Panel number, maintained by get_panel_number_.  Initialised negative
    private variable int_panel_number_ -1

    #  Size of labels in widgets
    private variable labelwidth_ 12

    #  Options which are available only in `expert' mode in ellpro/fou
    private variable expert_parameter_list_ \
	    {fine fract lim1 lim2 lim3 zerop minmod}

    #  Options which are specific to ellpro.  All of these are expert-only
    private variable ellpro_only_ {lim3 minmod}

    #  List/table of results obtained from ESP run
    private variable ESP_results_

    # Files to be deleted within destructor
    private variable temporary_files_ {}

    # semaphore for bg_from_hsub_
    private variable hsub_semaphore_ {}

    #  notebook_characteristics_ is a hash in which we store various
    #  characteristics of the different ESP applications, keyed on
    #  the string `characteristic,application'.  Characteristics are
    #  `files' and `reqsrc'.  The `files' characteristic contains a
    #  list of file type names which the `run' function checks have
    #  names -- for each entry, c, in this list, it checks that
    #  $values_($this,$c) has a value.  The `reqsrc' characteristic is
    #  a boolean: if true, then the objectlist_ is checked to be
    #  non-empty when `run' is called.
    private common notebook_characteristics_

    #  -- Common variables (shared by all instances)

    #  Array for passing around globally.  Indexed by ($this,param).
    common values_

    #  Widgets which are in the main panel are as follows
    common common_widgets_ \
	    {run close resetpage resetall}
}
