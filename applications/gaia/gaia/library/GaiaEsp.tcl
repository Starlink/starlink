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
#    Copyright 2000 Central Laboratory of the Research Councils
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA
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
#  Inheritance:
#    util::TopLevelWidget
#
#  Weaknesses:
#    The following aren't really bugs, but things it would be nice to
#    improve, given the opportunity:
#
#      - Supporting some of the other ESP tools (see notes below).
#      - Graphing the output of the ellprofou tool.
#
#  Notes:
#
#    In an early phase of this interface's development, I (NG)
#    implemented _all_ the ESP tools, in a fairly rough fashion.
#    Subsequently, I substantially rewrote the ellprofou tool, without
#    having time to rewrite the other tools in the same way.  The code
#    supporting the other tools can't be reused trivially, but it
#    might still be of some use when and if support for those other
#    tools is added.  That old code can be found in the CVS
#    repository, in versions of this module before 1.15.
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
        set File [add_menubutton "File"]
        configure_menubutton File -underline 0
        set Options [add_menubutton "Options"]
        configure_menubutton Options -underline 0
        set Colours [add_menubutton "Colours"]
        configure_menubutton Colours -underline 0

        # Add window help
        add_help_button espusage "On Window..."
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
        $Options add checkbutton \
                -label {Show integrated counts} \
                -variable [scope itk_option(-integrate)] \
                -command [code $this changed_integrate_]

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

        #  OBJECT SELECTION
        set thisnum [get_panel_number_]
        $itk_component(notebook) add -label {Objects} \
                -command [code $this reveal_ $thisnum]
        set pages_($thisnum) objectselection
        set revealed_($thisnum) 0
        set notebook_characteristics_(disable,objectselection) {run}
        set notebook_characteristics_(reqsrc,objectselection) 0

        #  ELLPRO/ELLFOU
        set thisnum [get_panel_number_]
        $itk_component(notebook) add -label {Ellipse fit} \
                -command [code $this reveal_ $thisnum]
        set pages_($thisnum) ellprofou
        set revealed_($thisnum) 0
        set notebook_characteristics_(files,ellprofou) \
                {sourcefile outputtextfile outputstlfile}
        set notebook_characteristics_(reqsrc,ellprofou) 1

        #  RESULTS
        set thisnum [get_panel_number_]
        $itk_component(notebook) add -label Results \
                -command [code $this reveal_ $thisnum]
        set pages_($thisnum) results
        set revealed_($thisnum) 0

        # create the inverse of the array pages_
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
        pack $itk_component(actionframe) \
                -side bottom -fill x -pady 5 -padx 5
        pack $itk_component(status) \
                -side bottom -fill both -expand 1 -pady 5 -padx 5

        # buttons
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
        set namer_ [gaia::GaiaImageName #auto]
    }

    # Destructor
    destructor {
        foreach f $temporary_files_ {
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
        # delete the results of any previous run from _this_ toolbox
        delete_canvas_graphics_ esp_out$w_

        #set image [$itk_option(-rtdimage) cget -file]
        set image [$itk_option(-rtdimage) fullname]
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
                    switch $ftype {
                        "outputstlfile" {
                            # Note that this filename _must_ end in .txt,
                            # so that a STL catalogue will be generated,
                            # as this file is to be parsed by class
                            # StarSTLFile.
                            set fname "GaiaEspCat.txt"
                        }
                        "outputtextfile" {
                            set fname "GaiaEspResults.txt"
                        }
                        "outputndffile" {
                            set fname "GaiaEspResults.sdf"
                        }
                        "ardfile" {
                            set fname "GaiaEspArd.txt"
                        }
                        "sourcefile" {
                            set fname "GaiaEspSources.txt"
                        }
                        default {
                            # I'm rather surprised this is happening --
                            # what file type is this?  Still, never say die!
                            set fname [format "GaiaEsp_%s" $ftype]
                        }
                    }
                    set values_($this,$ftype) $fname
                }
                # Check that none of these output files already exist.
                if {[file exists $fname]} {
                    set fileexists [format "%s%s " $fileexists $fname]
                }
            }
            # If there is one or more of the output files which
            # already exists, then warn the user, and offer to delete
            # the file before we go on.  If we didn't do this, the ESP
            # application would fail, as the routines in there
            # generally refuse to open already-existing files for
            # output.  Omit the warning if -warn-if-overwrite is false.
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
                        puts stderr "ERROR: no value for parameter '$parameter' and no default"
                    }
                } else {
                    lappend norm_arglist $setting
                }
            }

            set invoke_cmd [lindex $norm_arglist 0]
            set invoke_args [join [lrange $norm_arglist 1 end]]

            if {$showit} {
                # We've been invoked with the $showit argument true,
                # so echo the command-line we've just constructed.
                puts stdout "invocation: $invoke_cmd $invoke_args"
            }
            if {$execit} {
                # We've been invoked with the $execit argument true,
                # so carry on and do the work.

                # Establish a control object for this task, if not already done
                blt::busy hold $w_

                if {$star_app_ == {} || $star_app_name_ != $invoke_cmd} {
                    set star_app_ [GaiaApp #auto \
                            -show_output $itk_component(status) \
                            -notify [code $this completed_${whichpage}_] \
                            -application $invoke_cmd
                    ]
                    set star_app_name_ $invoke_cmd
                }

                # Clear the log window
                $itk_component(status) clear 0 end

                # Make sure image is updated, if volatile (cube slices).
                $itk_option(-image) save_if_volatile

                # RUN ESP!!!
                $star_app_ runwiths $invoke_args
            }
        }
    }

    # Any cleanup when task is completed
    private method completed_ {} {
        blt::busy release $w_
    }

    private method completed_ellprofou_ {} {
        blt::busy release $w_

        if {! $revealed_($indexes_(results))} {
            # The results notebook page has to be revealed (ie,
            # constructed) before we can add material to the
            # results-menu component below.
            reveal_ $indexes_(results)
        }

        set stl [gaia::StarSTLFile #auto $values_($this,outputstlfile)]
        if {[$stl status]} {
            # nellipses is the number of rows read from the STL catalogue.
            set nellipses [$stl parameter _nrows]
            # ESP_results_ is the list of results from the catalogue,
            # selected from the columns present there.
            set ESP_results_ {}

            $itk_component(results-menu) clear
            set sourcen 0

            for {set elln 1} {$elln <= $nellipses} {incr elln} {
                $stl reset_isvalid
                set ell_src [$stl table $elln sourcen]
                set ell_x   [$stl table $elln x]
                set ell_y   [$stl table $elln y]
                set ell_sma [$stl table $elln semimajor]
                set ell_count [$stl table $elln count]
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
                    [list $sourcen $ell_x $ell_y $ell_sma $ell_count $ell_pos [expr 1/$ell_invell]]
            }
            show_ellprofou_results_ 1
            # Force the label on the menu to be updated.
            $itk_component(results-menu) configure -value 1
        } else {
            error_dialog "Error reading ELLPRO output file:\n[$stl error_msg]"
        }
    }

    private method show_ellprofou_results_ {sourcen} {
        # Display the subset of rows in the array ESP_results_ which
        # have column 0 equal to $sourcen
        if {! $revealed_($indexes_(results))} {
            reveal_ $indexes_(results)
        }
        $itk_component(results) clear
        set x {}
        set y {}
        foreach row $ESP_results_ {
           if {[lindex $row 0] == $sourcen} {
              $itk_component(results) append_row $row
              lassign $row i xc yc sma count pos inv
              lappend x $sma
              lappend y $count
           }
        }
        $xVector_ set $x
        if { $itk_option(-integrate) } {
           set sum 0.0
           set iy {}
           foreach value $y {
              set sum [expr $sum+$value]
              lappend iy $sum
           }
           set y $iy
        }
        $yVector_ set $y
        set numValues_ [$xVector_ length]
        set last_sourcen_ $sourcen
        $itk_component(results) new_info
    }

    # return the string `true' or `false' depending on whether
    # $value_($this,$var) is true or false
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
                $objectlist_ default_config             ;# default
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

    ### ELLPRO...
    # Save the ELLPRO/FOU source selections to a file
    private method save_ellprofou_sourcefile_ {filename} {
        set olist [$objectlist_ get_sourcelist]
        if {[llength $olist] > 0 && $filename != {}} {
            busy {
                set fid [::open $filename w]
                puts $fid "# GAIA ESP ELLPRO sources file"
                puts $fid "# Columns:  X, Y Background (ignored) rlim"
                # Tell ESP that the following lines are in the GRID
                # WCS domain (ie, they're pixel coordinates rather
                # than world ones).  This behaviour is not publicly
                # documented.
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
        lappend arglist "back=$values_($this,back)"
        lappend arglist "sigma=$values_($this,sigma)"
        if {$is_ellpro} {
            # fast is true if ellprofoumethod is pro, false if it's proslow
            lappend arglist "fast=[expr {($fullmeth == "pro") ? "true" : "false"}]"
        }
        if {[$itk_component(fine) is_enabled]} {
            lappend arglist "fine=$values_($this,fine)"
        }

        set sourcepos $values_($this,ellprofouoriginflag)
        if {[string index $sourcepos 0] == {y}} {
            lappend arglist autol
            lappend arglist "autolt=[bool_value autolt]"
        } else {
            lappend arglist noautol
        }
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
        set values_($this,expert) $defaults_(expert)
        $itk_component(fine) reset
        $itk_component(fract) reset
        $itk_component(lim1) reset
        $itk_component(lim2) reset
        $itk_component(lim3) reset
        $itk_component(zerop) reset
        $itk_component(minmod) configure -value $defaults_(minmod)
    }

    protected method add_ellprofou_selections_ {parent} {
        set lwidth $labelwidth_
        set vwidth 5

        add_common_widgets_ $parent {outtextname outstlname inardname back} ellprofou

        itk_component add method-ellprofou {
            util::LabelMenu $parent.method \
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
            util::LabelMenu $parent.originflag \
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
            util::LabelMenu $parent.autolt \
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
            gaia::StarLabelCheck $parent.expert \
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
            util::LabelMenu $parent.minmod \
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
        toggle_originflag_
        toggle_expert_
    }


    ### RESULTS...
    private method make_results_command_ {image} {
        # do nothing
        return {}
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
            util::LabelMenu $parent.results-menu \
                    -text "Source:"
        }
        $itk_component(results-menu) add -label "None" -value 0
        pack $itk_component(results-menu) -side top

        itk_component add results {
            TableList $parent.results \
                    -title "Results" \
                    -hscroll 1 -vscroll 1 \
                    -headings {SourceN X Y SemiMajor Count PA Ellipt} \
                    -sizes {10 10 10 10 10 10 10}
        }
        $itk_component(results) set_option X Precision 1
        $itk_component(results) set_option Y Precision 1
        $itk_component(results) set_option SemiMajor Precision 1
        $itk_component(results) set_option Count Precision 1
        $itk_component(results) set_option PA Precision 3
        $itk_component(results) set_option Ellipt Precision 3
        $itk_component(results) set_option SourceN Show 0
        pack $itk_component(results) -fill both -expand 1
        # Now clear the table, even though there's nothing in it.
        # This repaints the table, so that the SourceN column will be
        # properly hidden as required.
        $itk_component(results) clear

        # Reveal the graph.
        make_graph_ $parent
    }

    #  Set an element of the values_ array
    protected method set_values_ {elem value} {
        set values_($this,$elem) $value
    }

    # Set defaults for _all_ of the parameters which are used in ESP
    protected method set_defaults_ {} {

        # Set local defaults
        set values_($this,angcon) 1     ;# boolean
        set values_($this,angoff) 0.0
        set values_($this,autol) 0      ;# boolean
        set values_($this,back) 0.0
        set values_($this,calcsd) 1     ;# boolean
        set values_($this,expert) 0     ;# doesn't correspond to parameter
        set values_($this,fwhm) 1       ;# boolean
        set values_($this,maxiter) -1
        set values_($this,nsigma) 0
        set values_($this,psize) 1
        set values_($this,sigma) 0.0

        # ellprofou-specific
        set values_($this,ellprofoumethod) pro
        set values_($this,autolt) 1     ;# boolean
        set values_($this,fast) 1       ;# boolean
        set values_($this,fine) 1.0
        set values_($this,fract) 40
        set values_($this,frzori) 1     ;# boolean
        set values_($this,lim1) 1.25
        set values_($this,lim2) 0.5
        set values_($this,lim3) 20
        set values_($this,lim3flag) 0
        set values_($this,zerop) 27.5
        set values_($this,minmod) 0
        set values_($this,ellprofouoriginflag) yy

        # File names
        set values_($this,sourcefile) {}
        set values_($this,outputndffile) {GaiaEspResults.sdf}
        set values_($this,outputtextfile) {GaiaEspResults.txt}
        set values_($this,outputstlfile) {GaiaEspCat.txt}
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

        #set image [$itk_option(-rtdimage) cget -file]
        set image [$itk_option(-rtdimage) fullname]
        if {$image != {}} {
            $namer_ configure -imagename $image
            set image [$namer_ ndfname]
        }

        set invoke_cmd "$itk_option(-esp_dir)/hsub"
        # When given the `noformatted' parameter, HSUB writes its
        # output
        set invoke_args "in=$image sfact=-1 type=0 out=! outcat=$hsub_wfile_"

        # Establish a control object for this task, if not already done
        blt::busy hold $w_

        set hsub_star_app_ [GaiaApp #auto \
                -show_output $itk_component(status) \
                -notify [code $this completed_bg_from_hsub_] \
                -application $invoke_cmd
        ]

        # Clear the log window
        $itk_component(status) clear 0 end

        # Make sure image is updated, if volatile (cube slices).
        $itk_option(-image) save_if_volatile

        # Run HSUB
        $hsub_star_app_ runwiths $invoke_args

        if {$wait_for_it} {
            # Don't return until the application has completed.  The
            # method completed_bg_from_hsub_ sets the variable
            # hsub_semaphore_ to 0, so we wait on that happening by
            # using the tkwait function.
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
                puts stderr "Can't read $hsub_wfile_: [$kw error_msg]"
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
                gaia::LabelFileChooser $parent.inardname \
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
                gaia::LabelFileChooser $parent.outndfname \
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
                gaia::LabelFileChooser $parent.outtextname \
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
                gaia::LabelFileChooser $parent.outstltname \
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
                util::LabelEntry $parent.nsigma \
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

    #  Make the graph subwindow. Override to get control of ranging and
    #  add a widget to display the data value (so we get X,Y and data value of
    #  position in the image).
    private method make_graph_ {parent} {

       #  Add the graph
       itk_component add graph {
	  blt::graph $parent.graph \
             -width 5i \
             -height 3i \
             -borderwidth 3 \
             -relief groove \
             -title "Radial profile"
       } {}
       set graph_ $itk_component(graph)
       pack $itk_component(graph) \
          -fill both -expand 1 -padx 1m -pady 1m

       add_short_help $itk_component(graph) \
          {Graph: intensity profile, {bitmap dragb1} = zoom, {bitmap b2} = restore}
       #  Set axes labels.
       $graph_ yaxis configure -title {Count}
       $graph_ xaxis configure -title {Semimajor axis}

       #  blt2.4f vector names must start with a letter, no dots...
       regsub -all {\.} v$graph_.xVector _ xVector_
       regsub -all {\.} v$graph_.yVector _ yVector_

       $graph_ legend config -hide 1
       if { ! [info exists $xVector_] } {
          blt::vector create $xVector_
       }
       if { ! [ info exists $yVector_] } {
          blt::vector create $yVector_
       }
       $graph_ element create elem -xdata $xVector_ -ydata $yVector_ \
          -symbol square -linewidth 0 -fill {} -pixels 5

       # add BLT features
       ::Blt_ZoomStack $graph_
       ::Blt_ActiveLegend $graph_
       ::Blt_Crosshairs $graph_
       ::Blt_ClosestPoint $graph_
    }

    #  Call when any graphs should be updated to reflect integration state.
    private method changed_integrate_ {} {
       if { [info exists indexes_(results)] } {
          show_ellprofou_results_ $last_sourcen_
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

    #  Integrate counts in profile, otherwise show counts.
    itk_option define -integrate integrate Integrate 0

    #  --- Private variables (available to instance only)

    #  Symbolic names of pages (used to access names via index)
    private variable pages_
    private variable indexes_           ;# inverse to pages_
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

    #  Graph & BLT vectors.
    private variable graph_
    private variable xVector_ {}
    private variable yVector_ {}
    private variable numValues_ 0

    #  Last source used in results table.
    private variable last_sourcen_ 1

    #  -- Common variables (shared by all instances)

    #  Array for passing around globally.  Indexed by ($this,param).
    common values_

    #  Widgets which are in the main panel are as follows
    common common_widgets_ \
            {run close resetpage resetall}
}
