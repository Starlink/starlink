# E.S.O. - VLT project/ESO Archive
# @(#) $Id: AstroQuery.tcl,v 1.17 1999/03/11 20:59:31 abrighto Exp $
#
# AstroQuery.tcl - widget for searching astronomical catalogs.
#
# See man page AstroQuery(n) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 05 Jan 98   created


itk::usual AstroQuery {}


# An AstroQuery widget is defined as a frame containing entries for search
# options.

itcl::class cat::AstroQuery {
    inherit util::FrameWidget

    # constructor

    constructor {args} {
	itk_option add hull.borderwidth hull.relief
	eval itk_initialize $args
    }

    
    # called after options have been evaluated

    protected method init {} {
	# set flag to handle image servers differently
	if {"[$astrocat servtype]" == "imagesvr"} {
	    set iscat_ 0
	} else {
	    set iscat_ 1
	}

	add_search_options
	add_copyright
	set_short_help

	# create an object for running interruptable batch queries
	Batch $w_.batch \
	    -command [code $this query_done] \
	    -debug $itk_option(-debug)

	# set default values
	set_default_values
    }


    
    # update the search option entries after they have been edited

    public method update_search_options {} {
	if {"$search_col_info_" != "[$astrocat searchcols]"} {
	    add_search_options
	    add_copyright
	    set_default_values

	}
    }

  
    # add (or update) the search options panel

    protected method add_search_options {} {
	if {! [winfo exists $w_.options]} {
	    # Options frame
	    itk_component add options {
		frame $w_.options
	    } {
	    }
	    pack $w_.options \
		-side top -fill x

	    pack \
		[label $w_.options.head -text "Search Options" ] \
		-side top -pady 2
	}
	if {[winfo exists $w_.options.f]} {
	    destroy $w_.options.f
	}

	pack [set f [frame $w_.options.f]] \
	    -side top -fill x
	
	set search_opts_ [blt::table $f]
	set search_opts_row_ 0

	# if we are using world coords, display name, equinox, ra, dec
	if {[$astrocat iswcs]} {
	    
	    add_search_option \
		[set name_ \
		     [LabelEntry $f.name \
			  -text "Object Name:" \
			  -command $itk_option(-searchcommand) \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-labelfont)]] \
		[set equinox_ \
		     [LabelEntry $f.equinox \
			  -text "Equinox:" \
			  -value "J2000" \
			  -autoselect 1 \
			  -command $itk_option(-searchcommand) \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-labelfont)]]

	    add_search_option \
		[set ra_ \
		     [LabelEntry $f.ra \
			  -text "a:" \
			  -autoselect 1 \
			  -command $itk_option(-searchcommand) \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-wcsfont)]] \
		[set dec_ \
		     [LabelEntry $f.dec \
			  -text "d:" \
			  -autoselect 1 \
			  -command $itk_option(-searchcommand) \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-wcsfont)]]
	} elseif {[$astrocat ispix]} {
	    # if we are using image coords, display x and y
	    add_search_option \
		[set x_ \
		     [LabelEntry $f.x \
			  -text "X:" \
			  -autoselect 1 \
			  -command $itk_option(-searchcommand) \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-labelfont)]] \
		[set y_ \
		     [LabelEntry $f.y \
			  -text "Y:" \
			  -autoselect 1 \
			  -command $itk_option(-searchcommand) \
			  -labelwidth $itk_option(-labelwidth) \
			  -anchor $itk_option(-anchor) \
			  -valuefont $itk_option(-valuefont) \
			  -labelfont $itk_option(-labelfont)]]
	}

	if {[$astrocat iswcs] || [$astrocat ispix]} {
	    if {$iscat_} {
		# add min and max radius items (for catalogs)
		add_search_option \
		    [set rad1_ [LabelEntry $f.rad1 \
				    -text "Min Radius:" \
				    -command $itk_option(-searchcommand) \
				    -labelwidth $itk_option(-labelwidth) \
				    -anchor $itk_option(-anchor) \
				    -valuefont $itk_option(-valuefont) \
				    -labelfont $itk_option(-labelfont)]] \
		    [set rad2_ [LabelEntry $f.rad2 \
				    -text "Max Radius:" \
				    -command $itk_option(-searchcommand) \
				    -labelwidth $itk_option(-labelwidth) \
				    -anchor $itk_option(-anchor) \
				    -valuefont $itk_option(-valuefont) \
				    -labelfont $itk_option(-labelfont)]]
	    } else {
		# add width and height items (for image servers)
		add_search_option \
		    [set width_ [LabelEntry $f.width \
				    -text "Width:" \
				    -command $itk_option(-searchcommand) \
				    -labelwidth $itk_option(-labelwidth) \
				    -anchor $itk_option(-anchor) \
				    -valuefont $itk_option(-valuefont) \
				    -labelfont $itk_option(-labelfont)]] \
		    [set height_ [LabelEntry $f.height \
				    -text "Height:" \
				    -command $itk_option(-searchcommand) \
				    -labelwidth $itk_option(-labelwidth) \
				    -anchor $itk_option(-anchor) \
				    -valuefont $itk_option(-valuefont) \
				    -labelfont $itk_option(-labelfont)]]
	    }
	}
	
	# display search columns from config entry. If the max value is missing,
	# assume just one value, otherwise a range: min..max
	set n 0
	set search_col_info_ [$astrocat searchcols]
	catch {unset min_values_}
	catch {unset max_values_}
	set search_cols_ {}
	foreach col_info $search_col_info_ {
	    if {[llength $col_info] < 2} {
		error_dialog \
		    "invalid search_cols entry in config file: \
                     '$col_info' ([llength $col_info])" $w_
		continue
	    }
	    lassign $col_info col minStr maxStr
	    if {"$col" == ""} {
		error_dialog "missing column value in config file entry: '$col_info'" $w_
		continue
	    }
	    if {"$minStr" == ""} {
		error_dialog "missing min value in config file entry: '$col_info'" $w_
		continue
	    }
	    lappend search_cols_ $col
	    set min_values_($col) [LabelEntry $f.min$n \
				       -text "$minStr:" \
				       -command $itk_option(-searchcommand) \
				       -labelwidth $itk_option(-labelwidth) \
				       -anchor $itk_option(-anchor) \
				       -valuefont $itk_option(-valuefont) \
				       -labelfont $itk_option(-labelfont)]

	    if {"$maxStr" != ""} {
		set max_values_($col) [LabelEntry $f.max$n \
					   -text "$maxStr:" \
					   -command $itk_option(-searchcommand) \
					   -labelwidth $itk_option(-labelwidth) \
					   -anchor $itk_option(-anchor) \
					   -valuefont $itk_option(-valuefont) \
					   -labelfont $itk_option(-labelfont)]
		add_search_option $min_values_($col) $max_values_($col)
	    } else {
		add_search_option $min_values_($col)
	    }
	    
	    incr n
	}
	
	# if the url contains a string such as mag=%m1,%m2, make sure the
	# user knows the defaults (maybe the servers should be changed?)
	if {[info exists min_values_(mag)]} {
	    if {[string first {%m2} [$astrocat url]] != -1} {
		$min_values_(mag) config -value 0
		$max_values_(mag) config -value 99
	    }
	}

	# if there were any search options at all...
	if {$search_opts_row_ > 0} {
	    if {$iscat_} {
		add_search_option \
		    [set maxnum_ [LabelEntry $f.maxnum \
				      -text "Max Objects:" \
				      -command $itk_option(-searchcommand) \
				      -labelwidth $itk_option(-labelwidth) \
				      -anchor $itk_option(-anchor) \
				      -valuefont $itk_option(-valuefont) \
				      -labelfont $itk_option(-labelfont)]]
	    }
	    blt::table configure $f C1 -padx 2m
	} else {
	    # no options
	    destroy $w_.options
	}

	update idletasks
    }

    
    # add copyright info to the search panel, if present

    protected method add_copyright {} {
	set s [$astrocat copyright]
	if {"$s" != ""} {
	    blt::table $search_opts_ \
		[LabelValue $search_opts_.copyright \
		     -anchor w -relief flat \
		     -labelwidth 0 -valuewidth 0 \
		     -value $s \
		     -valuefont $itk_option(-labelfont)] \
		[incr search_opts_row_],0 -anchor e -fill x -pady 1m -columnspan 2
	}
    }

    
    # add a row to the search options panel (blt table).
    # The args may be one or more widget names to put in a row in the panel.
    # Each widget is placed in a new column. An empty argument results in an
    # empty column for that row. If all are empty, the row is left empty.

    protected method add_search_option {args} {
	set col 0
	foreach w $args {
	    if {"$w" != ""} {
		blt::table $search_opts_ $w $search_opts_row_,$col \
		    -fill x -pady 1m
	    }
	    incr col
	}
	incr search_opts_row_
    }


    # add short help texts to widgets
    
    protected method set_short_help {} {
	if {[$astrocat iswcs]} {
	    add_short_help $name_ {object name: resolved via name server, if given}
	    add_short_help $equinox_ {World Coordinates equinox, default J2000}
	    add_short_help $ra_ {World Coordinates: right ascension (RA)}
	    add_short_help $dec_ {World Coordinates: declination (DEC)}
	    if {$iscat_} {
		add_short_help $rad1_ {Radius: minimum radius in arcmin for circular search}
		add_short_help $rad2_ {Radius: maximum radius in arcmin for circular search}
		add_short_help $maxnum_ {Max number of stars/objects to display}
	    } else {
		add_short_help $width_ {Width in arcmin of image to get}
		add_short_help $height_ {Height in arcmin of image to get}
	    }
	} elseif {[$astrocat ispix]} {
	    add_short_help $x_ {Image coordinates: X value}
	    add_short_help $y_ {Image coordinates: Y value}
	    if {$iscat_} {
		add_short_help $rad1_ {Radius: minimum radius in pixels for circular search}
		add_short_help $rad2_ {Radius: maximum radius in pixels for circular search}
		add_short_help $maxnum_ {Max number of stars/objects to display}
	    } else {
		add_short_help $width_ {Width in pixels of image to get}
		add_short_help $height_ {Height in pixels of image to get}
	    }
	}

	foreach col $search_cols_ {
	    if {$iscat_} {
		# add catalog help
		if {[info exists max_values_($col)]} {
		    add_short_help $min_values_($col) \
			"If set, search for rows where $col is greater than this value"
		    add_short_help $max_values_($col) \
			"If set, search for rows where $col is less than this value"
		} else {
		    add_short_help $min_values_($col) \
			"If set, search for rows with this $col value"
		}
	    } else {
		# add image server help
		if {[info exists max_values_($col)]} {
		    add_short_help $min_values_($col) \
			"If set, fetch an image where $col is greater than this value"
		    add_short_help $max_values_($col) \
			"If set, fetch an image where $col is less than this value"
		} else {
		    add_short_help $min_values_($col) \
			"If set, fetch an image with this $col value"
		}
	    }
	}
    }

    
    # interrupt the current search 

    public method interrupt {} {
	$w_.batch interrupt
    }

    
    # start the catalog search based on the current search options
    # and display the results in the table.

    public method search {args} {
	
	if {$iscat_} {
	    set cmd "$astrocat query"
	} else {
	    set cmd "$astrocat getimage"
	}

	if {[$astrocat iswcs] || [$astrocat ispix]} {
	    set equinox ""
	    if {[$astrocat iswcs]} {
		set name [$name_ get]
		set x [$ra_ get]
		set y [$dec_ get]
		set equinox [$equinox_ get]
	    } elseif {[$astrocat ispix]} {
		set name ""
		set x [$x_ get]
		set y [$y_ get]
	    }

	    if {$iscat_} {
		set rad1 [$rad1_ get]
		set rad2 [$rad2_ get]
	    } else {
		set width [$width_ get]
		set height [$height_ get]
	    }

	    if {"$equinox" != ""} {
		lappend cmd "-equinox" $equinox
	    }
	    if {"$name" != ""} {
		lappend cmd "-nameserver" $namesvr "-name" $name
	    } elseif {"$x" != "" && "$y" != ""} {
		lappend cmd "-pos" [list $x $y]
	    } else {
		#warning_dialog "Please specify either an object name or a position in WCS" $w_
	    }

	    if {$iscat_} {
		if {"$rad1" != "" || "$rad2" != ""} {
		    lappend cmd "-radius" "$rad1 $rad2"
		}
		set maxnum [$maxnum_ get]
		if {"$maxnum" != ""} {
		    lappend cmd "-nrows" $maxnum
		}
		if {"[set sort_cols [$astrocat sortcols]]" != ""} {
		    lappend cmd "-sort" $sort_cols "-sortorder" [$astrocat sortorder] 
		}
	    } else {
		if {"$width" != "" || "$height" != ""} {
		    lappend cmd -width $width -height $height
		}
	    }
	}

	# add optional search columns
	if {"$search_cols_" != ""} {
	    set minvalues {}
	    set maxvalues {}
	    set search_cols {}
	    foreach col $search_cols_ {
		set min [$min_values_($col) get]
		if {[catch {set max [$max_values_($col) get]}]} {
		    # if only one value, compare for equality, otherwise range
		    set max $min
		}
		if {"$min" == "" && "$max" != "" || "$max" == "" && "$min" != ""} {
		    error_dialog "Please specify min and max values for $col"
		    return
		}
		if {"$min" == ""} {
		    continue
		}
		lappend search_cols $col
		lappend minvalues $min
		lappend maxvalues $max
	    }
	    if {[llength $search_cols]} {
		lappend cmd -searchcols $search_cols -minvalues $minvalues -maxvalues $maxvalues
	    }
	}
	
	if {"$itk_option(-feedbackcommand)" != ""} {
	    eval $itk_option(-feedbackcommand) on
	}
	$w_.batch bg_eval [code $this do_query $cmd]
    }

    
    # return the result of a catalog query. Since this may be run in a separate
    # process via fork, all of the necessary info is returned in the format:
    #
    #  {results headings entry querypos more}
    #
    # where:
    #
    #  results are the query results (list of rows)
    #
    #  headings are the result headings
    #
    #  entry is the catalog config entry
    #
    #  querypos is the center pos of the query
    #
    #  more is a flag, 1 if more rows available
    #
    # For image servers, the result of the command is the name of the
    # file containing the image and the headings field is left empty.

    protected method do_query {cmd} {
	if {$iscat_} {
	    return [list [eval $cmd] \
			[$astrocat headings] \
			[$astrocat entry get] \
			[$astrocat querypos] \
			[$astrocat more]]
	} else {
	    return [list [eval $cmd] \
			{}\
			[$astrocat entry get] \
			[$astrocat querypos] \
			0]
	}
    }
    
    
    # This method is called when the background query is done.
    # The arguments are the error status (0 is OK) and the result
    # of evaluating the Tcl commands. 
    #
    # The result arg contains a list of items:
    #
    #   {info headings entry pos more}
    #
    # Where:
    #
    #   info is a list of rows (result of query)
    #
    #   headings are the column headings 
    #
    #   entry is the catalog config entry (including info from the header 
    #   in the query result), 
    #
    #   pos is the query center position (possibly expanded by a name server)
    #
    #   more is a flag indicating if there were more rows available that were
    #   not returned due to the maxrows limit set.

    protected method query_done {status result} {
	if {"$itk_option(-feedbackcommand)" != ""} {
	    eval $itk_option(-feedbackcommand) off
	}
	set cmd $itk_option(-command)
	if {$status} {
	    lappend cmd $result {} {} 0
	} else {
	    lassign $result info_ headings_ entry pos more
		
	    # update the catalog config entry with the entry data from the subprocess
	    # (may have been modified by info in the query result header)
	    $astrocat entry update $entry

	    if {[llength $pos] >= 2} {
		if {[$astrocat iswcs]} {
		    $name_ config -value ""
		    lassign $pos ra dec equinox
		    $ra_ config -value $ra
		    $dec_ config -value $dec
		} elseif {[$astrocat ispix]} {
		    lassign $pos x y
		    $x_ config -value $x
		    $y_ config -value $y
		}
	    }
	    lappend cmd "" $headings_ $info_ $more
	}

	# eval caller's command, if there is one
	if {"$itk_option(-command)" != ""} {
	    eval $cmd
	}
    }


    # Set the default values for the form entries:
    # set the default position (RA, DEC) to the image center, 
    # set the min radius to 0 and the max radius to the distance 
    # from the center to the origin.

    public method set_default_values {} {
	# set the center coords and default radius
	if {[$astrocat iswcs]} {
	    $name_ config -value ""
	    $ra_ config -value ""
	    $dec_ config -value ""
	    $equinox_ config -value 2000
	    if {$iscat_} {
		$rad1_ config -value 0.0
		$rad2_ config -value 10.0
		$maxnum_ config -value 1000
	    } else {
		$width_ config -value 10.0
		$height_ config -value 10.0
	    }
	} elseif {[$astrocat ispix]}  {
	    $x_ config -value ""
	    $y_ config -value ""
	    if {$iscat_} {
		$rad1_ config -value 0.0
		$rad2_ config -value 255.0
		$maxnum_ config -value 1000
	    } else {
		$width_ config -value 255.0
		$height_ config -value 255.0
	    }
	}
    }


    # return the current equinox, or an empty string if the catalog does 
    # not support world coordinates. The user can change the equinox used
    # to display coordinates by typing in a different value in the panel.

    public method get_catalog_equinox {} {
	if {[$astrocat iswcs]} {
	    set equinox [$equinox_ get]
	    if {"$equinox" == ""} {
		set equinox J2000
	    }
	    return $equinox
	}
    }

    
    # set the values for the position and radius entries from the given
    # list, which should be in the format {ra dec equinox radius} if
    # we are using wcs, or {x y radius} for image pixel coords.
    # This method is only used for catalogs (not image servers).

    public method set_pos_radius {list} {
	set n [llength $list]
	if {[$astrocat iswcs] && $n == 4} {
	    # using world coords 
	    lassign $list ra dec equinox radius
	    $ra_ config -value $ra
	    $dec_ config -value $dec
	    $equinox_ config -value $equinox
	    $rad1_ config -value 0.0
	    $rad2_ config -value $radius
	} elseif {[$astrocat ispix] && $n == 3}  {
	    # using image coords
	    lassign $list x y radius
	    $x_ config -value $x
	    $y_ config -value $y
	    $rad1_ config -value 0.0
	    $rad2_ config -value $radius
	}
    }

    # Set the values for the position, width and height entries from the 
    # given list, which should be in the format:
    #    {ra dec equinox width height name} 
    # if we are using wcs, or:
    #    {x y width height} for image pixel coords.
    # Any missing values at the end of the list are treated as blank.
    # This method is only used for image servers.

    public method set_pos_width_height {list} {
	set n [llength $list]
	if {[$astrocat iswcs] || $n == 5} {
	    # using world coords 
	    lassign $list ra dec equinox width height name
	    $ra_ config -value $ra
	    $dec_ config -value $dec
	    $equinox_ config -value $equinox
	    catch {$width_ config -value [format "%.2f" $width]}
	    catch {$height_ config -value [format "%.2f" $height]}
	    $name_ config -value $name
	} elseif {[$astrocat ispix] || $n == 4}  {
	    # using image coords
	    lassign $list x y width height
	    $x_ config -value $x
	    $y_ config -value $y
	    $equinox_ config -value ""
	    catch {$width_ config -value [format "%.2f" $width]}
	    catch {$height_ config -value [format "%.2f" $height]}
	}
    }


    # get the values from the position and radius entries and return
    # a list in the format {ra dec equinox radius} if we are using wcs, 
    # or {x y radius} for image pixel coords.

    public method get_pos_radius {} {
	if {[$astrocat iswcs]} {
	    # using world coords 
	    return [list [$ra_ get] [$dec_ get] [$equinox_ get] [$rad2_ get]]
	} elseif {[$astrocat ispix]}  {
	    # using image coords
	    return [list [$x_ get] [$y_ get] [$rad2_ get]]
	}
    }

    # Return the value in the equinox entry, of 2000 if there isn't one.

    public method get_equinox {} {
	if {[$astrocat iswcs]} {
	    # using world coords 
	    return [$equinox_ get]
	} else  {
	    # using image coords
	    return "2000"
	}
    }



    # -- options --

    # Specify the command to evaluate with the results of a query.
    # The command is evaluated whenever a query is done with the
    # following args:
    #
    #   errmsg - If this is not empty, it contains an error message and
    #   the following args should be ignored. If there were no
    #   errors, this arg is an empty string.
    #
    #   headings - are the column headings 
    #
    #   info - is a list of rows (result of query)
    #
    #   more - is a flag set to 1 if there were more rows available that were
    #   not returned due to the maxrows limit set.
    itk_option define -command command Command {}

    # Specify the command to call to open and close a pipe used to give
    # feedback during HTTP transfers. The command takes 1 arg: "on" or "off".
    itk_option define -feedbackcommand feedbackCommand FeedbackCommand {}

    # Specify the search command to evaluate when the user types <Enter>
    # (Can be used to set button states, etc. Defaults to [code $this search])
    itk_option define -searchcommand searchCommand SearchCommand {} {
	if {"$itk_option(-searchcommand)" == ""} {
	    set itk_option(-searchcommand) [code $this search]
	}
    }

    # font used for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*

    # font used for values
    itk_option define -valuefont valueFont ValueFont -adobe-courier-medium-r-*-*-*-120*

    # font used for ra,dec labels
    itk_option define -wcsfont wcsFont WcsFont -*-symbol-*-*-*-*-14*

    # set the width for  displaying labels
    itk_option define -labelwidth labelWidth LabelWidth 15

    # set the anchor for labels
    itk_option define -anchor anchor Anchor e

    # flag: if true, run queries in foreground for better debugging
    itk_option define -debug debug Debug 0

    # -- public variables --

    # astrocat (C++ based) object for accessing catalogs
    public variable astrocat

    # name server to use to resolve object names
    public variable namesvr {}

    # -- protected variables --

    # blt table for search options
    protected variable search_opts_

    # current row number for $search_opts_
    protected variable search_opts_row_ 0

    # saved value of catalog search_cols_ entry, for comparison
    protected variable search_col_info_ {}

    # widget name for name field
    protected variable name_

    # widget name for equinox field
    protected variable equinox_

    # widget name for RA field
    protected variable ra_

    # widget name for DEC field
    protected variable dec_

    # widget name for X field
    protected variable x_

    # widget name for Y field
    protected variable y_

    # widget name for min radius field
    protected variable rad1_

    # widget name for max radius field
    protected variable rad2_

    # widget name for image width field
    protected variable width_

    # widget name for image height field
    protected variable height_

    # widget name for "max number of objects" field
    protected variable maxnum_

    # list of columns to search by
    protected variable search_cols_ {}

    # array(colName) of widget name for search column min fields
    protected variable min_values_

    # array(colName) of widget name for search column max fields
    protected variable max_values_
    
    # copyright field
    protected variable copyright_

    # flag: true if catalog is not an image server
    protected variable iscat_ 1
}

