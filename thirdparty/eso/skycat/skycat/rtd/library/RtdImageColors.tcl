#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImageColors.tcl,v 1.1.1.1 2006/01/12 16:38:19 abrighto Exp $"
#
# RtdImageColors.tcl - itcl widget for managing colormap for an rtdimage
# 
# See man page RtdImageColors(n) for a complete description.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created
# pbiereic        04/11/03   Workaround bug in tcl 8.4.3 (SourceForge Request ID 835020)

itk::usual RtdImageColors {}

# This [incr Tk] widget presents a user interface for manipulating
# colors and colormaps for an RtdImage widget. The widget creates a new
# toplevel window containing items for "color scaling" the image,
# loading a MIDAS style colormap or ITT (intensity transfer table) and
# for setting the number of color cells allocated in the colormap.

itcl::class rtd::RtdImageColors {
    inherit util::TopLevelWidget

    #  constructor: create a new instance of this class

    constructor {args} {
	wm title $w_ "Image Colors"
	wm iconname $w_ "Image Colors"

	# evaluate the options
	eval itk_initialize $args

	# top frame
	itk_component add top {
	    frame $w_.top -borderwidth 2 -relief groove
	}
	pack $itk_component(top) \
	    -side top -fill both -ipadx 2m  -ipady 2m -expand 1

	# LabelChoice(n) widget for choosing a color scale algorithm
	itk_component add scale {
	    util::LabelChoice $w_.scale \
		-text "Color Scale Algorithm:" \
		-orient vertical \
		-choice {Linear Logarithmic {Square Root} {Histogram Equalization}} \
		-command [code $this set_color_scale]
	}
	pack $itk_component(scale) -side left -fill x -in $w_.top
	$itk_component(scale) config -value Linear

	# choose colormap/ITT
	set cmap_files [lsort [$image_ cmap list]]
	set itt_files [lsort [$image_ itt list]]

	# Chooser(n) widget listing available colormaps
	itk_component add colormaps {
	    util::Chooser $w_.colormaps \
		 -title "Colormap" \
		 -relief groove -borderwidth 2 \
		 -width 0 \
		 -dir $itk_option(-cmap_dir) \
		 -suffix $itk_option(-cmap_suffix) \
		 -files $cmap_files \
		 -default $itk_option(-default_cmap) \
		 -command [code $this set_cmap]	
	}
	
	# Chooser(n) widget listing available intensity tables.
	itk_component add itts {
	    util::Chooser $w_.itts \
		 -title "Intensity" \
		 -relief groove -borderwidth 2 \
		 -width 0 \
		 -dir $itk_option(-cmap_dir) \
		 -suffix $itk_option(-itt_suffix) \
		 -files $itt_files \
		 -default $itk_option(-default_itt) \
		 -command [code $this set_itt]
	}
	pack $itk_component(colormaps) $itk_component(itts) \
	    -side left -fill both -expand 1 -in $itk_component(top)
	    
	# Frame for displaying allocated/free colors
	itk_component add alloc {
	    frame $w_.alloc -borderwidth 2 -relief groove
	}
	pack $itk_component(alloc) -side top -ipadx 2m -ipady 2m -fill x

	# if we are using a private colormap, ignore -max_colors
	if {[$image_ cmap isprivate]} {
	    set itk_option(-max_colors) 128
	}

	# if we are using a read-only colormap, ignore -min and max colors
	if {[$image_ cmap isreadonly]} {
	    set itk_option(-min_free) 0
	    set itk_option(-max_colors) 256
	}

	# LabelEntryScale widget displaying the number of allocated colors
	itk_component add allocated {
	    util::LabelEntryScale $w_.allocated \
		 -text "Allocated Colors" \
		 -valuewidth 4 \
		 -show_arrows 1 \
		 -increment 1 \
		 -from $itk_option(-min_free) \
		 -to $itk_option(-max_colors) \
		 -value $itk_option(-min_free) \
		 -validate numeric
	}
	pack $itk_component(allocated) \
	    -in $itk_component(alloc) -side left -fill x -expand 1

	# LabelValue(n) widget displaying the number of free colors
	itk_component add free {
	    util::LabelValue $w_.free \
		 -text "Free Colors:" \
		 -valuewidth 4 \
		 -value 0
	}
	pack $itk_component(free) \
	    -side left -padx 2m -in $itk_component(alloc)

	update_allocated
	$itk_component(allocated) config -command [code $this set_allocated]

	
	# Frame for buttons
	itk_component add buttons {
	    frame $w_.buttons -borderwidth 2 -relief groove
	}
	pack $itk_component(buttons) -side top -fill x -padx 1m -pady 1m
	
	# Apply button
	itk_component add apply {
	    button $w_.apply \
		 -text "Reallocate" \
		 -command [code $this reallocate]
	}
	
	# Close button
	itk_component add close {
	    button $w_.close \
		 -text "Close" \
		 -command "wm withdraw $w_"
	}

	# Defaults button
	itk_component add defaults {
	    button $w_.defaults \
		 -text "Defaults" \
		 -command "[code $this set_defaults]"
	}
	pack $itk_component(apply) $itk_component(close) $itk_component(defaults)  \
	    -side left -expand 1 -padx 2m -pady 2m -in $w_.buttons
	
	# add short help 
	make_short_help
	
	# Default values may have been changed by the command line args
	set_color_scale [$image_ colorscale]
    }

    
    # set the default colormap and itt
    
    public method set_defaults {} {
	$itk_component(colormaps) set_choice $itk_option(-default_cmap)
	$itk_component(itts) set_choice $itk_option(-default_itt)
	$itk_component(scale) config -value Linear
	set_color_scale Linear
    }


    # if flag is true, use a private colormap, otherwise the default

    public method use_private_colormap {} {
	$image_ cmap private
	reallocate
    }

    
    # add a short help window

    protected method make_short_help {} {
	# TopLevelWidget::make_short_help
	add_short_help $itk_component(scale) \
	    {Color Scaling: {bitmap b1} = apply selected color scaling algorithm}

	add_short_help $itk_component(colormaps) \
	    {MIDAS Colormaps: {bitmap b1} = apply selected color map}

	add_short_help $itk_component(itts) \
	    {Intensity transfer tables: {bitmap b1} = apply selected ITT}

	add_short_help $itk_component(allocated) \
	    {Color allocation: {bitmap dragb1} = use more/less colors (press Reallocate to apply)}

	add_short_help $itk_component(free) \
	    {Free colors: displays number of colors not being used}

	add_short_help $itk_component(apply) \
	    {Reallocate: {bitmap b1} = reallocate colors to use the selected number of colors}

	add_short_help $itk_component(close) {Close: {bitmap b1} = close this window}

	add_short_help $itk_component(defaults) {Defaults: {bitmap b1} = reset to use default values}
    }
    

    # this method is called to set the number of allocated colors
    
    protected method set_allocated {num_colors} {
	set free [expr {$free_ + ($allocated_ - $num_colors)}]
	if {$free >= 0} {
	    set free_ $free
	    set allocated_ $num_colors
	    $itk_component(free) config -value $free
	} else {
	    set free_ 0
	    set allocated_ [expr {$num_colors+$free}]
	    $itk_component(free) config -value $free_
	    $itk_component(allocated) config -value $allocated_
	}
	set free_ [$itk_component(free) get]
	set allocated_ [$itk_component(allocated) get]
    }
 

    # called when the scale value is changed to reallocate the colors
    
    public method reallocate {} {
	busy {
	    set free_ [$itk_component(free) get]
	    set allocated_ [$itk_component(allocated) get]

	    foreach i [array names images_] {
		$images_($i) alloccolors $allocated_
		$i update_colors
	    }
	    update_allocated
	}
    }

    
    # update the display to show the number of free and allocated colors

    public method update_allocated {} {
	update idletasks
	busy {
	    lassign [$image_ alloccolors] allocated_ free_
	    # XXX needed for bug in tcl 8.4.3
	    set bug "$allocated_ $free_"

	    set n $itk_option(-min_free)
	    set to [max $n [expr {($free_+$allocated_)-$n}]]
	    $itk_component(allocated) config \
		-to $to \
		-value $allocated_
	    $itk_component(free) config -value $free_
	}
    }


    # this method is called to set the colormap for the image
    
    public method set_cmap {cmap} {
	$image_ cmap file $cmap
	
	# if the colormap is read-only, we need to regenerate the color ramp
	if {[$image_ cmap isreadonly]} {
	    catch {$itk_option(-image) component colorramp update_colors}
	}
    }

 
    # this method is called to set the itt for the image
    
    public method set_itt {itt} {
	$image_ itt file $itt

	# if the colormap is read-only, we need to regenerate the color ramp
	if {[$image_ cmap isreadonly]} {
	    catch {$itk_option(-image) component colorramp update_colors}
	}
    }
 

    # this method is called to set the color scaling algorithm
    
    public method set_color_scale {alg} {
	# choice variable
	global ::$w_.scale.choice
	set var $w_.scale.choice

	switch $alg {
	    Linear -
	    linear {
		$image_ colorscale linear
		set $var Linear
	    }
	    Logarithmic -
	    log {
		$image_ colorscale log
		set $var Logarithmic
	    }
	    {Square Root} -
	    sqrt {
		$image_ colorscale sqrt
		set $var {Square Root}
	    }
	    {Histogram Equalization} -
	    histeq {
		$image_ colorscale histeq
		set $var {Histogram Equalization}
	    }
	}
    }

    
    # add the given image to the list of images whose colormaps are managed
    # by this class

    public proc add_image {im} {
	set images_($im) [$im get_image]
    }


    # remove the given image from the list of images whose colormaps are managed
    # by this class

    public proc remove_image {im} {
	if {[info exists images_($im)]} {
	    set i $images_($im)
	    unset images_($im)
	    if {"$image_" == "$i"} {
		set i [lindex [array names images_] 0]
		if {"$i" != ""} {
		    set image_ $images_($i)
		}
	    }
	}
    }

    
    # update the display with the values set in the given rtdimage

    public method update_values {im} {
	component colormaps set_choice [file rootname [$im cmap file]]
	component itts set_choice [file rootname [$im itt file]]
	set_color_scale [$im colorscale]
    }


    # -- public vars --

    # name of RtdImage itcl widget, set by caller
    itk_option define -image image Image {} {
 	set image_ [$itk_option(-image) get_image]
   }

    # directory for colormap and ITT files
    itk_option define -cmap_dir cmap_dir Cmap_dir "" {
	if {"$itk_option(-cmap_dir)" == ""} {
	    global ::rtd_library
	    set itk_option(-cmap_dir) "$rtd_library/colormaps"
	}
    }

    # suffix for colormap files
    itk_option define -cmap_suffix cmap_suffix Cmap_suffix {lasc}

    # suffix for ITT files
    itk_option define -itt_suffix itt_suffix Itt_suffix {iasc}

    # max number of colors to allocate
    itk_option define -max_colors max_colors Max_colors 200

    # min number of free colors to leave
    itk_option define -min_free min_free Min_free 5

    # default (midas) colormap
    itk_option define -default_cmap default_cmap Default_cmap {real}

    # default (midas) intensity transfer table
    itk_option define -default_itt default_itt Default_itt {ramp}


    # -- protected vars --
    
    # number of colors allocated
    protected variable allocated_ 0

    # number of free colors
    protected variable free_ 0


    # -- common (shared) variables -- 

    # name of current internal rtdimage object
    protected common image_ {}

    # array (itk RtdImage) of C++ RtdImage objects, for updating clone colors
    protected common images_
}

